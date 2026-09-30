{-# LANGUAGE LambdaCase #-}

-- | Compiling a group of combinators to a native module and installing
-- the result in their cells. See docs/jit-m1.md.
module Unison.Runtime.JIT.Compile
  ( JITState (..),
    compileGroup,
  )
where

import Control.Monad (forM_, when)
import Data.Either (partitionEithers)
import Data.Maybe (isJust)
import Data.Word (Word64)
import Foreign.Ptr (Ptr, castFunPtrToPtr)
import Data.Map qualified as Map
import GHC.Clock (getMonotonicTimeNSec)
import System.FilePath ((</>))
import Unison.Runtime.JIT.Codegen qualified as CG
import Unison.Runtime.JIT.Codegen (CtxOffsets, Function (..), RtsFacts, genFunction, modulePrelude)
import Unison.Runtime.JIT.Config
import Unison.Runtime.JIT.Exits (registerExits)
import Unison.Runtime.JIT.Frames (registerFrames)
import Unison.Runtime.JIT.LLVM
import Unison.Runtime.JIT.Layout (Layouts)
import Unison.Runtime.JIT.Pool (PoolKey (..), poolIndices)
import Unison.Runtime.ANF (PackedTag (..))
import Unison.Reference (Reference)
import Unison.Runtime.MCode
import Unison.Runtime.Machine.Types (MCombs, MSection)
import Unison.Util.EnumContainers qualified as EC
import Data.Bits ((.&.))
import Data.Set qualified as Set


-- | Everything in a section tree, innermost sections included, in order.
sectionsOf :: MSection -> [MSection]
sectionsOf s = s : rest
  where
    rest = case s of
      Let b _ _ bd _ -> sectionsOf b ++ sectionsOf bd
      Ins _ nx -> sectionsOf nx
      Match _ bs -> goB bs
      DMatch _ _ bs -> goB bs
      NMatch _ _ bs -> goB bs
      RMatch _ p bs -> sectionsOf p ++ concatMap goB (map snd (EC.mapToList bs))
      _ -> []
    goB = \case
      Test1 _ a d -> sectionsOf a ++ sectionsOf d
      Test2 _ a _ b d -> sectionsOf a ++ sectionsOf b ++ sectionsOf d
      TestW d m -> sectionsOf d ++ concatMap sectionsOf (map snd (EC.mapToList m))
      TestT d m -> sectionsOf d ++ concatMap sectionsOf (Map.elems m)
      TestY d m -> sectionsOf d ++ concatMap sectionsOf (Map.elems m)

-- | The cells carried by every Let in a section tree.
letCellsOf :: MSection -> [Ptr NativeCell]
letCellsOf s = [c | Let _ _ _ _ c <- sectionsOf s]

-- | The constants a section tree needs from the pool.
poolKeysOf :: MSection -> [PoolKey]
poolKeysOf s = concat [keys i | Ins i _ <- sectionsOf s]
  where
    keys = \case
      Pack r t ZArgs -> [KeyEnum r t]
      Pack r _ _ -> [KeyEnum r (PackedTag 0)]
      Lit l@(MT _) -> [KeyLit l]
      Lit l@(MM _) -> [KeyLit l]
      Lit l@(MY _) -> [KeyLit l]
      _ -> []

-- | What compilation needs, found once at startup.
data JITState = JITState
  { jsLayouts :: Layouts,
    jsCtx :: CtxOffsets,
    jsRts :: RtsFacts
  }

-- | Compiles the combinators of one top-level definition as one module.
-- Combinators the code generator can't handle are left interpreted.
compileGroup :: JITState -> Map.Map Reference [Int] -> Reference -> Word64 -> MCombs -> IO ()
compileGroup st types ref grp combs = do
  t0 <- getMonotonicTimeNSec
  poolIxs <- poolIndices (concat [poolKeysOf s | Comb (LamI _ _ s _) <- map snd (EC.mapToList combs)])
  let env (base, fbase) = CG.Env (jsLayouts st) (jsCtx st) base fbase (stressPoll config > 0) (stressCallee config > 0) combs poolIxs (jsRts st) types
      -- Let body combinators are only entered through the cell their Let
      -- carries, so those that no Let refers to (the ones inside bindings)
      -- are not worth compiling. Local functions have zero in the low bits.
      letCells = Set.fromList [c | Comb (LamI _ _ s _) <- map snd (EC.mapToList combs), c <- letCellsOf s, c /= noNativeCell]
      candidates =
        [ (i, cix, a, f, entry, cell)
          | (i, Comb (LamI a f entry cell)) <- EC.mapToList combs,
            i .&. 0xFFFF == 0 || Set.member cell letCells,
            let cix = CIx ref grp i
        ]
      name i = "u" ++ show grp ++ "_" ++ show i
      gen base (i, cix, a, f, entry, cell) = genFunction (env base) (name i) cix a f entry cell
      -- first pass: find out which functions compile and how many exits each has
      (skipped, firstPass) = partitionEithers [either (Left . (,) i) Right (gen (0, 0) c) | c@(i, _, _, _, _, _) <- candidates]
  forM_ skipped $ \(i, why) -> jitLog (name i ++ ": not compiled: " ++ why)
  when (not (null firstPass)) $ do
    let counts = map (length . fnExits) firstPass
        fcounts = map (length . fnFrames) firstPass
    base <- registerExits (concatMap fnExits firstPass)
    fbase <- registerFrames (concatMap fnFrames firstPass)
    -- second pass, with each function's real exit and frame bases
    let bases = zip (scanl (+) base counts) (scanl (+) fbase fcounts)
        compiled = [f | (b, c) <- zip bases (filter ((`notElem` map fst skipped) . sel1) candidates), Right f <- [gen b c]]
        sel1 (i, _, _, _, _, _) = i
        ir = modulePrelude ++ unlines (map fnIR compiled)
        modName = "unison_" ++ show grp
    -- The dump has each function's MCode as a comment above its IR, and
    -- says which combinators were not compiled and why.
    forM_ (dumpIR config) $ \dir -> do
      let mcodeOf i = case EC.lookup i combs of
            Just (Comb (LamI a f s _)) -> ";   arity " ++ show a ++ ", frame size " ++ show f ++ "\n" ++ unlines (map ("; " ++) (lines (prettySection 4 s "")))
            _ -> ""
          annotated =
            unlines $
              [ "; module " ++ modName ++ " for " ++ show ref,
                "; " ++ show (length compiled) ++ " of " ++ show (length candidates) ++ " combinators compiled"
              ]
                ++ [ "; " ++ name i ++ " not compiled: " ++ why | (i, why) <- skipped ]
                ++ [modulePrelude]
                ++ concat
                  [ ["; " ++ fnName f ++ " = " ++ show (CIx ref grp i), mcodeOf i, fnIR f]
                    | f <- compiled,
                      let i = read (drop 1 (dropWhile (/= '_') (fnName f)))
                  ]
      if dir == "-"
        then jitDump annotated
        else writeFile (dir </> modName ++ ".ll") annotated
    r <- addModule (isJust (dumpIR config)) "default<O2>" ir
    case r of
      Left e -> jitLog (modName ++ ": " ++ e)
      Right optimized -> do
        -- the module after O2: what actually runs
        forM_ ((,) <$> dumpIR config <*> optimized) $ \(dir, txt) ->
          if dir == "-"
            then jitDump ("; module " ++ modName ++ " after O2\n" ++ txt)
            else writeFile (dir </> modName ++ ".opt.ll") txt
        forM_ compiled $ \f -> do
          sym <- lookupSymbol (fnName f)
          case sym of
            Left e -> jitLog (fnName f ++ ": " ++ e)
            Right fp -> writeNativeCode (fnCell f) (castFunPtrToPtr fp)
        t1 <- getMonotonicTimeNSec
        jitLog
          ( modName ++ ": compiled " ++ show (length compiled) ++ " of " ++ show (length candidates)
              ++ " combinators, " ++ show (sum counts) ++ " exits, in "
              ++ show (fromIntegral (t1 - t0) / 1e6 :: Double) ++ " ms"
          )

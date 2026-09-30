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
import Data.Word (Word64)
import Foreign.Ptr (castFunPtrToPtr)
import GHC.Clock (getMonotonicTimeNSec)
import System.FilePath ((</>))
import Unison.Runtime.JIT.Codegen qualified as CG
import Unison.Runtime.JIT.Codegen (CtxOffsets, Function (..), genFunction)
import Unison.Runtime.JIT.Config
import Unison.Runtime.JIT.Exits (registerExits)
import Unison.Runtime.JIT.LLVM
import Unison.Runtime.JIT.Layout (Layouts)
import Unison.Reference (Reference)
import Unison.Runtime.MCode
import Unison.Runtime.Machine.Types (MCombs)
import Unison.Util.EnumContainers qualified as EC

-- | What compilation needs, found once at startup.
data JITState = JITState
  { jsLayouts :: Layouts,
    jsCtx :: CtxOffsets
  }

-- | Compiles the combinators of one top-level definition as one module.
-- Combinators the code generator can't handle are left interpreted.
compileGroup :: JITState -> Reference -> Word64 -> MCombs -> IO ()
compileGroup st ref grp combs = do
  t0 <- getMonotonicTimeNSec
  let env base = CG.Env (jsLayouts st) (jsCtx st) base (stressPoll config > 0)
      candidates =
        [ (i, cix, a, f, entry, cell)
          | (i, Comb (LamI a f entry cell)) <- EC.mapToList combs,
            let cix = CIx ref grp i
        ]
      name i = "u" ++ show grp ++ "_" ++ show i
      gen base (i, cix, a, f, entry, cell) = genFunction (env base) (name i) cix a f entry cell
      -- first pass: find out which functions compile and how many exits each has
      (skipped, firstPass) = partitionEithers [either (Left . (,) i) Right (gen 0 c) | c@(i, _, _, _, _, _) <- candidates]
  forM_ skipped $ \(i, why) -> jitLog (name i ++ ": not compiled: " ++ why)
  when (not (null firstPass)) $ do
    let counts = map (length . fnExits) firstPass
    base <- registerExits (concatMap fnExits firstPass)
    -- second pass, with each function's real exit base
    let bases = scanl (+) base counts
        compiled = [f | (b, c) <- zip bases (filter ((`notElem` map fst skipped) . sel1) candidates), Right f <- [gen b c]]
        sel1 (i, _, _, _, _, _) = i
        ir = unlines (map fnIR compiled)
        modName = "unison_" ++ show grp
    forM_ (dumpIR config) $ \dir -> writeFile (dir </> modName ++ ".ll") ir
    r <- addModule "default<O2>" ir
    case r of
      Left e -> jitLog (modName ++ ": " ++ e)
      Right () -> do
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

-- | The JIT's entry points for the rest of the runtime. See docs/jit-design.md.
module Unison.Runtime.JIT
  ( startJIT,
    jitCompileGroup,
    printJITStats,
  )
where

import Control.Monad (forM_, when)
import Data.IORef
import Data.List (sortOn)
import Data.Word (Word64)
import System.IO.Unsafe (unsafePerformIO)
import Unison.Reference (Reference)
import Unison.Runtime.JIT.Codegen (CtxOffsets (..))
import Unison.Runtime.JIT.Compile
import Unison.Runtime.JIT.Config
import Unison.Runtime.JIT.Exits
import Unison.Runtime.JIT.LLVM
import Unison.Runtime.JIT.Layout (probeLayouts)
import Unison.Runtime.JIT.Native (configureNative, ctxLayout)
import Unison.Runtime.MCode (prettySection)
import Unison.Runtime.Machine.Types (MCombs)

-- | Set once by 'startJIT' when everything checks out. Nothing means off.
jitState :: IORef (Maybe JITState)
jitState = unsafePerformIO (newIORef Nothing)
{-# NOINLINE jitState #-}

-- | Starts LLVM if the JIT is enabled. Called once when the runtime starts.
-- If anything is wrong the JIT stays off, with a message.
startJIT :: IO ()
startJIT = case mode config of
  Off -> pure ()
  Eager -> do
    r <- initLLVM
    case r of
      Left e -> jitLog (e ++ "; the JIT is off")
      Right () -> do
        layouts <- probeLayouts
        (offs, _) <- ctxLayout
        case (layouts, offs) of
          (Left e, _) -> jitLog (e ++ "; the JIT is off")
          (Right ls, [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q]) -> do
            configureNative (stressPoll config) (stressCallee config) (stressCStack config) (trace config)
            writeIORef jitState (Just (JITState ls (CtxOffsets a b c d e f g h i j k l m n o p q)))
            triple <- targetTriple
            jitLog ("mode " ++ show (mode config) ++ ", LLVM ready, target " ++ triple)
          _ -> jitLog "unexpected Ctx layout; the JIT is off"

-- | Compiles a freshly loaded top-level definition, in eager mode.
jitCompileGroup :: Reference -> Word64 -> MCombs -> IO ()
jitCompileGroup ref grp combs =
  readIORef jitState >>= \case
    Nothing -> pure ()
    Just st -> compileGroup st ref grp combs

-- | Prints how often each exit was taken, when UNISON_JIT_STATS is set.
-- Meant to be called when the runtime shuts down.
printJITStats :: IO ()
printJITStats = when (stats config) $ do
  counts <- exitCounts
  jitDump ("[jit] exits taken (" ++ show (length counts) ++ " sites):")
  forM_ (sortOn (\(_, _, n) -> negate n) counts) $ \(i, e, n) ->
    jitDump ("  " ++ show n ++ "\t#" ++ show i ++ "\t" ++ describe e)
  where
    describe = \case
      Resume cix sect -> "resume " ++ show cix ++ " at " ++ takeWhile (/= '\n') (dropWhile (== ' ') (prettySection 0 sect ""))
      GrowStack n _ -> "grow stack by " ++ show n
      Reenter _ -> "reenter (poll)"
      Named name e -> name ++ ": " ++ describe e

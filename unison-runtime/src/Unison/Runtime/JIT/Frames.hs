-- | The global frame table. A native caller whose callee exits writes a
-- frame record naming an entry here; the trampoline turns the record into
-- the @Push@ frame the interpreter would have pushed. See docs/jit-design.md,
-- "On dynamically constructing K frames".
module Unison.Runtime.JIT.Frames
  ( Frame (..),
    FrameIndex,
    registerFrames,
    replaceFrames,
    lookupFrame,
  )
where

import Data.IORef
import Data.IntMap.Strict qualified as IM
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)
import Unison.Runtime.MCode (CombIx, NativeCell)
import Unison.Runtime.Machine.Types (MSection)

-- | The fixed parts of a @Push@ frame for one @Let@: the body's @CombIx@,
-- stack guard and section, and the cell holding the body's native code.
data Frame = Frame !CombIx !Int !MSection !(Ptr NativeCell)

-- | 1-based.
type FrameIndex = Int

data Table = Table !Int !(IM.IntMap Frame)

table :: IORef Table
table = unsafePerformIO (newIORef (Table 1 IM.empty))
{-# NOINLINE table #-}

-- | Adds a module's frames and returns the index of the first one. The
-- rest follow consecutively, in the order given.
registerFrames :: [Frame] -> IO FrameIndex
registerFrames frames = atomicModifyIORef' table $ \(Table next m) ->
  let n = length frames
   in (Table (next + n) (m <> IM.fromList (zip [next ..] frames)), next)

-- | Overwrites frames registered earlier, starting at the given index
-- (see replaceExits).
replaceFrames :: FrameIndex -> [Frame] -> IO ()
replaceFrames base frames = atomicModifyIORef' table $ \(Table next m) ->
  (Table next (IM.fromList (zip [base ..] frames) <> m), ())

lookupFrame :: FrameIndex -> IO Frame
lookupFrame i = do
  Table _ m <- readIORef table
  case IM.lookup i m of
    Just f -> pure f
    Nothing -> error ("JIT: unknown frame index " ++ show i)

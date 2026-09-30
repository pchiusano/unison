-- | The global exits table. Native code returns an index into it to say
-- what the interpreter should do next. See docs/jit-design.md.
module Unison.Runtime.JIT.Exits
  ( Exit (..),
    ExitIndex,
    registerExits,
    replaceExits,
    lookupExit,
    countExit,
    exitCounts,
  )
where

import Data.IORef
import Data.IntMap.Strict qualified as IM
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Ptr (Ptr)
import Unison.Runtime.MCode (CombIx, NativeCell)
import Unison.Runtime.Machine.Types (MInstr, MSection)

data Exit
  = -- | interpret from this section of this combinator
    Resume !CombIx !MSection
  | -- | grow the Unison stack to fit a frame of this size, then call the
    -- function whose cell this is again (it may not be the one the
    -- trampoline entered: tail calls change functions)
    GrowStack !Int !(Ptr NativeCell)
  | -- | the runtime asked the thread to stop; call the function whose
    -- cell this is again when the thread next runs
    Reenter !(Ptr NativeCell)
  | -- | run this instruction with the interpreter (it pushes the given
    -- number of values), then call the function in this cell, which
    -- continues with the section after it. If the instruction pushed a
    -- different number of values, interpret that section instead.
    CallOut !CombIx !MInstr !MSection !Int !(Ptr NativeCell)
  | -- | a description, for statistics
    Named String Exit

-- | 1-based; 0 is OK and negative values are errors.
type ExitIndex = Int

data Table = Table !Int !(IM.IntMap Exit) !(IM.IntMap Int) -- next index, exits, counts

table :: IORef Table
table = unsafePerformIO (newIORef (Table 1 IM.empty IM.empty))
{-# NOINLINE table #-}

-- | Adds a module's exits and returns the index of the first one. The
-- rest follow consecutively, in the order given.
registerExits :: [Exit] -> IO ExitIndex
registerExits exits = atomicModifyIORef' table $ \(Table next m c) ->
  let n = length exits
   in (Table (next + n) (m <> IM.fromList (zip [next ..] exits)) c, next)

-- | Overwrites exits registered earlier, starting at the given index: the
-- second code generation pass produces the final ones (with the cells of
-- the auxiliary functions), the first only their number.
replaceExits :: ExitIndex -> [Exit] -> IO ()
replaceExits base exits = atomicModifyIORef' table $ \(Table next m c) ->
  (Table next (IM.fromList (zip [base ..] exits) <> m) c, ())

lookupExit :: ExitIndex -> IO Exit
lookupExit i = do
  Table _ m _ <- readIORef table
  case IM.lookup i m of
    Just e -> pure e
    Nothing -> error ("JIT: unknown exit index " ++ show i)

-- | Counts an exit; gives the total taken so far.
countExit :: ExitIndex -> IO Int
countExit i = atomicModifyIORef' table $ \(Table n m c) ->
  let c' = IM.insertWith (+) i 1 c in (Table n m c', sum (IM.elems c'))

-- | Every exit that has been taken, with its count.
exitCounts :: IO [(ExitIndex, Exit, Int)]
exitCounts = do
  Table _ m c <- readIORef table
  pure [(i, e, n) | (i, n) <- IM.toList c, Just e <- [IM.lookup i m]]

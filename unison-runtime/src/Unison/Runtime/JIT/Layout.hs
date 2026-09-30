{-# LANGUAGE BangPatterns #-}

-- | Closure layouts, found by probing sample closures at startup
-- (decision D7 in docs/jit-implementation-plan.md). Generated code
-- uses these as constants. If a layout isn't what the code generator
-- expects, the JIT is turned off.
module Unison.Runtime.JIT.Layout
  ( Layout (..),
    Layouts (..),
    probeLayouts,
  )
where

import Data.Primitive.Array (MutableArray, newArray, writeArray)
import GHC.Exts (Any, RealWorld)
import Unison.Runtime.ANF (PackedTag (..))
import Unison.Runtime.JIT.Native (probeClosure)
import Unison.Runtime.Stack
import Unison.Type qualified as Ty
import Unsafe.Coerce (unsafeCoerce)

data Layout = Layout
  { -- | info pointer, to write into new closures of this kind
    lInfo :: !Int,
    -- | the low bits of a pointer to this constructor
    lPtrTag :: !Int,
    -- | pointer fields come first in the payload; this is how many
    lPtrs :: !Int,
    lNptrs :: !Int,
    -- | byte offset from the untagged closure address to payload word i
    lFieldOffset :: Int -> Int
  }

data Layouts = Layouts
  { -- | @Enum ref tag@: the tag word is the first non-pointer field
    lEnum :: !Layout,
    -- | @Data1 ref tag (Val u b)@
    lData1 :: !Layout,
    -- | @Data2 ref tag (Val u b) (Val u b)@
    lData2 :: !Layout,
    -- | header size in bytes; payload starts after it
    lHeaderBytes :: !Int
  }

any' :: a -> Any
any' = unsafeCoerce

probe :: Any -> [Any] -> IO (Either String Layout)
probe sample0 fields = do
  -- Force everything here: the array must hold the evaluated objects, not
  -- thunks that produce them. (Without optimization the arguments are thunks.)
  let !sample = sample0
  arr <- newArray (1 + length fields) sample :: IO (MutableArray RealWorld Any)
  mapM_ (\(i, f0) -> let !f = f0 in writeArray arr i f) (zip [1 ..] fields)
  r <- probeClosure arr
  case r of
    (info : tag : _ty : ptrs : nptrs : _con : rest) -> do
      let total = ptrs + nptrs
          matches = take ptrs (drop total rest)
      -- every pointer field must be one of the objects we put there, in order
      if matches == [1 .. ptrs] && length fields == ptrs
        then pure (Right (Layout info tag ptrs nptrs (\i -> 8 + 8 * i)))
        else pure (Left ("probe: " ++ show r))
    _ -> pure (Left ("probe: " ++ show r))

-- | Probes the layouts the code generator relies on. Returns an
-- explanation if anything is unexpected.
probeLayouts :: IO (Either String Layouts)
probeLayouts = do
  let !ref = Ty.booleanRef
      !nat = natTypeTag
      !nil = Enum ref (PackedTag 0)
  e <- probe (any' $! Enum ref (PackedTag 7)) [any' ref]
  d1 <- probe (any' $! Data1 ref (PackedTag 7) (Val 9 nat)) [any' ref, any' nat]
  d2 <- probe (any' $! Data2 ref (PackedTag 7) (Val 9 nat) (Val 10 nil)) [any' ref, any' nat, any' nil]
  pure $ case (e, d1, d2) of
    (Right le, Right ld1, Right ld2)
      | lPtrTag le == 2 && lPtrs le == 1 && lNptrs le == 1,
        lPtrTag ld1 == 3 && lPtrs ld1 == 2 && lNptrs ld1 == 2,
        lPtrTag ld2 == 4 && lPtrs ld2 == 3 && lNptrs ld2 == 3 ->
          Right (Layouts le ld1 ld2 8)
      | otherwise -> Left ("closure layouts are not what the code generator expects: " ++ unwords (map summary [le, ld1, ld2]))
    _ -> Left ("closure layouts could not be probed: " ++ show [either id (const "ok") x | x <- [e, d1, d2]])
  where
    summary l = show (lPtrTag l, lPtrs l, lNptrs l)

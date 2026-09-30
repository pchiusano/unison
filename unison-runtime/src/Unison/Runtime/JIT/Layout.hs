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

import Data.Primitive.Array (MutableArray, arrayFromList, newArray, writeArray)
import Data.IORef (newIORef)
import Data.Primitive.ByteArray (byteArrayFromList)
import Foreign.Ptr (IntPtr (..), ptrToIntPtr)
import GHC.Exts (Any, RealWorld)
import Unison.Runtime.ANF (PackedTag (..))
import Unison.Runtime.JIT.Native (probeClosure)
import Unison.Runtime.MCode (CombIx (..), GCombInfo (..), GSection (..), noNativeCell)
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
    -- | @DataG ref tag (useg, bseg)@: pointers ref, useg, bseg; one non-pointer, the tag
    lDataG :: !Layout,
    -- | @PAp (CIx ref grp w) (LamI arity fsize entry cell) (useg, bseg)@, with
    -- CombIx and GCombInfo unpacked: pointers ref, entry, useg, bseg;
    -- non-pointers grp, w, arity, fsize, cell
    lPAp :: !Layout,
    -- | info pointers of the lifted boxes around a Seg's arrays
    lByteArrayBoxInfo, lArrayBoxInfo :: !Int,
    -- | @Foreign x@: pointer tag 7, one pointer, the Foreign
    lForeignInfo :: !Int,
    -- | @WrapIORef ref@: pointer tag 7, one pointer, the MutVar#
    lIORefInfo :: !Int,
    -- | @WrapMutableArray arr@: pointer tag 7, one pointer, the MutableArray#
    lMutableArrayInfo :: !Int,
    -- | @Val u b@: pointer tag 1; pointer b, then non-pointer u
    lVal :: !Layout,
    -- | header size in bytes; payload starts after it
    lHeaderBytes :: !Int
  }

any' :: a -> Any
any' = unsafeCoerce

probe :: Any -> [Any] -> [Int] -> IO (Either String Layout)
probe sample0 fields planted = do
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
          words = take nptrs (drop ptrs rest)
      -- every pointer field must be one of the objects we put there, in
      -- order, and the non-pointer words must be the values planted, in order
      if matches == [1 .. ptrs] && length fields == ptrs && words == planted
        then pure (Right (Layout info tag ptrs nptrs (\i -> 8 + 8 * i)))
        else pure (Left ("probe: " ++ show r))
    _ -> pure (Left ("probe: " ++ show r))

-- | The info pointer of a box with one pointer field and pointer tag 1.
probeBox :: Any -> IO (Either String Int)
probeBox = probeInfo 1

-- | The info pointer of a constructor with the given pointer tag and one
-- pointer field.
probeInfo :: Int -> Any -> IO (Either String Int)
probeInfo tag sample0 = do
  let !sample = sample0
  arr <- newArray 1 sample :: IO (MutableArray RealWorld Any)
  r <- probeClosure arr
  pure $ case r of
    (info : t : _ty : 1 : 0 : _) | t == tag -> Right info
    _ -> Left ("info probe: " ++ show r)

-- | Probes the layouts the code generator relies on. Returns an
-- explanation if anything is unexpected.
probeLayouts :: IO (Either String Layouts)
probeLayouts = do
  let !ref = Ty.booleanRef
      !nat = natTypeTag
      !nil = Enum ref (PackedTag 0)
      !useg = byteArrayFromList [11 :: Int, 12]
      !bseg = arrayFromList [nat, nil]
  e <- probe (any' $! Enum ref (PackedTag 7)) [any' ref] [7]
  d1 <- probe (any' $! Data1 ref (PackedTag 7) (Val 9 nat)) [any' ref, any' nat] [7, 9]
  d2 <- probe (any' $! Data2 ref (PackedTag 7) (Val 9 nat) (Val 10 nil)) [any' ref, any' nat, any' nil] [7, 9, 10]
  dg <- probe (any' $! DataG ref (PackedTag 7) (useg, bseg)) [any' ref, any' useg, any' bseg] [7]
  let !entry = Exit
      !cell = noNativeCell
      IntPtr cellAddr = ptrToIntPtr cell
  pap <- probe (any' $! PAp (CIx ref 21 22) (LamI 23 24 entry cell) (useg, bseg)) [any' ref, any' entry, any' useg, any' bseg] [21, 22, 23, 24, cellAddr]
  -- the boxes: one pointer field each (the unlifted array), tag 1
  ub <- probeBox (any' useg)
  bb <- probeBox (any' bseg)
  -- a Ref: Foreign (WrapIORef ref), the IORef unpacked to its MutVar#
  ref' <- newIORef (Val 9 nat)
  let !w = WrapIORef ref'
      !fo = Foreign w
  fi <- probeInfo 7 (any' fo)
  wi <- probeInfo 7 (any' w)
  marr <- newArray 1 (Val 9 nat) :: IO (MutableArray RealWorld Val)
  let !wa = WrapMutableArray marr
  ai <- probeInfo 7 (any' wa)
  v <- probe (any' $! Val 9 nat) [any' nat] [9]
  pure $ case (e, d1, d2, dg, pap, ub, bb, fi, wi, ai, v) of
    (Right le, Right ld1, Right ld2, Right ldg, Right lp, Right ubi, Right bbi, Right fii, Right wii, Right aii, Right lv)
      | lPtrTag le == 2 && lPtrs le == 1 && lNptrs le == 1,
        lPtrTag ld1 == 3 && lPtrs ld1 == 2 && lNptrs ld1 == 2,
        lPtrTag ld2 == 4 && lPtrs ld2 == 3 && lNptrs ld2 == 3,
        lPtrTag ldg == 5 && lPtrs ldg == 3 && lNptrs ldg == 1,
        lPtrTag lp == 1 && lPtrs lp == 4 && lNptrs lp == 5,
        lPtrTag lv == 1 && lPtrs lv == 1 && lNptrs lv == 1 ->
          Right (Layouts le ld1 ld2 ldg lp ubi bbi fii wii aii lv 8)
      | otherwise -> Left ("closure layouts are not what the code generator expects: " ++ unwords (map summary [le, ld1, ld2, ldg, lp, lv]))
    _ -> Left ("closure layouts could not be probed: " ++ show ([either id (const "ok") x | x <- [e, d1, d2, dg, pap, v]] ++ [either id (const "ok") x | x <- [ub, bb, fi, wi, ai]]))
  where
    summary l = show (lPtrTag l, lPtrs l, lNptrs l)

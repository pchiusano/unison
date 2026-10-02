{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

-- | Closure layouts, found by probing sample closures at startup
-- (decision D7 in docs/jit-implementation-plan.md). Generated code
-- uses these as constants. If a layout isn't what the code generator
-- expects, the JIT is turned off.
module Unison.Runtime.JIT.Layout
  ( Layout (..),
    Layouts (..),
    probeLayouts,
    probeLists,
    probeTexts,
    probeNames,
  )
where

import Control.Monad (foldM, forM)
import Data.Maybe (catMaybes)
import Data.Bits (shiftR, xor, (.&.), (.|.))
import Data.Primitive.Array (MutableArray, arrayFromList, newArray, readArray, writeArray)
import Data.IORef (newIORef)
import Data.Primitive.ByteArray (byteArrayFromList)
import Foreign.Ptr (IntPtr (..), ptrToIntPtr)
import GHC.Exts (Any, RealWorld)
import Unison.Runtime.ANF (PackedTag (..))
import Unison.Builtin.Decls qualified as Ty (optionalRef, seqViewRef)
import Unison.Runtime.JIT.Native (closureInit, listCheck, listInit, listTest, nameTest, probeClosure, textCheck, textInit, textTest)
import Unison.Runtime.MCode (CombIx (..), GCombInfo (..), GSection (..), noNativeCell)
import Unison.Runtime.Stack
import Unison.Runtime.TypeTags qualified as TT
import Unison.Type qualified as Ty
import Unison.Util.Deque2 qualified as Sq
import Unison.Util.Text qualified as UText
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

-- | Lists: teaches the C helpers (jit_rt.c) the constructors of
-- Unison.Util.Deque2 from samples, checks the structure of a range of lists
-- against what the helpers assume, and then runs every helper against the
-- Haskell operation it stands in for; each list a helper builds is checked
-- for its structure too. Returns an explanation if anything differs.
--
-- @steps@ (stress mode @lists=N@) adds a longer test: that many random
-- operations, each done by a helper on the results of earlier ones and
-- compared with the Haskell operation.
probeLists :: Layouts -> Int -> IO (Either String ())
probeLists ls steps = do
  let nat i = NatVal (fromIntegral (i :: Int))
      wrap :: Sq.Deque Val -> Any
      wrap d = any' $! Foreign (WrapSeq d)
      !x = nat 1000
      !sample = x Sq.<| (Sq.empty Sq.|> nat 1 Sq.|> nat 2 Sq.|> nat 3)
      PackedTag elemTag = TT.seqViewElemTag
      PackedTag someTag = TT.someTag
      !emptyView = Enum Ty.seqViewRef TT.seqViewEmptyTag
      !none = Enum Ty.optionalRef TT.noneTag
      range a b = Sq.fromList (map nat [a .. b])
      -- lists of many shapes: built from either end, appended, cut, and drained
      built n = [range 1 n, foldr (Sq.<|) Sq.empty (map nat [1 .. n])]
      samples =
        concatMap built ([0 .. 12] ++ [63, 64, 65, 100, 200, 513, 1000, 3000])
          ++ [range 1 12000]
          ++ [range 1 a Sq.>< range 1 b | (a, b) <- [(9, 10), (33, 300), (150, 47), (1200, 2500)]]
          ++ [Sq.drop k (range 1 n) | (n, k) <- [(100, 7), (1000, 60), (5000, 1), (5000, 97)]]
          ++ [Sq.take k (range 1 n) | (n, k) <- [(100, 50), (1000, 93), (5000, 3)]]
          ++ take 10 (iterate (\d -> case d of _ Sq.:<| r -> r; r -> r) (range 1 300))
          ++ take 10 (iterate (\d -> case d of r Sq.:|> _ -> r; r -> r) (range 1 300))
  -- the array must hold the evaluated objects, never thunks that produce them
  let put :: MutableArray RealWorld Any -> Int -> Any -> IO ()
      put a i v = v `seq` writeArray a i v
      !first = wrap sample
  arr <- newArray 4 first :: IO (MutableArray RealWorld Any)
  put arr 1 (any' x)
  put arr 2 (wrap Sq.empty)
  put arr 3 (wrap (range 1 1000))
  ok <- listInit arr [lForeignInfo ls, lInfo (lVal ls), lInfo (lData1 ls), lInfo (lData2 ls)]
  if ok /= 1
    then pure (Left ("the list constructors are not laid out as the JIT's helpers expect (check " ++ show ok ++ ")"))
    else do
      let same a b = BoxedVal a == BoxedVal b
          listOf c = case c of
            Foreign (WrapSeq d) -> Just d
            _ -> Nothing
          -- a helper on the list d, with its other arguments; its result
          run :: Sq.Deque Val -> Int -> Any -> Int -> Int -> IO (Maybe Closure)
          run d op other a b = do
            put arr 0 (wrap d)
            put arr 1 other
            h <- listTest arr op a b
            if h then Just . unsafeCoerce <$> readArray arr 3 else pure Nothing
          -- a few positions of a list of n elements: all, if it is short
          spread n
            | n <= 40 = [0 .. n - 1]
            | otherwise = [0 .. 11] ++ [(n * k) `div` 23 | k <- [1 .. 22]] ++ [n - 12 .. n - 1]
          agree full d e
            | Sq.length d /= Sq.length e = False
            -- (the C walk has checked the structure; the Haskell check of it is for the longer test)
            | full = (steps == 0 || either (const False) (const True) (Sq.valid d)) && d == e
            | otherwise = all (\i -> Sq.lookup i d == Sq.lookup i e) (spread (Sq.length e))
          -- What a helper built against the list it should be: Nothing if
          -- they differ, else the helper's list. Checking every element is
          -- for lists that aren't long (full).
          checked :: Bool -> Sq.Deque Val -> Maybe Closure -> IO (Maybe (Sq.Deque Val))
          checked full expected = \case
            Just c | Just d <- listOf c -> do
              put arr 0 (any' c)
              k <- listCheck arr
              pure (if k >= 0 && agree full d expected then Just d else Nothing)
            _ -> pure Nothing
          push full front v d =
            run d (if front then 2 else 3) (any' natTypeTag) v 0
              >>= checked full (if front then nat v Sq.<| d else d Sq.|> nat v)
          view full left d = do
            r <- run d (if left then 0 else 1) (any' emptyView) (fromIntegral elemTag) 0
            case (r, left, d) of
              (Just c, _, Sq.Empty) -> pure (if same c emptyView then Just d else Nothing)
              (Just (Data2 rf t a (BoxedVal c)), True, e Sq.:<| rest)
                | rf == Ty.seqViewRef, t == TT.seqViewElemTag, a == e -> checked full rest (Just c)
              (Just (Data2 rf t (BoxedVal c) b), False, rest Sq.:|> e)
                | rf == Ty.seqViewRef, t == TT.seqViewElemTag, b == e -> checked full rest (Just c)
              _ -> pure Nothing
          -- a negative count is a Nat too large to be a size
          cut full tk k d =
            run d (if tk then 5 else 6) (any' none) k 0
              >>= checked full (if tk then (if k < 0 then d else Sq.take k d) else (if k < 0 then Sq.empty else Sq.drop k d))
          split full left k d = do
            r <- run d (if left then 7 else 8) (any' emptyView) k (fromIntegral elemTag)
            let n = Sq.length d
                (ea, eb) = Sq.splitAt (if left then k else n - k) d
            case r of
              Just c | n < k -> pure (if same c emptyView then Just (d, d) else Nothing)
              Just (Data2 rf t (BoxedVal a) (BoxedVal b)) | rf == Ty.seqViewRef, t == TT.seqViewElemTag -> do
                ra <- checked full ea (Just a)
                rb <- checked full eb (Just b)
                pure ((,) <$> ra <*> rb)
              _ -> pure Nothing
          cat full a b = run a 9 (wrap b) 0 0 >>= checked full (a Sq.>< b)
          lit k v = run Sq.empty 10 (any' natTypeTag) k v >>= checked True (Sq.fromList (map nat [v .. v + k - 1]))
          at d i = do
            r <- run d 4 (any' none) i (fromIntegral someTag)
            pure (maybe False (\c -> same c (maybe none (Data1 Ty.optionalRef TT.someTag) (Sq.lookup i d))) r)
          yes = maybe False (const True)
          -- one sample: its structure, then each helper against Haskell
          check :: Sq.Deque Val -> IO (Either String Int)
          check d = do
            let n = Sq.length d
                full = n <= 64
                cuts = [-1, 1, 10, n `div` 2, n - 10, n + 1]
            put arr 0 (wrap d)
            kinds <- listCheck arr
            if kinds < 0
              then pure (Left ("a list of " ++ show n ++ " elements isn't laid out as expected"))
              else do
                ixOk <- and <$> mapM (at d) ([-1, n] ++ spread n)
                viewOk <- and <$> mapM (\left -> yes <$> view full left d) [True, False]
                pushOk <- and <$> mapM (\front -> yes <$> push full front 77 d) [True, False]
                cutOk <- and <$> sequence [yes <$> cut full tk k d | tk <- [True, False], k <- cuts]
                splitOk <- and <$> sequence [yes <$> split full left k d | left <- [True, False], k <- [3, n `div` 2, n + 1]]
                pure $ case () of
                  _
                    | not ixOk -> Left ("List.at on a list of " ++ show n ++ " elements differs from the interpreter's")
                    | not viewOk -> Left ("a list pattern on a list of " ++ show n ++ " elements differs from the interpreter's")
                    | not pushOk -> Left ("adding to a list of " ++ show n ++ " elements differs from the interpreter")
                    | not cutOk -> Left ("List.take or List.drop on a list of " ++ show n ++ " elements differs from the interpreter's")
                    | not splitOk -> Left ("splitting a list of " ++ show n ++ " elements differs from the interpreter")
                    | otherwise -> Right kinds
          firstLeft :: [IO (Either String a)] -> IO (Either String [a])
          firstLeft = foldM (\acc io -> case acc of Left e -> pure (Left e); Right rs -> fmap (: rs) <$> io) (Right [])
          -- lists that helpers built, used again: a full digit at each
          -- level, then drained from the other end
          grow front = go (200 :: Int) Sq.empty
            where
              go 0 d = shrink (200 :: Int) d
              go k d = push (k > 160) front k d >>= maybe (pure (Left "a list built by the helpers differs from the interpreter's")) (go (k - 1))
              shrink 0 _ = pure (Right ())
              shrink k d = view (k < 40) (not front) d >>= maybe (pure (Left "a list drained by the helpers differs from the interpreter's")) (shrink (k - 1))
          pairs = [d | (i, d) <- zip [0 :: Int ..] samples, i `mod` 6 == 0]
          appends = [(\r -> if yes r then Right () else Left ("List.++ on lists of " ++ show (Sq.length a) ++ " and " ++ show (Sq.length b) ++ " elements differs from the interpreter's")) <$> cat (Sq.length a + Sq.length b <= 64) a b | a <- pairs, b <- pairs]
          lits = [(\r -> if yes r then Right () else Left ("a list literal of " ++ show k ++ " elements differs from the interpreter's")) <$> lit k 5 | k <- [0 .. 24]]
          -- (The checks above are sized to cost little at every startup: they
          -- confirm the layouts and that each helper works here. The thorough
          -- test of the helpers is the longer one, run when they change.)
          -- the longer test: random operations on eight lists
          mix :: Int -> Int
          mix z0 =
            let z1 = (z0 `xor` (z0 `shiftR` 30)) * (-4658895280553007687)
                z2 = (z1 `xor` (z1 `shiftR` 27)) * (-7723592293110705685)
             in (z2 `xor` (z2 `shiftR` 31)) .&. maxBound
          stress :: Int -> [Sq.Deque Val] -> IO (Either String ())
          stress step pool
            | step >= steps = pure (Right ())
            | otherwise = do
                let r k = mix (step * 16 + k)
                    i = r 0 `mod` 8
                    d = pool !! i
                    e = pool !! (r 1 `mod` 8)
                    n = Sq.length d
                    full = n <= 3000 || r 2 `mod` 16 == 0
                    pos = case r 3 `mod` 4 of
                      0 -> r 4 `mod` 12
                      1 -> n - r 4 `mod` 12
                      _ -> r 4 `mod` (n + 2) - 1
                    op = r 5 `mod` 16
                    front = even (r 7)
                    times :: Int -> (Sq.Deque Val -> IO (Maybe (Sq.Deque Val))) -> Sq.Deque Val -> IO (Maybe (Sq.Deque Val))
                    times 0 _ l = pure (Just l)
                    times k f l = f l >>= maybe (pure Nothing) (times (k - 1) f)
                res <-
                  if
                    | op < 2 -> push full True step d
                    | op < 4 -> push full False step d
                    | op == 4 -> view full True d
                    | op == 5 -> view full False d
                    | op == 6 -> cut full True pos d
                    | op == 7 -> cut full False pos d
                    | op == 8 -> fmap fst <$> split full True pos d
                    | op == 9 -> fmap snd <$> split full False pos d
                    | op <= 11 -> if n + Sq.length e > 400000 then cut full True (r 6 `mod` 50) d else cat full d e
                    | op == 12 -> (\good -> if good then Just d else Nothing) <$> at d pos
                    | op == 13 -> times (r 6 `mod` 300) (view (n <= 300) front) d
                    | otherwise -> times (r 6 `mod` 300) (push (n <= 300) front step) d
                case res of
                  Nothing -> pure (Left ("the list helpers' test failed at step " ++ show step ++ " (operation " ++ show op ++ " on a list of " ++ show n ++ " elements)"))
                  Just d' -> stress (step + 1) (take i pool ++ [d'] ++ drop (i + 1) pool)
      r <- firstLeft (map check samples)
      r2 <- firstLeft ([grow True, grow False] ++ appends ++ lits)
      r3 <- if steps > 0 then stress 0 (replicate 8 Sq.empty) else pure (Right ())
      pure $ case (r, r2, r3) of
        (Left e, _, _) -> Left e
        (_, Left e, _) -> Left e
        (_, _, Left e) -> Left e
        (Right kinds, _, _)
          | foldr (.|.) 0 kinds /= 15 -> Left ("the list samples don't cover every constructor (" ++ show (foldr (.|.) 0 kinds) ++ ")")
          | otherwise -> Right ()

-- | Text: the same for the text helpers. Each helper is run against the
-- Haskell operation on texts of many shapes (one chunk, deep ropes, several
-- bytes per character), and every text a helper builds is checked for its
-- structure too.
probeTexts :: Layouts -> IO (Either String ())
probeTexts ls = do
  let wrap :: UText.Text -> Any
      wrap t = any' $! Foreign (WrapText t)
      put :: MutableArray RealWorld Any -> Int -> Any -> IO ()
      put a i v = v `seq` writeArray a i v
      !abc = UText.pack "abc"
      !first = wrap abc
      pieces = ["", "a", "abc", "h\233llo w\246rld", "\8364\128512x\128512", "0123456789abcdef", replicate 31 'x', replicate 32 'y', replicate 33 'z', concat (replicate 40 "\955x"), replicate 600 'q']
      base = map UText.pack pieces
      -- ropes with structure: built up piece by piece from either side, by
      -- repetition, and cut
      grown = [foldl (\acc i -> acc <> UText.pack (show i)) mempty [1 .. n :: Int] | n <- [5, 40, 300]]
      grownL = [foldl (\acc i -> UText.pack (show i) <> acc) mempty [1 .. n :: Int] | n <- [40, 300]]
      big = UText.replicate 2000 (UText.pack "a\233")
      samples = base ++ grown ++ grownL ++ [big, UText.drop 7 big, UText.take 1500 big, UText.drop 100 (grown !! 2), UText.take 400 (grownL !! 1)]
  arr <- newArray 4 first :: IO (MutableArray RealWorld Any)
  put arr 1 (wrap (UText.appendUnbalanced abc (UText.pack "de")))
  put arr 2 (wrap UText.empty)
  ok <- textInit arr [lForeignInfo ls]
  if ok /= 1
    then pure (Left ("the text constructors are not laid out as the JIT's helpers expect (check " ++ show ok ++ ")"))
    else do
      let result :: IO (Maybe UText.Text)
          result = do
            c <- unsafeCoerce <$> readArray arr 3 :: IO Closure
            good <- do
              put arr 0 (any' c)
              textCheck arr
            pure $ case c of
              Foreign (WrapText t) | good -> Just t
              _ -> Nothing
          one :: UText.Text -> IO [String]
          one t = do
            let n = UText.size t
            put arr 0 (wrap t)
            shape <- textCheck arr
            size <- textTest arr 3 0
            cuts <- forM ([-1 .. min n 70] ++ [n - 3 .. n + 1]) $ \k -> do
              put arr 0 (wrap t)
              h1 <- textTest arr 1 k
              r1 <- result
              put arr 0 (wrap t)
              h2 <- textTest arr 2 k
              r2 <- result
              -- a negative count is a Nat too large to be a size
              let tk = if k < 0 then t else UText.take k t
                  dr = if k < 0 then UText.empty else UText.drop k t
              pure (h1 == 1 && r1 == Just tk && h2 == 1 && r2 == Just dr)
            pure . catMaybes $
              [ if shape then Nothing else Just "a text isn't laid out as expected",
                if size == n then Nothing else Just "Text.size differs from the interpreter's",
                if and cuts then Nothing else Just ("Text.take or Text.drop differs from the interpreter's on a text of " ++ show n ++ " characters")
              ]
          two :: UText.Text -> UText.Text -> IO [String]
          two a b = do
            put arr 0 (wrap a)
            put arr 1 (wrap b)
            eq <- textTest arr 4 0
            h <- textTest arr 0 0
            r <- result
            pure . catMaybes $
              [ if eq == (if a == b then 1 else 0) then Nothing else Just "Text equality differs from the interpreter's",
                if h == 1 && r == Just (a <> b) then Nothing else Just ("Text.++ differs from the interpreter's on texts of " ++ show (UText.size a) ++ " and " ++ show (UText.size b) ++ " characters")
              ]
      e1 <- concat <$> mapM one samples
      -- equal texts cut into chunks differently must compare equal
      let recut t = UText.take 3 t <> UText.drop 3 t
      e2 <- concat <$> sequence ([two a b | a <- samples, b <- samples] ++ [two t (recut t) | t <- samples])
      pure $ case e1 ++ e2 of
        [] -> Right ()
        e : _ -> Left e

-- | Partial applications: the helper for the @Name@ instruction against
-- what the interpreter builds, for closures with and without arguments
-- already captured.
probeNames :: Layouts -> IO (Either String ())
probeNames ls = do
  closureInit [lInfo (lPAp ls), lByteArrayBoxInfo ls, lArrayBoxInfo ls]
  let put :: MutableArray RealWorld Any -> Int -> Any -> IO ()
      put a i v = v `seq` writeArray a i v
      !ref = Ty.booleanRef
      !cix = CIx ref 21 22
      !comb = LamI 5 7 Exit noNativeCell
      !nat = natTypeTag
      pap us bs = let !u = byteArrayFromList (us :: [Int]); !b = arrayFromList bs in PAp cix comb (u, b)
      !first = any' $! pap [] []
  arr <- newArray 4 first :: IO (MutableArray RealWorld Any)
  put arr 1 (any' nat)
  oks <- forM [(old, n) | old <- [0, 1, 3], n <- [1 .. 4 :: Int]] $ \(old, n) -> do
    let olds = [50 .. 49 + old]
    put arr 0 (any' $! pap olds (map (const nat) olds))
    h <- nameTest arr n
    r <- unsafeCoerce <$> readArray arr 3 :: IO Closure
    let expected = pap (reverse [100 .. 99 + n] ++ olds) (replicate (n + old) nat)
    pure (h && BoxedVal r == BoxedVal expected)
  pure (if and oks then Right () else Left "partial applications built by the JIT's helper differ from the interpreter's")

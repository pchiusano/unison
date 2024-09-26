module Main where

-- import Data.Vector qualified as IV

import Data.IORef
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.Vector.Mutable qualified as V
import Data.Vector.Primitive.Mutable qualified as UV
import Data.Word (Word16, Word64)
import System.CPUTime (getCPUTime)
import Text.Printf

countUpTo :: Word64
countUpTo = 1000 * 1000 * 1000

-- countUpTo = 1000 * 1000 * 1000

-- | An absolute reference to a code array
type Ref = Int

-- | An absolute stack index
type Slot = Int

-- | A local variable reference, relative to the current stack frame
type Var = Int

-- currently unused
data Value = Null | Closure [Word64] [Value] deriving (Eq, Ord, Show)

-- data Frame = Frame { unboxed :: !(UV.IOVector Word64), boxed :: !(V.IOVector Value),  }
data Stack = Stack
  { unboxed :: !(UV.IOVector Word64),
    boxed :: !(V.IOVector Value),
    tail :: !(IORef Stack)
  }

-- is this sufficient? you set the IORef Stack to null when you find matching mark
-- when reinstating the continuation, you set the Stack to something else
-- I think this does it
newtype MCode2 = MCode2
  { interpret ::
      ( IntMap (MCode2, IORef Stack) ->
        (UV.IOVector Word64) ->
        (V.IOVector Value) ->
        Var ->
        Stack ->
        IO ()
      )
  }

-- UV.IOVector Word64 -> V.IOVector Value -> Slot -> Var -> Slot -> IO ()

data Function2 = Function2 {code2 :: MCode2, varCount :: !Int, arity :: !Int}

main :: IO ()
main = putStrLn "hi"

let_ :: Var -> MCode2 -> MCode2 -> MCode2
let_ var expr body = MCode2 go
  where
    go handlers unboxed boxed out stack = do
      interpret expr handlers unboxed boxed var stack
      interpret body handlers unboxed boxed out stack

return_ :: Var -> MCode2
return_ var = MCode2 go
  where
    go _ unboxed boxed out _stack = do
      uv <- UV.read unboxed var
      bv <- V.read boxed var
      UV.write unboxed out uv
      V.write boxed out bv

nat :: Word64 -> MCode2
nat n = MCode2 go
  where
    go _ unboxed boxed out _stack = do
      UV.write unboxed out n
      V.write boxed out Null

if_ :: Var -> MCode2 -> MCode2 -> MCode2
if_ var true false = MCode2 go
  where
    go handlers unboxed boxed out stack = do
      cond <- UV.read unboxed var
      if cond /= 0
        then interpret true handlers unboxed boxed out stack
        else interpret false handlers unboxed boxed out stack

trace :: String -> Var -> MCode2
trace msg var = MCode2 go
  where
    go _handlers unboxed boxed _out _stack = do
      uv <- UV.read unboxed var
      bv <- V.read boxed var
      putStrLn (msg ++ ": " <> show (uv, bv))

time :: IO t -> IO t
time a = do
  start <- getCPUTime
  v <- a
  end <- getCPUTime
  let diff = fromIntegral (end - start) / 1e12
  printf "Computation time  : %0.3f sec\n" (diff :: Double)
  printf "Ops / s (millions): %0.3f \n" (fromIntegral countUpTo * (1 / diff :: Double) / 1e6)
  return v

{-
main :: IO ()
main = do
  {-
  prog acc rem =
    if rem == 0 then printLine acc
    else
      acc' = increment acc
      rem' = decrement rem
      prog acc' rem'
  -}
  run2 prog'
  where
    prog' = let_ 1 (nat 0) (let_ 2 (nat countUpTo) (let_ 3 (call fn [1,2]) (trace "result" 3)))
    fn = Function2 go 2 4
    go =
      let
        acc = 0
        rem = 1
        acc' = 2
        rem' = 3
      in
        if_ rem
          (let_ acc' (call natIncrement [acc])
            $ let_ rem' (call natDecrement [rem])
            $ tailcall fn [acc', rem'])
          (return_ acc)

-- what about handlers?
-- we have a stack of handlers which are installed at various positions
-- this is another argument to MCode2
-- we find the appropriate handler, then copy the stack with a memcopy
-- then invoke the handler - the continuation is basically a region of the stack
-- when resuming the continuation, that just copies that region back onto the stack
-- inefficiency is due to the fact that the handler itself probably just needed a tiny
-- bit of stack
-- if at handle site you just leave a bit of a gap then you're good?
-- another option is to make the stack "append only"
-- that is, allocate call frames on the heap, they have (say) 16 slots by default
-- no copying happens when you make a request, because the handler just forks the stack
-- and the continuation something something

tailcall :: Function2 -> [Var] -> MCode2
tailcall (Function2 code arity) [a, b] =
  if arity == 2 then exact
  else undefined
  where
    staged = code id
    exact !unboxed !boxed !framePtr !_maxVar !_ = do
      au <- UV.read unboxed (framePtr - fromIntegral a)
      ab <- V.read boxed (framePtr - fromIntegral a)
      bu <- UV.read unboxed (framePtr - fromIntegral b)
      bb <- V.read boxed (framePtr - fromIntegral b)
      let aslot = framePtr - 1
      let bslot = framePtr - 2
      UV.write unboxed aslot au
      UV.write unboxed bslot bu
      V.write boxed aslot ab
      V.write boxed bslot bb
      staged unboxed boxed framePtr 2 framePtr
tailcall (Function2 code arity) [a] =
  if arity == 1 then exact
  else undefined
  where
    staged = code id
    exact !unboxed !boxed !framePtr !_maxVar !_ = do
      au <- UV.read unboxed (framePtr - fromIntegral a)
      ab <- V.read boxed (framePtr - fromIntegral a)
      let aslot = framePtr - 1
      UV.write unboxed aslot au
      V.write boxed aslot ab
      staged unboxed boxed framePtr 1 framePtr
tailcall _ _ = undefined

call :: Function2 -> [Var] -> MCode2
call (Function2 code arity) [a, b] =
  if arity == 2 then code args
  else undefined
  where args 1 = a
        args 2 = b
        args _ = error "variable out of range"
call (Function2 code arity) [a] =
  if arity == 1 then code (const a)
  else error "arity mismatch in call"
call _ _ = undefined

natDecrement :: Function2
natDecrement = Function2 code 1
  where
    code varmap = go
      where
      !var = fromIntegral (varmap 1)
      go unboxed boxed framePtr _ out = do
        au <- UV.read unboxed (framePtr - var)
        UV.write unboxed out (au - 1)
        V.write boxed out Null

natPlus :: Function2
natPlus = Function2 code 2
  where
    code varmap = go
      where
      !var1 = fromIntegral (varmap 1)
      !var2 = fromIntegral (varmap 2)
      go unboxed boxed framePtr _ out = do
        au <- UV.read unboxed (framePtr - var1)
        bu <- UV.read unboxed (framePtr - var2)
        UV.write unboxed out (au + bu)
        V.write boxed out Null

natIncrement :: Function2
natIncrement = Function2 code 1
  where
    code varmap = go
      where
      !var = fromIntegral (varmap 1)
      go unboxed boxed framePtr _ out = do
        au <- UV.read unboxed (framePtr - var)
        UV.write unboxed out (au + 1)
        V.write boxed out Null

{-
Machine code representation.
Uses a register-based VM with an infinite number of registers.
-}
data MCode ref
  = Nat !Word64                -- 42
  | TailCall ref !Var !Var    -- foo x y
  | NatIncrement !Var          -- increment x
  | NatDecrement !Var          -- decrement x
  | If0 !Var !(MCode ref) !(MCode ref)     -- if cond then t else f
  | Let !Var !(MCode ref) !(MCode ref)     -- let x = <expr> in body
  | Print !Var                 -- printLine x
  -- | DynamicCall !Slot !Slot !Slot
  deriving (Eq, Ord, Show)

data Function = Function { code :: !(MCode Function), arity :: !Int }

run2 :: MCode2 -> IO ()
run2 prog = do
  let n = 1024
  boxed <- V.replicate n Null
  unboxed <- UV.replicate n 0
  time $ prog unboxed boxed (n - 1) 0 (n - 1)

run :: MCode Function -> IO ()
run prog = do
  let n = 1024
  boxed <- V.replicate n Null
  unboxed <- UV.replicate n 0
  time $ go unboxed boxed (n - 1) 0 (n - 1) prog
  pure ()
  where
    {-
    Results are always written to `out :: Slot`.

    `unboxed` and `boxed` are manipulated in lockstep. Operations
    like `NatIncrement` write `Null` to the boxed stack.

    The stack grows downward toward index 0. This is thought to have
    better locality, since when the CPU loads position i in the stack,
    it will cache the items immediately below i in the stack.

    First argument or local variable of a function is at `framePtr - 1`.
    Second is at `framePtr - 2`, etc.
    Function args are just local variables!

    No debruijn indexing is used.

    Local variable declarations with `Let` should be of strictly
    increasing number within a branch of the function.

    So `Let 10 (Nat 42) (Let 3 (Nat 16) body)` is no good, since
    it declares variable `10` before variable `3`.

    maxVar is the maximum local variable declared in the current
    call frame. You can grab all the local variables by slicing
    from `framePtr - maxVar` to `framePtr`.
    -}
    go :: UV.IOVector Word64 -> V.IOVector Value -> Slot -> Var -> Slot -> MCode Function -> IO ()
    go !unboxed !boxed !framePtr !maxVar !out !prog = do
      -- stack <- UV.foldr (:) [] unboxed
      -- putStrLn ("stack:    " <> show stack)
      -- putStrLn ("frame:    " <> show framePtr)
      -- putStrLn ("maxVar:   " <> show maxVar)
      -- putStrLn ("out:      " <> show out)
      -- putStrLn ("rem:      " <> show prog)
      -- putStrLn ""
      case prog of
        Nat n -> do
          UV.write unboxed out n
          V.write boxed out Null
        TailCall ref a b -> do
          au <- UV.read unboxed (framePtr - fromIntegral a)
          ab <- V.read boxed (framePtr - fromIntegral a)
          bu <- UV.read unboxed (framePtr - fromIntegral b)
          bb <- V.read boxed (framePtr - fromIntegral b)
          let aslot = framePtr - 1
          let bslot = framePtr - 2
          UV.write unboxed aslot au
          UV.write unboxed bslot bu
          V.write boxed aslot ab
          V.write boxed bslot bb
          let mc = code ref
          go unboxed boxed framePtr 2 out mc
        NatIncrement a -> do
          au <- UV.read unboxed (framePtr - fromIntegral a)
          UV.write unboxed out (au + 1)
          V.write boxed out Null
        NatDecrement a -> do
          au <- UV.read unboxed (framePtr - fromIntegral a)
          UV.write unboxed out (au - 1)
          V.write boxed out Null
        If0 a t f -> do
          au <- UV.read unboxed (framePtr - fromIntegral a)
          if au == 0 then go unboxed boxed framePtr maxVar out t
          else go unboxed boxed framePtr maxVar out f
        Let var a b -> do
          go unboxed boxed framePtr var (framePtr - fromIntegral var) a
          go unboxed boxed framePtr var out b
          -- we assume that `Let` slots are assigned in increasing order
          -- otherwise we'd do:
          -- go unboxed boxed framePtr (max out maxVar) b
        Print a -> do
          au <- UV.read unboxed (framePtr - fromIntegral a)
          ab <- V.read boxed (framePtr - fromIntegral a)
          putStrLn (show (au, ab))
-}

{-# LANGUAGE CPP #-}

-- | The foreign interface to LLVM, through the C shim in @cbits/jit_llvm.c@.
-- With the @jit@ package flag off, every function here reports that the
-- JIT isn't built in, and nothing links against LLVM.
module Unison.Runtime.JIT.LLVM
  ( jitBuiltIn,
    initLLVM,
    addModule,
    lookupSymbol,
    defineSymbol,
    targetTriple,
  )
where

import Data.Word (Word64)
import Foreign.Ptr (FunPtr)

#ifdef UNISON_JIT
import Foreign.C.String (CString, peekCString, withCString, withCStringLen)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Ptr (WordPtr (..), castPtrToFunPtr, wordPtrToPtr)

foreign import ccall safe "unison_jit_init" c_init :: IO CInt

foreign import ccall safe "unison_jit_add_module" c_addModule :: CString -> CSize -> CString -> IO CInt

foreign import ccall safe "unison_jit_lookup" c_lookup :: CString -> IO Word64

foreign import ccall safe "unison_jit_define_symbol" c_defineSymbol :: CString -> Word64 -> IO CInt

foreign import ccall unsafe "unison_jit_last_error" c_lastError :: IO CString

foreign import ccall unsafe "unison_jit_triple" c_triple :: IO CString

jitBuiltIn :: Bool
jitBuiltIn = True

lastError :: String -> IO String
lastError what = do
  e <- peekCString =<< c_lastError
  pure ("JIT: " ++ what ++ ": " ++ e)

-- | Starts LLVM. Safe to call more than once. Returns an error message on failure.
initLLVM :: IO (Either String ())
initLLVM = do
  r <- c_init
  if r == 0 then pure (Right ()) else Left <$> lastError "initializing LLVM"

-- | Parses a module from IR text, runs the given pass pipeline
-- (such as @default<O2>@, or @""@ for none) and hands it to the JIT.
addModule :: String -> String -> IO (Either String ())
addModule passes ir =
  withCStringLen ir $ \(p, n) -> withCString passes $ \ps -> do
    r <- c_addModule p (fromIntegral n) ps
    if r == 0 then pure (Right ()) else Left <$> lastError "adding a module"

-- | The address of a compiled function. Code is generated on first lookup.
lookupSymbol :: String -> IO (Either String (FunPtr a))
lookupSymbol name = do
  a <- withCString name c_lookup
  if a == 0
    then Left <$> lastError ("looking up " ++ name)
    else pure (Right (castPtrToFunPtr (wordPtrToPtr (WordPtr (fromIntegral a)))))

-- | Makes @name@ in generated code refer to @addr@.
defineSymbol :: String -> Word64 -> IO (Either String ())
defineSymbol name addr = do
  r <- withCString name (\n -> c_defineSymbol n addr)
  if r == 0 then pure (Right ()) else Left <$> lastError ("defining " ++ name)

targetTriple :: IO String
targetTriple = peekCString =<< c_triple

#else

jitBuiltIn :: Bool
jitBuiltIn = False

notBuilt :: IO (Either String a)
notBuilt = pure (Left "JIT: not built in (build with --flag unison-runtime:jit)")

initLLVM :: IO (Either String ())
initLLVM = notBuilt

addModule :: String -> String -> IO (Either String ())
addModule _ _ = notBuilt

lookupSymbol :: String -> IO (Either String (FunPtr a))
lookupSymbol _ = notBuilt

defineSymbol :: String -> Word64 -> IO (Either String ())
defineSymbol _ _ = notBuilt

targetTriple :: IO String
targetTriple = pure "none"

#endif

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
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, WordPtr (..), castPtrToFunPtr, nullPtr, wordPtrToPtr)
import Foreign.Storable (peek, poke)

foreign import ccall safe "unison_jit_init" c_init :: IO CInt

foreign import ccall safe "unison_jit_add_module" c_addModule :: CString -> CSize -> CString -> Ptr CString -> IO CInt

foreign import ccall unsafe "unison_jit_free_string" c_freeString :: CString -> IO ()

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
-- When asked, also returns the module's text after the passes.
addModule :: Bool -> String -> T.Text -> IO (Either String (Maybe String))
addModule wantOptimized passes ir =
  unsafeUseAsCStringLen (encodeUtf8 ir) $ \(p, n) -> withCString passes $ \ps -> alloca $ \out -> do
    poke out nullPtr
    r <- c_addModule p (fromIntegral n) ps (if wantOptimized then out else nullPtr)
    if r /= 0
      then Left <$> lastError "adding a module"
      else do
        cs <- peek out
        if cs == nullPtr
          then pure (Right Nothing)
          else do
            txt <- peekCString cs
            c_freeString cs
            pure (Right (Just txt))

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

addModule :: Bool -> String -> T.Text -> IO (Either String (Maybe String))
addModule _ _ _ = notBuilt

lookupSymbol :: String -> IO (Either String (FunPtr a))
lookupSymbol _ = notBuilt

defineSymbol :: String -> Word64 -> IO (Either String ())
defineSymbol _ _ = notBuilt

targetTriple :: IO String
targetTriple = pure "none"

#endif

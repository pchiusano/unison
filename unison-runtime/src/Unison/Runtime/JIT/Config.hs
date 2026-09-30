-- | Settings for the JIT, read once from the environment.
-- See docs/jit-implementation-plan.md, D18.
module Unison.Runtime.JIT.Config
  ( Mode (..),
    Config (..),
    config,
    jitLog,
    jitDump,
  )
where

import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe, mapMaybe)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)

data Mode
  = -- | never compile
    Off
  | -- | compile everything as it is loaded (for testing)
    Eager
  deriving (Show, Eq)

data Config = Config
  { mode :: Mode,
    -- | log each compiled module to stderr
    logging :: Bool,
    -- | directory to write each module's IR into, with the MCode of each
    -- function as a comment; @-@ prints it to stderr instead
    dumpIR :: Maybe FilePath,
    -- | print the MCode of every loaded definition to stderr
    dumpMCode :: Bool,
    -- | log every transition between native code and the interpreter
    trace :: Bool,
    -- | print exit counts when the process ends
    stats :: Bool,
    -- | fire the entry poll every N entries (stress mode @poll=N@)
    stressPoll :: Int,
    -- | initial Unison stack size in slots (stress mode @ustack=N@)
    stressStack :: Maybe Int,
    -- | treat every Nth callee as not compiled (stress mode @callee=N@)
    stressCallee :: Int,
    -- | C stack budget for native calls, in bytes (stress mode @cstack=N@)
    stressCStack :: Int,
    -- | allocation budget between polls, in words (stress mode @alloc=N@)
    stressAlloc :: Int
  }
  deriving (Show)

config :: Config
config = unsafePerformIO $ do
  mode <- lookupEnv "UNISON_JIT"
  logging <- lookupEnv "UNISON_JIT_LOG"
  dump <- lookupEnv "UNISON_JIT_DUMP_IR"
  stats <- lookupEnv "UNISON_JIT_STATS"
  mcode <- lookupEnv "UNISON_JIT_DUMP_MCODE"
  tr <- lookupEnv "UNISON_JIT_TRACE"
  stress <- maybe [] (splitOn ',') <$> lookupEnv "UNISON_JIT_STRESS"
  let setting key = listToMaybe' (mapMaybe (stripPrefix (key ++ "=")) stress) >>= readMaybe
  pure
    Config
      { mode = case map toLower (fromMaybe "off" mode) of
          "eager" -> Eager
          _ -> Off,
        logging = maybe False (not . null) logging,
        dumpIR = dump,
        dumpMCode = maybe False (not . null) mcode,
        trace = maybe False (not . null) tr,
        stats = maybe False (not . null) stats,
        stressPoll = fromMaybe 0 (setting "poll"),
        stressStack = setting "ustack",
        stressCallee = fromMaybe 0 (setting "callee"),
        stressCStack = fromMaybe 0 (setting "cstack"),
        stressAlloc = fromMaybe 0 (setting "alloc")
      }
  where
    splitOn c s = case break (== c) s of
      (a, []) -> [a]
      (a, _ : rest) -> a : splitOn c rest
    listToMaybe' [] = Nothing
    listToMaybe' (x : _) = Just x
{-# NOINLINE config #-}

jitLog :: String -> IO ()
jitLog msg
  | logging config = hPutStrLn stderr ("[jit] " ++ msg)
  | otherwise = pure ()

-- | Writes diagnostic text to stderr, unconditionally.
jitDump :: String -> IO ()
jitDump = hPutStrLn stderr

-- | Settings for the JIT, read once from the environment.
-- See docs/jit-implementation-plan.md, D18.
module Unison.Runtime.JIT.Config
  ( Mode (..),
    Config (..),
    config,
    jitLog,
    jitDump,
    outputHook,
    noteOutput,
  )
where

import Control.Monad (join, when)
import Data.Char (toLower)
import Data.IORef (IORef, newIORef, readIORef)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe, mapMaybe)
import System.Environment (lookupEnv)
import System.IO (hFlush, hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)

data Mode
  = -- | never compile
    Off
  | -- | compile what gets hot, in the background
    On
  | -- | compile everything as it is loaded (for testing)
    Eager
  deriving (Show, Eq)

data Config = Config
  { mode :: Mode,
    -- | with 'On': interpreted calls before a definition is compiled
    -- (@UNISON_JIT_THRESHOLD@)
    threshold :: Int,
    -- | with 'On': the most definitions compiled together as one module
    -- (@UNISON_JIT_BATCH@)
    batch :: Int,
    -- | what leaving native code and coming back costs, in units of the
    -- interpreter's overhead for one simple instruction: a function is
    -- left interpreted when its certain exits times this exceed what
    -- compiling it saves (@UNISON_JIT_EXIT_COST@; 0 compiles everything).
    -- See JIT.Estimate.
    exitCost :: Double,
    -- | what entering native code from the interpreter costs, in the same
    -- units: a compiled function that saves less than this per call is
    -- still interpreted when the interpreter calls it
    -- (@UNISON_JIT_ENTRY_COST@; 0 always enters)
    entryCost :: Double,
    -- | log each compiled module to stderr
    logging :: Bool,
    -- | directory to write each module's IR into, with the MCode of each
    -- function as a comment; @-@ prints it to stderr instead
    dumpIR :: Maybe FilePath,
    -- | print the MCode of every loaded definition to stderr
    dumpMCode :: Bool,
    -- | log every transition between native code and the interpreter
    trace :: Bool,
    -- | print exit counts after each evaluation (@UNISON_JIT_STATS@)
    stats :: Bool,
    -- | with stats: print the exits taken since the last time whenever
    -- the program writes output, and start counting again
    -- (@UNISON_JIT_STATS=each@), so that a program that prints a line per
    -- phase gets its exits listed per phase
    statsEach :: Bool,
    -- | with stats: also print the counts every this many exits (for
    -- evaluations that never finish)
    statsEvery :: Int,
    -- | fire the entry poll every N entries (stress mode @poll=N@)
    stressPoll :: Int,
    -- | initial Unison stack size in slots (stress mode @ustack=N@)
    stressStack :: Maybe Int,
    -- | treat every Nth callee as not compiled (stress mode @callee=N@)
    stressCallee :: Int,
    -- | C stack budget for native calls, in bytes (stress mode @cstack=N@)
    stressCStack :: Int,
    -- | allocation budget between polls, in words (stress mode @alloc=N@)
    stressAlloc :: Int,
    -- | milliseconds to sleep between installing each function of a
    -- module (stress mode @install=N@)
    stressInstall :: Int,
    -- | size of the constant pool before it has to grow (stress mode @pool=N@)
    stressPool :: Maybe Int,
    -- | random operations to run through the list helpers at startup,
    -- each checked against the interpreter's (stress mode @lists=N@)
    stressLists :: Int,
    -- | the same for the text helpers (stress mode @texts=N@)
    stressTexts :: Int,
    -- | features turned off for debugging, from @UNISON_JIT_DISABLE@
    -- (comma separated): @app@ (closure calls), @apply@ (the interpreter
    -- entering native code for a closure), @ref@, @array@, @cmp@
    -- (universal comparison), @callout@ (call-outs become resumes), @direct@
    -- (calls within a module go through cells), @worker@ (no workers with
    -- register arguments)
    disabled :: [String]
  }
  deriving (Show)

config :: Config
config = unsafePerformIO $ do
  mode <- lookupEnv "UNISON_JIT"
  logging <- lookupEnv "UNISON_JIT_LOG"
  dump <- lookupEnv "UNISON_JIT_DUMP_IR"
  stats <- lookupEnv "UNISON_JIT_STATS"
  every <- lookupEnv "UNISON_JIT_STATS_EVERY"
  thresh <- lookupEnv "UNISON_JIT_THRESHOLD"
  batchSize <- lookupEnv "UNISON_JIT_BATCH"
  cost <- lookupEnv "UNISON_JIT_EXIT_COST"
  entry <- lookupEnv "UNISON_JIT_ENTRY_COST"
  mcode <- lookupEnv "UNISON_JIT_DUMP_MCODE"
  tr <- lookupEnv "UNISON_JIT_TRACE"
  stress <- maybe [] (splitOn ',') <$> lookupEnv "UNISON_JIT_STRESS"
  disabledFeatures <- maybe [] (splitOn ',') <$> lookupEnv "UNISON_JIT_DISABLE"
  let setting key = listToMaybe' (mapMaybe (stripPrefix (key ++ "=")) stress) >>= readMaybe
  pure
    Config
      { mode = case map toLower (fromMaybe "off" mode) of
          "eager" -> Eager
          "on" -> On
          _ -> Off,
        threshold = max 1 (fromMaybe 100 (thresh >>= readMaybe)),
        batch = max 1 (fromMaybe 32 (batchSize >>= readMaybe)),
        exitCost = max 0 (fromMaybe 7 (cost >>= readMaybe)),
        entryCost = max 0 (fromMaybe 3 (entry >>= readMaybe)),
        logging = maybe False (not . null) logging,
        dumpIR = dump,
        dumpMCode = maybe False (not . null) mcode,
        trace = maybe False (not . null) tr,
        stats = maybe False (not . null) stats,
        statsEach = stats == Just "each",
        stressPoll = fromMaybe 0 (setting "poll"),
        stressStack = setting "ustack",
        stressCallee = fromMaybe 0 (setting "callee"),
        stressCStack = fromMaybe 0 (setting "cstack"),
        stressAlloc = fromMaybe 0 (setting "alloc"),
        stressInstall = fromMaybe 0 (setting "install"),
        stressPool = setting "pool",
        stressLists = fromMaybe 0 (setting "lists"),
        stressTexts = fromMaybe 0 (setting "texts"),
        statsEvery = fromMaybe 0 (every >>= readMaybe),
        disabled = disabledFeatures
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
jitDump msg = hPutStrLn stderr msg >> hFlush stderr

-- | What 'noteOutput' runs; set by the JIT when it starts.
outputHook :: IORef (IO ())
outputHook = unsafePerformIO (newIORef (pure ()))
{-# NOINLINE outputHook #-}

-- | Called by the runtime after the program has written to a handle.
noteOutput :: IO ()
noteOutput = when (statsEach config) (join (readIORef outputHook))

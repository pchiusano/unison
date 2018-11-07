module Unison.Util.Autocomplete where

newtype Autocomplete m a
  = Autocomplete (String -> Either (m [Suggestion]) a)

type Suggestion = String

instance Applicative m => Monad (Autocomplete m a) where
  return a = Autcomplete (const $ Right a)
  Autocomplete a >>= f = \s -> case a s of


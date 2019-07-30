{-# Language OverloadedStrings #-}

module Unison.Codebase.Editor.RemoteRepo (RemoteRepo(..), installNamespace) where

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text as Text
import qualified Codec.Binary.Base32Hex as Base32Hex

data RemoteRepo = GitRepo { url :: Text, commit :: Text }
  deriving (Eq, Ord, Show)

installNamespace :: RemoteRepo -> Text
installNamespace (GitRepo url commit) = 
  "install." <> base32Hex (url <> commitTxt)
  where
  commitTxt = if commit == "master" then "" else "." <> commit

base32Hex :: Text -> Text
base32Hex txt =
  -- v is the multibase prefix: 
  -- https://github.com/multiformats/multibase#multibase-table-v100-rc-semver
  ("v" <>) . Text.toLower . Text.dropWhileEnd (== '=') 
           . decodeUtf8 . Base32Hex.encode $ encodeUtf8 txt

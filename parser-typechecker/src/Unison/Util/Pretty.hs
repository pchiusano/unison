{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}

module Unison.Util.Pretty (
   Pretty,
   bulleted,
   -- breakable
   column2,
   commas,
   dashed,
   group,
   hang',
   hang,
   hangUngrouped',
   hangUngrouped,
   indent,
   indentN,
   leftPad,
   lines,
   linesSpaced,
   lit,
   map,
   nest,
   nestN,
   newline,
   numbered,
   orElse,
   orElses,
   parenthesize,
   parenthesizeCommas,
   parenthesizeIf,
   preferredWidth,
   preferredHeight,
   render,
   renderUnbroken,
   rightPad,
   sep,
   sepSpaced,
   softbreak,
   spaceIfBreak,
   spacesIfBreak,
   spaced,
   spacedMap,
   surroundCommas,
   text,
   toANSI,
   toPlain,
   wrap,
   wrapWords,
   black, red, green, yellow, blue, purple, cyan, white, hiBlack, hiRed, hiGreen, hiYellow, hiBlue, hiPurple, hiCyan, hiWhite, bold
  ) where

import           Data.Foldable                  ( toList )
import           Data.List                      ( foldl' , foldr1)
import qualified Data.List as List
import           Data.Sequence                  ( Seq )
import           Data.String                    ( IsString , fromString )
import           Data.Text                      ( Text )
import           Prelude                 hiding ( lines , map )
import qualified Unison.Util.ColorText         as CT
import           Unison.Util.Monoid             ( intercalateMap )
import qualified Data.ListLike                 as LL
import qualified Data.Sequence                 as Seq
import qualified Data.Text                     as Text

type Width = Int

data Pretty s = Pretty { delta :: Delta, out :: F s (Pretty s) }

data F s r
  = Empty | Newline | Lit s | Group r | OrElse r r
  | Wrap (Seq r) | Append (Seq r) | Nest r r
  deriving (Show, Foldable, Traversable, Functor)

lit :: IsString s => String -> Pretty s
lit s = intercalateMap newline go (List.lines s)
  where go s = lit' (foldMap chDelta s) (fromString s)

lit' :: Delta -> s -> Pretty s
lit' d s = Pretty d (Lit s)

orElse :: Pretty s -> Pretty s -> Pretty s
orElse p1 p2 = Pretty (delta p1) (OrElse p1 p2)

orElses :: [Pretty s] -> Pretty s
orElses [] = mempty
orElses ps = foldr1 orElse ps

wrap :: IsString s => [Pretty s] -> Pretty s
wrap [] = mempty
wrap (p:ps) = wrap_ . Seq.fromList $
  p : fmap (\p -> (" " <> p) `orElse` (newline <> p)) ps

wrap_ :: Seq (Pretty s) -> Pretty s
wrap_ ps = Pretty (foldMap delta ps) (Wrap ps)

wrapWords :: IsString s => String -> Pretty s
wrapWords = wrap . fmap fromString . words

group :: Pretty s -> Pretty s
group p = Pretty (delta p) (Group p)

toANSI :: Width -> Pretty CT.ColorText -> String
toANSI avail p = CT.toANSI (render avail p)

toPlain :: Width -> Pretty CT.ColorText -> String
toPlain avail p = CT.toPlain (render avail p)

renderUnbroken :: (Monoid s, IsString s) => Pretty s -> s
renderUnbroken = render maxBound

data Mode a = Try a | Break a | PopNest

render :: (Monoid s, IsString s) => Width -> Pretty s -> s
render availableWidth p = go mempty mempty [Try p] where
  go _   _      []       = mempty
  go cur indent (p:rest) = case p of
    Try p -> -- `p` might fit, let's try it!
      if p `fits` cur then flow indent p <> go (cur <> delta p) indent rest
      else go cur indent (Break p : rest) -- nope, switch to breaking mode
    Break p -> case out p of -- `p` requires breaking
      Append ps  -> go cur indent ((Break <$> toList ps) <> rest)
      Empty      -> go cur indent rest
      Group p    -> go cur indent (Try p : rest)
      -- Note: literals can't be broken further; they're added to output unconditionally
      Lit l      -> l <> go (cur <> delta p) indent rest
      OrElse _ p -> go cur indent (Try p : rest)
      Nest by p  -> go cur (by : indent) (Break p : PopNest : rest)
      Wrap ps    -> go cur indent ((Try <$> toList ps) <> rest)
      Newline    -> flow indent "\n" <> go (cur <> delta nl) indent rest where
                    nl = ("\n" <> mconcat (reverse indent))
    PopNest -> go cur (drop 1 indent) rest

  flow indent p = case out p of
    Nest by p -> flow (by : indent) p
    Newline -> "\n" <> foldMap (flow mempty) (reverse indent)
    Append ps -> foldMap (flow indent) ps
    Empty -> mempty
    Group p -> flow indent p
    Lit s -> s
    OrElse p _ -> flow indent p
    Wrap ps -> foldMap (flow indent) ps

  fits p cur =
    let cur' = cur { maxCol = col cur }
    in maxCol (cur' <> delta p) < availableWidth

newline :: IsString s => Pretty s
newline = Pretty (chDelta '\n') Newline

spaceIfBreak :: IsString s => Pretty s
spaceIfBreak = "" `orElse` " "

spacesIfBreak :: IsString s => Int -> Pretty s
spacesIfBreak n = "" `orElse` (fromString $ replicate n ' ')

softbreak :: IsString s => Pretty s
softbreak = " " `orElse` newline

spaced :: (Foldable f, IsString s) => f (Pretty s) -> Pretty s
spaced = intercalateMap softbreak id

spacedMap :: (Foldable f, IsString s) => (a -> Pretty s) -> f a -> Pretty s
spacedMap f as = spaced . fmap f $ toList as

commas :: (Foldable f, IsString s) => f (Pretty s) -> Pretty s
commas = intercalateMap ("," <> softbreak) id

parenthesizeCommas :: (Foldable f, IsString s) => f (Pretty s) -> Pretty s
parenthesizeCommas = surroundCommas "(" ")"

surroundCommas :: (Foldable f, IsString s) => Pretty s -> Pretty s -> f (Pretty s) -> Pretty s
surroundCommas start stop fs = group $
  start <> spaceIfBreak
        <> intercalateMap ("," <> softbreak <> align) id fs
        <> stop
  where align = spacesIfBreak (preferredWidth start + 1)

sepSpaced :: (Foldable f, IsString s) => Pretty s -> f (Pretty s) -> Pretty s
sepSpaced between = sep (between <> softbreak)

sep :: (Foldable f, IsString s) => Pretty s -> f (Pretty s) -> Pretty s
sep between = intercalateMap between id

parenthesize :: IsString s => Pretty s -> Pretty s
parenthesize p = group $ "(" <> p <> ")"

parenthesizeIf :: IsString s => Bool -> Pretty s -> Pretty s
parenthesizeIf False s = s
parenthesizeIf True s = parenthesize s

lines :: (Foldable f, IsString s) => f (Pretty s) -> Pretty s
lines = intercalateMap newline id

linesSpaced :: (Foldable f, IsString s) => f (Pretty s) -> Pretty s
linesSpaced = intercalateMap (newline <> newline) id

bulleted :: (Foldable f, LL.ListLike s Char, IsString s) => f (Pretty s) -> Pretty s
bulleted = intercalateMap newline (\b -> "* " <> nest "  " b)

dashed :: (Foldable f, LL.ListLike s Char, IsString s) => f (Pretty s) -> Pretty s
dashed = intercalateMap newline (\b -> "- " <> nest "  " b)

numbered :: (Foldable f, LL.ListLike s Char, IsString s) => (Int -> Pretty s) -> f (Pretty s) -> Pretty s
numbered num ps = column2 (fmap num [1..] `zip` toList ps)

leftPad, rightPad :: IsString s => Int -> Pretty s -> Pretty s
leftPad n p =
  let rem = n - preferredWidth p
  in if rem > 0 then fromString (replicate rem ' ') <> p
     else p
rightPad n p =
  let rem = n - preferredWidth p
  in if rem > 0 then p <> fromString (replicate rem ' ')
     else p

column2 :: (LL.ListLike s Char, IsString s) => [(Pretty s, Pretty s)] -> Pretty s
column2 rows = lines (group <$> alignedRows) where
  maxWidth = foldl' max 0 (preferredWidth . fst <$> rows) + 1
  alignedRows = [ rightPad maxWidth col0 <> nestN maxWidth col1
                | (col0, col1) <- rows ]

text :: IsString s => Text -> Pretty s
text t = fromString (Text.unpack t)

hang' :: IsString s => Pretty s -> Pretty s -> Pretty s -> Pretty s
hang' from by p = group $
  if preferredHeight p > 0 then from <> "\n" <> group (indent by p)
  else (from <> " " <> group p) `orElse`
       (from <> "\n" <> group (indent by p))

hangUngrouped' :: IsString s => Pretty s -> Pretty s -> Pretty s -> Pretty s
hangUngrouped' from by p =
  if preferredHeight p > 0 then from <> "\n" <> indent by p
  else (from <> " " <> p) `orElse`
       (from <> "\n" <> indent by p)

hangUngrouped :: IsString s => Pretty s -> Pretty s -> Pretty s
hangUngrouped from p = hangUngrouped' from "  " p

hang :: IsString s => Pretty s -> Pretty s -> Pretty s
hang from p = hang' from "  " p

indent :: IsString s => Pretty s -> Pretty s -> Pretty s
indent by p = by <> nest by p

indentN :: IsString s => Width -> Pretty s -> Pretty s
indentN by = indent (fromString $ replicate by ' ')

nestN :: IsString s => Width -> Pretty s -> Pretty s
nestN by = nest (fromString $ replicate by ' ')

nest :: IsString s => Pretty s -> Pretty s -> Pretty s
nest by p = Pretty d (Nest by p) where
  dp = delta p
  tweak n = n + maxCol dp
  d = if line dp == 0 then dp
      -- this is overly pessimistic, would need richer `Delta`
      -- type to do this more accurately
      else Delta (line dp) (tweak $ col dp) (tweak $ maxCol dp)

instance IsString s => IsString (Pretty s) where
  fromString s = intercalateMap newline go (List.lines s) where
    go line = lit' (foldMap chDelta s) (fromString line)

instance Semigroup (Pretty s) where (<>) = mappend
instance Monoid (Pretty s) where
  mempty = Pretty mempty Empty
  mappend p1 p2 = Pretty (delta p1 <> delta p2) .
    Append $ case (out p1, out p2) of
      (Append ps1, Append ps2) -> ps1 <> ps2
      (Append ps1, _) -> ps1 <> pure p2
      (_, Append ps2) -> pure p1 <> ps2
      (_,_) -> pure p1 <> pure p2

data Delta =
  Delta { line :: !Int, col :: !Int, maxCol :: !Int }
  deriving (Eq,Ord,Show)

instance Semigroup Delta where (<>) = mappend
instance Monoid Delta where
  mempty = Delta 0 0 0
  mappend (Delta l c mc) (Delta 0 c2 mc2) = Delta l (c + c2) (mc `max` mc2 `max` (c+c2))
  mappend (Delta l _ mc) (Delta l2 c2 mc2) = Delta (l + l2) c2 (mc `max` mc2)

chDelta :: Char -> Delta
chDelta '\n' = Delta 1 0 0
chDelta _ = Delta 0 1 1

preferredWidth :: Pretty s -> Width
preferredWidth p = maxCol (delta p)

preferredHeight :: Pretty s -> Width
preferredHeight p = line (delta p)

black, red, green, yellow, blue, purple, cyan, white, hiBlack, hiRed, hiGreen,
  hiYellow, hiBlue, hiPurple, hiCyan, hiWhite, bold :: Pretty CT.ColorText -> Pretty CT.ColorText
black = map CT.black
red = map CT.red
green = map CT.green
yellow = map CT.yellow
blue = map CT.blue
purple = map CT.purple
cyan = map CT.cyan
white = map CT.white
hiBlack = map CT.hiBlack
hiRed = map CT.hiRed
hiGreen = map CT.hiGreen
hiYellow = map CT.hiYellow
hiBlue = map CT.hiBlue
hiPurple = map CT.hiPurple
hiCyan = map CT.hiCyan
hiWhite = map CT.hiWhite
bold = map CT.bold

instance Show s => Show (Pretty s) where
  show p = render 80 (metaPretty p)

metaPretty :: Show s => Pretty s -> Pretty String
metaPretty p = go (0::Int) p where
  go prec p = case out p of
    Newline -> "Newline"
    Nest by p -> parenthesizeIf (prec > 0) $
      "Nest" `hang` spaced [go 1 by, go 1 p]
    Lit s -> parenthesizeIf (prec > 0) $ "Lit" `hang` lit (show s)
    Empty -> "Empty"
    Group g -> parenthesizeIf (prec > 0) $ "Group" `hang` go 1 g
    Wrap s -> parenthesizeIf (prec > 0) $ "Wrap" `hang`
      surroundCommas "[" "]" (go 0 <$> s)
    OrElse a b -> parenthesizeIf (prec > 0) $
      "OrElse" `hang` spaced [go 1 a, go 1 b]
    Append s -> surroundCommas "[" "]" (go 0 <$> s)

map :: LL.ListLike s2 Char => (s -> s2) -> Pretty s -> Pretty s2
map f p = case out p of
  Newline -> Pretty (delta p) Newline
  Nest by p -> Pretty (delta p) (Nest (map f by) (map f p))
  Append ps -> foldMap (map f) ps
  Empty -> mempty
  Group p -> group (map f p)
  Lit s -> lit' (foldMap chDelta $ LL.toList s2) s2 where s2 = f s
  OrElse p1 p2 -> orElse (map f p1) (map f p2)
  Wrap p -> wrap_ (map f <$> p)

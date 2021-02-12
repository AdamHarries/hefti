-- {-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE QuasiQuotes       #-}
module MuseScore.Instruments (
    Instrument (..),
    parseInst,
    Key (..),
    key,
) where

import           Data.Text   as TE
import           Text.Parsec as PS

data Instrument
  = Piano
  | AltoSax
  | TenorSax
  | BaritoneSax
  | Trumpet
  | Trombone
  | Clarinet
  | Drums
  | Guitar
  | Bass
  | Unknown TE.Text
  deriving (Eq, Ord)

instance Show Instrument where
  show Piano       = "Piano"
  show AltoSax     = "Alto Sax"
  show TenorSax    = "Tenor Sax"
  show BaritoneSax = "Baritone Sax"
  show Trumpet     = "B♭ Trumpet"
  show Trombone    = "Trombone"
  show Clarinet    = "B♭ Clarinet"
  show Drums       = "Drumset"
  show Guitar      = "Acoustic Guitar"
  show Bass        = "Bass Guitar"
  show (Unknown t) = "Unknown Instrument" ++ show t

parseInst :: TE.Text -> Instrument
parseInst name = case parse instrumentParser "" (TE.unpack name) of
    Left pe -> Unknown name
    Right i -> i

instrumentParser :: Parsec String st Instrument
instrumentParser =
      fixedParser Piano
  <|> fixedParser Trumpet
  <|> fixedParser Clarinet
  <|> optionParser Guitar ["Acoustic Guitar", "Guitar"]
  <|> optionParser Bass ["Bass", "Bass Guitar", "Acoustic Bass"]
  <|> optionParser Drums ["Drums", "Drumset"]
  <|> saxParser ["Alto"] AltoSax
  <|> saxParser ["Tenor"] TenorSax
  <|> saxParser ["Bari", "Baritone"] BaritoneSax

fixedParser :: Instrument -> Parsec String st Instrument
fixedParser inst = try $ string (show inst) >> pure inst

optionParser :: Instrument -> [String] -> Parsec String st Instrument
optionParser inst names = try $ choice $ Prelude.map (\s -> try $ string s >> pure inst) names

saxParser :: [String] -> Instrument -> Parsec String st Instrument
saxParser saxtype saxvalue = try $ do
  choice $ Prelude.map (\s -> try $ string s) saxtype
  sax <- optional $ do
        string " Sax"
        optional $ string "ophone"
  pure saxvalue

-- Their Keys
data Key = C | Eb | Bb deriving (Eq)
instance Show Key where
  show C  = "C"
  show Eb = "Eb"
  show Bb = "Bb"

key :: Instrument -> Key
key AltoSax  = Eb
key TenorSax = Bb
key Trumpet  = Bb
key Piano    = C
key Clarinet = Bb
key _        = C

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
  show Trumpet     = "B♭ Trumpet"
  show (Unknown t) = "Unknown Instrument" ++ show t

parseInst :: TE.Text -> Instrument
parseInst name = case parse instrumentParser "" (TE.unpack name) of
    Left pe -> Unknown (TE.pack $ show pe)
    Right i -> i

instrumentParser :: Parsec String st Instrument
instrumentParser =
      fixedParser "Piano" Piano
  <|> fixedParser "B♭ Trumpet" Trumpet
  <|> fixedParser "B♭ Clarinet" Clarinet
  <|> fixedParser "Acoustic Guitar" Guitar
  <|> fixedParser "Bass Guitar" Bass
  <|> fixedParser "Drumset" Drums
  <|> saxParser "Alto" AltoSax
  <|> saxParser "Tenor" TenorSax
  <|> saxParser "Baritone" BaritoneSax

fixedParser :: String -> Instrument -> Parsec String st Instrument
fixedParser name value = try $ string name >> pure value


saxParser :: String -> Instrument -> Parsec String st Instrument
saxParser saxtype saxvalue = try $ do
  string saxtype
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

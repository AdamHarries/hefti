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
parseInst "Piano"           = Piano
parseInst "Alto Sax"        = AltoSax
parseInst "Alto Saxophone"  = AltoSax
parseInst "Tenor Saxophone" = TenorSax
parseInst "B♭ Trumpet"      = Trumpet
parseInst "B♭ Clarinet"     = Clarinet
parseInst "Acoustic Guitar" = Guitar
parseInst "Bass Guitar"     = Bass
parseInst "Drumset"         = Drums
parseInst t                 = Unknown t

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

type TNumber = Int

data TOperator = TAdd
               | TSubtract
                 deriving (Eq, Ord, Show)

data TExpression = TNode (TExpression) TOperator (TExpression)
                 | TTerminal TNumber
                   deriving (Show)

pianoParser :: Parsec String st Instrument
pianoParser = string "Piano" >> pure Piano

saxParser :: Parsec String st Instrument
saxParser = do
    kind <- string "Alto"
    sax <- do
        string "Sax"
        optional $ string "ophone"
    pure AltoSax

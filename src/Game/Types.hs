{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Types where

import Data.Map (Map)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Char (toLower)

-- Tipos base

type Dado = Int
type Tiro = [Dado]
type Puntaje = Int
type Jugador = String

-- Enumeración de combinaciones Yatzy

data Combinacion = Unos | Doses | Treses | Cuatro | Cinco | Seis
                  | Trio | Cuarteto | FullHouse | PequenaEscalera
                  | GranEscalera | Yatzy | Chance
                  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON Combinacion where
  toJSON c = toJSON (map toLower (show c))

instance FromJSON Combinacion where
  parseJSON v = do
    s <- parseJSON v
    let lower = map toLower (s :: String)
        dict = [(map toLower (show x), x) | x <- [minBound .. maxBound]]
    case lookup lower dict of
      Just c -> pure c
      Nothing -> fail "Combinacion desconocida"

-- Estado del juego puro

data GameState = GameState
  { jugadores :: [Jugador]
  , turnoActual :: Int              -- índice del jugador actual
  , tiradasRealizadas :: Int        -- tiradas realizadas en el turno (0..3)
  , dadosActuales :: Tiro           -- dados visibles (si no se han tirado aún puede estar vacío)
  , dadosConservados :: [Dado]      -- dados que el jugador decidió conservar entre tiradas
  , combinacionesDisponibles :: Map Jugador [Combinacion] -- combinaciones aún no usadas por jugador
  , puntajes :: Map Jugador (Map Combinacion Puntaje)      -- puntajes ya fijados
  } deriving (Show, Generic)

-- No instancia ToJSON para GameState: el servidor expone una vista simplificada.

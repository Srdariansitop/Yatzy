module Game.AI (
    aiChooseDice,
    aiChooseCombo
) where

import Game.Types
import Game.Logic
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import qualified Data.Map as M

-- Decide qué dados conservar (retorna índices 0-based)
aiChooseDice :: Tiro -> [Int]
aiChooseDice dados = 
  let ranked = rankByValue dados
  in map fst $ take (length dados - countToKeep dados) ranked
  where
    countToKeep d = length $ filter (\x -> count x d >= 2) [1..6]

-- Estrategia simple: conservar dados que aparecen múltiples veces
rankByValue :: Tiro -> [(Int, Dado)]
rankByValue dados =
  let pairs = zip [0..] dados
      scored = [(i, d, countFreq d dados) | (i, d) <- pairs]
  in map (\(i, d, _) -> (i, d)) $ sortBy (comparing (\(_, _, freq) -> Down freq)) scored

countFreq :: Dado -> Tiro -> Int
countFreq x = length . filter (== x)

count :: Int -> Tiro -> Int
count x = length . filter (== x)

-- Elige la mejor combinación disponible
aiChooseCombo :: GameState -> Combinacion
aiChooseCombo st =
  let player = jugadores st !! turnoActual st
      available = M.findWithDefault [] player (combinacionesDisponibles st)
      dados = dadosActuales st
      scored = [(combo, puntaje combo dados) | combo <- available]
      sorted = sortBy (comparing (\(_, p) -> Down p)) scored
  in if null sorted then head available else fst (head sorted)

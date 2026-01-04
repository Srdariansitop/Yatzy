module Game.AI (
    aiChooseDice,
    aiChooseCombo
) where

import Game.Types
import Game.Logic

import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import qualified Data.Map as M

--------------------------------------------------------------------------------
-- IA: ELECCIÓN DE DADOS (HEURÍSTICA)
--------------------------------------------------------------------------------

-- Decide qué dados conservar (retorna índices 0-based)
aiChooseDice :: Tiro -> [Int]
aiChooseDice dados =
  let indexed = zip [0..] dados
      scored  = map (\(i,d) -> (i, scoreDie d dados)) indexed
      sorted  = sortBy (comparing (Down . snd)) scored
      keepN   = optimalKeepCount dados
  in map fst $ take keepN sorted

-- Puntaje heurístico de cada dado
scoreDie :: Dado -> Tiro -> Int
scoreDie d dados =
  let freq = countFreq d dados
      highValue = if d >= 5 then 2 else 0
      straightBonus = if contributesToStraight d dados then 1 else 0
  in freq * 4 + highValue + straightBonus

-- Decide cuántos dados conservar
optimalKeepCount :: Tiro -> Int
optimalKeepCount dados
  | hasNOfKind 5 dados        = 5   -- Yatzy
  | hasNOfKind 4 dados        = 4   -- Cuarteto
  | hasNOfKind 3 dados        = 3   -- Trio / Full
  | hasStraightPotential dados = 4
  | otherwise                 = 2

--------------------------------------------------------------------------------
-- HELPERS DE DADOS
--------------------------------------------------------------------------------

countFreq :: Dado -> Tiro -> Int
countFreq x = length . filter (== x)

count :: Int -> Tiro -> Int
count x = length . filter (== x)

hasNOfKind :: Int -> Tiro -> Bool
hasNOfKind n dados =
  any (>= n) [count x dados | x <- [1..6]]

hasStraightPotential :: Tiro -> Bool
hasStraightPotential dados =
  length (uniq dados) >= 4

contributesToStraight :: Dado -> Tiro -> Bool
contributesToStraight d dados =
  any (\s -> d `elem` s && length (filter (`elem` s) dados) >= 3)
      [[1..5], [2..6]]

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) = x : uniq (filter (/= x) xs)

--------------------------------------------------------------------------------
-- IA: ELECCIÓN DE COMBINACIÓN (ESTRATÉGICA)
--------------------------------------------------------------------------------

aiChooseCombo :: GameState -> Combinacion
aiChooseCombo st =
  let player    = jugadores st !! turnoActual st
      available = M.findWithDefault [] player (combinacionesDisponibles st)
      dados     = dadosActuales st
      scored    = [(c, comboScore c dados) | c <- available]
      sorted    = sortBy (comparing (Down . snd)) scored
  in fst (head sorted)

-- Puntaje ponderado de una combinación
comboScore :: Combinacion -> Tiro -> Int
comboScore c dados =
  puntaje c dados * comboWeight c

-- Peso estratégico basado en los tipos reales
comboWeight :: Combinacion -> Int
comboWeight c = case c of
  Yatzy             -> 6
  GranEscalera      -> 5
  PequenaEscalera   -> 4
  FullHouse         -> 4
  Cuarteto          -> 4
  Trio              -> 3
  Chance            -> 2
  Unos              -> 1
  Doses             -> 1
  Treses            -> 1
  Cuatro            -> 1
  Cinco             -> 1
  Seis              -> 1

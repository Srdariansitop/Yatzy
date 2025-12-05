module Game.Logic (
    puntaje,
    hasFullHouse,
    contains,
    inicializarEstado,
    puedeTirar,
    aplicarTirada,
    conservarDados,
    elegirCombinacion
) where

import Game.Types
import System.Random (StdGen, randomR)
import Data.List (group, sort)
import qualified Data.Map as M

-- Cálculo de puntaje por combinación
puntaje :: Combinacion -> Tiro -> Puntaje
puntaje Unos dados  = sum [d | d <- dados, d == 1]
puntaje Doses dados = sum [d | d <- dados, d == 2]
puntaje Treses dados = sum [d | d <- dados, d == 3]
puntaje Cuatro dados = sum [d | d <- dados, d == 4]
puntaje Cinco dados = sum [d | d <- dados, d == 5]
puntaje Seis dados = sum [d | d <- dados, d == 6]
puntaje Trio dados = if any (\x -> count x dados >= 3) [1..6]
                     then sum dados else 0
puntaje Cuarteto dados = if any (\x -> count x dados >= 4) [1..6]
                        then sum dados else 0
puntaje FullHouse dados = if hasFullHouse dados then 25 else 0
puntaje PequenaEscalera dados = if sort dados `contains` [1,2,3,4,5] then 30 else 0
puntaje GranEscalera dados = if sort dados `contains` [2,3,4,5,6] then 40 else 0
puntaje Yatzy dados = if any (\x -> count x dados == 5) [1..6] then 50 else 0
puntaje Chance dados = sum dados

count :: Int -> Tiro -> Int
count x = length . filter (== x)

hasFullHouse :: Tiro -> Bool
hasFullHouse dados = sort (map length (group (sort dados))) == [2,3]

contains :: Eq a => [a] -> [a] -> Bool
contains xs ys = all (`elem` xs) ys

-- Inicializar estado del juego
inicializarEstado :: [Jugador] -> GameState
inicializarEstado js = GameState
  { jugadores = js
  , turnoActual = 0
  , tiradasRealizadas = 0
  , dadosActuales = []
  , dadosConservados = []
  , combinacionesDisponibles = M.fromList [(j, [minBound .. maxBound]) | j <- js]
  , puntajes = M.fromList [(j, M.empty) | j <- js]
  }

-- ¿Puede tirar de nuevo?
puedeTirar :: GameState -> Bool
puedeTirar st = tiradasRealizadas st < 3

-- Aplicar una tirada (tira N dados que faltan) devolviendo nuevo estado y nuevo generador
aplicarTirada :: StdGen -> GameState -> (StdGen, GameState)
aplicarTirada gen st
  | not (puedeTirar st) = (gen, st)
  | otherwise =
      let faltan = 5 - length (dadosConservados st)
          (nuevosDados, gen') = lanzarDados gen faltan []
          dadosTotales = dadosConservados st ++ nuevosDados
          st' = st { dadosActuales = dadosTotales
                   , tiradasRealizadas = tiradasRealizadas st + 1 }
      in (gen', st')

lanzarDados :: StdGen -> Int -> [Dado] -> ([Dado], StdGen)
lanzarDados gen 0 acc = (acc, gen)
lanzarDados gen n acc =
  let (v, gen') = randomR (1,6) gen
  in lanzarDados gen' (n-1) (v:acc)

-- Conservar dados por índice (1..5)
conservarDados :: [Int] -> GameState -> GameState
conservarDados indices st =
  let actuales = dadosActuales st
      nuevosConservados = [ actuales !! (i-1) | i <- indices, i >=1, i <= length actuales ]
  in st { dadosConservados = nuevosConservados }

-- Elegir combinación: fija puntaje, limpia dados y avanza turno
-- Ignora si la combinación no está disponible (no modifica estado)
elegirCombinacion :: Combinacion -> GameState -> GameState
elegirCombinacion c st =
  let jugadorActual = jugadores st !! turnoActual st
      disponibles = M.findWithDefault [] jugadorActual (combinacionesDisponibles st)
  in if c `elem` disponibles then
       let p = puntaje c (dadosActuales st)
           nuevasDisp = M.adjust (filter (/= c)) jugadorActual (combinacionesDisponibles st)
           nuevosScores = M.adjust (M.insert c p) jugadorActual (puntajes st)
           turnoSiguiente = (turnoActual st + 1) `mod` length (jugadores st)
           st' = st { combinacionesDisponibles = nuevasDisp
                    , puntajes = nuevosScores
                    , turnoActual = turnoSiguiente
                    , tiradasRealizadas = 0
                    , dadosActuales = []
                    , dadosConservados = [] }
       in st'
     else st

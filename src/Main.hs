module Main where

import System.Random
import Control.Monad
import Data.Char
import Data.Maybe (mapMaybe)
import Game.Types
import Game.Logic (puntaje)

-- Pedir al usuario qué dados conservar (CLI original)
pedirConservados :: [Dado] -> IO [Int]
pedirConservados dados = do
    putStrLn $ "Dados actuales: " ++ show dados
    putStrLn "Ingresa los indices (1-5) de los dados que quieres conservar, separados por espacios:"
    line <- getLine
    let indices = mapMaybe readMaybe (words line)
    return [dados !! (i-1) | i <- indices, i >=1, i <=5]

readMaybe :: String -> Maybe Int
readMaybe s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing

-- Turno interactivo (simplificado)
turno :: Jugador -> [Combinacion] -> IO (Combinacion, Puntaje)
turno jugador combinacionesRestantes = do
    putStrLn $ "\nTurno de " ++ jugador
    tiro1 <- lanzarTiros 5
    dadosGuardados1 <- pedirConservados tiro1
    let dadosParaTirar2 = 5 - length dadosGuardados1
    tiro2 <- lanzarTiros dadosParaTirar2
    let nuevosDados2 = dadosGuardados1 ++ tiro2
    dadosGuardados2 <- pedirConservados nuevosDados2
    let dadosParaTirar3 = 5 - length dadosGuardados2
    tiro3 <- lanzarTiros dadosParaTirar3
    let tiroFinal = dadosGuardados2 ++ tiro3
    putStrLn $ "Tiro final: " ++ show tiroFinal
    let elegir = do
          putStrLn "Combinaciones disponibles:"
          mapM_ print combinacionesRestantes
          putStrLn "Elige una combinación para puntuar:"
          linea <- getLine
          case readCombinacion linea of
            Just c | c `elem` combinacionesRestantes -> do
                let pts = puntaje c tiroFinal
                putStrLn $ "Puntaje: " ++ show pts
                return (c, pts)
            _ -> do
                putStrLn "Combinación inválida, intenta de nuevo."
                elegir
    elegir

lanzarDado :: IO Dado
lanzarDado = randomRIO (1,6)

lanzarTiros :: Int -> IO [Dado]
lanzarTiros n = replicateM n lanzarDado

readCombinacion :: String -> Maybe Combinacion
readCombinacion s = lookup (map toLower s) dict
  where
    dict = [("unos",Unos),("doses",Doses),("treses",Treses),("cuatro",Cuatro),("cinco",Cinco),("seis",Seis),
            ("trio",Trio),("cuarteto",Cuarteto),("fullhouse",FullHouse),("pequeñaescalera",PequenaEscalera),
            ("granescalera",GranEscalera),("yatzy",Yatzy),("chance",Chance)]

juego :: IO ()
juego = do
    let jugadores = ["Jugador1","Jugador2"]
    puntajes <- jugarRondas jugadores 13 (replicate (length jugadores) [minBound .. maxBound])
    putStrLn "\nResultados finales:"
    mapM_ print puntajes
    let ganador = fst $ maximumByCmp puntajes
    putStrLn $ "¡El ganador es " ++ ganador ++ "!"

maximumByCmp :: [(Jugador, Puntaje)] -> (Jugador, Puntaje)
maximumByCmp = foldr1 (\a@(_,pa) b@(_,pb) -> if pa >= pb then a else b)

jugarRondas :: [Jugador] -> Int -> [[Combinacion]] -> IO [(Jugador, Puntaje)]
jugarRondas _ 0 _ = return []
jugarRondas jugadores n combinacionesPorJugador
  | all null combinacionesPorJugador = return []
  | otherwise = do
      resultadosYRestantes <- forM (zip jugadores combinacionesPorJugador) $ \(jug, combs) -> do
          (c, pts) <- turno jug combs
          let combsRestantes = filter (/= c) combs
          return ((jug, pts), combsRestantes)
      let (resultados, nuevasCombinaciones) = unzip resultadosYRestantes
      siguientes <- jugarRondas jugadores (n-1) nuevasCombinaciones
      return (resultados ++ siguientes)

main :: IO ()
main = juego
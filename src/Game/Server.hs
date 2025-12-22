{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Game.Server (
  startServer
) where

import Web.Scotty
import Network.HTTP.Types.Status (status404)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
import Data.IORef
import System.Random (randomRIO)
import Game.Types
import Game.Logic (puntaje)
import Data.Aeson (FromJSON, ToJSON, toJSON, object, (.=), parseJSON)
import GHC.Generics (Generic)
import Data.Char (toLower)
import Network.Wai.Middleware.Static (staticPolicy, addBase)

-- Requests
newtype NewGameRequest = NewGameRequest { players :: [Jugador] } deriving (Show, Generic)
instance FromJSON NewGameRequest

newtype KeepRequest = KeepRequest { indices :: [Int] } deriving (Show, Generic)
instance FromJSON KeepRequest

newtype ChooseRequest = ChooseRequest { combination :: String } deriving (Show, Generic)
instance FromJSON ChooseRequest

-- Simplified view
data GameView = GameView
  { turn :: Int
  , currentPlayer :: Jugador
  , dice :: [Dado]
  , kept :: [Dado]
  , rollsUsed :: Int
  , available :: [Combinacion]
  , scores :: [(Jugador, [(Combinacion, Puntaje)])]
  } deriving (Show, Generic)

instance ToJSON GameView

stateToView :: GameState -> GameView
stateToView st =
  let jugadorActual = jugadores st !! turnoActual st
      disponibles = M.findWithDefault [] jugadorActual (combinacionesDisponibles st)
      scoresList = [(j, M.toList mp) | (j, mp) <- M.toList (puntajes st)]
    in GameView { turn = turnoActual st
          , currentPlayer = jugadorActual
          , dice = dadosActuales st
          , kept = dadosConservados st
          , rollsUsed = tiradasRealizadas st
          , available = disponibles
          , scores = scoresList }

-- Storage: (nextId, map id -> state)
startServer :: IO ()
startServer = do
  storage <- newIORef (0 :: Int, M.empty :: M.Map Int GameState)
  scotty 3000 $ do
    middleware $ staticPolicy (addBase "frontend")
    let addCORS = do
          setHeader "Access-Control-Allow-Origin" "*"
          setHeader "Access-Control-Allow-Headers" "Content-Type"
          setHeader "Access-Control-Allow-Methods" "GET,POST,OPTIONS"

    -- Serve frontend at root
    get "/" $ file "frontend/index.html"

    -- Preflight OPTIONS handlers for CORS
    options "/game" $ addCORS >> text ""
    options "/game/:id" $ addCORS >> text ""
    options "/game/:id/roll" $ addCORS >> text ""
    options "/game/:id/keep" $ addCORS >> text ""
    options "/game/:id/choose" $ addCORS >> text ""
    post "/game" $ do
      addCORS
      req <- jsonData :: ActionM NewGameRequest
      let js = players req
      let st = inicializar js
      (nid, _) <- liftIO $ atomicModifyIORef' storage $ \(i, mp) -> let ni = i + 1 in ((ni, M.insert ni st mp), (ni, ()))
      json $ object ["gameId" .= nid, "state" .= stateToView st]

    get "/game/:id" $ do
      addCORS
      gid <- param "id"
      (_, mp) <- liftIO $ readIORef storage
      case M.lookup gid mp of
        Just st -> json (stateToView st)
        Nothing -> status status404 >> json (object ["error" .= ("Juego no encontrado" :: String)])

    post "/game/:id/roll" $ do
      addCORS
      gid <- param "id"
      liftIO (modifyIORef' storage id) -- ensure atomic
      (next, mp) <- liftIO $ readIORef storage
      case M.lookup gid mp of
        Just st -> if tiradasRealizadas st >= 3
          then json (object ["error" .= ("Máximo de tiradas alcanzado" :: String)])
          else do
            let faltan = 5 - length (dadosConservados st)
            nuevos <- liftIO $ mapM (const $ randomRIO (1,6)) [1..faltan]
            let st' = st { dadosActuales = dadosConservados st ++ nuevos
                         , tiradasRealizadas = tiradasRealizadas st + 1 }
            liftIO $ atomicModifyIORef' storage $ \(ni, mp2) -> ((ni, M.insert gid st' mp2), ())
            json (stateToView st')
        Nothing -> status status404 >> json (object ["error" .= ("Juego no encontrado" :: String)])

    post "/game/:id/keep" $ do
      addCORS
      gid <- param "id"
      req <- jsonData :: ActionM KeepRequest
      (_, mp) <- liftIO $ readIORef storage
      case M.lookup gid mp of
        Just st -> do
          let actuales = dadosActuales st
              nuevosConservados = [ actuales !! (i-1) | i <- indices req, i >=1, i <= length actuales ]
              st' = st { dadosConservados = nuevosConservados }
          liftIO $ atomicModifyIORef' storage $ \(ni, mp2) -> ((ni, M.insert gid st' mp2), ())
          json (stateToView st')
        Nothing -> status status404 >> json (object ["error" .= ("Juego no encontrado" :: String)])

    post "/game/:id/choose" $ do
      addCORS
      gid <- param "id"
      req <- jsonData :: ActionM ChooseRequest
      (_, mp) <- liftIO $ readIORef storage
      case M.lookup gid mp of
        Just st -> do
          let combStr = map toLower (combination req)
              dict = [(map toLower (show v), v) | v <- [minBound .. maxBound]]
          case lookup combStr dict of
            Just c ->
              let jugadorActual = jugadores st !! turnoActual st
                  disponibles = M.findWithDefault [] jugadorActual (combinacionesDisponibles st)
              in if c `elem` disponibles then do
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
                    liftIO $ atomicModifyIORef' storage $ \(ni, mp2) -> ((ni, M.insert gid st' mp2), ())
                    json (stateToView st')
                 else json (object ["error" .= ("Combinación no disponible" :: String)])
            Nothing -> json (object ["error" .= ("Combinación inválida" :: String)])
        Nothing -> status status404 >> json (object ["error" .= ("Juego no encontrado" :: String)])

-- Initialize pure state (simplified for server)
inicializar :: [Jugador] -> GameState
inicializar js = GameState
  { jugadores = js
  , turnoActual = 0
  , tiradasRealizadas = 0
  , dadosActuales = []
  , dadosConservados = []
  , combinacionesDisponibles = M.fromList [(j, [minBound .. maxBound]) | j <- js]
  , puntajes = M.fromList [(j, M.empty) | j <- js]
  }

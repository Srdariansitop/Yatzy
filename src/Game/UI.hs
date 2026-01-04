{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Game.UI (runUI) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (StdGen, mkStdGen)
import qualified Data.Map as M
import Data.List (sortBy)
import Game.Types
import Game.Logic
import Game.AI (aiChooseDice, aiChooseCombo)

--------------------------------------------------------------------------------
-- 1. CONSTANTES DE DISEÑO
--------------------------------------------------------------------------------

windowW, windowH :: Int
windowW = 1200
windowH = 800

clrBack    = makeColor 0.11 0.12 0.13 1.0
clrPanel   = makeColor 0.15 0.16 0.18 1.0
clrPrimary = makeColor 0.50 0.40 0.90 1.0 
clrAccent  = makeColor 1.00 0.47 0.40 1.0 
clrText    = makeColor 0.90 0.90 0.92 1.0
clrSuccess = makeColor 0.30 0.80 0.50 1.0 

panelLX    = -320 
scoreRX    = 280  
gridTop    = 220
cellH      = 38
cellW      = 90

darken :: Color -> Color
darken c = case rgbaOfColor c of
  (r, g, b, a) -> makeColor (r * 0.7) (g * 0.7) (b * 0.7) a

lighten :: Color -> Color
lighten c = case rgbaOfColor c of
  (r, g, b, a) -> makeColor (min 1.0 (r * 1.3)) (min 1.0 (g * 1.3)) (min 1.0 (b * 1.3)) a

--------------------------------------------------------------------------------
-- 2. TIPOS DE DATOS
--------------------------------------------------------------------------------

data MenuState = MainMenu | PlayingGame | GameOver deriving (Eq, Show)
data AIAction = AIRolling | AIKeeping | AIChoosing | NoAction deriving (Eq, Show)

data UIButton = UIButton
  { bLabel  :: String
  , bPos    :: (Float, Float)
  , bSize   :: (Float, Float)
  , bColor  :: Color
  , bAction :: UIState -> UIState
  }

data UIState = UIState
  { gameState    :: GameState
  , rng          :: StdGen
  , selectedDice :: [Int]
  , mousePos     :: (Float, Float)
  , aiPlayers    :: M.Map Jugador Bool
  , menuState    :: MenuState
  , aiThinkTime  :: Float
  , aiAction     :: AIAction
  , aiDiceToKeep :: [Int]
  , gameWinner   :: Maybe String
  }

--------------------------------------------------------------------------------
-- 3. FUNCIONES DE UTILIDAD
--------------------------------------------------------------------------------

allCombinations :: [Combinacion]
allCombinations = [minBound .. maxBound :: Combinacion]

juegoTerminado :: GameState -> Bool
juegoTerminado st =
  let players = jugadores st
      scores = puntajes st
      totalCombinations = length allCombinations
  in all (\player -> 
          let playerScores = M.findWithDefault M.empty player scores
          in M.size playerScores >= totalCombinations) players

calcularGanador :: GameState -> Maybe String
calcularGanador st =
  let players = jugadores st
      scores = puntajes st
      playerTotals = map (\player -> 
        let playerScores = M.findWithDefault M.empty player scores
        in (player, sum $ M.elems playerScores)) players
      sorted = sortBy (\(_,a) (_,b) -> compare b a) playerTotals
  in case sorted of
    [] -> Nothing
    ((winner, maxScore):rest) -> 
      if null rest || snd (head rest) < maxScore
      then Just winner
      else Nothing

isCurrentPlayerAI :: UIState -> Bool
isCurrentPlayerAI UIState{gameState} = 
  M.findWithDefault False (jugadores gameState !! turnoActual gameState) (M.fromList [("Tu", False), ("IA", True)]) -- Fallback seguro

--------------------------------------------------------------------------------
-- 4. RENDERIZADO DE COMPONENTES
--------------------------------------------------------------------------------

drawText :: Float -> String -> Picture
drawText s str = scale s s $ color clrText $ text str

rectWithBorder :: Float -> Float -> Color -> Picture
rectWithBorder w h c = pictures
  [ color (darken c) $ rectangleSolid (w+4) (h+4)
  , color c $ rectangleSolid w h
  ]

roundedRect :: Float -> Float -> Float -> Picture
roundedRect w h r = pictures 
  [ rectangleSolid (w - 2*r) h
  , rectangleSolid w (h - 2*r)
  , translate (w/2-r) (h/2-r)   $ circleSolid r
  , translate (-w/2+r) (h/2-r)  $ circleSolid r
  , translate (w/2-r) (-h/2+r)  $ circleSolid r
  , translate (-w/2+r) (-h/2+r) $ circleSolid r
  ]

drawDie :: [Int] -> Int -> Int -> Picture
drawDie selected idx val =
  let isSelected = idx `elem` selected
      x = fromIntegral idx * 75 
      bg = if isSelected then clrAccent else white
  in translate x 0 $ pictures
       [ color (greyN 0.2) $ roundedRect 54 54 8
       , color bg $ roundedRect 50 50 8
       , color clrBack $ drawPips val
       ]

drawPips :: Int -> Picture
drawPips n = pictures $ map drawPip (pipCoords n)
  where
    drawPip (px, py) = translate px py $ circleSolid 4
    d = 12 
    pipCoords 1 = [(0,0)]
    pipCoords 2 = [(-d, d), (d, -d)]
    pipCoords 3 = [(0,0), (-d, d), (d, -d)]
    pipCoords 4 = [(-d, d), (d, d), (-d, -d), (d, -d)]
    pipCoords 5 = [(0,0), (-d, d), (d, d), (-d, -d), (d, -d)]
    pipCoords 6 = [(-d, d), (d, d), (-d, -d), (d, -d), (-d, 0), (d, 0)]
    pipCoords _ = []

--------------------------------------------------------------------------------
-- 5. LÓGICA DE BOTONES
--------------------------------------------------------------------------------

isInside :: (Float, Float) -> UIButton -> Bool
isInside (mx, my) UIButton{bPos=(bx, by), bSize=(bw, bh)} =
  mx >= bx - bw/2 && mx <= bx + bw/2 &&
  my >= by - bh/2 && my <= by + bh/2

activeButtons :: UIState -> [UIButton]
activeButtons ui@UIState{menuState} = case menuState of
  MainMenu ->
    [ UIButton "2 JUGADORES" (-220, -50) (300, 80) clrPrimary 
        (\s -> s { menuState = PlayingGame, aiPlayers = M.fromList [("P1", False), ("P2", False)], 
                   gameState = inicializarEstado ["P1", "P2"], gameWinner = Nothing })
    , UIButton "CONTRA IA"   (220, -50) (300, 80) clrAccent 
        (\s -> s { menuState = PlayingGame, aiPlayers = M.fromList [("Tu", False), ("IA", True)], 
                   gameState = inicializarEstado ["Tu", "IA"], gameWinner = Nothing })
    ]
  
  PlayingGame -> 
    if not (isCurrentPlayerAI ui) 
    then [ UIButton "TIRAR DADOS" (panelLX - 100, -100) (180, 50) clrPrimary ejecutarTiradaBtn
         , UIButton "CONSERVAR"   (panelLX + 100, -100) (180, 50) clrSuccess ejecutarConservarBtn
         ]
    else []
  
  GameOver ->
    [ UIButton "VOLVER AL MENU" (0, -180) (350, 70) clrPrimary 
        (\s -> s { menuState = MainMenu, gameState = inicializarEstado ["P1", "P2"], 
                   gameWinner = Nothing, selectedDice = [], aiAction = NoAction })
    , UIButton "CERRAR JUEGO" (0, -270) (350, 70) clrAccent 
        (\_ -> error "Juego cerrado")
    ]

ejecutarTiradaBtn :: UIState -> UIState
ejecutarTiradaBtn ui@UIState{gameState, rng} 
  | puedeTirar gameState = let (rng', st') = aplicarTirada rng gameState in ui { gameState = st', rng = rng', selectedDice = [] }
  | otherwise = ui

ejecutarConservarBtn :: UIState -> UIState
ejecutarConservarBtn ui@UIState{gameState, selectedDice}
  | not (null $ dadosActuales gameState) = ui { gameState = conservarDados (map (+1) selectedDice) gameState, selectedDice = [] }
  | otherwise = ui

--------------------------------------------------------------------------------
-- 6. UPDATE Y MOTOR IA
--------------------------------------------------------------------------------

update :: Float -> UIState -> UIState
update dt ui@UIState{gameState, menuState, aiThinkTime}
  | menuState == GameOver = ui
  | juegoTerminado gameState = 
      let winner = calcularGanador gameState
      in ui { menuState = GameOver, gameWinner = winner, aiThinkTime = 0, aiAction = NoAction }
  | menuState == MainMenu = ui
  | not (isCurrentPlayerAI ui) = ui { aiThinkTime = 0, aiAction = NoAction }
  | otherwise = 
      let nuevoTiempo = aiThinkTime + dt
      in if nuevoTiempo < 1.0 
         then ui { aiThinkTime = nuevoTiempo }
         else actuarIA ui

actuarIA :: UIState -> UIState
actuarIA ui@UIState{gameState, rng, aiAction} =
  let tiradas = tiradasRealizadas gameState
      dados   = dadosActuales gameState
  in case aiAction of
    NoAction -> 
      if tiradas == 0 
      then ejecutarTiradaBtn ui { aiAction = AIRolling, aiThinkTime = 0 }
      else ui { aiAction = AIKeeping, aiDiceToKeep = aiChooseDice dados, aiThinkTime = 0 }
    
    AIKeeping ->
      let indicesAconservar = map (+1) (aiDiceToKeep ui)
          ui' = ui { gameState = conservarDados indicesAconservar gameState 
                   , aiAction = if tiradas < 3 then AIRolling else AIChoosing
                   , aiThinkTime = 0 }
      in ui'

    AIRolling ->
      if tiradas < 3
      then let (rng', st') = aplicarTirada rng gameState
           in ui { gameState = st', rng = rng', aiAction = NoAction, aiThinkTime = 0 }
      else ui { aiAction = AIChoosing, aiThinkTime = 0 }

    AIChoosing ->
      let mejorCombo = aiChooseCombo gameState
      in ui { gameState = elegirCombinacion mejorCombo gameState, aiAction = NoAction, aiThinkTime = 0 }

--------------------------------------------------------------------------------
-- 7. RENDERIZADO
--------------------------------------------------------------------------------

render :: UIState -> Picture
render ui@UIState{menuState} = pictures $ 
  [ color clrBack $ rectangleSolid 1200 800 ] ++ 
  [ case menuState of
      MainMenu    -> renderMainMenu ui
      PlayingGame -> renderGame ui
      GameOver    -> pictures [renderGame ui, renderGameOver ui]
  ] ++ (map (drawButton ui) (activeButtons ui))

drawButton :: UIState -> UIButton -> Picture
drawButton UIState{mousePos} btn@UIButton{bPos, bSize, bColor, bLabel} =
  let hover = isInside mousePos btn
      c = if hover then lighten bColor else bColor
  in translate (fst bPos) (snd bPos) $ pictures
       [ rectWithBorder (fst bSize) (snd bSize) c
       , translate (- (fst bSize / 2) + 20) (-10) $ drawText 0.15 bLabel
       ]

renderMainMenu :: UIState -> Picture
renderMainMenu _ = pictures 
  [ translate (-100) 200 $ drawText 0.6 "YATZI"
  , translate (-120) 100 $ drawText 0.15 "Selecciona modo de juego"
  ]

renderGameOver :: UIState -> Picture
renderGameOver ui@UIState{gameWinner} = 
  let winnerText = case gameWinner of
        Just name -> "¡GANADOR: " ++ name ++ "!"
        Nothing -> "¡EMPATE!"
  in pictures
      [ color (makeColor 0 0 0 0.8) $ rectangleSolid 1200 800
      , translate 0 50 $ rectWithBorder 500 600 clrPanel
      , translate (-180) 280 $ color clrSuccess $ drawText 0.35 winnerText
      , translate (-150) 200 $ color clrText $ drawText 0.15 "PUNTUACIONES FINALES:"
      , translate 0 30 $ renderFinalScores ui
      ]

renderFinalScores :: UIState -> Picture
renderFinalScores UIState{gameState} =
  let players = jugadores gameState
      scores = puntajes gameState
      playerTotals = map (\player -> 
        let playerScores = M.findWithDefault M.empty player scores
            total = sum $ M.elems playerScores
        in (player, total)) players
      sorted = sortBy (\(_,a) (_,b) -> compare b a) playerTotals
  in pictures $ 
      zipWith (\(player, score) i ->
        let y = 50 - fromIntegral i * 60
            color' = if i == 0 then clrSuccess else clrText
        in pictures
            [ translate (-150) y $ color color' $ drawText 0.20 player
            , translate 100 y $ color color' $ drawText 0.20 (show score)
            ]) sorted [0..]

renderGame :: UIState -> Picture
renderGame ui@UIState{gameState, aiAction, menuState} = pictures
  [ translate panelLX 150 $ rectWithBorder 450 450 clrPanel
  , translate (panelLX - 180) 330 $ drawText 0.2 $ "Turno: " ++ (jugadores gameState !! turnoActual gameState)
  , translate (panelLX - 180) 290 $ drawText 0.12 $ "Tiradas: " ++ show (tiradasRealizadas gameState)
  , translate panelLX 150 $ renderDiceSection ui
  , translate scoreRX 0 $ renderScoreboard ui
  , if isCurrentPlayerAI ui && menuState == PlayingGame 
    then translate panelLX (-280) $ drawText 0.15 ("IA pensando: " ++ show aiAction) 
    else blank
  ]

renderDiceSection :: UIState -> Picture
renderDiceSection UIState{gameState, selectedDice} = pictures 
  [ translate (-200) 60 $ drawText 0.12 "DISPONIBLES:"
  , translate (-160) 0 $ pictures $ zipWith (drawDie selectedDice) [0..] (dadosActuales gameState)
  , translate (-200) (-80) $ drawText 0.12 "CONSERVADOS:"
  , translate (-160) (-140) $ pictures $ zipWith (drawDie []) [0..] (dadosConservados gameState)
  ]

renderScoreboard :: UIState -> Picture
renderScoreboard UIState{gameState} = 
  let combos = allCombinations
      players = jugadores gameState
  in pictures $
    [ rectWithBorder 520 700 clrPanel
    , translate (-30) 275 $ drawText 0.12 (players !! 0)
    , translate 70 275  $ drawText 0.12 (if length players > 1 then players !! 1 else "")
    ] ++ zipWith (drawScoreRow gameState) [0..] combos

drawScoreRow :: GameState -> Int -> Combinacion -> Picture
drawScoreRow st row combo =
  let y = gridTop - (fromIntegral row * cellH)
      p1 = jugadores st !! 0
      p2 = if length (jugadores st) > 1 then jugadores st !! 1 else ""
      sc1 = M.lookup combo (M.findWithDefault M.empty p1 (puntajes st))
      sc2 = if p2 /= "" then M.lookup combo (M.findWithDefault M.empty p2 (puntajes st)) else Nothing
      c1 = if turnoActual st == 0 && sc1 == Nothing then clrPrimary else clrBack
      c2 = if turnoActual st == 1 && sc2 == Nothing then clrPrimary else clrBack
  in translate 0 y $ pictures
       [ translate (-240) (-10) $ drawText 0.09 (show combo)
       , translate (-30) 0 $ scoreCell sc1 c1
       , translate 70 0  $ scoreCell sc2 c2
       ]
  where scoreCell val c = pictures 
          [ color c $ rectangleSolid 80 (cellH-4)
          , color white $ rectangleWire 80 (cellH-4)
          , translate (-10) (-10) $ drawText 0.1 (maybe "-" show val) 
          ]

--------------------------------------------------------------------------------
-- 8. EVENTOS Y MAIN
--------------------------------------------------------------------------------

handleEvent :: Event -> UIState -> UIState
handleEvent (EventMotion pos) ui = ui { mousePos = pos }
handleEvent (EventKey (MouseButton LeftButton) Down _ pos) ui = 
  let buttons = activeButtons ui
      clicked = filter (isInside pos) buttons
  in if not (null clicked) 
     then (bAction (head clicked)) ui 
     else if (menuState ui) == PlayingGame then handleGridAndDice pos ui else ui
handleEvent _ ui = ui

handleGridAndDice :: (Float, Float) -> UIState -> UIState
handleGridAndDice (mx, my) ui@UIState{gameState, selectedDice}
  | my > 120 && my < 180 && mx > (panelLX - 190) && mx < (panelLX + 250) =
      let idx = floor ((mx - (panelLX - 190)) / 75)
      in if idx >= 0 && idx < length (dadosActuales gameState)
         then ui { selectedDice = if idx `elem` selectedDice then filter (/= idx) selectedDice else idx : selectedDice } else ui
  | mx > (scoreRX - 150) && mx < (scoreRX + 150) && not (isCurrentPlayerAI ui) =
      let row = floor ((gridTop + (cellH/2) - my) / cellH)
          combos = allCombinations
      in if row >= 0 && row < length combos then ui { gameState = elegirCombinacion (combos !! row) gameState } else ui
  | otherwise = ui

initialState :: UIState
initialState = UIState 
  (inicializarEstado ["Tu", "IA"]) 
  (mkStdGen 42) 
  [] 
  (0,0) 
  (M.fromList [("Tu", False), ("IA", True)]) 
  MainMenu 
  0 
  NoAction 
  [] 
  Nothing

runUI :: IO ()
runUI = play (InWindow "Yatzi Pro" (windowW, windowH) (50, 50)) clrBack 30 initialState render handleEvent update
{-# LANGUAGE RecordWildCards #-}
module Game.UI (runUI) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (StdGen, mkStdGen)
import qualified Data.Map as M
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
  }

--------------------------------------------------------------------------------
-- 3. RENDERIZADO DE COMPONENTES
--------------------------------------------------------------------------------

drawText :: Float -> String -> Picture
drawText s str = scale s s $ color clrText $ text str

rectWithBorder :: Float -> Float -> Color -> Picture
rectWithBorder w h c = pictures
  [ color (dark c) $ rectangleSolid (w+4) (h+4)
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
-- 4. LOGICA DE BOTONES Y EVENTOS
--------------------------------------------------------------------------------

isInside :: (Float, Float) -> UIButton -> Bool
isInside (mx, my) UIButton{bPos=(bx, by), bSize=(bw, bh)} =
  mx >= bx - bw/2 && mx <= bx + bw/2 &&
  my >= by - bh/2 && my <= by + bh/2

activeButtons :: UIState -> [UIButton]
activeButtons ui@UIState{..} = case menuState of
  MainMenu ->
    [ UIButton "2 JUGADORES" (-200, 0) (300, 80) clrPrimary 
        (\s -> s { menuState = PlayingGame, aiPlayers = M.fromList [("P1", False), ("P2", False)], gameState = inicializarEstado ["P1", "P2"] })
    , UIButton "CONTRA IA"   (200, 0) (300, 80) clrAccent 
        (\s -> s { menuState = PlayingGame, aiPlayers = M.fromList [("Tú", False), ("IA", True)], gameState = inicializarEstado ["Tú", "IA"] })
    ]
  PlayingGame -> 
    if not (isCurrentPlayerAI ui) 
    then [ UIButton "TIRAR DADOS" (panelLX - 100, -100) (180, 50) clrPrimary ejecutarTiradaBtn
         , UIButton "CONSERVAR"   (panelLX + 100, -100) (180, 50) clrSuccess ejecutarConservarBtn
         ]
    else []
  GameOver ->
    [ UIButton "VOLVER AL MENU" (0, -150) (350, 70) clrPrimary (\_ -> initialState) ]

ejecutarTiradaBtn ui@UIState{..} 
  | puedeTirar gameState = let (rng', st') = aplicarTirada rng gameState in ui { gameState = st', rng = rng', selectedDice = [] }
  | otherwise = ui

ejecutarConservarBtn ui@UIState{..}
  | not (null $ dadosActuales gameState) = ui { gameState = conservarDados (map (+1) selectedDice) gameState, selectedDice = [] }
  | otherwise = ui

--------------------------------------------------------------------------------
-- 5. UPDATE (MOTOR DE LA IA)
--------------------------------------------------------------------------------

update :: Float -> UIState -> UIState
update dt ui@UIState{..}
  | menuState /= PlayingGame || not (isCurrentPlayerAI ui) = ui { aiThinkTime = 0, aiAction = NoAction }
  | otherwise = 
      let nuevoTiempo = aiThinkTime + dt
      in if nuevoTiempo < 1.0 
         then ui { aiThinkTime = nuevoTiempo }
         else actuarIA ui

actuarIA :: UIState -> UIState
actuarIA ui@UIState{..} =
  let tiradas = tiradasRealizadas gameState
      dados   = dadosActuales gameState
  in case aiAction of
    NoAction -> 
      if tiradas == 0 
      then ejecutarTiradaBtn ui { aiAction = AIRolling, aiThinkTime = 0 }
      else ui { aiAction = AIKeeping, aiDiceToKeep = aiChooseDice dados, aiThinkTime = 0 }
    
    AIKeeping ->
      let indicesAconservar = map (+1) aiDiceToKeep
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
-- 6. RENDERIZADO GENERAL
--------------------------------------------------------------------------------

render :: UIState -> Picture
render ui@UIState{..} = pictures $ 
  [ color clrBack $ rectangleSolid 1200 800 ] ++ 
  (map (drawButton ui) (activeButtons ui)) ++
  [ case menuState of
      MainMenu    -> renderMainMenu
      PlayingGame -> renderGame ui
      GameOver    -> renderGameOver ui
  ]

drawButton :: UIState -> UIButton -> Picture
drawButton UIState{..} btn@UIButton{..} =
  let hover = isInside mousePos btn
      c = if hover then light bColor else bColor
  in translate (fst bPos) (snd bPos) $ pictures
       [ rectWithBorder (fst bSize) (snd bSize) c
       , translate (- (fst bSize / 2) + 20) (-10) $ drawText 0.15 bLabel
       ]

renderMainMenu = pictures [ translate 0 200 $ drawText 0.5 "YATZI", translate (-150) 120 $ drawText 0.15 "Selecciona modo" ]

renderGameOver UIState{gameState=st} = translate (-200) 200 $ drawText 0.3 "FIN DEL JUEGO"

renderGame ui@UIState{..} = pictures
  [ translate panelLX 150 $ rectWithBorder 450 450 clrPanel
  , translate (panelLX - 180) 330 $ drawText 0.2 $ "Turno: " ++ (jugadores gameState !! turnoActual gameState)
  , translate (panelLX - 180) 290 $ drawText 0.12 $ "Tiradas: " ++ show (tiradasRealizadas gameState)
  , translate panelLX 150 $ renderDiceSection ui
  , translate scoreRX 0 $ renderScoreboard ui
  , if isCurrentPlayerAI ui then translate panelLX (-280) $ drawText 0.15 ("IA: " ++ show aiAction) else blank
  ]

renderDiceSection UIState{..} = pictures 
  [ translate (-200) 60 $ drawText 0.12 "DISPONIBLES:", translate (-160) 0 $ pictures $ zipWith (drawDie selectedDice) [0..] (dadosActuales gameState)
  , translate (-200) (-80) $ drawText 0.12 "CONSERVADOS:", translate (-160) (-140) $ pictures $ zipWith (drawDie []) [0..] (dadosConservados gameState)
  ]

renderScoreboard UIState{..} = 
  let combos = [minBound .. maxBound] :: [Combinacion]
      players = jugadores gameState
  in pictures $
    [ rectWithBorder 520 700 clrPanel
    , translate (-30) 275 $ drawText 0.12 (players !! 0)
    , translate 70 275  $ drawText 0.12 (players !! 1)
    ] ++ zipWith (drawScoreRow gameState) [0..] combos

drawScoreRow st row combo =
  let y = gridTop - (fromIntegral row * cellH)
      p1 = jugadores st !! 0
      p2 = jugadores st !! 1
      sc1 = M.lookup combo (M.findWithDefault M.empty p1 (puntajes st))
      sc2 = M.lookup combo (M.findWithDefault M.empty p2 (puntajes st))
      c1 = if turnoActual st == 0 && sc1 == Nothing then clrPrimary else clrBack
      c2 = if turnoActual st == 1 && sc2 == Nothing then clrPrimary else clrBack
  in translate 0 y $ pictures
       [ translate (-240) (-10) $ drawText 0.09 (show combo)
       , translate (-30) 0 $ scoreCell sc1 c1
       , translate 70 0  $ scoreCell sc2 c2
       ]
  where scoreCell val c = pictures [ color c $ rectangleSolid 80 (cellH-4), color white $ rectangleWire 80 (cellH-4), translate (-10) (-10) $ drawText 0.1 (maybe "-" show val) ]

--------------------------------------------------------------------------------
-- 7. EVENTOS Y MAIN
--------------------------------------------------------------------------------

handleEvent (EventMotion pos) ui = ui { mousePos = pos }
handleEvent (EventKey (MouseButton LeftButton) Down _ pos) ui = 
  let buttons = activeButtons ui
      clicked = filter (isInside pos) buttons
  in if not (null clicked) then (bAction (head clicked)) ui else handleGridAndDice pos ui
handleEvent _ ui = ui

handleGridAndDice (mx, my) ui@UIState{..}
  | my > 120 && my < 180 && mx > (panelLX - 190) && mx < (panelLX + 250) =
      let idx = floor ((mx - (panelLX - 190)) / 75)
      in if idx >= 0 && idx < length (dadosActuales gameState)
         then ui { selectedDice = if idx `elem` selectedDice then filter (/= idx) selectedDice else idx : selectedDice } else ui
  | mx > (scoreRX - 150) && mx < (scoreRX + 150) && not (isCurrentPlayerAI ui) =
      let row = floor ((gridTop + (cellH/2) - my) / cellH)
          combos = [minBound .. maxBound] :: [Combinacion]
      in if row >= 0 && row < length combos then ui { gameState = elegirCombinacion (combos !! row) gameState } else ui
  | otherwise = ui

initialState = UIState (inicializarEstado ["Tú", "IA"]) (mkStdGen 42) [] (0,0) (M.fromList [("Tú", False), ("IA", True)]) MainMenu 0 NoAction []

runUI = play (InWindow "Yatzi Pro" (windowW, windowH) (50, 50)) clrBack 30 initialState render handleEvent update

isCurrentPlayerAI UIState{..} = M.findWithDefault False (jugadores gameState !! turnoActual gameState) aiPlayers
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
-- 1. CONSTANTES DE DISEÑO (LAYOUT)
--------------------------------------------------------------------------------

windowW, windowH :: Int
windowW = 1200
windowH = 800

-- Colores tipo "Dracula Theme" / Moderno
clrBack     = makeColor 0.11 0.12 0.13 1.0
clrPanel    = makeColor 0.15 0.16 0.18 1.0
clrPrimary  = makeColor 0.50 0.40 0.90 1.0 -- Morado
clrAccent   = makeColor 1.00 0.47 0.40 1.0 -- Coral
clrText     = makeColor 0.90 0.90 0.92 1.0
clrSuccess  = makeColor 0.30 0.80 0.50 1.0 -- Verde

-- Posiciones
panelLX     = -320 -- Centro panel izquierdo
scoreRX     = 280  -- Centro panel derecho
gridTop     = 220
cellH       = 38
cellW       = 90

--------------------------------------------------------------------------------
-- 2. TIPOS DE DATOS
--------------------------------------------------------------------------------

-- Añade Show aquí --------------------------vvvv
data MenuState = MainMenu | PlayingGame | GameOver deriving (Eq, Show)

-- Y también aquí -----------------------------------vvvv
data AIAction = AIRolling | AIKeeping | AIChoosing | NoAction deriving (Eq, Show)

-- Abstracción de Botón para colisiones perfectas
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
-- 3. UTILIDADES DE RENDERIZADO Y COLISIÓN
--------------------------------------------------------------------------------

-- Texto centrado escalado
drawText :: Float -> String -> Picture
drawText s str = scale s s $ color clrText $ text str

-- Rectángulo con borde (estilo botón)
rectWithBorder :: Float -> Float -> Color -> Picture
rectWithBorder w h c = pictures
  [ color (dark c) $ rectangleSolid (w+4) (h+4)
  , color c $ rectangleSolid w h
  ]

-- Comprobar si un punto está dentro de un botón
isInside :: (Float, Float) -> UIButton -> Bool
isInside (mx, my) UIButton{bPos=(bx, by), bSize=(bw, bh)} =
  mx >= bx - bw/2 && mx <= bx + bw/2 &&
  my >= by - bh/2 && my <= by + bh/2

--------------------------------------------------------------------------------
-- 4. LOGICA DE BOTONES
--------------------------------------------------------------------------------

-- Definimos los botones según el estado actual
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
    [ UIButton "VOLVER AL MENU" (0, -150) (350, 70) clrPrimary (\s -> initialState) ]

-- Acciones de botones
ejecutarTiradaBtn :: UIState -> UIState
ejecutarTiradaBtn ui@UIState{..} 
  | puedeTirar gameState = let (rng', st') = aplicarTirada rng gameState in ui { gameState = st', rng = rng', selectedDice = [] }
  | otherwise = ui

ejecutarConservarBtn :: UIState -> UIState
ejecutarConservarBtn ui@UIState{..}
  | not (null $ dadosActuales gameState) = ui { gameState = conservarDados (map (+1) selectedDice) gameState, selectedDice = [] }
  | otherwise = ui

--------------------------------------------------------------------------------
-- 5. RENDERIZADO
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

renderMainMenu :: Picture
renderMainMenu = pictures
  [ translate 0 200 $ drawText 0.5 "YATZI"
  , translate (-150) 120 $ drawText 0.15 "Selecciona modo de juego"
  ]

renderGameOver :: UIState -> Picture
renderGameOver UIState{gameState=st} = pictures
  [ translate (-200) 200 $ drawText 0.3 "FIN DEL JUEGO"
  -- Aquí iría el cálculo del ganador
  ]

renderGame :: UIState -> Picture
renderGame ui@UIState{..} = pictures
  [ -- Panel Izquierdo (Dados y Acción)
    translate panelLX 150 $ rectWithBorder 450 450 clrPanel
  , translate (panelLX - 180) 330 $ drawText 0.2 $ "Turno: " ++ (jugadores gameState !! turnoActual gameState)
  , translate (panelLX - 180) 290 $ drawText 0.12 $ "Tiradas: " ++ show (tiradasRealizadas gameState) ++ "/3"
  
  -- Dados
  , translate panelLX 150 $ renderDiceSection ui
  
  -- Scoreboard (Derecha)
  , translate scoreRX 0 $ renderScoreboard ui
  
  -- AI Status
  , if isCurrentPlayerAI ui then translate panelLX (-280) $ renderAIStatus ui else blank
  ]

renderDiceSection :: UIState -> Picture
renderDiceSection UIState{..} = 
  let dados = dadosActuales gameState
      conservados = dadosConservados gameState
  in pictures 
    [ translate (-200) 50  $ drawText 0.12 "DISPONIBLES (Click para elegir):"
    , translate (-180) 0   $ pictures $ zipWith (drawDie selectedDice) [0..] dados
    , translate (-200) (-80) $ drawText 0.12 "CONSERVADOS:"
    , translate (-180) (-130) $ pictures $ zipWith (drawDie []) [0..] conservados
    ]

drawDie :: [Int] -> Int -> Int -> Picture
drawDie selected idx val =
  let isSelected = idx `elem` selected
      x = fromIntegral idx * 60
      c = if isSelected then clrAccent else white
  in translate x 0 $ pictures
       [ color (greyN 0.2) $ rectangleSolid 50 50
       , color c $ rectangleSolid 46 46
       , color clrBack $ scale 0.15 0.15 $ translate (-80) (-80) $ text (show val)
       ]

renderScoreboard :: UIState -> Picture
renderScoreboard UIState{..} = 
  let combos = [minBound .. maxBound] :: [Combinacion]
  in pictures $
    [ rectWithBorder 550 700 clrPanel
    , translate (-220) 280 $ drawText 0.15 "COMBINACIONES"
    ] ++ zipWith (drawScoreRow gameState) [0..] combos

drawScoreRow :: GameState -> Int -> Combinacion -> Picture
drawScoreRow st row combo =
  let y = gridTop - (fromIntegral row * cellH)
      p1 = jugadores st !! 0
      p2 = jugadores st !! 1
      sc1 = M.findWithDefault 0 combo (M.findWithDefault M.empty p1 (puntajes st))
      has1 = M.member combo (M.findWithDefault M.empty p1 (puntajes st))
  in translate 0 y $ pictures
       [ translate (-250) 0 $ drawText 0.1 (show combo)
       , translate 0 0 $ cell (if has1 then show sc1 else "-") (if turnoActual st == 0 then clrPrimary else clrBack)
       ]
  where cell txt c = pictures [ color c $ rectangleWire cellW cellH, translate (-10) (-10) $ drawText 0.1 txt ]

renderAIStatus :: UIState -> Picture
renderAIStatus UIState{..} = pictures
  [ color clrAccent $ rectangleWire 400 50
  , translate (-150) (-10) $ drawText 0.12 $ "IA: " ++ show aiAction
  ]

--------------------------------------------------------------------------------
-- 6. EVENTOS Y UPDATE (Colliders Corregidos)
--------------------------------------------------------------------------------

handleEvent :: Event -> UIState -> UIState
handleEvent (EventMotion pos) ui = ui { mousePos = pos }
handleEvent (EventKey (MouseButton LeftButton) Down _ pos) ui = 
  let buttons = activeButtons ui
      clicked = filter (isInside pos) buttons
  in if not (null clicked)
     then (bAction (head clicked)) ui
     else handleGridAndDice pos ui
handleEvent _ ui = ui

handleGridAndDice :: (Float, Float) -> UIState -> UIState
handleGridAndDice (mx, my) ui@UIState{..}
  -- Click en Dados (Panel Izquierdo)
  | my > 120 && my < 180 && mx > (panelLX - 210) && mx < (panelLX + 100) =
      let idx = floor ((mx - (panelLX - 210)) / 60)
      in if idx >= 0 && idx < length (dadosActuales gameState)
         then ui { selectedDice = if idx `elem` selectedDice then filter (/= idx) selectedDice else idx : selectedDice }
         else ui
  -- Click en Scoreboard (Elegir combinación)
  | mx > scoreRX - 50 && mx < scoreRX + 50 && not (isCurrentPlayerAI ui) =
      let row = floor ((gridTop + (cellH/2) - my) / cellH)
          combos = [minBound .. maxBound] :: [Combinacion]
      in if row >= 0 && row < length combos 
         then ui { gameState = elegirCombinacion (combos !! row) gameState }
         else ui
  | otherwise = ui

update :: Float -> UIState -> UIState
-- (Aquí mantén tu lógica de update de la IA que ya tenías, solo ajusta los estados)
update dt ui = ui -- Simplificado para el ejemplo, integrar con tu lógica de IA

initialState :: UIState
initialState = UIState
  { gameState = inicializarEstado ["Jugador", "IA"]
  , rng = mkStdGen 42
  , selectedDice = []
  , mousePos = (0, 0)
  , aiPlayers = M.fromList [("Jugador", False), ("IA", True)]
  , menuState = MainMenu
  , aiThinkTime = 0
  , aiAction = NoAction
  , aiDiceToKeep = []
  }

runUI :: IO ()
runUI = play (InWindow "Yatzi Pro" (windowW, windowH) (50, 50)) clrBack 30 initialState render handleEvent update

isCurrentPlayerAI :: UIState -> Bool
isCurrentPlayerAI UIState{..} = 
  let p = jugadores gameState !! turnoActual gameState
  in M.findWithDefault False p aiPlayers
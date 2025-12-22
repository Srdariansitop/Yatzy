{-# LANGUAGE RecordWildCards #-}
module Game.UI (runUI) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (StdGen, mkStdGen, split)
import qualified Data.Map as M
import Game.Types
import Game.Logic
import Game.AI (aiChooseDice, aiChooseCombo)

-- Menú state
data MenuState = MainMenu | PlayingGame | GameOver
  deriving (Eq)

-- Estado de la IA
data AIAction = AIRolling | AIKeeping | AIChoosing | NoAction
  deriving (Eq)

centerText :: Float -> String -> Picture
centerText size str =
  let w = fromIntegral (length str) * size * 60
      h = size * 60
  in translate (-w / 2) (-h / 2) $
       scale size size $ text str

inside :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool
inside (mx,my) (x,y) (w,h) =
  mx >= x - w/2 && mx <= x + w/2 &&
  my >= y - h/2 && my <= y + h/2

-- UI State
data UIState = UIState
  { gameState :: GameState
  , rng :: StdGen
  , selectedDice :: [Int]
  , hoveredCombo :: Maybe Combinacion
  , mousePos :: (Float, Float)
  , aiPlayers :: M.Map Jugador Bool  -- True si es IA
  , menuState :: MenuState
  , aiThinkTime :: Float  -- Contador para delay de IA
  , aiAction :: AIAction  -- Acción actual de la IA
  , aiDiceToKeep :: [Int]  -- Dados que la IA va a conservar
  }

-- Colores inspirados en el diseño web
bgColor, cardBg, primaryColor, secondaryColor, scoreboardBg :: Color
bgColor = makeColor 0.05 0.06 0.08 1.0
cardBg = makeColor 0.08 0.09 0.11 1.0
primaryColor = makeColor 0.36 0.55 1.0 1.0
secondaryColor = makeColor 0.06 0.09 0.15 1.0
scoreboardBg = makeColor 0.95 0.90 0.81 1.0

rollColor, scoreColor, labelColor :: Color
rollColor = makeColor 0.85 0.44 0.34 1.0
scoreColor = makeColor 0.50 0.69 0.77 1.0
labelColor = makeColor 0.94 0.88 0.76 1.0

-- Inicializar UI
initialState :: UIState
initialState = UIState
  { gameState = inicializarEstado ["Jugador 1", "Jugador 2"]
  , rng = mkStdGen 42
  , selectedDice = []
  , hoveredCombo = Nothing
  , mousePos = (0, 0)
  , aiPlayers = M.fromList [("Jugador 1", False), ("Jugador 2", False)]
  , menuState = MainMenu
  , aiThinkTime = 0
  , aiAction = NoAction
  , aiDiceToKeep = []
  }

-- Renderizado principal
render :: UIState -> Picture
render ui@UIState{menuState=menu} = case menu of
  MainMenu -> renderMainMenu ui
  PlayingGame -> renderGame ui
  GameOver -> renderGameOver ui

renderMainMenu :: UIState -> Picture
renderMainMenu _ = pictures
  [ color bgColor $ rectangleSolid 1200 800
  , translate 0 150 $ scale 0.3 0.3 $ color primaryColor $ centerText 1 "YATZI"
  , translate 0 50 $ scale 0.15 0.15 $ color white $ centerText 1 "Elige modo de juego"
  , translate (-250) (-50) $ renderMenuButton "2 JUGADORES" primaryColor
  , translate 250 (-50) $ renderMenuButton "VS IA" rollColor
  ]

renderMenuButton :: String -> Color -> Picture
renderMenuButton label clr = pictures
  [ color clr $ rectangleSolid 300 80
  , color white $ centerText 0.15 label
  ]

renderGameOver :: UIState -> Picture
renderGameOver UIState{gameState=st} = pictures
  [ color bgColor $ rectangleSolid 1200 800
  , translate 0 200 $ scale 0.25 0.25 $ color primaryColor $ centerText 1 "JUEGO TERMINADO"
  , translate 0 80 $ scale 0.15 0.15 $ color scoreColor $ centerText 1 ("Ganador: " ++ winner)
  , translate 0 0 $ scale 0.12 0.12 $ color white $ centerText 1 scoreText
  , translate 0 (-100) $ renderMenuButton "VOLVER AL MENÚ" primaryColor
  ]
  where
    players = jugadores st
    scoreMap = puntajes st
    scores = [(p, sum (M.elems (M.findWithDefault M.empty p scoreMap))) | p <- players]
    winner = fst $ maximum scores
    scoreText = unlines [p ++ ": " ++ show pts | (p, pts) <- scores]

renderGame :: UIState -> Picture
renderGame ui@UIState{..} = pictures
  [ color bgColor $ rectangleSolid 1200 800
  , translate (-500) 300 $ renderHeader gameState
  , translate (-500) 150 $ renderDiceArea gameState selectedDice
  , translate (-500) (-50) $ renderActions gameState
  , translate (150) 150 $ renderScoreboard ui
  , translate (-500) (-350) $ renderMessages gameState
  , if isCurrentPlayerAI ui then translate (-500) 400 $ renderAIStatus ui else blank
  ]

renderAIStatus :: UIState -> Picture
renderAIStatus UIState{..} = pictures
  [ color (makeColor 0.2 0.5 0.2 0.8) $ rectangleSolid 400 60
  , case aiAction of
      AIRolling -> translate (-80) (-5) $ scale 0.12 0.12 $ color white $ text "IA TIRANDO DADOS..."
      AIKeeping -> translate (-60) (-5) $ scale 0.12 0.12 $ color white $ text "IA CONSERVANDO..."
      AIChoosing -> translate (-80) (-5) $ scale 0.12 0.12 $ color white $ text "IA ELIGIENDO..."
      NoAction -> translate (-80) (-5) $ scale 0.12 0.12 $ color white $ text "IA PENSANDO..."
  ]

isCurrentPlayerAI :: UIState -> Bool
isCurrentPlayerAI UIState{..} =
  let player = jugadores gameState !! turnoActual gameState
  in M.findWithDefault False player aiPlayers

isGameOver :: GameState -> Bool
isGameOver st =
  let players = jugadores st
      scoreMap = puntajes st
  in all (\player -> 
      let scores = M.findWithDefault M.empty player scoreMap
      in length scores >= 13) players

-- Header con info del turno
renderHeader :: GameState -> Picture
renderHeader st = pictures
  [ color cardBg $ rectangleSolid 400 80
  , translate (-140) 20 $ color white $ centerText 0.15 "TURNO ACTUAL"
  , translate (-60) (-5) $ color primaryColor $ centerText 0.12 player
  , translate (-70) (-25) $ color white $ centerText 0.1 rollInfo
  ]
  where
    player = jugadores st !! turnoActual st
    rollInfo = "Tiradas: " ++ show (tiradasRealizadas st) ++ " / 3"

-- Área de dados
renderDiceArea :: GameState -> [Int] -> Picture
renderDiceArea st selected = pictures
  [ color cardBg $ rectangleSolid 400 120
  , translate (-140) 40 $ color white $ centerText 0.1 "DADOS"
  , translate (-140) 0 $ renderDice (dadosActuales st) selected
  , translate (-120) (-40) $ color white $ centerText 0.1 "CONSERVADOS"
  , translate (-140) (-70) $ renderKeptDice (dadosConservados st)
  ]

renderDice :: [Dado] -> [Int] -> Picture
renderDice dados selected = pictures $ zipWith renderOneDie [0..] dados
  where
    renderOneDie i val =
      let isSelected = i `elem` selected
          x = fromIntegral i * 50
          clr = if isSelected then primaryColor else secondaryColor
      in translate x 0 $ renderDie val clr

renderKeptDice :: [Dado] -> Picture
renderKeptDice dados = pictures $ zipWith (\i val -> translate (fromIntegral i * 50) 0 $ renderDie val scoreColor) [0..] dados

renderDie :: Dado -> Color -> Picture
renderDie val clr = pictures
  [ color clr $ rectangleSolid 40 40
  , color white $ renderPips val
  ]

renderPips :: Dado -> Picture
renderPips 1 = translate 0 0 $ circleSolid 3
renderPips 2 = pictures [translate (-8) 8 $ circleSolid 3, translate 8 (-8) $ circleSolid 3]
renderPips 3 = pictures [translate (-8) 8 $ circleSolid 3, translate 0 0 $ circleSolid 3, translate 8 (-8) $ circleSolid 3]
renderPips 4 = pictures [translate (-8) 8 $ circleSolid 3, translate 8 8 $ circleSolid 3, translate (-8) (-8) $ circleSolid 3, translate 8 (-8) $ circleSolid 3]
renderPips 5 = pictures [translate (-8) 8 $ circleSolid 3, translate 8 8 $ circleSolid 3, translate 0 0 $ circleSolid 3, translate (-8) (-8) $ circleSolid 3, translate 8 (-8) $ circleSolid 3]
renderPips 6 = pictures [translate (-8) 8 $ circleSolid 3, translate 8 8 $ circleSolid 3, translate (-8) 0 $ circleSolid 3, translate 8 0 $ circleSolid 3, translate (-8) (-8) $ circleSolid 3, translate 8 (-8) $ circleSolid 3]
renderPips _ = blank

-- Botones de acción
renderActions :: GameState -> Picture
renderActions st = pictures
  [ color cardBg $ rectangleSolid 400 80
  , translate (-100) 20 $ renderButton "TIRAR DADOS" primaryColor (puedeTirar st)
  , translate 100 20 $ renderButton "CONSERVAR" secondaryColor (not $ null $ dadosActuales st)
  , translate 10 (-20) $ renderRollIndicator (tiradasRealizadas st)
  ]

renderButton :: String -> Color -> Bool -> Picture
renderButton label clr enabled = pictures
  [ color (if enabled then clr else greyN 0.3) $ rectangleSolid 180 35
  , color white $ centerText 0.12 label
  ]

renderRollIndicator :: Int -> Picture
renderRollIndicator rolls = pictures $
  [ translate (-20) 0 $ color rollColor $ rectangleSolid 60 25
  , translate (-20) 0 $ color white $ centerText 0.08 "GIRA"
  ] ++
  [ translate (fromIntegral i * 30 + 50) 0 $ renderStep i rolls | i <- [1..3] ]
  where
    renderStep i current =
      let active = i <= current
          clr = if active then labelColor else greyN 0.2
      in pictures
        [ color clr $ rectangleSolid 25 25
        , color (greyN 0.1) $ centerText 0.08 (show i)
        ]

-- Tablero de puntajes
renderScoreboard :: UIState -> Picture
renderScoreboard UIState{gameState=st, hoveredCombo=hovered} = pictures
  [ color scoreboardBg $ rectangleSolid 520 620
  , translate (-120) 240 $ renderScoreHeader st
  , translate (-120) 200 $ renderScoreGrid st hovered
  , translate (-120) (-270) $ renderBonus st
  ]

renderScoreHeader :: GameState -> Picture
renderScoreHeader st = pictures
  [ translate (-30) 0 $ color rollColor $ centerText 0.12 (jugadores st !! 0)
  , translate 150 0 $ color (greyN 0.2) $ rectangleSolid 50 25
  , translate 150 0 $ color white $ centerText 0.1 "VS"
  , translate 280 0 $ color scoreColor $ centerText 0.12 (jugadores st !! min 1 (length (jugadores st) - 1))
  ]

renderScoreGrid :: GameState -> Maybe Combinacion -> Picture
renderScoreGrid st hovered =
  let combos = [minBound .. maxBound] :: [Combinacion]
      players = jugadores st
      currentPlayer = players !! turnoActual st
      availMap = combinacionesDisponibles st
      scoreMap = puntajes st
  in pictures $ concat $ zipWith (\i c -> renderScoreRow i c players currentPlayer availMap scoreMap hovered) [0..] combos

renderScoreRow :: Int -> Combinacion -> [Jugador] -> Jugador -> M.Map Jugador [Combinacion] -> M.Map Jugador (M.Map Combinacion Puntaje) -> Maybe Combinacion -> [Picture]
renderScoreRow idx combo players currentPlayer availMap scoreMap hovered =
  let y = -fromIntegral idx * 40
      label = translate 0 y $ renderComboLabel combo
      cells = zipWith (\pIdx player ->
                  let x = 120 + fromIntegral pIdx * 100
                      scores = M.findWithDefault M.empty player scoreMap
                      avail = M.findWithDefault [] player availMap
                      hasScore = M.member combo scores
                      isAvail = combo `elem` avail && player == currentPlayer
                      isHovered = Just combo == hovered && isAvail
                  in translate x y $ renderScoreCell combo scores hasScore isAvail isHovered
              ) [0..] players
  in label : cells

renderComboLabel :: Combinacion -> Picture
renderComboLabel c = pictures
  [ color labelColor $ rectangleSolid 100 35
  , color (greyN 0.1) $ centerText 0.08 (comboText c)
  ]


comboText :: Combinacion -> String
comboText Unos = "1"
comboText Doses = "2"
comboText Treses = "3"
comboText Cuatro = "4"
comboText Cinco = "5"
comboText Seis = "6"
comboText Trio = "3X"
comboText Cuarteto = "4X"
comboText FullHouse = "FULL"
comboText PequenaEscalera = "SMALL"
comboText GranEscalera = "LARGE"
comboText Yatzy = "YATZY"
comboText Chance = "?"

renderScoreCell :: Combinacion -> M.Map Combinacion Puntaje -> Bool -> Bool -> Bool -> Picture
renderScoreCell combo scores hasScore isAvail isHovered =
  let clr = if hasScore then scoreColor else if isAvail then (if isHovered then light rollColor else rollColor) else labelColor
      val = M.findWithDefault 0 combo scores
  in pictures
    [ color clr $ rectangleSolid 90 35
    , if hasScore then color white $ centerText 0.1 (show val) else blank
    ]

renderBonus :: GameState -> Picture
renderBonus st =
  let players = jugadores st
      scoreMap = puntajes st
      playerCells = concat $ zipWith (\pIdx player ->
            let x = 120 + fromIntegral pIdx * 100
                scores = M.findWithDefault M.empty player scoreMap
                upper = sum [M.findWithDefault 0 c scores | c <- [Unos, Doses, Treses, Cuatro, Cinco, Seis]]
            in [ translate x 0 $ color scoreColor $ rectangleSolid 90 35
               , translate x 0 $ color white $ centerText 0.08 (show upper ++ "/63")
               ]
         ) [0..] players
      labelCell = [ translate 0 0 $ color labelColor $ rectangleSolid 100 35
                  , color rollColor $ centerText 0.08 "BONUS +35"
                  ]
  in pictures (playerCells ++ labelCell)

renderMessages :: GameState -> Picture
renderMessages _ = pictures
  [ color cardBg $ rectangleSolid 1000 40
  , color (makeColor 1 0.7 0.28 1) $ centerText 0.1 "Haz clic en dados para seleccionar, luego conservar o tirar"
  ]

-- Manejo de eventos
handleEvent :: Event -> UIState -> UIState
handleEvent (EventKey (MouseButton LeftButton) Down _ pos) ui = handleClick pos ui
handleEvent (EventMotion pos) ui = ui { mousePos = pos }
handleEvent _ ui = ui

handleClick :: (Float, Float) -> UIState -> UIState
handleClick pos ui@UIState{menuState=MainMenu}
  -- Click "2 JUGADORES"
  | inside pos (-250, -50) (300, 80) =
      let gs = inicializarEstado ["Jugador 1", "Jugador 2"]
          aiMap = M.fromList [("Jugador 1", False), ("Jugador 2", False)]
      in ui { gameState = gs, menuState = PlayingGame, aiPlayers = aiMap }
  -- Click "VS IA"
  | inside pos (250, -50) (300, 80) =
      let gs = inicializarEstado ["Tú", "IA"]
          aiMap = M.fromList [("Tú", False), ("IA", True)]
      in ui { gameState = gs, menuState = PlayingGame, aiPlayers = aiMap }
  | otherwise = ui

handleClick pos ui@UIState{menuState=GameOver}
  -- Click "VOLVER AL MENÚ"
  | inside pos (0, -100) (300, 80) =
      ui { menuState = MainMenu, aiAction = NoAction, aiDiceToKeep = [] }
  | otherwise = ui

handleClick pos ui@UIState{menuState=PlayingGame, ..}
  -- Click en tirar dados
  | inside pos (-600, -30) (180, 35) && puedeTirar gameState && not (isCurrentPlayerAI ui) =
      let (rng', st') = aplicarTirada rng gameState
      in ui { gameState = st', rng = rng', selectedDice = [] }
  
  -- Click en conservar
  | inside pos (-400, -30) (180, 35) && not (null $ dadosActuales gameState) && not (isCurrentPlayerAI ui) =
      let st' = conservarDados (map (+1) selectedDice) gameState
      in ui { gameState = st', selectedDice = [] }
  
  -- Click en dados
  | inside pos (-490, 150) (250, 40) && not (isCurrentPlayerAI ui) =
      let (x, _) = pos
          idx = floor ((x + 490) / 50)
      in if idx >= 0 && idx < length (dadosActuales gameState)
         then ui { selectedDice = if idx `elem` selectedDice then filter (/= idx) selectedDice else idx : selectedDice }
         else ui
  
  -- Click en combinaciones del tablero
  | inside pos (265, 85) (290, 570) && not (isCurrentPlayerAI ui) =
      let (x, y) = pos
          row = floor ((240 - (y - 150)) / 40)
          combos = [minBound .. maxBound] :: [Combinacion]
          player = jugadores gameState !! turnoActual gameState
          col = if x >= 150 && x <= 250 then 0 else if x >= 270 && x <= 370 then 1 else -1
          clickedPlayer = if col >= 0 && col < length (jugadores gameState) then jugadores gameState !! col else ""
      in if row >= 0 && row < length combos && clickedPlayer == player
         then let combo = combos !! row
                  avail = M.findWithDefault [] player (combinacionesDisponibles gameState)
              in if combo `elem` avail
                 then ui { gameState = elegirCombinacion combo gameState }
                 else ui
         else ui
  
  | otherwise = ui

-- Update (sin animaciones por ahora)
update :: Float -> UIState -> UIState
update dt ui@UIState{..}
  -- Detectar fin de juego
  | menuState == PlayingGame && isGameOver gameState =
      ui { menuState = GameOver }
  
  | menuState == PlayingGame && aiThinkTime > 0 =
      ui { aiThinkTime = aiThinkTime - dt }
  
  -- IA comienza a tirar
  | menuState == PlayingGame && aiThinkTime <= 0 && isCurrentPlayerAI ui && aiAction == NoAction && tiradasRealizadas gameState < 3 && null (dadosActuales gameState) =
      let (rng', st') = aplicarTirada rng gameState
      in ui { gameState = st', rng = rng', aiThinkTime = 1.0, aiAction = AIRolling }
  
  -- IA comienza a conservar dados (si puede tirar más)
  | menuState == PlayingGame && aiThinkTime <= 0 && isCurrentPlayerAI ui && aiAction == AIRolling && tiradasRealizadas gameState < 3 && not (null (dadosActuales gameState)) =
      let diceToKeep = aiChooseDice (dadosActuales gameState)
      in ui { aiDiceToKeep = diceToKeep, aiThinkTime = 1.0, aiAction = AIKeeping }
  
  -- Ejecutar conservación de dados y tirar de nuevo
  | menuState == PlayingGame && aiThinkTime <= 0 && isCurrentPlayerAI ui && aiAction == AIKeeping && tiradasRealizadas gameState < 3 =
      let st' = conservarDados (map (+1) aiDiceToKeep) gameState
          (rng', st'') = aplicarTirada rng st'
      in ui { gameState = st'', rng = rng', aiThinkTime = 1.0, aiAction = AIRolling, aiDiceToKeep = [] }
  
  -- IA pasa a estado de elección si no puede tirar más
  | menuState == PlayingGame && aiThinkTime <= 0 && isCurrentPlayerAI ui && aiAction == AIRolling && tiradasRealizadas gameState >= 3 =
      ui { aiThinkTime = 1.0, aiAction = AIChoosing }
  
  -- IA elige combinación
  | menuState == PlayingGame && aiThinkTime <= 0 && isCurrentPlayerAI ui && aiAction == AIChoosing =
      let combo = aiChooseCombo gameState
          st' = elegirCombinacion combo gameState
          st'' = st' { dadosActuales = [], dadosConservados = [], tiradasRealizadas = 0 }
      in ui { gameState = st'', selectedDice = [], aiThinkTime = 0.5, aiAction = NoAction }
  
  -- Reiniciar acción si el turno cambió
  | menuState == PlayingGame && not (isCurrentPlayerAI ui) =
      ui { aiAction = NoAction, aiDiceToKeep = [] }
  
  | otherwise = ui

-- Lanzar UI
runUI :: IO ()
runUI = play
  (InWindow "Yatzi" (1200, 800) (100, 100))
  bgColor
  30
  initialState
  render
  handleEvent
  update

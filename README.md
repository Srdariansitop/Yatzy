# Yatzy

![Yatzy](img.png)

Un juego de dados **Yatzy/Yahtzee** implementado en **Haskell** con soporte para interfaz gráfica de escritorio y servidor web interactivo.

---

## Descripción del Proyecto

Yatzy es una implementación completa del clásico juego de dados donde los jugadores tiran dados, los conservan estratégicamente y marcan combinaciones para ganar puntos. El proyecto está diseñado con arquitectura modular y ofrece dos formas de jugar:

1. **Desktop (GUI)**: Interfaz gráfica con soporte para jugar contra IA
2. **Web**: Servidor HTTP con interfaz web moderna para jugar multijugador

---

## Estructura del Proyecto

```
yatzy/
├── yatzi.cabal              # Configuración de Cabal (dependencias y ejecutables)
├── README.md                # Este archivo
├── src/
│   ├── Main.hs              # Punto de entrada para la versión Desktop
│   ├── ServerMain.hs        # Punto de entrada para el servidor Web
│   └── Game/
│       ├── Types.hs         # Tipos de datos principales
│       ├── Logic.hs         # Lógica del juego (puntajes, reglas)
│       ├── Server.hs        # Servidor HTTP (API REST)
│       ├── UI.hs            # Interfaz gráfica de escritorio (Gloss)
│       └── AI.hs            # Sistema de inteligencia artificial
├── frontend/                # Aplicación web (HTML/CSS/JS)
│   ├── index.html           # Página principal
│   ├── app.js               # Lógica cliente (fetch API)
│   └── style.css            # Estilos modernos
└── dist-newstyle/           # Artefactos de compilación
```

---

## Características Principales

### Lógica del Juego
- **5 dados** que se lanzan hasta 3 veces por turno
- **Conservación selectiva** de dados entre tiradas
- **13 combinaciones** válidas (Unos, Doses, Treses, Cuatro, Cinco, Seis, Trío, Cuarteto, Full House, Pequeña Escalera, Gran Escalera, Yatzy, Chance)
- **Sistema de puntajes** automático según reglas Yatzy
- **Soporte multijugador** con turnos alternos

### Versión Desktop
- Interfaz gráfica renderizada con **Gloss** (Graphics)
- Juego contra **IA** (toma decisiones automáticas)
- Animaciones suaves y diseño visual intuitivo
- Gestión de múltiples jugadores

### Versión Web
- **Servidor Scotty** con API REST
- Interfaz web moderna y responsiva
- Almacenamiento de sesiones de juego en memoria
- Soporte **CORS** para desarrollo
- Interfaz de usuario interactiva con JavaScript

---

## Dependencias

Las dependencias se definen en `yatzi.cabal` y incluyen:

- **base** ≥4.14: Librería estándar de Haskell
- **containers**: Estructuras de datos (Map, Set)
- **random**: Generación de números aleatorios
- **aeson**: Serialización JSON
- **text**: Manejo eficiente de strings
- **scotty**: Framework web ligero
- **transformers**: Mónadas transformadoras
- **http-types**: Tipos HTTP
- **wai-middleware-static**: Servir archivos estáticos
- **gloss**: Librería gráfica 2D

---

## Instalación y Construcción

### Requisitos Previos
- GHC (Glasgow Haskell Compiler) 9.6.7 o superior
- Cabal 3.8 o superior
- Stack (opcional)

### Construcción

```bash
# Limpiar compilaciones anteriores (opcional)
cabal clean

# Compilar el proyecto completo
cabal build all

# O compilar cada ejecutable individualmente:
cabal build yatzi              # Versión Desktop
cabal build yatzi-server       # Versión Web
```

---

## Uso

### Versión Desktop

```bash
cabal run yatzi
```

Esto abrirá una ventana con la interfaz gráfica del juego. Los controles incluyen:
- **Click en dados**: Seleccionar/deseleccionar dados para conservar
- **Botón "Tirar"**: Lanzar dados (máximo 3 tiradas por turno)
- **Botón "Conservar"**: Guardar los dados seleccionados
- **Click en combinación**: Marcar una combinación y avanzar turno

### Versión Web

```bash
cabal run yatzi-server
```

Luego abre tu navegador en:
```
http://localhost:3000
```

**Uso en el navegador:**
1. Ingresa nombres de jugadores separados por coma (ej: "Ana,Beto")
2. Haz clic en "Crear juego"
3. Para cada turno:
   - Haz clic en "Tirar dados" (máximo 3 veces)
   - Selecciona dados para conservar haciendo clic en ellos
   - Haz clic en "Conservar seleccionados"
   - Elige una combinación disponible para marcar puntos
4. Avanza de turno automáticamente

---

## Sistema de Puntajes

Las combinaciones y sus valores son:

| Combinación | Descripción | Puntaje |
|------------|------------|---------|
| **Unos** | Suma de todos los unos | ∑ unos |
| **Doses** | Suma de todos los doses | ∑ doses |
| **Treses** | Suma de todos los treses | ∑ treses |
| **Cuatro** | Suma de todos los cuatros | ∑ cuatros |
| **Cinco** | Suma de todos los cincos | ∑ cincos |
| **Seis** | Suma de todos los seises | ∑ seises |
| **Trío** | 3+ dados iguales | Suma total |
| **Cuarteto** | 4+ dados iguales | Suma total |
| **Full House** | 3 iguales + 2 iguales | 25 puntos |
| **Pequeña Escalera** | 4 dados consecutivos | 30 puntos |
| **Gran Escalera** | 5 dados consecutivos | 40 puntos |
| **Yatzy** | 5 dados iguales | 50 puntos |
| **Chance** | Cualquier combinación | Suma total |

---

## Módulos Principales

### `Game.Types`
Define las estructuras de datos fundamentales:
- `Dado`: Tipo alias para Int (1-6)
- `Tiro`: Lista de dados
- `Combinacion`: Enumeración de las 13 combinaciones válidas
- `GameState`: Estado completo del juego (jugadores, turno, dados, puntajes)

### `Game.Logic`
Implementa la lógica de juego pura:
- `puntaje`: Calcula puntos para una combinación
- `inicializarEstado`: Crea un nuevo juego
- `aplicarTirada`: Lanza dados aleatorios
- `conservarDados`: Guarda dados seleccionados
- `elegirCombinacion`: Marca una combinación y avanza turno

### `Game.Server`
Servidor HTTP con API REST:
- `POST /game`: Crear nuevo juego
- `GET /game/:id`: Obtener estado del juego
- `POST /game/:id/roll`: Tirar dados
- `POST /game/:id/keep`: Conservar dados
- `POST /game/:id/choose`: Elegir combinación
- Middleware CORS y servicio de archivos estáticos

### `Game.UI`
Interfaz gráfica de escritorio:
- Renderizado con Gloss
- Manejo de eventos (mouse, teclado)
- Animaciones de turno
- Integración con IA

### `Game.AI`
Sistema de inteligencia artificial:
- `aiChooseDice`: Decide qué dados conservar
- `aiChooseCombo`: Selecciona una combinación estratégicamente

---

## API REST (Versión Web)

### Crear Juego
```http
POST /game
Content-Type: application/json

{
  "players": ["Ana", "Beto", "Carlos"]
}

Response:
{
  "gameId": 1,
  "state": { ... }
}
```

### Obtener Estado
```http
GET /game/1

Response:
{
  "turn": 0,
  "currentPlayer": "Ana",
  "dice": [3, 4, 1, 5, 2],
  "kept": [],
  "rollsUsed": 1,
  "available": ["Unos", "Doses", ...],
  "scores": [["Ana", []], ["Beto", []], ...]
}
```

### Tirar Dados
```http
POST /game/1/roll

Response:
{
  "turn": 0,
  "currentPlayer": "Ana",
  "dice": [2, 5, 1, 4, 3],
  "rollsUsed": 2,
  ...
}
```

### Conservar Dados
```http
POST /game/1/keep
Content-Type: application/json

{
  "indices": [1, 4]
}
```

### Elegir Combinación
```http
POST /game/1/choose
Content-Type: application/json

{
  "combination": "Trio"
}
```

---

## Tecnologías Utilizadas

| Aspecto | Tecnología |
|--------|-----------|
| **Lenguaje Backend** | Haskell |
| **Framework Web** | Scotty |
| **Gráficos (Desktop)** | Gloss |
| **Serialización** | Aeson (JSON) |
| **Lenguaje Frontend** | JavaScript Vanilla |
| **Estilos** | CSS3 (Grid, Flexbox, Gradientes) |
| **Build Tool** | Cabal |

---

## Desarrollo

### Compilar en modo Watch
```bash
# Compilar automáticamente al cambiar archivos
cabal build --enable-tests --ghc-options=-fhpc
```

### Ejecutar Tests
```bash
# Si hay tests definidos
cabal test
```

### Recompilar Everything
```bash
cabal clean && cabal build all
```

---

## Notas de Arquitectura

1. **Separación de Responsabilidades**: 
   - `Logic.hs` contiene lógica pura (sin IO)
   - `Server.hs` y `UI.hs` manejan IO

2. **Inmutabilidad**: 
   - El estado del juego es inmutable
   - Cada operación retorna un nuevo estado

3. **Multijugador**:
   - Desktop: Turnos alternos con IA opcional
   - Web: Múltiples sesiones simultáneas en memoria

4. **JSON Serialization**:
   - Tipos derivan automáticamente `ToJSON`/`FromJSON`
   - Conversión de Enumeraciones a strings lowercase


¡Disfruta jugando Yatzy!

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window
import Text
import Debug
import Random

-- CONSTANTS

leftWall : Platform
leftWall =
  Platform 1300 10 -300 0

rightWall : Platform
rightWall =
  Platform 1300 10 300 0

-- MODEL

type State = Playing | Over

type alias Game =
  { state : State
  , mario : Model
  , platforms : List Platform
  }


type alias Model =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , dir : Direction
  }

type alias Platform =
  { h : Int
  , w : Int
  , x : Float
  , y : Float
  }

type Direction = Left | Right


type alias Keys = { x:Int, y:Int }


initialMario : Model
initialMario =
  { x = 0
  , y = 0
  , vx = 0
  , vy = 0
  , dir = Right
  }


defaultGame : Game
defaultGame =
  { state = Playing
  , mario = initialMario
  , platforms = initialPlatforms 60 []
  }


-- UPDATE

update : (Float, Keys) -> Game -> Game
update (dt, keys) game =
  { game |
    mario = updateMario (dt, keys) game.mario game.platforms
  }

updateMario : (Float, Keys) -> Model -> List Platform -> Model
updateMario (dt, keys) mario platforms =
  mario
    |> gravity dt platforms
    |> jump keys
    |> walk keys
    |> constraints platforms
    |> physics dt

gravity : Float -> List Platform -> Model -> Model
gravity dt platforms mario =
  { mario |
      vy = if (List.any ((\n -> n mario) within) platforms && mario.vy <= 0) || mario.y == 0 then 0 else mario.vy - dt/4
  }

jump : Keys -> Model -> Model
jump keys mario =
  if keys.y > 0 && mario.vy == 0 then
      { mario | vy = 8.0 }
  else
      mario

walk : Keys -> Model -> Model
walk keys mario =
  { mario |
      vx = toFloat keys.x * 2,
      dir =
        if keys.x < 0 then
            Left

        else if keys.x > 0 then
            Right

        else
            mario.dir
  }

constraints : List Platform -> Model -> Model
constraints platforms mario =
  { mario |
      vx = if ((within mario leftWall) && mario.dir == Left) || ((within mario rightWall) && mario.dir == Right) then 0 else mario.vx,
      vy = if List.any ((\n -> n mario) within) platforms && mario.vy < 0 then 0 else mario.vy
  }

physics : Float -> Model -> Model
physics dt mario =
  { mario |
      x = mario.x + dt * mario.vx,
      y = max 0 (mario.y + dt * mario.vy)
  }

within : Model -> Platform -> Bool
within mario platform = 
  near platform.x ((toFloat platform.w) / 2) mario.x && near platform.y ((toFloat platform.h) / 2) mario.y

near : number -> number -> number -> Bool
near c h n =
  n >= c-h && n <= c+h


initialPlatforms : Float -> List Platform -> List Platform
initialPlatforms y platforms =
  if y > 300 then
    platforms
  else
    initialPlatforms (y + 60) ((platformGenerator 1 y) ::  platforms)

platformGenerator : Float -> Float -> Platform
platformGenerator dt y =
  let

    a = Debug.log "atata" (Time.timestamp (Signal.constant ()))
    b = \(time, _) -> Random.initialSeed (round time)
    randomSeed = Signal.map b a

    x3 = fst (Random.generate (Random.float leftWall.x rightWall.x) randomSeed)

    x2 = Debug.log "ff" x3

    x = 15
  in
    Platform 15 100 x y


-- VIEW

view : (Int, Int) -> Game -> Element
view (w',h') game =
  let
    (w,h) = (toFloat w', toFloat h')

    verb =
      if game.mario.vy /= 0 then
          "jump"

      else if game.mario.vx /= 0 then
          "walk"

      else
          "stand"

    dir =
      case game.mario.dir of
        Left -> "left"
        Right -> "right"

    src =
      "../img/mario/"++ verb ++ "/" ++ dir ++ ".gif"

    marioImage =
      image 35 35 src

    groundY = 62 - h/2

    platforms = platformsView game.platforms

  in
    collage w' h' (List.append platforms
      [ rect w 50
          |> filled Color.charcoal
          |> move (0, 24 - h/2)
      , rect (toFloat leftWall.w) (toFloat leftWall.h) -- left wall
          |> filled Color.red
          |> move (leftWall.x, leftWall.y)
      , rect (toFloat rightWall.w) (toFloat rightWall.h) -- right wall
          |> filled Color.red
          |> move (rightWall.x, rightWall.y)
      , toForm (leftAligned (Text.fromString (toString game.mario.x)))
          |> move (0, 40 - h/2)
      , marioImage
          |> toForm
          |> move (game.mario.x, game.mario.y + groundY)
      ])

platformsView : List Platform -> List Graphics.Collage.Form
platformsView platforms =
  List.map platformView platforms

platformView : Platform -> Graphics.Collage.Form
platformView platform =
  let
    groundY = 62 - 995/2

    platRect = filled Color.blue (rect (toFloat platform.w) (toFloat platform.h))
  in
    platRect |> move (platform.x, platform.y + groundY)

fromJust : Maybe a -> a
fromJust x = case x of
    Just y -> y
    Nothing -> Debug.crash "error: fromJust Nothing"

-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update defaultGame input)


input : Signal (Float, Keys)
input =
  let
    delta = Signal.map (\t -> t/20) (fps 35)
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)
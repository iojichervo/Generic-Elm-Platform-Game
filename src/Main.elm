import Html exposing (Html)
import Html.App as Html
import Color
import Collage
import Element
import Text
import Time exposing (Time, second)
import Debug
import Keyboard
import Collage
import Random
import Utils exposing (..)

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


initialMario : Model
initialMario =
  { x = 0
  , y = 0
  , vx = 0
  , vy = 0
  , dir = Right
  }


defaultGame : (Game, Cmd Msg)
defaultGame =
  ({ state = Playing
  , mario = initialMario
  , platforms = initialPlatforms 60 []
  }, Cmd.none)

-- UPDATE
type Msg
    = Tick Time
    | KeyMsg Keyboard.KeyCode

update : Msg -> Game -> (Game, Cmd Msg)
update msg game =
  case msg of
    Tick newTime ->
      (updateGame (1, keyCodeToKey 0) game, Cmd.none)
    KeyMsg key ->
      (updateGame (1, keyCodeToKey key) game, Cmd.none)

updateGame : (Float, Keys) -> Game -> Game
updateGame (dt, keys) game =
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
      vx = toFloat keys.x * 5,
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
    --x3 = fst (Random.generate (Random.float leftWall.x rightWall.x) (Random.initialSeed 123))

    (tuple, nextSeed) = Random.step floatTuple (Random.initialSeed 123)

    x2 = Debug.log "ff" tuple

    x = 0
  in
    Platform 15 100 x y

floatTuple : Random.Generator (Float, Float)
floatTuple =
    Random.float 0.0 1000.0
    `Random.andThen`
    (\val1 -> Random.map ((,) val1) (Random.float 0 val1))

-- SUBSCRIPTIONS

subscriptions : Game -> Sub Msg
subscriptions game =
    Sub.batch
    [ Time.every (15 * Time.millisecond) Tick
    , Keyboard.presses KeyMsg
    ]

-- VIEW

view : Game -> Html Msg
view game =
  let
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
      Element.image 35 35 src

    h = 600
    w = 800

    groundY = 62 - h/2

    platforms = platformsView game.platforms

  in
    Element.toHtml (Collage.collage w h (List.append platforms
      [ Collage.rect w 50
          |> Collage.filled Color.charcoal
          |> Collage.move (0, 24 - h/2)
      , Collage.rect (toFloat leftWall.w) (toFloat leftWall.h) -- left wall
          |> Collage.filled Color.red
          |> Collage.move (leftWall.x, leftWall.y)
      , Collage.rect (toFloat rightWall.w) (toFloat rightWall.h) -- right wall
          |> Collage.filled Color.red
          |> Collage.move (rightWall.x, rightWall.y)
      , Collage.toForm (Element.leftAligned (Text.fromString (toString game.mario.x)))
          |> Collage.move (0, 40 - h/2)
      , marioImage
          |> Collage.toForm
          |> Collage.move (game.mario.x, game.mario.y + groundY)
      ]))

platformsView : List Platform -> List Collage.Form
platformsView platforms =
  List.map platformView platforms

platformView : Platform -> Collage.Form
platformView platform =
  let
    groundY = 62 - 600/2

    platRect = Collage.filled Color.blue (Collage.rect (toFloat platform.w) (toFloat platform.h))
  in
    platRect |> Collage.move (platform.x, platform.y + groundY)

fromJust : Maybe a -> a
fromJust x = case x of
    Just y -> y
    Nothing -> Debug.crash "error: fromJust Nothing"

-- MAIN

main : Program Never
main =
  Html.program
    { init = defaultGame
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

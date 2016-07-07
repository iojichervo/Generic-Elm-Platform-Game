import Html exposing (Html)
import Html.App as Html
import Color
import Collage
import Element exposing (..)
import Text
import Time exposing (Time, second)
import Debug
import Keyboard exposing (KeyCode)
import Collage exposing (..)
import Random
import AnimationFrame
import Key exposing (..)
import Window exposing (Size)
import Task

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
  , randomPosition : Float
  , size : Size
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
  , randomPosition = 0
  , size = Size 0 0
  }, Task.perform (\_ -> NoOp) Resize (Window.size))


-- UPDATE
type Msg
    = TimeUpdate Time
    | KeyDown KeyCode
    | KeyUp KeyCode
    | NewPlatform Float
    | Resize Size
    | NoOp


update : Msg -> Game -> (Game, Cmd Msg)
update msg game =
  case msg of
    TimeUpdate newTime ->
      ( updateGame 1 game , Random.generate NewPlatform (Random.float leftWall.x rightWall.x) )

    KeyDown keyCode ->
      ( { game | mario = keyDown keyCode game.mario game.platforms } , Cmd.none )

    KeyUp keyCode ->
      ( { game | mario = keyUp keyCode game.mario } , Cmd.none )

    NewPlatform position ->
      ( { game | randomPosition = position } , Cmd.none )

    Resize size ->
      ( { game | size = size } , Cmd.none )

    NoOp ->
      ( game , Cmd.none )


keyDown : KeyCode -> Model -> List Platform -> Model
keyDown keyCode model platforms =
    case Key.fromCode keyCode of
        ArrowUp ->
            jump model platforms

        ArrowLeft ->
            walk -1 model

        ArrowRight ->
            walk 1 model

        _ ->
            model


keyUp : KeyCode -> Model -> Model
keyUp keyCode model =
    case Key.fromCode keyCode of
        ArrowLeft ->
            walk 0 model

        ArrowRight ->
            walk 0 model

        _ ->
            model


jump : Model -> List Platform -> Model
jump mario platforms =
  if mario.vy == 0 && (List.any ((\n -> n mario) within) platforms || mario.y == 0) then
      { mario | vy = 8.0 }
  else
      mario


walk : Int -> Model -> Model
walk val mario =
  { mario |
      vx = toFloat val * 3,
      dir =
        if val < 0 then
            Left
        else if val > 0 then
            Right
        else
            mario.dir
  }


updateGame : Float -> Game -> Game
updateGame dt game =
  { game | mario = updateMario dt game.mario game.platforms }


updateMario : Float -> Model -> List Platform -> Model
updateMario dt mario platforms =
  mario
    |> gravity dt platforms
    |> constraints platforms
    |> physics dt


gravity : Float -> List Platform -> Model -> Model
gravity dt platforms mario =
  { mario |
      vy = if mario.vy <= 0 && (List.any ((\n -> n mario) within) platforms || mario.y == 0) then 0 else mario.vy - dt/4
  }


constraints : List Platform -> Model -> Model
constraints platforms mario =
  { mario |
      vx = if ((within mario leftWall) && mario.dir == Left) || ((within mario rightWall) && mario.dir == Right) then 0 else mario.vx
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
  if y > 600 then
    platforms
  else
    initialPlatforms (y + 60) (platformGenerator y ::  platforms)


platformGenerator : Float -> Platform
platformGenerator y =
    Platform 15 100 0 y


-- SUBSCRIPTIONS

subscriptions : Game -> Sub Msg
subscriptions game =
    Sub.batch
    [ AnimationFrame.diffs TimeUpdate
    , Keyboard.downs KeyDown
    , Keyboard.ups KeyUp
    , Window.resizes Resize
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
      image 35 35 src

    w = 800
    h = 600

    {width, height} = game.size

    groundY = 62 - h/2

    platforms = platformsView game.platforms

  in
    toHtml <|
    container width height middle <|
    collage w h (List.append platforms
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

platformsView : List Platform -> List Form
platformsView platforms =
  List.map platformView platforms

platformView : Platform -> Form
platformView platform =
  let
    groundY = 62 - 600/2

    platRect = filled Color.blue (rect (toFloat platform.w) (toFloat platform.h))
  in
    platRect |> move (platform.x, platform.y + groundY)

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

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
import List.Extra exposing (last)

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
  , score : Float
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
  , platforms = [Platform 50 615 0 -23]
  , randomPosition = 0
  , size = Size 0 0
  , score = 0.0
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
        if game.state == Playing then
            ( { game | mario = keyDown keyCode game.mario game.platforms } , Cmd.none )
        else
            ( game, Cmd.none )

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
  if mario.vy == 0 && List.any ((\n -> n mario) within) platforms then
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
  { game |
    state = if game.mario.y < -50 then Over else Playing
  , mario = updateMario dt game.mario game.platforms
  , platforms = 
    generatePlatforms game.randomPosition <|
    scroll game.mario game.platforms
  , score = if game.mario.y > game.score then game.mario.y else game.score }


updateMario : Float -> Model -> List Platform -> Model
updateMario dt mario platforms =
  mario
    |> gravity dt platforms
    |> constraints platforms
    |> physics dt


gravity : Float -> List Platform -> Model -> Model
gravity dt platforms mario =
  { mario |
      vy = if mario.vy <= 0 && List.any ((\n -> n mario) within) platforms then 0 else mario.vy - dt/4
  }


constraints : List Platform -> Model -> Model
constraints platforms mario =
  { mario |
      vx = if ((within mario leftWall) && mario.dir == Left) || ((within mario rightWall) && mario.dir == Right) then 0 else mario.vx
  }


physics : Float -> Model -> Model
physics dt mario =
  { mario |
      x = mario.x + dt * mario.vx
  ,   y = mario.y + dt * mario.vy
  }


scroll : Model -> List Platform -> List Platform
scroll mario platforms =
    if mario.y >= 300 then
        List.filter removePlatform <|
        List.map (scrollPlatform 2) platforms
    else
        platforms


scrollPlatform : Float -> Platform -> Platform
scrollPlatform value platform =
    { platform | y = platform.y - value }


removePlatform : Platform -> Bool
removePlatform platform =
    platform.y > -50


generatePlatforms : Float -> List Platform -> List Platform
generatePlatforms position platforms =
    case last platforms of
        Just platform ->
            if platform.y < 600 then
                    platforms ++ [Platform 15 100 position (platform.y + 60)]
                else
                    platforms

        Nothing -> platforms


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
    if y == 0 then
        Platform 50 615 0 -23
    else
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
    {width, height} = game.size

    shown = 
        if game.state == Playing then
            elementGame game
        else
            "Game Over!\n\nYour Score is: " ++ toString game.score
            |> Text.fromString
            |> centered

  in
    toHtml <|
    container width height middle <| shown


elementGame : Game -> Element
elementGame game = 
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

    groundY = 62 - h/2

    platforms = platformsView game.platforms

  in
    collage w h
      ([ rect w h -- background
          |> filled (Color.rgb 174 238 238)] ++ platforms ++
      [ marioImage
          |> toForm
          |> move (game.mario.x, game.mario.y + groundY)
      , rect (toFloat leftWall.w) (toFloat leftWall.h) -- left wall
          |> filled Color.lightCharcoal
          |> move (leftWall.x - 8, leftWall.y)
      , rect (toFloat rightWall.w) (toFloat rightWall.h) -- right wall
          |> filled Color.lightCharcoal
          |> move (rightWall.x + 9, rightWall.y)
      , "Score: " ++ toString game.score
          |> Text.fromString
          |> centered
          |> toForm
          |> move (-250, 280)
      ])

platformsView : List Platform -> List Form
platformsView platforms =
  List.map platformView platforms

platformView : Platform -> Form
platformView platform =
  let
    groundY = 62 - 600/2

    color = if platform.w > 600 then Color.lightCharcoal else Color.lightGreen

    platRect = filled color (rect (toFloat platform.w) (toFloat platform.h))

    fixY = if platform.w > 600 then 15 else toFloat platform.h
  in
    platRect |> move (platform.x, platform.y + groundY - fixY)

-- MAIN

main : Program Never
main =
  Html.program
    { init = defaultGame
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

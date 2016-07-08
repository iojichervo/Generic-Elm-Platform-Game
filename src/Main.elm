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
  Platform 1300 10 -200 0 1 None

rightWall : Platform
rightWall =
  Platform 1300 10 200 0 1 None

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
  , life : Float
  , movable : Movable
  }

type Movable = Horizontal Direction | None


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
  , platforms = [Platform 50 425 0 -23 1 None]
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
      ( scroll (updateGame 1 game) , Random.generate NewPlatform (Random.float (leftWall.x + 45) (rightWall.x - 45)) )

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
    movePlatforms <|
    consumePlatforms game.mario game.score <|
    generatePlatforms game.randomPosition game.platforms
  , score = if game.state == Playing then game.score + 0.03 else game.score }


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


scroll : Game -> Game
scroll game =
    let
        globalScroll = game.score / 100

        sc = if game.mario.y >= 300 then 2 + globalScroll else globalScroll
    in
        { game |
            platforms = List.filter availablePlatform <|
                List.map (scrollPlatform sc) game.platforms
        ,   mario = scrollMario game.mario sc
        }


scrollPlatform : Float -> Platform -> Platform
scrollPlatform value platform =
    { platform | y = platform.y - value }


availablePlatform : Platform -> Bool
availablePlatform platform =
    platform.y > -80 && platform.life > 0


scrollMario : Model -> Float -> Model
scrollMario mario sc =
    { mario | y = mario.y - sc }


generatePlatforms : Float -> List Platform -> List Platform
generatePlatforms position platforms =
    case last platforms of
        Just platform ->
            if platform.y < 600 then
                    platforms ++ [Platform 15 100 position (platform.y + 60) 1 (Horizontal Left)]
                else
                    platforms

        Nothing -> platforms


consumePlatforms : Model -> Float -> List Platform -> List Platform
consumePlatforms mario score platforms =
    List.map (consumePlatform mario score) platforms


consumePlatform : Model -> Float -> Platform -> Platform
consumePlatform mario score platform =
    if platform.w < 400 && within mario platform then
        { platform | life = platform.life - (min 0.1 (score / 10000)) }
    else
        platform


movePlatforms : List Platform -> List Platform
movePlatforms platforms =
    List.map movePlatform platforms


movePlatform : Platform -> Platform
movePlatform platform =
    case platform.movable of
        None ->
            platform

        Horizontal dir ->
            let
                newDir = case dir of
                    Left ->
                        if platform.x - 45 <= leftWall.x then Right else Left

                    Right ->
                        if platform.x + 45 >= rightWall.x then Left else Right

                deltaX = if dir == Right then 1 else -1
            in
                { platform | x = platform.x + deltaX, movable = Horizontal newDir }


within : Model -> Platform -> Bool
within mario platform =
  near platform.x ((toFloat platform.w) / 2) mario.x && near platform.y ((toFloat platform.h) / 2) mario.y


near : number -> number -> number -> Bool
near c h n =
  n >= c-h && n <= c+h


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
            "Game Over!\n\nYour Score is: " ++ toString (truncate game.score)
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
          |> filled (Color.rgb 51 204 255)] ++ platforms ++
      [ marioImage
          |> toForm
          |> move (game.mario.x, game.mario.y + groundY)
      , rect (toFloat leftWall.w) (toFloat leftWall.h) -- left wall
          |> filled Color.lightCharcoal
          |> move (leftWall.x - 8, leftWall.y)
      , rect (toFloat rightWall.w) (toFloat rightWall.h) -- right wall
          |> filled Color.lightCharcoal
          |> move (rightWall.x + 9, rightWall.y)
      , "Time: " ++ toString (truncate game.score)
          |> Text.fromString
          |> centered
          |> toForm
          |> move (-350, 280)
      ])

platformsView : List Platform -> List Form
platformsView platforms =
  List.map platformView platforms

platformView : Platform -> Form
platformView platform =
  let
    groundY = 62 - 600/2

    color = if platform.w > 400 then Color.lightCharcoal else Color.rgba 224 30 76 platform.life

    platRect = filled color (rect (toFloat platform.w) (toFloat platform.h))

    fixY = if platform.w > 400 then 15 else toFloat platform.h
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

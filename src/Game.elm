module Game exposing (..)

import GamePlatform exposing (..)
import Mario exposing (..)
import Window exposing (Size)
import Task
import Random exposing (Generator)
import Keyboard exposing (KeyCode)
import Key exposing (..)
import Time exposing (Time, second)

-- MODEL

type State = Playing | Over


type alias Game =
  { state : State
  , mario : Model
  , platforms : List Platform
  , randomPlatform : (Float, Movable)
  , size : Size
  , score : Float
  }


type Msg =
  TimeUpdate Time
  | KeyDown KeyCode
  | KeyUp KeyCode
  | NewPlatform (Float, Movable)
  | Resize Size
  | NoOp

-- FUNCTIONS

defaultGame : (Game, Cmd Msg)
defaultGame =
  ({ state = Playing
  , mario = initialMario
  , platforms = [Platform 50 425 0 -23 1 None]
  , randomPlatform = (0, None)
  , size = Size 0 0
  , score = 0.0
  }, Task.perform (\_ -> NoOp) Resize (Window.size))


update : Msg -> Game -> (Game, Cmd Msg)
update msg game =
  case msg of
    TimeUpdate newTime ->
      ( scroll (updateGame 1 game)
      , Random.generate NewPlatform platformGenerator )

    KeyDown keyCode ->
      if game.state == Playing then
        ( { game | mario = keyDown keyCode game.mario game.platforms }
        , Cmd.none )
      else
        ( game, Cmd.none )

    KeyUp keyCode ->
      ( { game | mario = keyUp keyCode game.mario } , Cmd.none )

    NewPlatform (position, movable) ->
      ( { game | randomPlatform = (position, movable) } , Cmd.none )

    Resize size ->
      ( { game | size = size } , Cmd.none )

    NoOp ->
      ( game , Cmd.none )


updateGame : Float -> Game -> Game
updateGame dt game =
  { game |
    state = if game.mario.y < -50 then Over else Playing
  , mario = updateMario dt game.mario game.platforms
  , platforms = 
    movePlatforms <|
    consumePlatforms game.mario game.score <|
    generatePlatforms game.randomPlatform game.platforms
  , score = if game.state == Playing then game.score + 0.03 else game.score }


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


consumePlatforms : Model -> Float -> List Platform -> List Platform
consumePlatforms mario score platforms =
  List.map (consumePlatform mario score) platforms


consumePlatform : Model -> Float -> Platform -> Platform
consumePlatform mario score platform =
  if platform.w < 400 && within mario platform then
    { platform | life = platform.life - (min 0.1 (score / 10000)) }
  else
    platform
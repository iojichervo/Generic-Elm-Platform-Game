module Game exposing (..)

import GamePlatform exposing (..)
import Hero exposing (..)
import Window exposing (Size)
import Task
import Random exposing (Generator)
import Keyboard exposing (KeyCode)
import Key exposing (..)
import Time exposing (Time, second)

-- hero

type State = Playing | Over


type alias Game =
  { state : State
  , hero : Hero
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
  , hero = initialhero
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
        ( { game | hero = keyDown keyCode game.hero game.platforms }
        , Cmd.none )
      else
        ( game, Cmd.none )

    KeyUp keyCode ->
      ( { game | hero = keyUp keyCode game.hero } , Cmd.none )

    NewPlatform (position, movable) ->
      ( { game | randomPlatform = (position, movable) } , Cmd.none )

    Resize size ->
      ( { game | size = size } , Cmd.none )

    NoOp ->
      ( game , Cmd.none )


updateGame : Float -> Game -> Game
updateGame dt game =
  { game |
    state = if game.hero.y < -50 then Over else Playing
  , hero = updatehero dt game.hero game.platforms
  , platforms = 
    movePlatforms <|
    consumePlatforms game.hero game.score <|
    generatePlatforms game.randomPlatform game.platforms
  , score = if game.state == Playing then game.score + 0.03 else game.score }


scroll : Game -> Game
scroll game =
  let
    globalScroll = game.score / 100

    sc = if game.hero.y >= 300 then 2 + globalScroll else globalScroll
  in
    { game |
        platforms = List.filter availablePlatform <|
            List.map (scrollPlatform sc) game.platforms
    ,   hero = scrollhero game.hero sc
    }


keyDown : KeyCode -> Hero -> List Platform -> Hero
keyDown keyCode hero platforms =
  case Key.fromCode keyCode of
    ArrowUp ->
      jump hero platforms

    ArrowLeft ->
      walk -1 hero

    ArrowRight ->
      walk 1 hero

    _ ->
      hero


keyUp : KeyCode -> Hero -> Hero
keyUp keyCode hero =
  case Key.fromCode keyCode of
    ArrowLeft ->
      walk 0 hero

    ArrowRight ->
      walk 0 hero

    _ ->
      hero


consumePlatforms : Hero -> Float -> List Platform -> List Platform
consumePlatforms hero score platforms =
  List.map (consumePlatform hero score) platforms


consumePlatform : Hero -> Float -> Platform -> Platform
consumePlatform hero score platform =
  if platform.w < 400 && within hero platform then
    { platform | life = platform.life - (min 0.1 (score / 10000)) }
  else
    platform
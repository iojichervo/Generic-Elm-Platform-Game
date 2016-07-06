import Html exposing (Html)
import Html.App as Html
import Html.Attributes
import Color
import Collage
import Element
import Text
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Debug
import Keyboard
import Collage

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


defaultGame : (Game, Cmd Msg)
defaultGame =
  ({ state = Playing
  , mario = initialMario
  , platforms = []
  }, Cmd.none)

-- UPDATE
type Msg
    = Tick Time
    | KeyMsg Keyboard.KeyCode

update : Msg -> Game -> (Game, Cmd Msg)
update msg game =
  case msg of
    Tick newTime ->
      (game, Cmd.none)
    KeyMsg key ->
      (game, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Game -> Sub Msg
subscriptions game =
    Sub.batch
    [ Time.every second Tick
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

    groundY = 62 - 300/2
    h = 300
    w = 600

--    platforms = platformsView game.platforms
    platforms = []

  in
    Element.toHtml (Collage.collage 600 300 (List.append platforms
      [ Collage.rect 600 50
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

-- MAIN

main : Program Never
main =
  Html.program
    { init = defaultGame
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

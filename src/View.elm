module View exposing (..)

import Html exposing (Html)
import Color exposing (Color)
import Collage exposing (..)
import Element exposing (..)
import Text
import String
import GamePlatform exposing (..)
import Game exposing (..)

-- CONSTANTS

w : number
w = 800

h : number
h = 600

-- FUNCTIONS

view : Game -> Html Msg
view game =
  let
    {width, height} = game.size

    shown = 
      if game.state == Playing then
        elementGame game
      else
        game.score
        |> truncate
        |> toString
        |> String.append "Game Over!\n\nYour Score is: "
        |> Text.fromString
        |> centered

  in
    shown
    |> container width height middle
    |> toHtml


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

    marioImage = image 35 35 src

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
    groundY = 62 - h/2

    color = platformColor platform

    platRect =
      rect (toFloat platform.w) (toFloat platform.h)
      |> filled color

    fixY = if platform.w > 400 then 15 else toFloat platform.h
  in
    platRect |> move (platform.x, platform.y + groundY - fixY)


platformColor : Platform -> Color
platformColor platform = 
  if platform.w > 400 then
    Color.lightCharcoal
  else
    Color.rgba 224 30 76 platform.life
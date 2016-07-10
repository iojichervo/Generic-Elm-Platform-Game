module View exposing (..)

import Html exposing (Html)
import Color exposing (Color)
import Collage exposing (..)
import Element exposing (..)
import Text
import String
import GamePlatform exposing (..)
import Game exposing (..)
import Hero exposing (..)

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
    bgImg = image w h "../img/background.png"

    groundY = 62 - h/2

    platforms = platformsView game.platforms
  in
    collage w h
      ( toForm bgImg :: platforms ++
      [ heroImage game.hero
          |> toForm
          |> move (game.hero.x, game.hero.y + groundY)
      , rect (toFloat leftWall.w) (toFloat leftWall.h) -- left wall
          |> filled Color.darkCharcoal
          |> move (leftWall.x - 8, leftWall.y)
      , rect (toFloat rightWall.w) (toFloat rightWall.h) -- right wall
          |> filled Color.darkCharcoal
          |> move (rightWall.x + 9, rightWall.y)
      , "Time: " ++ toString (truncate game.score)
          |> Text.fromString
          |> centered
          |> toForm
          |> move (-350, 280)
      ])


heroImage : Hero -> Element
heroImage hero = 
  let
    verb =
      if hero.vy /= 0 then
        "jump"
      else if hero.vx /= 0 then
        "walk"
      else
        "stand"

    dir =
      case hero.dir of
        Left -> "left"
        Right -> "right"

    format = if verb == "walk" then ".gif" else ".png"

    src =
      "../img/hero/"++ verb ++ "/" ++ dir ++ format

    heroh = if verb == "stand" then 40 else 50
    herow = if verb == "stand" then 28 else 35
  in
    image herow heroh src


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
  in
    platRect |> move (platform.x, platform.y + groundY - 20)


platformColor : Platform -> Color
platformColor platform = 
  if platform.w > 400 then
    Color.darkCharcoal
  else
    Color.rgba 136 138 133 platform.life
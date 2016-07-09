module Mario exposing (..)

import GamePlatform exposing (..)

-- MODEL

type alias Model =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , dir : Direction
  }

-- FUNCTIONS

initialMario : Model
initialMario =
  { x = 0
  , y = 0
  , vx = 0
  , vy = 0
  , dir = Right
  }


jump : Model -> List Platform -> Model
jump mario platforms =
  if mario.vy == 0 && onPlatform mario platforms then
    { mario | vy = 8.0 }
  else
    mario


walk : Int -> Model -> Model
walk val mario =
  { mario |
    vx = toFloat val * 3
  , dir =
      if val < 0 then
        Left
      else if val > 0 then
        Right
      else
        mario.dir
  }


updateMario : Float -> Model -> List Platform -> Model
updateMario dt mario platforms =
  mario
    |> gravity dt platforms
    |> constraints
    |> physics dt


gravity : Float -> List Platform -> Model -> Model
gravity dt platforms mario =
  { mario |
    vy = 
      if mario.vy <= 0 && onPlatform mario platforms then
        0
      else
        mario.vy - dt/4
  }


constraints : Model -> Model
constraints mario =
  { mario |
    vx =
      if crashingWalls mario then
        0 
      else
        mario.vx
  }


crashingWalls : Model -> Bool
crashingWalls mario = 
  (within mario leftWall && mario.dir == Left)
    || (within mario rightWall && mario.dir == Right)


physics : Float -> Model -> Model
physics dt mario =
  { mario |
    x = mario.x + dt * mario.vx
  , y = mario.y + dt * mario.vy
  }


scrollMario : Model -> Float -> Model
scrollMario mario sc =
  { mario | y = mario.y - sc }


within : Model -> Platform -> Bool
within mario platform =
  near platform.x ((toFloat platform.w) / 2) mario.x 
    && near platform.y ((toFloat platform.h) / 2) mario.y


near : number -> number -> number -> Bool
near c h n =
  n >= c-h && n <= c+h


onPlatform : Model -> List Platform -> Bool
onPlatform mario platforms =
  List.any ((\n -> n mario) within) platforms
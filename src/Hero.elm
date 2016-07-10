module Hero exposing (..)

import GamePlatform exposing (..)


-- Hero


type alias Hero =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , dir : Direction
    }



-- FUNCTIONS


initialhero : Hero
initialhero =
    { x = 0
    , y = 0
    , vx = 0
    , vy = 0
    , dir = Right
    }


jump : Hero -> List Platform -> Hero
jump hero platforms =
    if hero.vy == 0 && onPlatform hero platforms then
        { hero | vy = 8.0 }
    else
        hero


walk : Int -> Hero -> Hero
walk val hero =
    { hero
        | vx = toFloat val * 3
        , dir =
            if val < 0 then
                Left
            else if val > 0 then
                Right
            else
                hero.dir
    }


updatehero : Float -> Hero -> List Platform -> Hero
updatehero dt hero platforms =
    hero
        |> gravity dt platforms
        |> constraints
        |> physics dt


gravity : Float -> List Platform -> Hero -> Hero
gravity dt platforms hero =
    { hero
        | vy =
            if hero.vy <= 0 && onPlatform hero platforms then
                0
            else
                hero.vy - dt / 4
    }


constraints : Hero -> Hero
constraints hero =
    { hero
        | vx =
            if crashingWalls hero then
                0
            else
                hero.vx
    }


crashingWalls : Hero -> Bool
crashingWalls hero =
    (within hero leftWall && hero.dir == Left)
        || (within hero rightWall && hero.dir == Right)


physics : Float -> Hero -> Hero
physics dt hero =
    { hero
        | x = hero.x + dt * hero.vx
        , y = hero.y + dt * hero.vy
    }


scrollhero : Hero -> Float -> Hero
scrollhero hero sc =
    { hero | y = hero.y - sc }


within : Hero -> Platform -> Bool
within hero platform =
    near platform.x ((toFloat platform.w) / 2) hero.x
        && near platform.y ((toFloat platform.h) / 2) hero.y


near : number -> number -> number -> Bool
near c h n =
    n >= c - h && n <= c + h


onPlatform : Hero -> List Platform -> Bool
onPlatform hero platforms =
    List.any ((\n -> n hero) within) platforms

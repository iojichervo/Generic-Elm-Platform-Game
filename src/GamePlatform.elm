module GamePlatform exposing (..)

import Random exposing (Generator)


-- CONSTANTS


leftWall : Platform
leftWall =
    Platform 1300 10 -200 0 1 None


rightWall : Platform
rightWall =
    Platform 1300 10 200 0 1 None



-- MODEL


type alias Platform =
    { h : Int
    , w : Int
    , x : Float
    , y : Float
    , life : Float
    , movable : Movable
    }


type Movable
    = Horizontal Direction
    | None


type Direction
    = Left
    | Right


platformGenerator : Generator ( Float, Movable )
platformGenerator =
    Random.pair platformXGenerator movableGenerator


platformXGenerator : Generator Float
platformXGenerator =
    Random.float (leftWall.x + 45) (rightWall.x - 45)


movableGenerator : Generator Movable
movableGenerator =
    Random.map movableAssignator (Random.int -3 3)


movableAssignator : Int -> Movable
movableAssignator val =
    case val of
        -1 ->
            Horizontal Left

        1 ->
            Horizontal Right

        _ ->
            None


scrollPlatform : Float -> Platform -> Platform
scrollPlatform value platform =
    { platform | y = platform.y - value }


availablePlatform : Platform -> Bool
availablePlatform platform =
    platform.y > -80 && platform.life > 0


generatePlatforms : ( Float, Movable ) -> List Platform -> List Platform
generatePlatforms ( position, movable ) platforms =
    case List.head platforms of
        Just platform ->
            if platform.y < 600 then
                newPlatform position platform.y movable :: platforms
            else
                platforms

        Nothing ->
            platforms


newPlatform : Float -> Float -> Movable -> Platform
newPlatform x previousY mov =
    Platform 15 100 x (previousY + 60) 1 mov


movePlatforms : List Platform -> List Platform
movePlatforms platforms =
    List.map movePlatform platforms


movePlatform : Platform -> Platform
movePlatform platform =
    case platform.movable of
        None ->
            platform

        Horizontal dir ->
            movePlatformDir platform dir


movePlatformDir : Platform -> Direction -> Platform
movePlatformDir platform dir =
    let
        newDir =
            case dir of
                Left ->
                    if platform.x - 45 <= leftWall.x then
                        Right
                    else
                        Left

                Right ->
                    if platform.x + 45 >= rightWall.x then
                        Left
                    else
                        Right

        deltaX =
            if dir == Right then
                1
            else
                -1
    in
        { platform
            | x = platform.x + deltaX
            , movable = Horizontal newDir
        }

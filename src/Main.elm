import Html.App as Html
import AnimationFrame
import Keyboard exposing (KeyCode)
import Window exposing (Size)
import View exposing (..)
import Game exposing (..)

-- SUBSCRIPTIONS

subscriptions : Game -> Sub Msg
subscriptions game =
  Sub.batch
    [ AnimationFrame.diffs TimeUpdate
    , Keyboard.downs KeyDown
    , Keyboard.ups KeyUp
    , Window.resizes Resize
    ]

-- MAIN

main : Program Never
main =
  Html.program
    { init = defaultGame
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

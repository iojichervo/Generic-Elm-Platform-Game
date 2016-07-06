module Utils exposing (..)

import Keyboard

type alias Keys = { x:Int, y:Int }

keyCodeToKey : Keyboard.KeyCode -> Keys
keyCodeToKey key =
    case key of
        97 -> {x = -1, y = 0} -- A
        100 -> {x = 1, y = 0} -- D
        115 -> {x = 0, y = -1} -- S
        119 -> {x = 0, y = 1} -- W
        _ -> {x = 0, y = 0}

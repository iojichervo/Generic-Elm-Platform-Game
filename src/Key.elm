module Key exposing (..)


type Key
    = ArrowUp
    | ArrowLeft
    | ArrowRight
    | Unknown


fromCode : Int -> Key
fromCode keyCode =
    case keyCode of
        37 ->
            ArrowLeft

        38 ->
            ArrowUp

        39 ->
            ArrowRight

        _ ->
            Unknown

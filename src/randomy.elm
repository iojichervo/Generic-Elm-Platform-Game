module Randomy where

import Time
import Time (Time, second)
import Text (asText)
import Mouse
import Signal
import Signal (Signal, (<~), (~))
import Random
import Random (Seed)
import Graphics.Element (Element)

randomInt : Seed -> Int
randomInt seed = seed |> (Random.generate <| Random.int 1 10) |> fst

otherInput : Signal (Int,Int)
otherInput = Mouse.position

timeSeed : Signal Seed
timeSeed = Random.initialSeed << round <~ Time.every second

inputs : Signal (Seed,(Int,Int))
inputs = (,) <~ timeSeed ~ otherInput

update : (Seed, (Int,Int)) -> (Int,Int) -> (Int,Int)
update (seed,(x,y)) (x',y') =
  let num = randomInt seed
  in (x-x'-num,y'-y+num) -- this update function is nonsense

main : Signal Element
main = asText <~ Signal.foldp update (0,0) inputs

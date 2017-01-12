module Wheel exposing (..)

import List exposing (..)

-- tuple or record?
type alias WheelItem =
  {
    name : String
  }

type alias WheelData = List WheelItem

wheelItemNames : List WheelItem -> List String
wheelItemNames ws = map .name ws

nth idx xs = Maybe.withDefault "" <| head <| drop idx xs
nthData idx xs =
  Maybe.withDefault
    { name = "Test" }
    <| head <| drop idx xs

headWD : List WheelData -> WheelData
headWD wds =
  Maybe.withDefault
    [{ name = "Test" }]
    <| head wds

getResponseOrDefault : List String -> WheelData -> Int ->  String
getResponseOrDefault resp data idx =
  if idx < length resp then
    nth idx resp
  else
    .name <| nthData idx data

createFullListFromResponses : WheelData -> List String -> List String
createFullListFromResponses ws rs =
  List.map
    (getResponseOrDefault rs ws)
    <| range 0 (List.length ws)

createNewWheelStrings : WheelData -> List String -> WheelData
createNewWheelStrings ws ns =
  map2 (\a b -> { name = a} ) ns ws

wheelData1 : WheelData
wheelData1 = [
  {name = "1"},
  {name = "2"},
  {name = "3"},
  {name = "4"},
  {name = "5"},
  {name = "6"},
  {name = "7"},
  {name = "8"},
  {name = "9"}
  ]

wheelData2 : WheelData
wheelData2 = [
  {name = "1x"},
  {name = "2x"},
  {name = "3x"},
  {name = "4x"},
  {name = "5x"},
  {name = "6x"},
  {name = "7x"},
  {name = "8x"},
  {name = "9x"}
  ]

wheelData3 : WheelData
wheelData3 = [
  {name = "1-"},
  {name = "2-"},
  {name = "3-"},
  {name = "4-"},
  {name = "5-"},
  {name = "6-"},
  {name = "7-"},
  {name = "8-"},
  {name = "9-"}
  ]


port module WheelApp exposing (..)

import PuzzleModule exposing (..)
import Wheel exposing (..)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Color exposing (..)

import String
import List exposing (..)


-- type alias ModelInputs  = (Int, String, String, String, String)
type alias ModelInputs  = { 
    count : Int
  , s1 : String
  , s2 : String
  , s3 : String
  , s4 : String 
  }

type alias ModelButtons = List Bool

-- values generated from UI input
-- type alias ModelResults =
--   (WheelPosition, WheelLoop, WheelLoop, WheelLoop,
--     List LoopsPermutation,                      List LoopsPermutation,
--     (List (LoopsPermAnswers, LoopsPermutation), List (LoopsPermAnswers, LoopsPermutation),
--       List (LoopsAnswerLoop, LoopsPermutation), List (LoopsAnswerLoop, LoopsPermutation)
--       , (LoopsPermAnswers, LoopsPermutation)
--       ))
type alias ModelResults = {
    firstList : WheelPosition
  , secLoop : WheelLoop
  , thrLoop : WheelLoop
  , ansLoop : WheelLoop
  , twoListPerms            : List LoopsPermutation
  , threeListPerms          : List LoopsPermutation
  , ansPlusList             : List (LoopsPermAnswers, LoopsPermutation)
  , specificAnswer          : List (LoopsPermAnswers, LoopsPermutation)
  , ansPermsPlusList        : List (LoopsAnswerLoop, LoopsPermutation)
  , specificAnswerPlusList  : List (LoopsAnswerLoop, LoopsPermutation)
  , findAnswerLazy3         : (LoopsPermAnswers, LoopsPermutation)
}

type alias Model = (ModelList, ModelInputs, ModelButtons, ModelResults)

type alias ModelList = List (ModelInputs, ModelButtons)

type Msg =
      NoOp |
      Back |
      Circle1Field String |
      Circle2Field String | Circle3Field String | Circle4Field String |
      ShowLoop2 | ShowLoop3 | ShowLoopAns |
      ShowPerms2 | ShowPerms3 |
      ShowAns |
      ShowState |
      ChangeWheel |
      D3Response (List String) |
      Rotate1 |
      Rotate2 |
      Rotate3

buttonVal : List Bool -> Int -> Bool
buttonVal list num = Maybe.withDefault False <| head (drop (num - 1) list)

-- max buttons is 9, 10 items in list
maxButton = 10
buttonListToggle list num = take (num-1) list ++ [not <| buttonVal list num] ++ take (maxButton - num) (drop num list)

emptyClassList      = classList []
textButtonClassList = classList [ ("textButton", True) ]

showButtonBasic action classList label =
  Html.button
    [ classList
    , Html.Events.onClick action
    ]
    [ Html.text label ]

uiButton : Msg -> String -> Html Msg
uiButton action label = 
  showButtonBasic action emptyClassList <| label

backButton : Html Msg
backButton = uiButton Back "Step Back"

rotButton labelNum action =
  div [ class "rotButton" ] [
    uiButton action <| "Rotate " ++ labelNum
  ]

showButtonToggle action classList labels hide = 
  showButtonBasic action classList <| labelChoice labels hide

showButtonToggleLoop  labels hide action = showButtonToggle action emptyClassList       labels hide 
showButtonToggleText  labels hide action = showButtonToggle action textButtonClassList  labels hide 

-- answersButton : Bool -> Html Msg
-- stateButton : Bool -> Html Msg
-- perms2Button : Bool -> Html Msg
-- perms3Button : Bool -> Html Msg
answersButton hide  = showButtonToggleText ("Show Answers", "Hide Answers") hide ShowAns 
stateButton   hide  = showButtonToggleText ("Show State",   "Hide State")   hide ShowState
perms2Button  hide  = showButtonToggleText ("Show Perms 2", "Hide Perms 2") hide ShowPerms2
perms3Button  hide  = showButtonToggleText ("Show Perms 3", "Hide Perms 3") hide ShowPerms3 

labelChoice labels hide = 
  if hide == True then
    Tuple.second labels
  else
    Tuple.first labels

inputField2 : String -> String -> String ->
              (String -> Msg) ->
              List (String, String) -> Html Msg
inputField2 idVal default text updateItem inputStyle =
  input
    [ placeholder default, Attr.value text
    , onInput updateItem
    , id idVal
    , class "form-control col-sm-2 wheelStyle"
    ]
    []
formGroup : String -> String -> String -> (String -> Msg) -> List (String, String) -> Msg -> Html Msg
formGroup lbl idVal val updateItem style msg =
  div [
    class "wheelInput"
  ] [
    label [
      for idVal
    , classList [("control-label", True), ("col-sm-4", True), ("wheelInputLabel", True)]
    ] [
      text <| "Wheel " ++ lbl
    ]
  , inputField2 idVal lbl val updateItem style
  ]

wheelOnlyRow idx wheelLabel wheelData =
    div [
          class "row"
          ] [
      div [
        class "col-sm-2 wheelRowLabel"
        ] [ text wheelLabel ]
      ,
      div [
        class "col-sm-2 wheelRowData"
        ] [ text <| wheelData ]
    ]

wheelRow idx wheelLabel loopLabel wheelData loopData action hide =
    div [
      classList [("row", True),("wheelRow", True)]
      ] [

        div [
          class "col-sm-2 wheelRowLabel"
        ] [ text wheelLabel ]
      , div [
      class "col-sm-2 wheelRowData"
      ] [ text <| wheelData ]
      , div [
      class "col-sm-1 plusAdjust"
      ] [ showButtonToggleLoop ("+", "-") hide action ]
      , div [
        class "col-sm-2", style <| (displayStyle hide) ++ [("font-weight", "700")]
        ] [
        text loopLabel
      ]

      , div [
        class "col-sm-2 loopDataAdjust", style <| displayStyle hide
        ] [ text loopData ]
    ]

foundAnswerIndicator : List (a,b) -> Bool -> Html Msg
foundAnswerIndicator answerList show =
  let
    found = not <| length answerList == 0
    foundString =
      if found then
        "Yes"
      else
        "No"
  in
    div [ class "foundAnswer", style <| (displayStyle show) ]
    [ text <| "Does solution exist? - "
    , span [ style <| colorStyle <| found ] [
        text <| foundString
      ]
    ]


myStyle : List (String, String)
myStyle = []

textStyle : List (String, String)
textStyle = []

displayStyle : Bool -> List (String, String)
displayStyle show =
  case show of
    True ->   [("display", "block")]
    False ->  [("display", "none")]

infoRow label info displayState =
  div [
  class "row",
        style
                      (displayStyle displayState)
  ] [
    div [
      class "col-sm-2 wheelRowLabel"
      ] [ text label ]
  , div [
      class "col-sm-8 permsData"
    ] [ text <| info ]
  ]


-- converts Signal Model to Signal Html, using non-signal view
--main : Signal Html
--main = viewLift

-- main = HtmlApp.program { init = init, view = view, update = updateModel, subscriptions = subscriptions }
main = Html.program { init = init, view = view, update = updateModel, subscriptions = subscriptions }

-- subscriptions, data responses from js
port dataProcessedItems : (List String -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model = dataProcessedItems D3Response

--viewLift : Signal Html
--viewLift = Signal.map (view updatesChnl.address) updateModelLift

-- used by main, as a non-signal function, to convert a Model to Html
view : Model -> Html Msg
view ( stateHistory,
        -- (i, s1, s2, s3, s4)
        inputs
        ,
        buttonList,
        results
      ) =
  let 
    i = inputs.count
    s1 = inputs.s1
    s2 = inputs.s2
    s3 = inputs.s3
    s4 = inputs.s4
    firstList = results.firstList
    secLoop   = results.secLoop
    thrLoop   = results.thrLoop
    ansLoop   = results.ansLoop
    twoListPerms            = results.twoListPerms
    threeListPerms          = results.threeListPerms
    ansPlusList             = results.ansPlusList
    specificAnswer          = results.specificAnswer
    ansPermsPlusList        = results.ansPermsPlusList
    specificAnswerPlusList  = results.specificAnswerPlusList
    findAnswerLazy3         = results.findAnswerLazy3
  in
  div [] [
  div [
    class "container"
    ]
  [
         div [
         class "row title"
         ] [
          h2 [] [text "Elm Puzzle Calculator"]
         ]

       , div [
          class "row"
          ] [
          div [
            class "btn-group"
            ] [
              perms2Button  <| buttonVal buttonList 5
            , perms3Button  <| buttonVal buttonList 6
            , answersButton <| buttonVal buttonList 1
          ]
        , div [
            classList [("btn-group", True), ("stateButton", True)]
          ] [
            backButton
          , stateButton <| buttonVal buttonList 7
        ]
      ]
    , br [] []

    , Html.form [
          classList [("wheelsForm", True), ("form-inline", True) ]
        ] [
            div [] [
            formGroup "1" "wheel1input"     s1 Circle1Field myStyle Rotate1
          , formGroup "2" "wheel2input"     s2 Circle2Field myStyle Rotate2
          ]
          , div [] [
            formGroup "3" "wheel3input"     s3 Circle3Field myStyle Rotate3
          , formGroup "Ans" "wheelAnsInput" s4 Circle4Field myStyle Rotate1
          ]
        ]
    , div [classList [("rotBtns", True)]] [
          rotButton "1" Rotate1
        , rotButton "2" Rotate2
        , rotButton "3" Rotate3
      ]
    , div [id "chart"
      ] []

    , br [] []

    , div [classList [("wheelCalcs", True)]] [
        wheelOnlyRow  1 "Wheel 1"                       s1
      , wheelRow      2 "Wheel 2"   "Loop 2"            s2 (toString secLoop) ShowLoop2   <| buttonVal buttonList 2
      , wheelRow      3 "Wheel 3"   "Loop 3"            s3 (toString thrLoop) ShowLoop3   <| buttonVal buttonList 3
      , wheelRow      4 "Wheel Answers" "Loop Answers"  s4 (toString ansLoop) ShowLoopAns <| buttonVal buttonList 4
    ]
  ]

  , br [] []

  , div [ classList [("answers", True)]] [
      div [
      class "container"
      ]
      [
          div [
          class "row"
          ] [
              div [
              ] [ foundAnswerIndicator specificAnswer True
              ]
          ]

        , div [
          class "row"
          ] [
              div [
              ] [ puzzleSolvedIndicator s1 s2 s3 s4
              ]
          ]

        , br [] []
        , infoRow "2 Loop Perms"  (toString twoListPerms)   <| buttonVal buttonList 5
        , infoRow "3 Loop Perms"  (toString threeListPerms) <| buttonVal buttonList 6
        , infoRow "answersPlus"   (toString ansPlusList )   <| buttonVal buttonList 1

        , br [] []

        , infoRow "findAnswers"   (toString specificAnswer )   <| buttonVal buttonList 1

        , infoRow "lazyAnswer - " (toString findAnswerLazy3) <| buttonVal buttonList 1
        , infoRow ("State change count: " ++ (toString i)) (toString stateHistory) <| buttonVal buttonList 7

        , div [ style <| textStyle ++ (displayStyle False)] [ text ("answersPerms - " ++ (toString ansPermsPlusList)) ]
        , div [ style <| textStyle ++ (displayStyle False)] [ text ("displayAnswer - " ++ (toString specificAnswerPlusList)) ]

      ]
    ]
  ]

-- outgoing port to js
port showWheel : List (List WheelItem) -> Cmd msg

-- converts Signal Update (from updatesChnl) to Signal Model, 
-- using non-signal updateModel
--updateModelLift : Signal Model
--updateModelLift = Signal.foldp
--                    updateModel
--                    initialModelState
--                    updatesChnl.signal

-- converts Update to new Model
updateModel : Msg -> Model -> (Model, Cmd Msg)
updateModel update ( stateHistory, inputs, buttonList, results ) =
  let
    i  = inputs.count
    s1 = inputs.s1    
    s2 = inputs.s2    
    s3 = inputs.s3    
    s4 = inputs.s4
    newCount   = i + 1
    (inputs, states) = Maybe.withDefault (initialInputs, initialStates) <| head stateHistory
    tailHistory      = Maybe.withDefault [] <| tail stateHistory

    createModel 
      inputs
      buttonStates forward =
      let
        i  = inputs.count
        s1 = inputs.s1
        s2 = inputs.s2
        s3 = inputs.s3
        s4 = inputs.s4
        newHistory =
          if forward == True then
            (inputs, buttonStates) :: stateHistory
          else
            tailHistory

        first     = wheelPositionFromString s1
        answers   = wheelPositionFromString s4
        secLoop   = makeSecLoop s2
        thrLoop   = makeThrLoop s3
        ansLoop   = makeAnsLoop s4

        newCalcs  = {
            firstList = first, secLoop = secLoop, thrLoop = thrLoop, ansLoop = ansLoop,
            twoListPerms            = twoWheelPerms           first secLoop, 
            threeListPerms          = threeLoopPerms          first secLoop thrLoop,
            ansPlusList             = answersPlusPerm         first secLoop thrLoop,
            specificAnswer          = findSpecificAnswer      first secLoop thrLoop ansLoop,
            ansPermsPlusList        = answersPermsPlusList    first secLoop thrLoop,
            specificAnswerPlusList  = displaySpecificAnswers  first secLoop thrLoop answers,
            findAnswerLazy3         = findAnswerCS            first secLoop thrLoop ansLoop
          }
      in
        (newHistory, inputs, buttonStates, newCalcs)
    mdl = createModel inputs buttonList True
    wd1 = d3DataFromString s1
    wd2 = d3DataFromString s2
    wd3 = d3DataFromString s3
    wd4 = d3DataFromString s4
    
    createModelCircle inputs wheelData = 
      (createModel inputs buttonList True, showWheel wheelData )

    createModelShow buttonNum = 
      createModel { inputs | count = newCount } (buttonListToggle buttonList buttonNum) True
  in
    case update of
      NoOp        ->    (createModel inputs buttonList True, Cmd.none)

      Back        ->    (createModel inputs states False, 
                            Cmd.none
                            -- showWheel [ wd1, wd2, wd3, wd4  ] 
                            )

      -- Circle1Field s -> (createModel { inputs | count = newCount, s1 = s } buttonList True, showWheel [ d3DataFromString s, wd2, wd3, wd4  ] )
      Circle1Field s -> createModelCircle { inputs | count = newCount, s1 = s } [ d3DataFromString s, wd2, wd3, wd4 ]
      Circle2Field s -> createModelCircle { inputs | count = newCount, s2 = s } [ wd1, d3DataFromString s, wd3, wd4 ]
      Circle3Field s -> createModelCircle { inputs | count = newCount, s3 = s } [ wd1, wd2, d3DataFromString s, wd4 ]
      Circle4Field s -> createModelCircle { inputs | count = newCount, s4 = s } [ wd1, wd2, wd3, d3DataFromString s ]

      -- ShowAns     ->    (createModel { inputs | count = newCount } (buttonListToggle buttonList 1) True, Cmd.none)
      ShowAns     ->    (createModelShow 1, Cmd.none)
      ShowLoop2   ->    (createModelShow 2, Cmd.none)
      ShowLoop3   ->    (createModelShow 3, Cmd.none)

      ShowLoopAns ->    (createModelShow 4, Cmd.none)

      ShowPerms2 ->     (createModelShow 5, Cmd.none)
      ShowPerms3 ->     (createModelShow 6, Cmd.none)

      ShowState ->      (createModelShow 7, Cmd.none)

      ChangeWheel ->    (mdl, showWheel [ wd1, wd2, wd3, wd4  ] )

      -- currently a no-op
      D3Response rs -> (createModel { inputs | count = i } buttonList True, Cmd.none)

      -- Rotate1 -> (createModel { inputs | count = i, s1 = rotateNumsString s1 } buttonList True,
      --               showWheel [ d3DataFromString <| rotateNumsString s1, wd2, wd3, wd4 ])
      Rotate1 -> createModelCircle  { inputs | count = i, s1 = rotateNumsString s1 } 
                                    [ d3DataFromString <| rotateNumsString s1, wd2, wd3, wd4 ]

      Rotate2 -> createModelCircle  { inputs | count = i, s2 = rotateNumsString s2 }
                                    [ wd1, d3DataFromString <| rotateNumsString s2, wd3, wd4 ]

      Rotate3 -> createModelCircle  { inputs | count = i, s3 = rotateNumsString s3 }
                                    [ wd1, wd2, d3DataFromString <| rotateNumsString s3, wd4 ]

initialInputs = { count = 0, s1 = "1,2,3", s2 = "4,5,6", s3 = "7,8,9", s4 = "12,15,18" }
initialStates = [False, False, False, False, False, False, False, False, False, False]
initialCalcs  = {
    firstList = [1,2,3]
  , secLoop = [[4,5,6]]
  , thrLoop = [[7,8,9]]
  , ansLoop = [[12,15,18]]
  , twoListPerms            = [[[2]]]
  , threeListPerms          = [[[3]]]
  , ansPlusList             = [([1], [[1]])]
  , specificAnswer          = [([1], [[1]])]
  , ansPermsPlusList        = [([[1]], [[1]])]
  , specificAnswerPlusList  = [([[1]], [[1]])]
  , findAnswerLazy3         = ([1], [[1]])
  }

initialModelState = ( 
    []
  , initialInputs
  , initialStates
  , initialCalcs
  )

init =
  let
    model = initialModelState
    wd1 = resultsToD3Data <| wheelData model input1
    wd2 = resultsToD3Data <| wheelData model input2
    wd3 = resultsToD3Data <| wheelData model input3
    wd4 = resultsToD3Data <| wheelData model input4
  in
    (model, showWheel [ wd1, wd2, wd3, wd4 ] )


-- type signatures have to come directly before fn, which ruins readability, 
-- so they are commented out.
-- input1 : ModelInputs -> String
-- input2 : ModelInputs -> String
-- input3 : ModelInputs -> String
-- input4 : ModelInputs -> String
input1 inputs = inputs.s1
input2 inputs = inputs.s2
input3 inputs = inputs.s3
input4 inputs = inputs.s4

modelInputs : Model -> ModelInputs
modelInputs
      ( stateHistory,
        inputs,
        buttonList,
        results
      ) = inputs

wheelData : Model -> (ModelInputs -> String ) -> List Int
wheelData model inp = wheelPositionFromString <| inp <| modelInputs model

resultsToD3Data : List Int -> List { name: String }
resultsToD3Data xs = List.map (\x -> { name = (toString x) }) xs

d3DataFromString : String -> List { name: String }
d3DataFromString = (\s -> resultsToD3Data <| wheelPositionFromString s)

wheelStringFromInt : List Int -> String
wheelStringFromInt xs = String.join ", " <| List.map (\x -> toString x) xs

rotateNumsString s = wheelStringFromInt <| turnWheel (wheelPositionFromString s) 1

currentAnswers s1 s2 s3 s4 =
             (List.map sumColumn <| PuzzleModule.zip3
               (wheelPositionFromString s1)
               (wheelPositionFromString s2)
               (wheelPositionFromString s3)
             )

puzzleSolved : String -> String -> String -> String -> Bool
puzzleSolved s1 s2 s3 s4 =
  currentAnswers s1 s2 s3 s4 == (wheelPositionFromString s4)

puzzleSolvedIndicator : String -> String -> String -> String -> Html Msg
puzzleSolvedIndicator s1 s2 s3 s4 =
  let
    solved = (puzzleSolved s1 s2 s3 s4)
    solvedString =
      if solved then
        "Yes"
      else
        "No " ++ (toString <| currentAnswers s1 s2 s3 s4)
  in
    div [ class "solvedPuzzle" ] [
      text <| "Puzzle solved? - "
    , span [ style <| colorStyle <| solved ] [
        text <| solvedString
      ]
    ]

colorStyle : Bool -> List (String, String)
colorStyle success =
  let 
    clrStyle color = [("color", color)]
  in
  case success of
    True  -> clrStyle "green"
    False -> clrStyle "red"


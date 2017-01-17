port module WheelApp exposing (..)

import PuzzleModule exposing (..)
import Wheel exposing (..)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Color exposing (..)

import String
import List exposing (..)

type alias ModelInputs  = { 
    count : Int
  , s1 : String
  , s2 : String
  , s3 : String
  , s4 : String 
  }

type alias ModelButtons = List Bool

-- values generated from UI input
type alias ModelResults = {
    firstList : WheelPosition
  , secLoop   : WheelLoop
  , thrLoop   : WheelLoop
  , ansLoop   : WheelLoop
  , twoListPerms            : List LoopsPermutation
  , threeListPerms          : List LoopsPermutation
  , ansPlusList             : List (LoopsPermAnswers, LoopsPermutation)
  , specificAnswer          : List (LoopsPermAnswers, LoopsPermutation)
  , ansPermsPlusList        : List (LoopsAnswerLoop, LoopsPermutation)
  , specificAnswerPlusList  : List (LoopsAnswerLoop, LoopsPermutation)
  , findAnswerLazy3         : (LoopsPermAnswers, LoopsPermutation)
}

type alias Model = (ModelHistory, ModelInputs, ModelButtons, ModelResults)

type alias ModelHistory = List (ModelInputs, ModelButtons)

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

-- model helper functions
modelInputs : Model -> ModelInputs
modelInputs (_, inputs, _, _) = inputs

input1 : ModelInputs -> String
input1 inputs = inputs.s1
input2 inputs = inputs.s2
input3 inputs = inputs.s3
input4 inputs = inputs.s4


-- BUTTONS START
buttonVal : List Bool -> Int -> Bool
buttonVal buttonlist num = Maybe.withDefault False <| head <| drop (num - 1) buttonlist

-- max buttons is 9, 10 items in list
maxButton = 10
buttonListToggle list num = take (num-1) list ++ [not <| buttonVal list num] ++ take (maxButton - num) (drop num list)

emptyClassList      = classList []
textButtonClassList = class "textButton"

showButtonBasic action classList label =
  Html.button [ classList, Html.Events.onClick action ] [ Html.text label ]

uiButton : Msg -> String -> Html Msg
uiButton action label = showButtonBasic action emptyClassList <| label

backButton : Html Msg
backButton = uiButton Back "Step Back"

rotButton labelNum action =
  div [ class "rotButton" ] [ uiButton action <| "Rotate " ++ labelNum ]

showButtonToggle action classList labels hide = 
  let
    labelChoice labels hide = 
      case hide of 
        True  -> Tuple.second labels
        False -> Tuple.first labels
  in
    showButtonBasic action classList <| labelChoice labels hide

showButtonToggleLoop  labels hide action = showButtonToggle action emptyClassList       labels hide 
showButtonToggleText  labels hide action = showButtonToggle action textButtonClassList  labels hide 

-- answersButton : Bool -> Html Msg
answersButton hide  = showButtonToggleText ("Show Answers", "Hide Answers") hide ShowAns 
stateButton   hide  = showButtonToggleText ("Show State",   "Hide State")   hide ShowState
perms2Button  hide  = showButtonToggleText ("Show Perms 2", "Hide Perms 2") hide ShowPerms2
perms3Button  hide  = showButtonToggleText ("Show Perms 3", "Hide Perms 3") hide ShowPerms3 
-- BUTTONS END

-- INPUTS START
inputField : String -> String -> String ->
              (String -> Msg) ->
              List (String, String) -> Html Msg
inputField idVal default text updateItem inputStyle =
  input
    [ placeholder default, Attr.value text, onInput updateItem
    , id idVal, class "form-control col-sm-2 wheelStyle"
    ] []
-- INPUTS END

-- FORMS START
formGroupBasic : String -> String -> String -> (String -> Msg) -> List (String, String) -> Msg -> Html Msg
formGroupBasic lbl idVal val updateItem style msg =
  div [ class "wheelInput" ] 
      [ label [ for idVal, class "control-label col-sm-4 wheelInputLabel" ] 
              [ text <| "Wheel " ++ lbl ]
      , inputField idVal lbl val updateItem style
      ]

formGroup : String -> String -> String -> (String -> Msg) -> Msg -> Html Msg
formGroup lbl idVal val updateItem msg = formGroupBasic lbl idVal val updateItem [] msg
-- FORMS END

-- ROWS START
wheelOnlyRow idx wheelLabel wheelData =
    div [ class "row" ] 
        [ div [ class "col-sm-2 wheelRowLabel" ] [ text wheelLabel ]
        , div [ class "col-sm-2 wheelRowData" ]  [ text wheelData ]
        ]

displayItem : Bool -> List (String, String)
displayItem show =
  case show of
    True ->   [("display", "block")]
    False ->  [("display", "none")]

wheelRow idx wheelLabel loopLabel wheelData loopData action hide =
    div [ class "row wheelRow" ] 
        [ div [ class "col-sm-2 wheelRowLabel" ] [ text wheelLabel ]
        , div [ class "col-sm-2 wheelRowData"  ] [ text wheelData ]
        , div [ class "col-sm-1 plusAdjust"    ] [ showButtonToggleLoop ("+", "-") hide action ]
        , div [ class "col-sm-2", style <| (displayItem hide) ++ [("font-weight", "700")] ] 
                                                 [ text loopLabel ]
        , div [ class "col-sm-2 loopDataAdjust", style <| displayItem hide ] 
                                                 [ text <| toString loopData ]
        ]

infoRow label info displayState =
  div [ class "row", style (displayItem displayState) ] 
      [ div [ class "col-sm-2 wheelRowLabel" ] [ text label ]
      , div [ class "col-sm-8 permsData" ]     [ text <| toString info ]
      ]

indicatorRow indicator = div [ class "row" ] [ div [] [ indicator ] ]

-- ROWS END

-- INDICATORS
currentAnswers s1 s2 s3 s4 =
             (List.map sumColumn <| PuzzleModule.zip3
               (wheelPositionFromString s1)
               (wheelPositionFromString s2)
               (wheelPositionFromString s3)
             )

puzzleSolved : String -> String -> String -> String -> Bool
puzzleSolved s1 s2 s3 s4 = currentAnswers s1 s2 s3 s4 == (wheelPositionFromString s4)

solvedColor : Bool -> List (String, String)
solvedColor success =
  let 
    clrStyle color = [("color", color)]
  in
  case success of
    True  -> clrStyle "green"
    False -> clrStyle "red"

foundAnswerIndicator : List (a,b) -> Bool -> Html Msg
foundAnswerIndicator answerList show =
  let
    found = not <| length answerList == 0
    foundString =
      case found of 
        True  -> "Yes"
        False -> "No"
  in
    div [ class "foundAnswer", style <| displayItem show ]
        [ text <| "Does solution exist? - " , span [ style <| solvedColor <| found ] [ text <| foundString ] ]

puzzleSolvedIndicator : String -> String -> String -> String -> Html Msg
puzzleSolvedIndicator s1 s2 s3 s4 =
  let
    solved = (puzzleSolved s1 s2 s3 s4)
    solvedString =
      case solved of 
        True  -> "Yes"
        False -> "No " ++ (toString <| currentAnswers s1 s2 s3 s4)
  in
    div [ class "solvedPuzzle" ] 
        [ text <| "Puzzle solved? - "
        , span  [ style <| solvedColor <| solved ] [ text <| solvedString ]
        ]


-- converts Signal Model to Signal Html, using non-signal view
--main : Signal Html
--main = viewLift

main = Html.program { init = init, view = view, update = updateModel, subscriptions = subscriptions }

-- subscriptions, data responses from js
port dataProcessedItems : (List String -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model = dataProcessedItems D3Response

--viewLift : Signal Html
--viewLift = Signal.map (view updatesChnl.address) updateModelLift

-- used by main, as a non-signal function, to convert a Model to Html
view : Model -> Html Msg
view ( modelHistory
     , inputs
     , buttonList
     , results
     ) =
  let 
    i = inputs.count
    s1 = inputs.s1
    s2 = inputs.s2
    s3 = inputs.s3
    s4 = inputs.s4
    firstList               = results.firstList
    secLoop                 = results.secLoop
    thrLoop                 = results.thrLoop
    ansLoop                 = results.ansLoop
    twoListPerms            = results.twoListPerms
    threeListPerms          = results.threeListPerms
    ansPlusList             = results.ansPlusList
    specificAnswer          = results.specificAnswer
    ansPermsPlusList        = results.ansPermsPlusList
    specificAnswerPlusList  = results.specificAnswerPlusList
    findAnswerLazy3         = results.findAnswerLazy3
    buttonValue             = buttonVal buttonList

    fpa = Maybe.withDefault ([], [[]]) <| List.head <| findSpecificAnswer firstList secLoop thrLoop ansLoop

-- ansTurn :: Int -> (WheelPosition, [WheelPosition])
    ansTurn n = 
      (turnWheel (Tuple.first fpa) n, 
              List.map (flip turnWheel <| n) (Tuple.second fpa) )

-- findScreenAns :: WheelPosition -> Int -> ([Int], [WheelPosition])
    findScreenAns ans n =
      let ansT = Tuple.first <| ansTurn n
      in
        case n == 0 of 
          True  -> ([], [])
          False -> 
            case ansT == ans of
              True   -> ansTurn n
              False  -> findScreenAns ans <| n - 1

-- findScreenAnsX :: WheelPosition -> ([Int], [WheelPosition])
    findScreenAnsX ans =
      findScreenAns ans <| List.length <| Tuple.first fpa

  in
  div 
    [] 
    [ div 
        [ class "container" ]
        [ div [ class "row title"] [ h2 [] [ text "Elm Puzzle Calculator" ] ]
        , div 
            [ class "row" ] 
            [ div [ class "btn-group" ] 
                  [ perms2Button  <| buttonValue 5
                  , perms3Button  <| buttonValue 6
                  , answersButton <| buttonValue 1 ]
            , div [ class "btn-group stateButton" ] 
                  [ backButton, stateButton <| buttonValue 7 ]
            ]
        , br [] []

        , Html.form 
            [ class "wheelsForm form-inline" ]
            [ div [] 
                  [ formGroup "1"   "wheel1input"   s1 Circle1Field Rotate1
                  , formGroup "2"   "wheel2input"   s2 Circle2Field Rotate2
                  ]
            , div [] 
                  [ formGroup "3"   "wheel3input"   s3 Circle3Field Rotate3
                  , formGroup "Ans" "wheelAnsInput" s4 Circle4Field Rotate1
                  ] 
            ]
        , div [class "rotBtns"] 
              [ rotButton "1" Rotate1, rotButton "2" Rotate2, rotButton "3" Rotate3 ]
        
        , div [id "chart"] []
        , br [] []

        , div [class "wheelCalcs"] 
              [ wheelOnlyRow  1 "Wheel 1"                       s1
              , wheelRow      2 "Wheel 2"   "Loop 2"            s2 secLoop ShowLoop2   <| buttonValue 2
              , wheelRow      3 "Wheel 3"   "Loop 3"            s3 thrLoop ShowLoop3   <| buttonValue 3
              , wheelRow      4 "Wheel Answers" "Loop Answers"  s4 ansLoop ShowLoopAns <| buttonValue 4 
              ]
        ]
    , br [] []

    , div 
        [ class "answers"] 
        [ div 
            [ class "container" ]
            [ 
              indicatorRow <| foundAnswerIndicator specificAnswer True
            , indicatorRow <| puzzleSolvedIndicator s1 s2 s3 s4
            , br [] []

            , infoRow "2 Loop Perms"  twoListPerms     <| buttonValue 5
            , infoRow "3 Loop Perms"  threeListPerms   <| buttonValue 6
            , infoRow "findAnswers2"  
                        (findScreenAnsX <| wheelPositionFromString s4)   
                                                       <| buttonValue 1            
            , infoRow "findAnswers"   specificAnswer   <| buttonValue 1
            , br [] []

            , infoRow "answersPlus"   ansPlusList      <| buttonValue 1
            , infoRow "lazyAnswer - " findAnswerLazy3  <| buttonValue 1
            , infoRow ("State change count: " ++ (toString i)) 
                                      modelHistory     <| buttonValue 7

            -- may no longer be shown, some thought needed
            -- --, div [ style <| textStyle ++ (displayItem False)] 
            -- , div [ style <| [] ++ (displayItem False)] 
            --       [ text ("answersPerms - " ++ (toString ansPermsPlusList)) ]
            -- , div [ style <| textStyle ++ (displayItem False)] 
            --       [ text ("displayAnswer - " ++ (toString specificAnswerPlusList)) ]
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
updateModel update ( modelHistory, inputs, buttonList, results ) =
  let
    i  = inputs.count
    s1 = inputs.s1    
    s2 = inputs.s2    
    s3 = inputs.s3    
    s4 = inputs.s4
    newCount   = i + 1
    (inputs, states) = Maybe.withDefault (initialInputs, initialStates) <| head modelHistory
    tailHistory      = Maybe.withDefault [] <| tail modelHistory

    -- d3DataFromString : String -> List { name: String }
    d3DataFromString = (\s -> resultsToD3Data <| wheelPositionFromString s)

    -- wheelText : WheelPosition -> String
    wheelText xs = String.join ", " <| List.map (\x -> toString x) xs

    rotateNumsString s = wheelText <| turnWheel (wheelPositionFromString s) 1

    createModel inputs buttonStates forward =
      let
        i  = inputs.count
        s1 = inputs.s1
        s2 = inputs.s2
        s3 = inputs.s3
        s4 = inputs.s4
        newHistory =
          case forward of 
            True  -> (inputs, buttonStates) :: modelHistory
            False -> tailHistory

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

    rotS1 = rotateNumsString s1
    rotS2 = rotateNumsString s2
    rotS3 = rotateNumsString s3
  in
    case update of
      NoOp        ->    (createModel inputs buttonList True, Cmd.none)

      Back        ->    (createModel inputs states False, Cmd.none -- showWheel [ wd1, wd2, wd3, wd4  ]                             
                        )

      Circle1Field s -> createModelCircle { inputs | count = newCount, s1 = s } [ d3DataFromString s, wd2, wd3, wd4 ]
      Circle2Field s -> createModelCircle { inputs | count = newCount, s2 = s } [ wd1, d3DataFromString s, wd3, wd4 ]
      Circle3Field s -> createModelCircle { inputs | count = newCount, s3 = s } [ wd1, wd2, d3DataFromString s, wd4 ]
      Circle4Field s -> createModelCircle { inputs | count = newCount, s4 = s } [ wd1, wd2, wd3, d3DataFromString s ]

      ShowAns     ->    (createModelShow 1, Cmd.none)
      ShowLoop2   ->    (createModelShow 2, Cmd.none)
      ShowLoop3   ->    (createModelShow 3, Cmd.none)

      ShowLoopAns ->    (createModelShow 4, Cmd.none)

      ShowPerms2 ->     (createModelShow 5, Cmd.none)
      ShowPerms3 ->     (createModelShow 6, Cmd.none)

      ShowState ->      (createModelShow 7, Cmd.none)

      ChangeWheel ->    (mdl, showWheel [ wd1, wd2, wd3, wd4  ] )

      Rotate1 -> createModelCircle  { inputs | count = i, s1 = rotS1 } [ d3DataFromString rotS1, wd2, wd3, wd4 ]
      Rotate2 -> createModelCircle  { inputs | count = i, s2 = rotS2 } [ wd1, d3DataFromString rotS2, wd3, wd4 ]
      Rotate3 -> createModelCircle  { inputs | count = i, s3 = rotS3 } [ wd1, wd2, d3DataFromString rotS3, wd4 ]
      -- currently a no-op
      D3Response rs -> (createModel { inputs | count = i } buttonList True, Cmd.none)

initialInputs = { count = 0, s1 = "1,2,3", s2 = "4,5,6", s3 = "7,8,9", s4 = "12,15,18" }
initialStates = [False, False, False, False, False, False, False, False, False, False]
initialCalcs  = {
    firstList = [1,2,3]
  , secLoop   = [[4,5,6]]
  , thrLoop   = [[7,8,9]]
  , ansLoop   = [[12,15,18]]
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


wheelData : Model -> (ModelInputs -> String ) -> WheelPosition
wheelData model inp = wheelPositionFromString <| inp <| modelInputs model

resultsToD3Data : WheelPosition -> List { name: String }
resultsToD3Data xs = List.map (\x -> { name = (toString x) }) xs




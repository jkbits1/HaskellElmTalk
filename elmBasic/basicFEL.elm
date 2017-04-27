import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)

import String exposing (..)
import List exposing (..)

type alias Model = String

type Msg =
      Change String 

main = Html.program { init = init, view = view, update = updateModel, subscriptions = subscriptions }

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

view : Model -> Html Msg
view s =
  div [
    class "container"
  ] [
    h2 [] [text "Elm Basic"]
    
  , input [ onInput Change ] []
  
  , br [] [], br [] [], text  s
  , br [] [], br [] [], text "UPPER - ", text  <| toUpper s
  , br [] [], br [] [], text "LOWER - ", text  <| toLower s
  ]

updateModel : Msg -> Model -> (Model, Cmd Msg)
updateModel update s = 
    case update of
      Change s1       ->    (s1, Cmd.none)


initialModelState = "hello, Front End London"

init = (initialModelState, Cmd.none )

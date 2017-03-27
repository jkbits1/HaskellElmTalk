module ClientCommands exposing (..)

import Http exposing (..)
import Json.Decode exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Maybe exposing (Maybe)

import ClientTypes exposing (..)
import ClientJsonDecoders exposing (..)

-- VIEW HELPERS

-- Html Event for select onChange
onChange : (String -> msg) -> Attribute msg
onChange f = on "change" <| Json.Decode.map f Html.Events.targetValue

onChangeSort : Attribute Msg
onChangeSort = 
  onChange 
    (\val ->  
      ( case val of 
          "length"  -> SortDetails SortByLength
          _         -> SortDetails SortByNumber ))

onChangeFileName : Attribute Msg
onChangeFileName = onChange (\s -> GetCurrentFileNameDetails s)          

checkbox : Msg -> Bool -> String -> Html.Html Msg
checkbox msg checkedState name =
  label
    [ 
      -- style [("padding", "20px")]
    ]
    [ input [ type_ "checkbox", checked checkedState, onClick msg ] []
    , Html.text name
    ]

infoListItems : List String -> List (Html Msg)
infoListItems xs = 
  List.map (\s -> li [] [text <| s]) xs

detailsDisplay : TitleDetail -> String
detailsDisplay td = 
  (String.padRight 20 '.' <| toString td.titleNumber) ++ " " ++ 
    (toString td.length)

detailsListItems : List TitleDetail -> List (Html Msg)
detailsListItems tds = 
  List.map (\td -> li [] [text <| detailsDisplay td]) tds


-- URLs

serverAddress     = "http://localhost:8000/"

createVidInfoURL : String -> String
createVidInfoURL  = (++) serverAddress

vidInfoFilesURL   = createVidInfoURL "vidInfo/files" 
vidInfoStubURL    = createVidInfoURL "vidInfo"
vidInfoURLWrapped = createVidInfoURL "vidInfoWrapped"


-- HTTP.REQUESTs

getFirstFileNameReq : Http.Request String
getFirstFileNameReq = 
  Http.get vidInfoFilesURL firstStringDecoder

getFirstFileNameMaybeReq : Http.Request (Maybe String)
getFirstFileNameMaybeReq = 
  Http.get vidInfoFilesURL maybeFirstStringDecoder

getFileNamesReq :                     Http.Request (List String)
getFileNamesReq = 
  Http.get vidInfoFilesURL stringListDecoder

getFileNamesMaybeReq :                Http.Request (List (Maybe String))
getFileNamesMaybeReq = 
  Http.get vidInfoFilesURL maybeStringListDecoder

getFileDetailsWrappedReq :  String -> Http.Request (List TitleDetail)
getFileDetailsWrappedReq string =
  Http.get 
    (vidInfoURLWrapped ++ "/" ++ string)
    titleDetailListWrapped

getFileDetailsReq :         String -> Http.Request (List TitleDetail)
getFileDetailsReq string =
  Http.get (vidInfoStubURL ++ "\\" ++ string) titleDetailListDecoder

getFileDetailsMaybeReq :         String -> Http.Request (List (Maybe TitleDetail))
getFileDetailsMaybeReq string =
  Http.get (vidInfoStubURL ++ "\\" ++ string) titleDetailListMaybeDecoder


-- CMDs

getFirstFileNameCmd :       String -> Cmd Msg
getFirstFileNameCmd string = 
  Http.send InfoFirstFileName       <| getFirstFileNameReq

getFirstFileNameMaybeCmd :  String -> Cmd Msg
getFirstFileNameMaybeCmd string = 
  Http.send InfoFirstFileNameMaybe  <| getFirstFileNameMaybeReq

getFileNamesCmd :           String -> Cmd Msg
getFileNamesCmd string = 
  Http.send InfoFileNames           <| getFileNamesReq

getFileNamesMaybeCmd :      String -> Cmd Msg
getFileNamesMaybeCmd string = 
  Http.send InfoFileNamesMaybe      <| getFileNamesMaybeReq

httpSendTitleDetailsCmd : Request (List TitleDetail) -> Cmd Msg
httpSendTitleDetailsCmd = 
  Http.send InfoTitleDetails

getFileDetailsWrappedCmd :  String -> Cmd Msg
getFileDetailsWrappedCmd string = 
  httpSendTitleDetailsCmd           <| getFileDetailsWrappedReq string

getFileDetailsCmd :         String -> Cmd Msg
getFileDetailsCmd string = 
  httpSendTitleDetailsCmd           <| getFileDetailsReq string

getFileDetailsMaybeCmd :         String -> Cmd Msg
getFileDetailsMaybeCmd string = 
  Http.send InfoTitleDetailsMaybe   <| getFileDetailsMaybeReq string

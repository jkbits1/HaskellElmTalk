import Debug exposing (log)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode exposing (field)
import String
import Task exposing (..)
import Time
import JsonBits2 exposing (..)
import List exposing (..)

-- VIEW
view model =
  div [ 
        class "contain"
      ]
    [ 
      div []
        [ 
          button [ onClick ButtonGet ]              [ text "send request" ]
        , button [ onClick ButtonGetFirstFileName ] [ text "send request - first file name" ]
        , button [ onClick ButtonGetFileNames ]     [ text "send request - file names" ]
        , button [ onClick ButtonGetFileDetails ]   [ text "send request - file details3" ]
        , button [ onClick ButtonGetFileDetailsWrapped ]   [ text "send request - file details wrapped" ]        
        ]
      , div [class "count"]
        [
          text <| "Count - "            ++ toString model.count
        ]
      , div [class "info"]
        [
          text <| "Info - "             ++ toString model.info
        ]
      , div [class "sortOptions"]
        [
          label [
            for "sortDetails"
          ] [text "Sort Details:"]
        , select [ id "sortDetails"
            , onChangeSort
            ]
            [
              option [] [ text <| "number" ]
            , option [] [ text <| "length" ]
            ]
        ]
      , div [ class "filters" ]
        [
          label [
            for "filterLen"
          ] [text "Filter:"]
        , input 
            [ id "filterLen"
            , placeholder "0.0001"
            , onInput FilterLen 
            ]
            []

        ]
      , div [ class "filters" ]
        [
          checkbox Filter "Apply Filter"
        ]
      , div [class "firstFileName"]
        [
          text <| "First file name - "  ++ toString model.firstFileName
        ]
      , div [class "fileNames"]
        [
          text "File names - "
        , ul [] <| infoListItems model.fileNames
        , text <| toString model.fileNames
        , text <| ("httpInfo: " ++ model.httpInfo)
        ]
      , div [class "sortOptions"]
        [
          label [
            for "filesList"
          ] [text "Files List:"]
        , select [ 
            id "filesList"
            , onChangeFileName
            ] 
            <| List.map (\s -> option [] [ text <| s ]) model.fileNames
        ]

      , div [class "titleDetails"]
        [
          text "Title details - "
        , ul [] <| detailsListItems 
                    <| 
                    List.filter 
                      (\td -> 
                        case model.filter of 
                          True -> td.length > 
                            model.filterLength
                          False -> True)
                    <| 
                    List.sortWith 
                      (\td1 td2 -> 
                        case model.sortDetailsByLength of
                          True ->
                            compare td1.length td2.length
                          False ->
                            compare td1.titleNumber td2.titleNumber
                        )
                        model.titleDetails
        ]        
    ]
    
firstSpecific : List TitleDetail -> TitleDetail
firstSpecific details = 
  let m = List.head details
  in 
    case m of 
      Just detail -> detail
      Nothing     -> TitleDetail 0 0.0

resultsAsString2 : List String -> String
resultsAsString2 results = String.join ", " results

resultsAsString : List String -> String
resultsAsString strings = List.foldr 
  (appendResults)  
  "" strings
  
appendResults : String -> String -> String
appendResults a b = a ++ ", " ++ b 

myStyle : List (String, String)
myStyle =
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]


imgStyle : Int -> String -> List (String, String)
imgStyle h src =
    [ ("background-image", "url('" ++ src ++ "')")
    , ("background-repeat", "no-repeat")
    , ("background-attachment", "fixed")
    , ("background-position", "center")
    , ("width", "100%")
    , ("height", toString h ++ "px")
    ]


-- JSON DECODERS

type alias Photo =
    { id : String
    , title : String
    }

type alias Size =
    { source : String
    , width : Int
    , height : Int
    }
    
-- HANDLE RESPONSES

getDetail : String -> Task Http.Error (List String)
getDetail string = succeed [string]

getDetails : List String -> Task Http.Error (List String)
getDetails strings =
  case strings of
    string :: _ -> succeed [
      Debug.log "files dets" "file details"
      ]
    [] ->
        succeed ["no details found"]
        
getTitleDetails : List TitleDetail -> Task Http.Error (List String)
getTitleDetails details =
  case details of
    string :: _ -> succeed 
      (List.map (toString) details)
    [] ->
        succeed ["no details found"]

-- WIRING

main = Html.program { 
    init = init
  , view = view
  , update = update
  , subscriptions = subscriptions 
  }

model : Model
model = { 
    count = 0
  , info = "initial state"
  , firstFileName = ""
  , currentFileName = ""
  , fileNames = []
  , xvals = []
  , titleDetails = [] 
  , sortDetailsByLength = False
  , filter = False
  , filterLength = 0.0001
  , httpInfo = ""
  }

init = (
    model
  , Cmd.none)

subscriptions model = Sub.none

customDecoder decoder toResult = 
   Json.Decode.andThen
             (\a ->
                   case toResult a of 
                      Ok b -> Json.Decode.succeed b
                      Err err -> Json.Decode.fail err
             )
             decoder

respInfo : Http.Response String -> String
respInfo resp = 
             resp.url 
          ++ " " ++ (toString resp.status.code) 
          ++ " " ++ resp.status.message
          ++ " " ++ (toString resp.headers) 
          ++ " " ++ resp.body        

handleError model s = ( {model | httpInfo = s }, Cmd.none)       

handleHttpError model err = 
  case err of 
    Timeout         -> handleError model "timeout"
    NetworkError    -> handleError model "nw error"
    BadUrl s        -> handleError model <| "bad url: " ++ s
    BadStatus resp  -> handleError model <| "bad status: " 
                        ++ (respInfo resp)
    BadPayload s resp  
                    -> handleError model <| "bad payload: " 
                        ++ s ++ " " 
                        ++ (respInfo resp)


update : Msg -> Model -> (Model, Cmd Msg )
update msg model = 
  case msg of 
    NoOp -> (model, Cmd.none)
    ButtonGet -> 
      ( {model | count = model.count + 1 }, getFileNamesAsStringCmd )

    ButtonGetFileNames -> 
      ( {model | count = model.count + 1, httpInfo = "reset" }, 
          getFileNamesMaybe "string" 
          )

    ButtonGetFirstFileName -> 
      ( {model | count = model.count + 1 }, getFirstFileName "string" )

    ButtonGetFileDetails ->
      ( {model | count = model.count + 1 }, getFileDetails model.currentFileName )

    ButtonGetFileDetailsWrapped  ->
      ( {model | count = model.count + 1 }, getFileDetailsWrapped model.currentFileName )

    SortDetails sort ->
      case sort of 
        SortByLength ->
          ( {model | count = model.count + 1, sortDetailsByLength = True }, Cmd.none )
        SortByNumber ->
          ( {model | count = model.count + 1, sortDetailsByLength = False }, Cmd.none )

    Filter ->
      ( {model | count = model.count + 1, filter = not model.filter }, Cmd.none )

    FilterLen s ->
      ( {model | count = model.count + 1, 
          filterLength = 
              (\s -> 
                case String.toFloat s of 
                  Ok val -> val
                  Err a  -> 0.0001
              ) s
        }, Cmd.none )

    CurrentFileName s -> 
      ( {model | count = model.count + 1, currentFileName = s }, getFileDetails s )
      
    Info (Ok jsonInfo) -> ( {model | firstFileName = jsonInfo }, Cmd.none)    
    Info (Err _) -> (model, Cmd.none)

    InfoFileNames (Ok fileNames) -> 
      let 
        firstFileName = Maybe.withDefault "zzz" <| head fileNames
      in
        ( {model | fileNames = fileNames, currentFileName = firstFileName }, getFileDetails firstFileName)    
    InfoFileNames (Err err) -> 
      handleHttpError model err

    InfoFileNamesMaybe (Ok fileNames) ->
      let 
        firstFileName = Maybe.withDefault "aaa" <| Maybe.withDefault (Just "xxx") <| head <| filterMaybes fileNames
      in
        ( {model | currentFileName = firstFileName, fileNames = List.map (\m -> Maybe.withDefault "dodgy data" m) <| filterMaybes fileNames 
          , count = model.count +3 }, Cmd.none)       

    InfoFileNamesMaybe (Err err) -> 
      handleHttpError model err

    InfoFirstFileName (Ok fileName) -> ( {model | firstFileName = fileName }, Cmd.none)    
    InfoFirstFileName (Err _) -> (model, Cmd.none)

    InfoTitleDetails (Ok titleDetails) -> ( {model | titleDetails = titleDetails }, Cmd.none)    
    InfoTitleDetails (Err err) -> handleHttpError model err

filterMaybes ms = 
  List.filter
  (\m -> 
    case m of 
      Just x -> True
      Nothing -> False
  )
  ms

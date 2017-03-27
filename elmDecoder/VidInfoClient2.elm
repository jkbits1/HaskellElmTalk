module VidInfoClient2 exposing (..)

import Debug exposing (log)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode exposing (field)
import String
import Task exposing (..)
import Time
import List exposing (..)

import JsonBits2 exposing (..)
import ClientTypes exposing (..)

--  elm make vidInfoClientDecoder2-018a.elm --output=vic2.js

view : Model -> Html Msg
view model =
  div [ 
        class "container"
    ]
    [
      div []
        [ 
          button [ onClick GetFirstFileName ] [ text "send request - first file name" ]
        , button [ onClick GetFileNames ]     [ text "send request - file names" ]
        , button [ onClick GetFileDetails ]   [ text "send request - file details3" ]
        , button [ onClick GetFileDetailsWrapped ]   [ text "send request - file details wrapped" ]        
        ]        
      , div [class "row"] [
          div [class "col-sm-6"] [
            text <| "Count - "            ++ toString model.count
          ]
        -- , div [class "col-sm-6"] [
        --     -- text <| "Info - "             ++ toString model.info
        --   ]
        ]
      , Html.form [] [ 
          div [class "form-group"]
          [
            label [
              for "sortDetails"
            ] [text "Sort Details:"]
          , select [
                class "form-control" 
              , id "sortDetails"
              , onChangeSort
              ]
              [
                option [] [ text <| "number" ]
              , option [] [ text <| "length" ]
              ]
          ]
        , div [ class "form-group" ]
          [
            label [
              for "filterLen"
            ] [text "Filter:"]
          , input 
              [ 
                class "form-control"
              , id "filterLen"
              , placeholder "0.0001"
              , onInput FilterLen 
              ]
              []
          ]
        , div [ class "checkbox" ]
          [
            checkbox Filter model.applyFilter "Apply Filter"
          ]
        , div [class "form-group"]
          [
            label [
              for "filesList"
            ] [text "Files List:"]
          , select [ 
                class "form-control"
              , id "filesList"
              , onChangeFileName
              ] 
              <| List.map (\s -> option [] [ text <| s ]) model.fileNames
          -- , text model.currentFileName
          ]
        , div [ class "checkbox" ]
          [
            checkbox ShowJsonErrors model.showJsonErrors "Show Json Errors"
          ]
        ]
      , div [class "firstFileName"]
        [
          text <| "First file name - "  ++ toString model.firstFileName
        ]
      , div [class "fileNames"]
        [
          text "File names - "
        , ul [] <| infoListItems model.fileNames
        -- , text <| toString model.fileNames
        , br [] []
        , h3 [] [text "http Info: "]
        , text model.httpInfo
        ]

      , div [class "titleDetails"]
        [
          h3 [] [text "Title details - "]
        , ul [] <| detailsListItems 
                    <| 
                    List.filter 
                      (\td -> 
                        case model.applyFilter of 
                          True -> td.length > 
                            model.filterLength
                          False -> True)
                    <| 
                    -- List.sortBy .length
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
    -- ]
    ]
    
-- firstSpecific : List TitleDetail -> TitleDetail
-- firstSpecific details = 
--   let m = List.head details
--   in 
--     case m of 
--       Just detail -> detail
--       Nothing     -> TitleDetail 0 0.0

-- resultsAsString2 : List String -> String
-- resultsAsString2 results = String.join ", " results

-- resultsAsString : List String -> String
-- resultsAsString strings = List.foldr 
--   --  (++) 
--   (appendResults)  
--   "" strings
  
-- appendResults : String -> String -> String
-- appendResults a b = a ++ ", " ++ b 


-- trigger : Signal Bool
-- trigger =
--   let stamped = Time.timestamp queryChnl.signal
--       delayed = Time.delay 500 stamped
--   in
-- --      Signal.map2 (==) stamped delayed
-- --        |> Signal.filter identity True
      
--   Signal.filter identity True
--     (Signal.map2 (==) stamped delayed)


-- port getVidInfoFileDetails : Signal (Task Http.Error ())        
-- port getVidInfoFileDetails =
--   Signal.map getFileDetails queryChnl.signal
--     |> Signal.sampleOn trigger
--     |> Signal.map (\task -> task `andThen` 
--                     Signal.send detailsChnl)
    
-- HANDLE RESPONSES

-- getDetail : String -> Task Http.Error (List String)
-- getDetail string = succeed [string]

-- getDetails : List String -> Task Http.Error (List String)
-- getDetails strings =
--   case strings of
--     string :: _ -> succeed [
--       Debug.log "files dets" "file details"
--       ]
--     [] ->
-- --      fail (Http.UnexpectedPayload "expecting 1 or more strings from server")
--         succeed ["no details found"]
        
-- getTitleDetails : List TitleDetail -> Task Http.Error (List String)
-- getTitleDetails details =
--   case details of
--     string :: _ -> succeed 
--       (List.map (toString) details)
-- --      [
-- ----      Debug.log "files dets" "file details"
-- --        "dummy title dets"
-- --      ]
--     [] ->
-- --      fail (Http.UnexpectedPayload "expecting 1 or more strings from server")
--         succeed ["no details found"]


-- GETSTRING FOR CUSTOM HEADERS

-- getStringCors : String -> Task Http.Error String
-- getStringCors url =
--   let request =
--         { verb = "GET"
--         , headers = [
-- --            ("Accept", "*/*")
--         ]
--         , url = url
--         , body = Http.empty
--         }
--   in
--       mapError promoteError (Http.send Http.defaultSettings request)
--         `andThen` handleResponse succeed

-- promoteError : Http.RawError -> Http.Error
-- promoteError rawError =
--   case rawError of
--     Http.RawTimeout -> Http.Timeout
--     Http.RawNetworkError -> Http.NetworkError
    
-- handleResponse : (String -> Task Http.Error a) -> Http.Response -> Task Http.Error a
-- handleResponse handle response =
--   if 200 <= response.status && response.status < 300 then

--       case response.value of
--         Http.Text str ->
--             handle str

--         _ ->
--             fail (Http.UnexpectedPayload "Response body is a blob, expecting a string.")

--   else
--       fail (Http.BadResponse response.status response.statusText)    


-- WIRING

main = Html.program { 
    init = init
  , view = view
  , update = update
  , subscriptions = subscriptions 
  }

-- These functions were removed from the Signal package. 
-- They allow the equivalent of map6 and beyond.
-- (<~) : (a -> b) -> Signal a -> Signal b
-- (<~) = Signal.map  
  
-- (~) : Signal (a -> b) -> Signal a -> Signal b
-- (~) funcs args =
--   Signal.map2 (\f v -> f v) funcs args

--infixl 4 ~

model : Model
model = { 
    count = 0
  -- , info = "initial state"
  , firstFileName = ""
  , currentFileName = ""
  , fileNames = []
  , xvals = []
  , titleDetails = [] 
  , sortDetailsByLength = False
  , applyFilter = False
  , showJsonErrors = True
  , filterLength = 0.0001
  , httpInfo = ""
  }

init = (
    model
  , Cmd.none)

subscriptions model = Sub.none

update : Msg -> Model -> (Model, Cmd Msg )
update msg model = 
  let 
    modelHttpReset = 
      { model | count = model.count + 1, httpInfo = "reset" }
  in
  case msg of 
    NoOp -> (model, Cmd.none)

-- UI settings
    SortDetails sort ->
      case sort of 
        SortByLength ->
          ( {model | count = model.count + 1, sortDetailsByLength = True }, Cmd.none )
        SortByNumber ->
          ( {model | count = model.count + 1, sortDetailsByLength = False }, Cmd.none )

    Filter ->
      ( {model | count = model.count + 1, applyFilter = not model.applyFilter }, Cmd.none )

    FilterLen s ->
      ( {model | count = model.count + 1, 
          filterLength = 
              (\s -> 
                case String.toFloat s of 
                  Ok val -> val
                  Err a  -> 0.0001
              ) s
        }, Cmd.none )

    ShowJsonErrors ->
      -- let
      --   _ = Debug.log "toggle show json errors" model.showJsonErrors 
      -- in
      ( { model | showJsonErrors = not model.showJsonErrors }, Cmd.none )


-- HTTP requests
    GetFirstFileName -> 
      ( modelHttpReset, 
          -- getFirstFileNameCmd  
          getFirstFileNameMaybeCmd 
            "string" 
      )

    GetFileNames -> 
      ( modelHttpReset, 
          -- getFileNamesCmd  
          getFileNamesMaybeCmd 
            "string" 
      )

    GetFileDetails ->
      ( modelHttpReset, 
          -- getFileDetailsCmd
          getFileDetailsMaybeCmd 
            model.currentFileName 
      )

    GetFileDetailsWrapped  ->
      ( modelHttpReset, getFileDetailsWrappedCmd model.currentFileName )

    GetCurrentFileNameDetails s -> 
      ( {model | count = model.count + 1, currentFileName = s }, 
          -- getFileDetailsCmd
          getFileDetailsMaybeCmd 
            s 
      )
      

-- HTTP responses
    InfoFirstFileName (Ok fileName) -> 
      ( {model | firstFileName = fileName }, Cmd.none)    
    -- InfoFirstFileName (Err _) -> (model, Cmd.none)
    InfoFirstFileName (Err err) -> handleHttpError model err

    InfoFirstFileNameMaybe (Ok maybeFileName) ->
      let
        fileName = 
          case maybeFileName of
            Just fileName -> fileName
            Nothing -> "dodgy data"
      in
      ( {model | firstFileName = fileName }, Cmd.none)
    InfoFirstFileNameMaybe (Err err) -> handleHttpError model err

    -- triggers http request for first file details
    InfoFileNames (Ok fileNames) -> 
      let 
        firstFileName = Maybe.withDefault "zzz" <| head fileNames
      in
        ( {model | fileNames = fileNames, currentFileName = firstFileName }, getFileDetailsCmd firstFileName)    
    InfoFileNames (Err err) -> 
      handleHttpError model err

    InfoFileNamesMaybe (Ok fileNames) ->
      let 
        filteredFileNames = filterMaybes model.showJsonErrors fileNames 
        firstFileName = 
          Maybe.withDefault "aaa" <| Maybe.withDefault (Just "xxx") <| head <| filteredFileNames
        _ = Debug.log "show json errors: " (toString model.showJsonErrors)
      in
        ( { model | currentFileName = firstFileName, 
                    fileNames = 
                      List.map (\m -> Maybe.withDefault "dodgy data" m) <| filteredFileNames 
          , count = model.count + 3 
          }, Cmd.none)       

    InfoFileNamesMaybe (Err err) -> 
      handleHttpError model err

    InfoTitleDetails (Ok titleDetails) -> 
      ( {model | titleDetails = titleDetails }, Cmd.none)    
    InfoTitleDetails (Err err) -> handleHttpError model err

    InfoTitleDetailsMaybe (Ok maybeTitleDetails) -> 
      ( { model | titleDetails = 
                    List.map (\m -> 
                      Maybe.withDefault ( { 
                          titleNumber = 0
                        , length = 0.0
                        }
                      ) m) <| filterMaybes model.showJsonErrors maybeTitleDetails 
        , count = model.count + 3 
        }, Cmd.none)       

    InfoTitleDetailsMaybe (Err err) -> 
      handleHttpError model err


filterMaybes showJsonErrors xs = 
  List.filter
  (\x -> 
    case x of 
      Just data -> True
      Nothing -> showJsonErrors
  )
  xs

respInfo : Http.Response String -> String
respInfo resp = 
             resp.url 
          ++ " Code: " ++ (toString resp.status.code) 
          ++ " Message: " ++ resp.status.message
          ++ " Headers: " ++ (toString resp.headers) 
          ++ " Body: " ++ resp.body        

handleError : Model -> String -> (Model, Cmd Msg)
handleError model s = ( {model | httpInfo = s }, Cmd.none)       

handleHttpError : Model -> Http.Error -> (Model, Cmd Msg) 
handleHttpError model err = 
  case err of 
    Timeout         -> handleError model "timeout"
    NetworkError    -> handleError model "nw error"
    BadUrl s        -> handleError model <| "bad url: " ++ s
    BadStatus resp  -> handleError model <| "bad status: " 
                        ++ (respInfo resp)
    BadPayload s resp  
                    -> handleError model <| "bad payload: " 
                        ++ "Details: " ++ s ++ " Response: " 
                        ++ (respInfo resp)

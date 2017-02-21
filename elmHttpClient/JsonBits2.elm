module JsonBits2 exposing (..)

import Http exposing (..)
import Json.Decode exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Maybe exposing (Maybe)

type alias Model = {
    count : Int
  , info : String
  , firstFileName : String 
  , currentFileName : String
  , fileNames : List String
  , xvals : List Int
  , titleDetails : List TitleDetail
  , sortDetailsByLength : Bool
  , filter : Bool
  , filterLength : Float
  , httpInfo : String
  }

type alias TitleDetail =
  { 
    titleNumber : Int
  , length: Float
  }

type SortBy = SortByLength | SortByNumber

type Msg = 
    NoOp
  | ButtonGet
  | ButtonGetFirstFileName
  | ButtonGetFileNames
  | ButtonGetFileDetails
  | ButtonGetFileDetailsWrapped

  | SortDetails SortBy

  | Filter
  | FilterLen String

  | CurrentFileName String

  | Info (Result Http.Error String)
  | InfoFirstFileName (Result Http.Error String)
  | InfoFileNames (Result Http.Error (List String))
  | InfoFileNamesMaybe (Result Http.Error (List (Maybe String)))
  | InfoTitleDetails (Result Http.Error (List TitleDetail))

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
onChangeFileName = onChange (\s -> CurrentFileName s)          

checkbox : Msg -> String -> Html.Html Msg
checkbox msg name =
  label
    [ 
      -- style [("padding", "20px")]
    ]
    [ input [ type_ "checkbox", onClick msg ] []
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

vidInfoFilesURL : String
vidInfoFilesURL = "http://localhost:8000/vidInfo/files" 

vidInfoURL : String
vidInfoURL = "http://localhost:8000/vidInfo"

vidInfoURLWrapped : String
vidInfoURLWrapped = "http://localhost:8000/vidInfoWrapped"

titleDetailsList3a : Json.Decode.Decoder (String)
titleDetailsList3a =
  (field "titleDetails" Json.Decode.string) 
    |> andThen 
    -- sort of a NoOp 
             (\s ->
                -- create Decoder String that decodes to first file name
                Json.Decode.succeed <| s
             )

titleDetailsList4 : Json.Decode.Decoder (List Int)
titleDetailsList4 =
  (field "titleDetails" string) |> 
    andThen 
      (\s ->
        decodeString (Json.Decode.list (field "titleNumber" int)) s |>
          (\r ->
            case r of 
              Ok xs -> Json.Decode.succeed xs
              Err err -> Json.Decode.fail <| err ++ "xx"
          )
      )

titleDetailDecoder : Decoder TitleDetail
titleDetailDecoder = map2 TitleDetail (field "titleNumber" int) (field "length" float)

titleDetailsList : Json.Decode.Decoder (List TitleDetail)
titleDetailsList =
  (field "titleDetails" string) |> 
    andThen 
      (\s ->
        decodeString (Json.Decode.list titleDetailDecoder) s |>
          (\result ->
            case result of 
              Ok xs -> Json.Decode.succeed xs
              Err err -> Json.Decode.fail <| err ++ "xx"
          )
      )

getFileDetailsReqWrapped : String -> Http.Request (List TitleDetail)
getFileDetailsReqWrapped string =
  Http.get 
    (vidInfoURLWrapped ++ "/" ++ string)
    titleDetailsListWrapped

httpSendTitleDetails : Request (List TitleDetail) -> Cmd Msg
httpSendTitleDetails = Http.send InfoTitleDetails

getFileDetailsWrapped : String -> Cmd Msg
getFileDetailsWrapped string = 
  httpSendTitleDetails <| getFileDetailsReqWrapped string

getFileDetails : String -> Cmd Msg
getFileDetails string = 
  httpSendTitleDetails <| getFileDetailsReq string

titleDetailsListWrapped : Json.Decode.Decoder (List TitleDetail)
titleDetailsListWrapped =
  Json.Decode.at ["wrapper", "titleDetails"] <| 
    Json.Decode.list <|
      titleDetailDecoder

getFileDetailsReq : String -> Http.Request (List TitleDetail)
getFileDetailsReq string =
  Http.get 
    (vidInfoURL ++ "\\" ++ string)
    -- titleDetailsListOrig
    titleDetailsList

--  customDecoder decoder toResult = 
--    Json.Decode.andThen
--            (\a ->
--                  case toResult a of 
--                     Ok b -> Json.Decode.succeed b
--                     Err err -> Json.Decode.fail err
--            )
--            decoder

-- decodeString (field "titleDetails" string) """{\"titleDetails\":\"[1,2]\"}"""
-- Ok "[1,2]" : Result.Result String String
-- decodeString (list (field "titleNumber" int)) "[{\"line\":\"ID_DVD_TITLE_1_LENGTH=0.480\",\"titleNumber\":1,\"length\":0.48}]"
-- Ok [1] : Result.Result String (List Int)


-- from elm packages page, works in elm repl
-- decodeString (list string) "[\"test1\", \"test2\"]"

getFirstStringx : List String -> Json.Decode.Decoder String
getFirstStringx strings =

-- ODDLY, this line below worked, with the anon fn below
-- all taken from customDecoder code
  -- stringList |> andThen 

-- customDecoder decoder toResult = 
  --  Json.Decode.andThen
            --  (\xs ->
                -- create Decoder String that decodes to first file name
                -- succeed : a -> Decoder a
                Json.Decode.succeed <|
                  Maybe.withDefault "" <| List.head strings --xs 
            --  )
            --  decoder


getFirstString : Decoder String
getFirstString = 
  stringList |> andThen 
             (\xs ->
                -- create Decoder String that decodes to first file name
                Json.Decode.succeed <|
                  Maybe.withDefault "" <| List.head xs 
             )
            --  decoder

stringList : Json.Decode.Decoder (List String)    
stringList = Json.Decode.list Json.Decode.string

maybeStringList : Json.Decode.Decoder (List (Maybe.Maybe String))    
maybeStringList = Json.Decode.list (nullable Json.Decode.string)
-- decodeString (list (nullable string)) """["42", null, "43"]"""
-- decodeString maybeStringListx """["42", null, "43"]"""

getFirstFileName : String -> Cmd Msg
getFirstFileName string = 
  Http.send InfoFirstFileName <|
    Http.get 
      vidInfoFilesURL 
      getFirstString

getFileNames : String -> Cmd Msg
getFileNames string = Http.send InfoFileNames <| getFileNamesReq

getFileNamesReq : Http.Request (List String)
getFileNamesReq = Http.get vidInfoFilesURL stringList

getFileNamesMaybe : String -> Cmd Msg
getFileNamesMaybe string = Http.send InfoFileNamesMaybe <| getFileNamesMaybeReq

getFileNamesMaybeReq : Http.Request (List (Maybe String))
getFileNamesMaybeReq = Http.get vidInfoFilesURL maybeStringList

getFirstFileNameTest : String -> Cmd Msg
getFirstFileNameTest string = 
  Http.send InfoFirstFileName <|
    Http.get 
      ("https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ "kitten")
      getFirstStringTest

getFirstStringTest : Json.Decode.Decoder String
getFirstStringTest =
  Json.Decode.at ["data", "image_url"] Json.Decode.string


  -- decodeString (list string) "[\"test1\", \"test2\"]"

-- see parse.html for testing this data
-- {"data":{"type":"gif","id":"3gTiFkr0fgt9K","url":"http:\/\/giphy.com\/gifs\/mom-almost-copy-3gTiFkr0fgt9K","image_original_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/giphy.gif","image_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/giphy.gif","image_mp4_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/giphy.mp4","image_frames":"51","image_width":"600","image_height":"723","fixed_height_downsampled_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/200_d.gif","fixed_height_downsampled_width":"166","fixed_height_downsampled_height":"200","fixed_width_downsampled_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/200w_d.gif","fixed_width_downsampled_width":"200","fixed_width_downsampled_height":"241","fixed_height_small_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/100.gif","fixed_height_small_still_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/100_s.gif","fixed_height_small_width":"83","fixed_height_small_height":"100","fixed_width_small_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/100w.gif","fixed_width_small_still_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/100w_s.gif","fixed_width_small_width":"100","fixed_width_small_height":"121","username":"","caption":""},"meta":{"status":200,"msg":"OK","response_id":"588193312fdd614bd4b29d28"}}

getFileNamesAsStringReq : String -> Http.Request String
getFileNamesAsStringReq string = Http.getString vidInfoFilesURL        

getFileNamesAsStringCmd : Cmd Msg
getFileNamesAsStringCmd = Http.send Info <| getFileNamesAsStringReq ""

-- various experiments in Elm repl, to get json parsing correct
-- 
-- "{\"fileName\":\"red-info.txt\",\"titleDetails\":\"[{\"line\":\"ID_DVD_TITLE_1_LENGTH=0.480\",\"titleNumber\":1,\"length\":0.48}]\"}"

-- 
-- {"fileName":"red-info.txt","titleDetails":

-- decodeString JsonBits.titleDetailsList3a "{\"fileName\":\"red-info.txt\",\"titleDetails\":\"[{\"line\":\"ID_DVD_TITLE_1_LENGTH=0.480\",\"titleNumber\":1,\"length\":0.48}]\"}"

-- decodeString JsonBits.titleDetailsList3a "{\"titleDetails\":\"[{\"line\":\"ID_DVD_TITLE_1\",\"titleNumber\":1,\"length\":0.48}]\"}"

-- decodeString JsonBits.titleDetailsList3a "{"fileName":"red-info.txt","titleDetails":"[{\"line\":\"ID_DVD_TITLE_1_LENGTH=0.480\",\"titleNumber\":1,\"length\":0.48}]"}"

-- can get a string value from a simple object
-- decodeString (field "fileName" string) """{ "fileName":"red-info.txt" }"""
-- Ok "red-info.txt" : Result.Result String String

-- so, it seems I can get a string from the field
-- decodeString (field "titleDetails" string) """{\"titleDetails\":\"[1,2]\"}"""
-- Ok "[1,2]" : Result.Result String String

-- and, given a string of a list, can get an actual list
-- decodeString (list int) "[1,2]"
-- Ok [1,2] : Result.Result String (List Int)

-- can get an empty list
-- decodeString (field "titleDetails" string) """{\"titleDetails\":\"[]\"}"""
-- Ok "[]" : Result.Result String String

-- and can get test list as decoded on its own (seem to have done this step earlier, too)
-- decodeString (field "titleDetails" string) """{\"titleDetails\":\"[1,2]\"}"""
-- Ok "[1,2]" : Result.Result String String

-- from the actual list, can get a single int
-- decodeString (list (field "titleNumber" int)) "[{\"line\":\"ID_DVD_TITLE_1_LENGTH=0.480\",\"titleNumber\":1,\"length\":0.48}]"
-- Ok [1] : Result.Result String (List Int)

-- from the actual list, can get a single string
-- decodeString (list (field "line" string)) "[{\"line\":\"ID_DVD_TITLE_1_LENGTH=0.480\",\"titleNumber\":1,\"length\":0.48}]"
-- Ok ["ID_DVD_TITLE_1_LENGTH=0.480"] : Result.Result String (List String)

-- from the actual list, can get multiple strings
-- decodeString (list (field "line" string)) "[{\"line\":\"ID_DVD_TITLE_1_LENGTH=0.480\",\"titleNumber\":1,\"length\":0.48}, {\"line\":\"ID_DVD_TITLE_1_LENGTH=0.481\",\"titleNumber\":2,\"length\":0.481}]"
-- Ok ["ID_DVD_TITLE_1_LENGTH=0.480","ID_DVD_TITLE_1_LENGTH=0.481"]
--     : Result.Result String (List String)



module FlickaRandom018 exposing (..)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (field)
import String
import Task exposing (..)
import Time exposing (..)
import Window
import Random exposing (..)

type Msg = 
      Source (Maybe String)
    | Search String
    | SizeChange Window.Size
    | InitialTime Time

type alias Model = {
    count   : Int
  , source  : String
  , winSize : Window.Size
  , seed    : Random.Seed
  }

main : Program Never Model Msg
main = Html.program { 
    init = init
  , view = view
  , update = update
  , subscriptions = subscriptions 
  }

model : Model
model = { 
    count = 0
  , source = "https://farm5.staticflickr.com/4165/34161186276_1a4b84661e_z.jpg"    
  , winSize = { width = 1000, height = 1000 }
  , seed = Random.initialSeed 0
  }

init : (Model, Cmd Msg) 
init = 
  (
    model
  , Cmd.batch 
      [ Task.perform InitialTime Time.now
      , Task.perform SizeChange Window.size
      ]
  )

subscriptions : Model -> Sub Msg
subscriptions model = Window.resizes (\size -> SizeChange size)

update : Msg -> Model -> (Model, Cmd Msg )
update msg model = 
  case msg of 
    Search s ->
      let 
        twoRands            = Random.map2 (,) randInt randInt
        ((r1, r2), newSeed) = Random.step twoRands model.seed
        flikr s             = 
          Task.perform Source (getFlickrImage (model.winSize.width, model.winSize.height) r1 r2 s)
      in
        ( { model | seed = newSeed, count = model.count + 1 }, flikr s)
    Source s ->
      case s of
        Just src ->
          ( { model | count = model.count + 1, source = src }, Cmd.none )
        Nothing ->
          ( { model | count = model.count + 1 }, Cmd.none )
          
    SizeChange size ->
        ( { model | count = model.count + 1, winSize = size }, Cmd.none )
    InitialTime t ->
        ( { model | seed = Random.initialSeed <| round t }, Cmd.none )

randInt : Generator Int
randInt = (Random.int 1 10)
    
view : Model -> Html Msg
view model =
  div [ 
        style (imgStyle model.winSize.height model.source)
      ]
    [ 
      input
      [ placeholder "Flickr Query"
      , onInput Search 
      , style myStyle
      ]
      []
    ]

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

getFlickrImage : (Int,Int) -> Int -> Int -> String -> Task x (Maybe String)
getFlickrImage dimensions r1 r2 tag =
  let searchArgs =
        [ ("sort", "random"), ("per_page", "10"), ("tags", tag) ]
  in
    (Http.toTask (Http.get (createFlickrURL "search" searchArgs r1)     photoList )) -- decoder
      |> (Task.andThen                                                  selectPhoto )
      |> (Task.andThen                                                  (\photo ->
                (Http.toTask 
                  (Http.get 
                    (createFlickrURL 
                      "getSizes" [ ("photo_id", photo.id) ] r2)         sizeList ))) -- decoder
         )
      |> (Task.andThen <|                                               pickSize dimensions)
      |> (onError 
            (\err ->
                                                                        succeed Nothing 
            )
         )
             
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

photoList : Json.Decode.Decoder (List Photo)
photoList =
  Json.Decode.at ["photos","photo"] <| Json.Decode.list <|
      Json.Decode.map2 Photo
        (field "id" Json.Decode.string)
        (field "title" Json.Decode.string)

sizeList : Json.Decode.Decoder (List Size)
sizeList =
  let number =
        Json.Decode.oneOf [ Json.Decode.int, customDecoder Json.Decode.string String.toInt ]
  in
      Json.Decode.at ["sizes","size"] <| Json.Decode.list <|
          Json.Decode.map3 Size
            (field "source" Json.Decode.string)
            (field "width" number)
            (field "height" number)

customDecoder decoder toResult = 
   Json.Decode.andThen (\a ->
                          case toResult a of 
                            Ok b -> Json.Decode.succeed b
                            Err err -> Json.Decode.fail err
                       )
                       decoder

-- HANDLE RESPONSES
selectPhoto : List Photo -> Task Http.Error Photo
selectPhoto photos =
  case photos of
    photo :: _ -> succeed photo
    [] ->
      let
        _ = Debug.log "no photo: " photos
      in         
        fail Http.Timeout -- easier to return this error in 018 for now

pickSize : (Int,Int) -> List Size -> Task Http.Error (Maybe String)
pickSize (width,height) sizes =
  let sizeRating size =
        let penalty =
              if size.width > width || size.height > height then 400 else 0
        in
            abs (width - size.width) + abs (height - size.height) + penalty
  in
      case List.sortBy sizeRating sizes of
        size :: _ -> succeed <| Just size.source
        [] -> fail Http.Timeout -- easier to return this error in 018 for now

-- NOTE: we randomly break the URL to test how well Elm copes
createFlickrURL : String -> List (String, String) -> Int -> String
createFlickrURL method args r =
  let
    mainUrlSectionBase = "https://api.flickr.com/services/rest/"
    mainUrlSection = 
      -- case rem r 3 of
      --   0         -> mainUrlSectionBase ++ "x"
      --   otherwise -> 
          mainUrlSectionBase  
  in
    url mainUrlSection <|
      [ ("format", "json")
      , ("nojsoncallback", "1")
      , ("api_key", "9be5b08cd8168fa82d136aa55f1fdb3c")
      , ("method", "flickr.photos." ++ method)
      ] ++ args

url : String -> List (String,String) -> String
url baseUrl args =
  case args of
    [] -> baseUrl
    _  -> baseUrl ++ "?" ++ String.join "&" (List.map queryPair args)

queryPair : (String,String) -> String
queryPair (key,value) =
  queryEscape key ++ "=" ++ queryEscape value

queryEscape : String -> String
queryEscape string =
  String.join "+" (String.split "%20" (Http.encodeUri string))

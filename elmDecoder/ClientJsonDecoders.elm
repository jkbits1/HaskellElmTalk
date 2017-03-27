module ClientJsonDecoders exposing (..)

import Json.Decode exposing (..)

import ClientTypes exposing (TitleDetail)

stringListDecoder : Json.Decode.Decoder (List String)    
stringListDecoder = Json.Decode.list Json.Decode.string

firstStringDecoder : Decoder String
firstStringDecoder = 
  stringListDecoder |> 
    andThen 
      (\xs ->
        -- create Decoder String that decodes to first file name
        Json.Decode.succeed <|
          Maybe.withDefault "" <| List.head xs 
      )

maybeStringListDecoder : Json.Decode.Decoder (List (Maybe String))    
maybeStringListDecoder = Json.Decode.list (nullable Json.Decode.string)
-- decodeString (list (nullable string)) """["42", null, "43"]"""
-- decodeString maybeStringListx """["42", null, "43"]"""

maybeFirstStringDecoder : Decoder (Maybe String)
maybeFirstStringDecoder = 
  maybeStringListDecoder |> 
    andThen 
      (\xs ->
        -- create Decoder String that decodes to first file name
        Json.Decode.succeed <| 
          Maybe.withDefault Nothing <| List.head xs
      )

titleDetailDecoder : Decoder TitleDetail
titleDetailDecoder = map2 TitleDetail (field "titleNumber" int) (field "length" float)

-- this decoder supplies a default value for length property
titleDetailOptLenDecoder : Decoder TitleDetail
titleDetailOptLenDecoder = 
  map2 TitleDetail 
    (field "titleNumber" int) 
    <| Json.Decode.succeed 0.0 

titleDetailListDecoder : Json.Decode.Decoder (List TitleDetail)
titleDetailListDecoder =
  field "titleDetails" string |> 
    andThen 
      (\s ->
        decodeString (Json.Decode.list titleDetailDecoder) s |>
          (\result ->
            case result of 
              Ok xs -> Json.Decode.succeed xs
              Err err -> Json.Decode.fail <| err ++ " fn: titleDetailListDecoder "
          )
      )

titleDetailListMaybeDecoder : Json.Decode.Decoder (List (Maybe TitleDetail))
titleDetailListMaybeDecoder =
  field "titleDetails" (nullable string) |> 
    andThen 
      (\m ->
        let 
          json = 
            case m of 
              Just s -> s
              Nothing -> ""
          detailsDecoder = 
            nullable <|
              oneOf [
                  titleDetailDecoder
                , titleDetailOptLenDecoder
                ]
        in
          decodeString (Json.Decode.list detailsDecoder) json |>
            (\result ->
              case result of 
                Ok xs -> Json.Decode.succeed xs
                Err err -> Json.Decode.fail <| err ++ " fn: titleDetailListMaybeDecoder "
            )
      )

titleDetailListWrapped : Json.Decode.Decoder (List TitleDetail)
titleDetailListWrapped =
  Json.Decode.at ["wrapper", "titleDetails"] <| 
    Json.Decode.list <|
      titleDetailDecoder

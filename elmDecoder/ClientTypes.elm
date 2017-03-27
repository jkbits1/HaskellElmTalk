module ClientTypes exposing (..)

import Http exposing (Error)

type alias Model = {
    count : Int
  , firstFileName : String 
  , currentFileName : String
  , fileNames : List String
  , xvals : List Int
  , titleDetails : List TitleDetail
  , sortDetailsByLength : Bool
  , applyFilter : Bool
  , showJsonErrors : Bool
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

  | GetFirstFileName
  | GetFileNames
  | GetFileDetails
  | GetFileDetailsWrapped

  | SortDetails SortBy

  | Filter
  | FilterLen String

  | ShowJsonErrors

  | GetCurrentFileNameDetails String

  | InfoFirstFileName (Result Http.Error String)
  | InfoFirstFileNameMaybe (Result Http.Error (Maybe String))

  | InfoFileNames (Result Http.Error (List String))
  | InfoFileNamesMaybe (Result Http.Error (List (Maybe String)))

  | InfoTitleDetails (Result Http.Error (List TitleDetail))
  | InfoTitleDetailsMaybe (Result Http.Error (List (Maybe TitleDetail)))



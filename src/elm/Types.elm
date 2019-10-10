module Types exposing (Filters, Flags, Model, Msg(..), Page, Paging, Sort(..), SortDirection(..))

import Data exposing (Establishment, Stars, UserRating)
import Http


type alias Flags =
    ()


type alias Model =
    { error : Maybe Http.Error
    , establishments : List Establishment
    , filters : Filters
    , paging : Paging
    , sort : Sort
    }


type Msg
    = GetEstablishments (Result Http.Error (List Establishment))
    | FilterName String
    | FilterStars (Maybe Stars)
    | FilterRating (Maybe Float)
    | FilterCost (Maybe ( Float, Float ))
    | FilterReset
    | UrlChange
    | UrlRequest


type alias Paging =
    { per : Int
    , current : Int
    }


type alias Page =
    { paging : Paging
    , total_pages : Int
    , total_items : Int
    }


type alias Filters =
    { name : Maybe String
    , stars : Maybe Stars
    , userRating : Maybe Float
    , minCost : Maybe ( Float, Float )
    }


type SortDirection
    = ASC
    | DESC


type Sort
    = Distance SortDirection
    | Stars SortDirection
    | MinCost SortDirection
    | UserRating SortDirection

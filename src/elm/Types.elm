module Types exposing (Flags, Model, Msg(..), Page, Paging)

import Data exposing (Establishment, Stars, UserRating)
import Filtering exposing (Filters)
import Http
import Sorting exposing (Sort, SortDirection, SortField)


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
    | UpdateSortField SortField
    | UpdateSortDirection SortDirection
    | FilterReset
    | NoOp


type alias Paging =
    { per : Int
    , current : Int
    }


type alias Page =
    { paging : Paging
    , total_pages : Int
    , total_items : Int
    }

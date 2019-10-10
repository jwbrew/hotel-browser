module Sorting exposing (Sort, SortDirection(..), SortField(..), fieldToString, parseField, sortEstablishments)

import Data exposing (Establishment)


type SortDirection
    = ASC
    | DESC


type SortField
    = Distance
    | Stars
    | MinCost
    | UserRating


type alias Sort =
    { field : SortField
    , dir : SortDirection
    }


applySortDirection : SortDirection -> List Establishment -> List Establishment
applySortDirection direction establishmentList =
    case direction of
        ASC ->
            establishmentList

        DESC ->
            List.reverse establishmentList


fieldToString : SortField -> String
fieldToString sortField =
    case sortField of
        Distance ->
            "Distance"

        Stars ->
            "Stars"

        MinCost ->
            "MinCost"

        UserRating ->
            "UserRating"


parseField : String -> Maybe SortField
parseField string =
    case string of
        "Distance" ->
            Just Distance

        "Stars" ->
            Just Stars

        "MinCost" ->
            Just MinCost

        "UserRating" ->
            Just UserRating

        _ ->
            Nothing


sortEstablishments : Sort -> List Establishment -> List Establishment
sortEstablishments sort establishmentList =
    applySortDirection sort.dir <|
        case sort.field of
            Distance ->
                List.sortBy (Data.establishmentAttributes >> .distance) establishmentList

            Stars ->
                List.sortBy (Data.establishmentAttributes >> .stars >> Data.starsToInt) establishmentList

            MinCost ->
                List.sortBy (Data.establishmentAttributes >> .minCost) establishmentList

            UserRating ->
                List.sortBy
                    (Data.establishmentAttributes
                        >> .userRating
                        >> Data.getUserRatingScore
                        >> Maybe.withDefault 0
                    )
                    establishmentList

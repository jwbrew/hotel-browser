module Filtering exposing (Filters, filterEstablishments, parseCostRange)

import Data exposing (Establishment, Stars)
import Helpers exposing (..)


type alias Filters =
    { name : Maybe String
    , stars : Maybe Stars
    , userRating : Maybe Float
    , minCost : Maybe ( Float, Float )
    }


filterEstablishments : Filters -> List Establishment -> List Establishment
filterEstablishments f establishmentList =
    let
        nameFilter =
            Data.establishmentAttributes >> .name >> String.toLower >> flip String.contains >> defaultTrue

        starsFilter =
            Data.establishmentAttributes >> .stars >> (==) >> defaultTrue

        costFilter =
            Data.establishmentAttributes
                >> .minCost
                >> (\minCost ( minimum, maximum ) -> minCost >= minimum && minCost <= maximum)
                >> defaultTrue

        ratingFilter =
            Data.establishmentAttributes
                >> .userRating
                >> (\r query ->
                        case Data.getUserRatingScore r of
                            Nothing ->
                                False

                            Just score ->
                                score >= query
                   )
                >> defaultTrue
    in
    List.filter
        (\e -> nameFilter e f.name && starsFilter e f.stars && costFilter e f.minCost && ratingFilter e f.userRating)
        establishmentList


parseCostRange : String -> Maybe ( Float, Float )
parseCostRange string =
    case
        String.split "-" string
            |> List.filterMap String.toFloat
    of
        [ x, y ] ->
            Just ( x, y )

        _ ->
            Nothing

module View exposing (view)

import Browser exposing (Document)
import Data exposing (Establishment(..), Stars(..), UserRating(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error(..))
import Types exposing (Filters, Model, Msg(..), Page, Paging, Sort)


flip : (a -> b -> c) -> b -> a -> c
flip fn b a =
    fn a b


defaultTrue : (a -> Bool) -> Maybe a -> Bool
defaultTrue testFn maybeFilter =
    Maybe.map testFn maybeFilter |> Maybe.withDefault True


filter : List Establishment -> Filters -> List Establishment
filter establishmentList f =
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


currentPage : Paging -> Filters -> Sort -> List Establishment -> ( List Establishment, Page )
currentPage paging f sort establishmentList =
    let
        establishments =
            filter establishmentList f

        total =
            List.length establishments

        page =
            { paging = paging, total_pages = total // paging.per, total_items = total }
    in
    ( establishments |> List.drop (page.paging.current * page.paging.per) |> List.take page.paging.per
    , page
    )


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


filters : Filters -> Html Msg
filters f =
    div [ class "filters" ]
        [ input
            [ type_ "text"
            , class "filters__input filters__input--text"
            , placeholder "Search"
            , onInput (String.toLower >> FilterName)
            ]
            []
        , select
            [ class "filters__input filters__input--select"
            , String.toInt >> Maybe.andThen Data.intToStars >> FilterStars |> onInput
            ]
            [ option [ selected <| f.stars == Nothing ] [ text "Any Star Rating" ]
            , option [ selected <| f.stars == Just FiveStar, Data.starsToInt FiveStar |> String.fromInt |> value ] [ text "5 Stars" ]
            , option [ selected <| f.stars == Just FourStar, Data.starsToInt FourStar |> String.fromInt |> value ] [ text "4 Stars" ]
            , option [ selected <| f.stars == Just ThreeStar, Data.starsToInt ThreeStar |> String.fromInt |> value ] [ text "3 Stars" ]
            , option [ selected <| f.stars == Just TwoStar, Data.starsToInt TwoStar |> String.fromInt |> value ] [ text "2 Stars" ]
            , option [ selected <| f.stars == Just OneStar, Data.starsToInt OneStar |> String.fromInt |> value ] [ text "1 Star" ]
            , option [ selected <| f.stars == Just ZeroStar, Data.starsToInt ZeroStar |> String.fromInt |> value ] [ text "0 Stars" ]
            ]
        , select
            [ class "filters__input filters__input--select", String.toFloat >> FilterRating |> onInput ]
            [ option [ selected <| f.userRating == Nothing ] [ text "Any User Rating" ]
            , option [ selected <| f.userRating == Just 8.0, value "8" ] [ text "8.0 +" ]
            , option [ selected <| f.userRating == Just 6.0, value "6" ] [ text "6.0 +" ]
            , option [ selected <| f.userRating == Just 4.0, value "4" ] [ text "4.0 +" ]
            , option [ selected <| f.userRating == Just 2.0, value "2" ] [ text "2.0 +" ]
            ]
        , select
            [ class "filters__input filters__input--select", parseCostRange >> FilterCost |> onInput ]
            [ option [ selected <| f.minCost == Nothing ] [ text "Any Price" ]
            , option [ selected <| f.minCost == Just ( 1000.0, 100000.0 ), value "1000-100000" ] [ text "£1000 +" ]
            , option [ selected <| f.minCost == Just ( 800.0, 1000.0 ), value "800-1000" ] [ text "£800 - £1000" ]
            , option [ selected <| f.minCost == Just ( 600.0, 800.0 ), value "600-800" ] [ text "£600 - £800" ]
            , option [ selected <| f.minCost == Just ( 400.0, 600.0 ), value "400-600" ] [ text "£400 - £600" ]
            , option [ selected <| f.minCost == Just ( 200.0, 400.0 ), value "200-400" ] [ text "£200 - £400" ]
            , option [ selected <| f.minCost == Just ( 0.0, 200.0 ), value "0-200" ] [ text "£0 - £200" ]
            ]
        ]


view : Model -> Document Msg
view model =
    { title = "My App"
    , body =
        [ div [ class "p-2 bg-gray-100" ] <|
            case model.error of
                Just e ->
                    [ error e ]

                Nothing ->
                    [ filters model.filters
                    , currentPage model.paging model.filters model.sort model.establishments |> listEstablishments
                    ]
        ]
    }


listItem : Establishment -> Html Msg
listItem establishment =
    let
        a =
            Data.establishmentAttributes establishment
    in
    li [ class "establishment mx-2" ]
        [ img [ src a.image.main ] []
        , div [ class "establishment__content" ]
            [ span [ class "establishment__title" ] [ text a.name ]
            , div [ class "flex justify-between mb-6" ]
                [ span [ class "establishment__location" ] [ text a.location ]
                , distance a.distance
                ]
            , div [ class "flex justify-between items-end" ]
                [ div
                    []
                    [ stars a.stars
                    , rating a.userRating
                    ]
                , div [ class "establishment__cost" ]
                    [ span [ class "establishment__cost--intro" ] [ text "from" ]
                    , money a.minCost
                    ]
                ]
            ]
        ]


distance : Float -> Html Msg
distance float =
    let
        d =
            float * 10 |> round |> toFloat |> (\x -> x / 10)
    in
    span [ class "establishment__distance" ] [ String.fromFloat d ++ " miles away" |> text ]


money : Float -> Html Msg
money float =
    span [ class "money" ]
        [ span [ class "money__symbol" ] [ text "£" ]
        , span [ class "money__amount" ] [ String.fromFloat float |> text ]
        ]


stars : Stars -> Html Msg
stars s =
    case s of
        ZeroStar ->
            text "☆☆☆☆☆"

        OneStar ->
            text "★☆☆☆☆"

        TwoStar ->
            text "★★☆☆☆"

        ThreeStar ->
            text "★★★☆☆"

        FourStar ->
            text "★★★★☆"

        FiveStar ->
            text "★★★★★"


rating : UserRating -> Html Msg
rating userRating =
    case userRating of
        Unrated ->
            span [ class "establishment__rating--detail" ] [ text "No Visitor Reviews" ]

        VeryPoor r c ->
            ratingDetail "Very Poor" r c

        Poor r c ->
            ratingDetail "Poor" r c

        Unsatisfactory r c ->
            ratingDetail "Unsatisfactory" r c

        BelowAverage r c ->
            ratingDetail "Below Average" r c

        Average r c ->
            ratingDetail "Average" r c

        AboveAverage r c ->
            ratingDetail "Above Average" r c

        Good r c ->
            ratingDetail "Good" r c

        VeryGood r c ->
            ratingDetail "Very Good" r c

        Great r c ->
            ratingDetail "Great" r c

        Excellent r c ->
            ratingDetail "Excellent" r c

        Magnificent r c ->
            ratingDetail "Magnificent" r c

        Exceptional r c ->
            ratingDetail "Exceptional" r c

        Spectacular r c ->
            ratingDetail "Spectacular" r c


ratingDetail : String -> Float -> Int -> Html Msg
ratingDetail intro r c =
    span [ class "establishment__rating" ]
        [ span [ class "establishment__rating--intro" ] [ text intro ]
        , span [ class "establishment__rating--detail" ] [ text <| String.fromFloat r ++ " average from " ++ String.fromInt c ++ " visitor ratings" ]
        ]


listEstablishments : ( List Establishment, Page ) -> Html Msg
listEstablishments ( establishmentList, page ) =
    List.map listItem establishmentList
        |> ul [ class "flex flex-wrap justify-center" ]


error : Http.Error -> Html Msg
error errorHttp =
    case errorHttp of
        BadUrl string ->
            div [ class "alert-error" ] [ text <| "Bad Url: " ++ string ]

        Timeout ->
            div [ class "alert-error" ] [ text "Timeout" ]

        NetworkError ->
            div [ class "alert-error" ] [ text "Network Error" ]

        BadStatus int ->
            div [ class "alert-error" ] [ text <| "Bad Status: " ++ String.fromInt int ]

        BadBody string ->
            pre [ class "alert-error whitespace-pre-wrap" ] [ text string ]

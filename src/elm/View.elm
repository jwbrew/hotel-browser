module View exposing (view)

import Browser exposing (Document)
import Data exposing (Establishment(..), Stars(..), UserRating(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error(..))
import Types exposing (Filters, Model, Msg, Page, Paging, Sort)


filter : List Establishment -> Filters -> List Establishment
filter establishmentList filters =
    establishmentList


currentPage : Paging -> Filters -> Sort -> List Establishment -> ( List Establishment, Page )
currentPage paging filters sort establishmentList =
    let
        establishments =
            filter establishmentList filters

        total =
            List.length establishments

        page =
            { paging = paging, total_pages = total // paging.per, total_items = total }
    in
    ( establishments |> List.drop (page.paging.current * page.paging.per) |> List.take page.paging.per
    , page
    )


view : Model -> Document Msg
view model =
    { title = "My App"
    , body =
        [ div [ class "container p-4 bg-gray-100" ] <|
            case model.error of
                Just e ->
                    [ error e ]

                Nothing ->
                    [ currentPage model.paging model.filters model.sort model.establishments |> listEstablishments ]
        ]
    }


listItem : Establishment -> Html Msg
listItem establishment =
    let
        a =
            Data.establishmentAttributes establishment
    in
    li [ class "establishment" ]
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
            text "No Visitor Reviews"

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
        |> ul []


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

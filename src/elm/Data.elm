module Data exposing
    ( Establishment(..)
    , Stars(..)
    , UserRating(..)
    , establishmentAttributes
    , getEstablishments
    , getUserRatingScore
    , intToStars
    , starsToInt
    )

import Http
import Json.Decode exposing (..)


type alias EstablishmentAttributes =
    { distance : Float
    , id : Int
    , location : String
    , minCost : Float
    , name : String
    , stars : Stars
    , userRating : UserRating
    , image : Image
    }


type Establishment
    = Hotel EstablishmentAttributes
    | Apartment EstablishmentAttributes
    | GuestHouse EstablishmentAttributes
    | Hostel EstablishmentAttributes


type Stars
    = ZeroStar
    | OneStar
    | TwoStar
    | ThreeStar
    | FourStar
    | FiveStar


type UserRating
    = Unrated
    | VeryPoor Float Int
    | Poor Float Int
    | Unsatisfactory Float Int
    | BelowAverage Float Int
    | Average Float Int
    | AboveAverage Float Int
    | Good Float Int
    | VeryGood Float Int
    | Great Float Int
    | Excellent Float Int
    | Magnificent Float Int
    | Exceptional Float Int
    | Spectacular Float Int


type alias Image =
    { main : String
    , thumbnail : String
    }


getEstablishments : (Result Http.Error (List Establishment) -> msg) -> Cmd msg
getEstablishments msg =
    Http.get
        { url = "http://localhost:8000/api/hotels.json"
        , expect = Http.expectJson msg (field "Establishments" <| list decoder)
        }


decoder : Decoder Establishment
decoder =
    field "EstablishmentType" string
        |> andThen establishmentDecoder


establishmentDecoder : String -> Decoder Establishment
establishmentDecoder string =
    case string of
        "Hotel" ->
            map Hotel establishmentAttributesDecoder

        "Apartment" ->
            map Apartment establishmentAttributesDecoder

        "Guest House" ->
            map GuestHouse establishmentAttributesDecoder

        "Hostel" ->
            map Hostel establishmentAttributesDecoder

        _ ->
            fail <| "Invalid EstablishmentType: " ++ string


imageDecoder : String -> Decoder Image
imageDecoder main =
    map (Image main) (field "ThumbnailUrl" string)


starsToInt : Stars -> Int
starsToInt stars =
    case stars of
        ZeroStar ->
            0

        OneStar ->
            1

        TwoStar ->
            2

        ThreeStar ->
            3

        FourStar ->
            4

        FiveStar ->
            5


intToStars : Int -> Maybe Stars
intToStars int =
    case int of
        0 ->
            Just ZeroStar

        1 ->
            Just OneStar

        2 ->
            Just TwoStar

        3 ->
            Just ThreeStar

        4 ->
            Just FourStar

        5 ->
            Just FiveStar

        _ ->
            Nothing


starsDecoder : Int -> Decoder Stars
starsDecoder stars =
    intToStars stars
        |> Maybe.map succeed
        |> Maybe.withDefault (fail <| "Invalid Stars: " ++ String.fromInt stars)


userRatingDecoder : String -> Decoder UserRating
userRatingDecoder title =
    let
        attrs =
            \c -> map2 c (field "UserRating" float) (field "UserRatingCount" int)
    in
    case title of
        "" ->
            succeed Unrated

        "Unrated" ->
            succeed Unrated

        "Very Poor" ->
            attrs VeryPoor

        "Poor" ->
            attrs Poor

        "Unsatisfactory" ->
            attrs Unsatisfactory

        "Below Average" ->
            attrs BelowAverage

        "Average" ->
            attrs Average

        "Above Average" ->
            attrs AboveAverage

        "Good" ->
            attrs Good

        "Very Good" ->
            attrs VeryGood

        "Great" ->
            attrs Great

        "Excellent" ->
            attrs Excellent

        "Magnificent" ->
            attrs Magnificent

        "Exceptional" ->
            attrs Exceptional

        "Spectacular" ->
            attrs Spectacular

        x ->
            fail <| "Invalid UserRating: " ++ x


getUserRatingScore : UserRating -> Maybe Float
getUserRatingScore userRating =
    case userRating of
        Unrated ->
            Nothing

        VeryPoor float int ->
            Just float

        Poor float int ->
            Just float

        Unsatisfactory float int ->
            Just float

        BelowAverage float int ->
            Just float

        Average float int ->
            Just float

        AboveAverage float int ->
            Just float

        Good float int ->
            Just float

        VeryGood float int ->
            Just float

        Great float int ->
            Just float

        Excellent float int ->
            Just float

        Magnificent float int ->
            Just float

        Exceptional float int ->
            Just float

        Spectacular float int ->
            Just float


establishmentAttributesDecoder : Decoder EstablishmentAttributes
establishmentAttributesDecoder =
    map8 EstablishmentAttributes
        (field "Distance" float)
        (field "EstablishmentId" int)
        (field "Location" string)
        (field "MinCost" float)
        (field "Name" string)
        (field "Stars" int |> andThen starsDecoder)
        (field "UserRatingTitle" string |> andThen userRatingDecoder)
        (field "ImageUrl" string |> andThen imageDecoder)


establishmentAttributes : Establishment -> EstablishmentAttributes
establishmentAttributes establishment =
    case establishment of
        Hotel a ->
            a

        Apartment a ->
            a

        GuestHouse a ->
            a

        Hostel a ->
            a

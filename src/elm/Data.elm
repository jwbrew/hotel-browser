module Data exposing (Establishment(..), Stars(..), UserRating(..), establishmentAttributes, getEstablishments)

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


starsDecoder : Int -> Decoder Stars
starsDecoder stars =
    case stars of
        0 ->
            succeed ZeroStar

        1 ->
            succeed OneStar

        2 ->
            succeed TwoStar

        3 ->
            succeed ThreeStar

        4 ->
            succeed FourStar

        5 ->
            succeed FiveStar

        n ->
            fail <| "Invalid Stars: " ++ String.fromInt n


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

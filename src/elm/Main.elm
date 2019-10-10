module Main exposing (main)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Data
import Html
import Types exposing (..)
import Url exposing (Url)
import View


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    ( { error = Nothing
      , establishments = []
      , filters = Filters Nothing Nothing Nothing Nothing
      , paging = Paging 12 0
      , sort = Stars DESC
      }
    , Data.getEstablishments GetEstablishments
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetEstablishments establishmentListErrorHttpResult ->
            case establishmentListErrorHttpResult of
                Ok establishments ->
                    ( { model | establishments = establishments }, Cmd.none )

                Err reason ->
                    ( { model | error = Just reason }, Cmd.none )

        FilterName "" ->
            ( { model | filters = (\f -> { f | name = Nothing }) model.filters }, Cmd.none )

        FilterName q ->
            ( { model | filters = (\f -> { f | name = Just q }) model.filters }, Cmd.none )

        FilterCost c ->
            ( { model | filters = (\f -> { f | minCost = c }) model.filters }, Cmd.none )

        FilterRating r ->
            ( { model | filters = (\f -> { f | userRating = r }) model.filters }, Cmd.none )

        FilterStars s ->
            ( { model | filters = (\f -> { f | stars = s }) model.filters }, Cmd.none )

        UrlChange ->
            ( model, Cmd.none )

        UrlRequest ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


onUrlRequest : UrlRequest -> Msg
onUrlRequest urlRequest =
    UrlRequest


onUrlChange : Url -> Msg
onUrlChange url =
    UrlChange


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = View.view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }

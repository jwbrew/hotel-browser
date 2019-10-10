module Main exposing (main)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Data
import Filtering exposing (Filters)
import Html
import Sorting exposing (Sort, SortDirection(..), SortField(..))
import Types exposing (..)
import Url exposing (Url)
import View


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    ( { error = Nothing
      , establishments = []
      , filters = Filters Nothing Nothing Nothing Nothing
      , paging = Paging 12 0
      , sort = Sort Stars DESC
      }
    , Data.getEstablishments GetEstablishments
    )


updateFilters : Model -> (Filters -> Filters) -> Model
updateFilters model function =
    { model | filters = function model.filters, paging = Paging 12 0 }


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
            ( updateFilters model (\f -> { f | name = Nothing }), Cmd.none )

        FilterName q ->
            ( updateFilters model (\f -> { f | name = Just q }), Cmd.none )

        FilterCost c ->
            ( updateFilters model (\f -> { f | minCost = c }), Cmd.none )

        FilterRating r ->
            ( updateFilters model (\f -> { f | userRating = r }), Cmd.none )

        FilterStars s ->
            ( updateFilters model (\f -> { f | stars = s }), Cmd.none )

        FilterReset ->
            ( updateFilters model <| always <| Filters Nothing Nothing Nothing Nothing, Cmd.none )

        LoadMore ->
            ( { model | paging = (\p -> { p | per = p.per + 12 }) model.paging }, Cmd.none )

        UpdateSortDirection dir ->
            ( { model | sort = (\s -> { s | dir = dir }) model.sort }, Cmd.none )

        UpdateSortField field ->
            ( { model | sort = (\s -> { s | field = field }) model.sort }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


onUrlRequest : UrlRequest -> Msg
onUrlRequest urlRequest =
    NoOp


onUrlChange : Url -> Msg
onUrlChange url =
    NoOp


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

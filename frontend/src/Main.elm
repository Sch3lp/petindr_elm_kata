module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import UrlParser exposing ((</>), s, int, parseHash)
import Navigation as Nav
import Pages.Home as Home exposing (viewProfile, Model, Msg)


type Page
    = Home Home.Model


type alias Model =
    { subModel : Home.Model
    }


type Msg
    = Noop
    | UrlChanged Nav.Location
    | HomeMsg Home.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        UrlChanged location -> 
            ( model, Cmd.none )

        HomeMsg homeMsg ->
            let
                (subModel, subCmd)
                    = Home.update homeMsg model.subModel
            in
                ({ model | subModel = subModel }, Cmd.map HomeMsg subCmd )

view : Model -> Html Msg
view {subModel} =
            Home.view subModel
                |> Html.map HomeMsg


parseUrl : Model -> Nav.Location -> ( Model, Cmd Msg )
parseUrl model location =
    ( model, Cmd.none )


initialModel : Model
initialModel =
    Model <| Home.initialModel


main =
    Nav.program
        UrlChanged
        { init = parseUrl initialModel
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

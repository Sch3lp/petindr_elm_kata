module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import UrlParser exposing (Parser, (</>), s, int, parseHash, top, oneOf, map)
import Navigation as Nav
import Pages.Home as Home exposing (Model, Msg)
import Pages.Chat as Chat exposing (Model, Msg)


type Page
    = Home
    | Chat Int


type alias Model =
    { homeSubModel : Home.Model
    , chatSubModel : Chat.Model
    , currentPage : Maybe Page
    }


type Msg
    = Noop
    | UrlChanged Nav.Location
    | HomeMsg Home.Msg
    | ChatMsg Chat.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        UrlChanged location ->
            deducePageFromLocation location model

        HomeMsg homeMsg ->
            let
                ( homeSubModel, subCmd ) =
                    Home.update homeMsg model.homeSubModel
            in
                ( { model | homeSubModel = homeSubModel }, Cmd.map HomeMsg subCmd )

        ChatMsg chatMsg ->
            let
                ( chatSubModel, subCmd ) =
                    Chat.update chatMsg model.chatSubModel
            in
                ( { model | chatSubModel = chatSubModel }, Cmd.map ChatMsg subCmd )


route : Parser (Page -> a) a
route =
    oneOf
        [ UrlParser.map Home top
        , UrlParser.map Chat (UrlParser.s "chat" </> int)
        ]


deducePageFromLocation : Nav.Location -> Model -> ( Model, Cmd Msg )
deducePageFromLocation location model =
    let
        deducedPage =
            parseHash route location

        newModel =
            { model | currentPage = deducedPage }
    in
        case deducedPage of
            Just (Chat petId) ->
                ( newModel, Cmd.map ChatMsg <| Chat.fetchPet petId )

            _ ->
                ( newModel, Cmd.none )


view : Model -> Html Msg
view { homeSubModel, chatSubModel, currentPage } =
    case currentPage of
        Just Home ->
            Home.view homeSubModel
                |> Html.map HomeMsg

        Just (Chat petId) ->
            Chat.view chatSubModel
                |> Html.map ChatMsg

        _ ->
            div [] [ text "Whoops. I guess this is my 404 page?" ]


parseUrl : Model -> Nav.Location -> ( Model, Cmd Msg )
parseUrl model location =
    ( model, Cmd.none )


initialModel : Model
initialModel =
    Model Home.initialModel Chat.initialModel <| Just <| Chat 4


subscriptions : Model -> Sub Msg
subscriptions { homeSubModel, chatSubModel } =
    Sub.map ChatMsg <| Chat.subscriptions chatSubModel


main =
    Nav.program
        UrlChanged
        { init = parseUrl initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

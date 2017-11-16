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
    { subModel : Home.Model
    , subModel2 : Chat.Model
    , page: Page
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
            (deducePageFromLocation location model, Cmd.none)

        HomeMsg homeMsg ->
            let
                ( subModel, subCmd ) =
                    Home.update homeMsg model.subModel
            in
                ( { model | subModel = subModel }, Cmd.map HomeMsg subCmd )

        ChatMsg chatMsg ->
            let
                ( subModel2, subCmd ) =
                    Chat.update chatMsg model.subModel2
            in
                ( { model | subModel2 = subModel2 }, Cmd.map ChatMsg subCmd )

route : Parser (Page -> a) a
route =
    oneOf
        [ UrlParser.map Home top
        , UrlParser.map Chat (UrlParser.s "chat" </> int)
        ]



deducePageFromLocation: Nav.Location -> Model -> Model
deducePageFromLocation location model 
    = model
        
    

view : Model -> Html Msg
view { subModel, subModel2 } =
    Home.view subModel
        |> Html.map HomeMsg


parseUrl : Model -> Nav.Location -> ( Model, Cmd Msg )
parseUrl model location =
    ( model, Cmd.none )


initialModel : Model
initialModel =
    Model Home.initialModel Chat.initialModel Home


main =
    Nav.program
        UrlChanged
        { init = parseUrl initialModel
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

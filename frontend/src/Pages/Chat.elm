module Pages.Chat exposing (..)

import Data.Pets exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Request)
import Json.Decode exposing (map)
import Char exposing (..)
import WebSocket exposing (listen, send)
import UrlParser exposing ((</>), s, int, parseHash)
import Navigation as Nav


type ChatMessage
    = SelfChatMessage String
    | MatchChatMessage String


type alias Conversation =
    List ChatMessage


type alias Model =
    { pet : Pet
    , ourText : String
    , conversation : Conversation
    }



{- initialize model using webservice /api/pets/:petid -}
{- where :petid matches the id that got parsed from the url -}
{- e.g.: http://localhost:8000/index.html/#chat/1 should load Princess, because http://localhost:3000/api/pets/1 return Princess -}


fetchPet : Int -> Cmd Msg
fetchPet petId =
    Http.send updateModelWithFetchedPet <| performGet <| toString petId


updateModelWithFetchedPet : Result Http.Error Pet -> Msg
updateModelWithFetchedPet result =
    case result of
        Ok pet ->
            InitializeModelWithPet pet

        Err _ ->
            Noop



-- HTTP calls


performGet : String -> Http.Request Pet
performGet petId =
    Http.get ("http://localhost:3000/api/pets/" ++ petId) petDecoder


petDecoder : Json.Decode.Decoder Pet
petDecoder =
    Json.Decode.map5 Pet
     (Json.Decode.field "id" Json.Decode.int)
     (Json.Decode.field "name" Json.Decode.string)
     (Json.Decode.field "distance" Json.Decode.int)
     (Json.Decode.field "text" Json.Decode.string)
     (Json.Decode.field "photoUrl" Json.Decode.string)



-- Model


initialModel : Model
initialModel =
    Model cricket
        ""
        []


type Msg
    = MessageWasEntered
    | TextWasEntered String
    | MatchChatMessageReceived String
    | InitializeModelWithPet Pet
    | UrlChanged Nav.Location
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextWasEntered text ->
            ( { model | ourText = text }, Cmd.none )

        MessageWasEntered ->
            let
                updatedConvo =
                    addSelfToConvo model.conversation model.ourText

                clearedText =
                    ""
            in
                ( { model | conversation = updatedConvo, ourText = clearedText }, WebSocket.send ("ws://localhost:3000/api/chat/" ++ (toString model.pet.id)) model.ourText )

        MatchChatMessageReceived message ->
            let
                updatedConvo =
                    addMatchMessageToConvo model.conversation message
            in
                ( { model | conversation = updatedConvo }, Cmd.none )

        InitializeModelWithPet fetchedPet ->
            ( { model | pet = fetchedPet }, Cmd.none )
        
        UrlChanged location ->
            parseUrl model location

        _ ->
            ( model, Cmd.none )


addMatchMessageToConvo : Conversation -> String -> Conversation
addMatchMessageToConvo convo msg =
    addToConversation convo <| MatchChatMessage msg


addSelfToConvo : Conversation -> String -> Conversation
addSelfToConvo convo msg =
    addToConversation convo <| SelfChatMessage msg


addToConversation : Conversation -> ChatMessage -> Conversation
addToConversation convo msg =
    convo ++ [ msg ]



-- View


view : Model -> Html Msg
view model =
    div [ class "body" ]
        [ header []
            [ button [ class "icon-left back-icon" ]
                []
            , span [ class "header-title" ]
                [ text model.pet.name ]
            , img [ class "header-profile-image", src model.pet.photoUrl ]
                []
            ]
        , div [ class "container chat-container" ] <|
            renderConversation model.conversation
        , div [ class "new-message" ]
            [ input
                [ type_ "text"
                , placeholder "enter message"
                , onInput TextWasEntered
                , onKeyUp sendOnEnter
                , value model.ourText
                ]
                []
            , button
                [ class "button-round button-primary"
                , onClick MessageWasEntered
                ]
                [ text "Send" ]
            ]
        ]


onKeyUp : (KeyCode -> Msg) -> Attribute Msg
onKeyUp tagger =
    onWithOptions "keyup" { stopPropagation = True, preventDefault = True } (Json.Decode.map tagger keyCode)


sendOnEnter : KeyCode -> Msg
sendOnEnter keyCode =
    case keyCode of
        13 ->
            MessageWasEntered

        _ ->
            Noop


renderConversation : Conversation -> List (Html Msg)
renderConversation convo =
    case convo of
        [] ->
            []

        messages ->
            List.map renderChatMessage messages


renderChatMessage : ChatMessage -> Html Msg
renderChatMessage msg =
    case msg of
        SelfChatMessage msg ->
            div [ class "chat-item-self" ]
                [ div [ class "chat-item-self-text" ]
                    [ text msg ]
                ]

        MatchChatMessage msg ->
            div [ class "chat-item-other" ]
                [ div [ class "chat-item-other-text" ]
                    [ text msg ]
                ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen ("ws://localhost:3000/api/chat/" ++ (toString model.pet.id)) MatchChatMessageReceived



-- url parsing


parseUrl : Model -> Nav.Location -> ( Model, Cmd Msg )
parseUrl model location =
    ( model, fetchPet <| getPetIdFromLocation location )


getPetIdFromLocation : Nav.Location -> Int
getPetIdFromLocation loc =
    Maybe.withDefault 999 <| parseHash (UrlParser.s "chat" </> int) loc



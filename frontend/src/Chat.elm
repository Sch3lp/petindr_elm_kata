module Home exposing (..)

import Pets exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing (map)
import Char exposing (..)
import WebSocket exposing(listen, send)


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


initialModel : Model
initialModel =
    Model cricket
        ""
        []


type Msg
    = MessageWasEntered
    | TextWasEntered String
    | MatchChatMessageReceived String
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

                clearedText = ""
            in
                ( { model | conversation = updatedConvo, ourText = clearedText }, WebSocket.send ("ws://localhost:3000/api/chat/" ++ (toString model.pet.id)) model.ourText )

        MatchChatMessageReceived message ->
            let
                updatedConvo =
                    addMatchMessageToConvo model.conversation message
            in
                ( {model | conversation = updatedConvo }, Cmd.none)

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
    onWithOptions "keyup" { stopPropagation = True, preventDefault = True } (Json.map tagger keyCode)

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


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen ("ws://localhost:3000/api/chat/" ++ (toString model.pet.id)) MatchChatMessageReceived


main =
    program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

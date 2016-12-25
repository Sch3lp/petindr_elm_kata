-- import Tasks exposing(..)
-- import Date exposing (Date)
-- now: Cmd Event
-- now = 
--   Task.perform (always (SetDate Nothing)) (Just >> SetDate) Date.now

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pets exposing (..)
import WebSocket

main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Model = 
    { currentLine: Maybe String
    , messages: List Message
    , pet: Maybe Pet
    }

type alias Message = 
    { text: String
    , self: Bool
    }


init: (Model, Cmd Event)
init = 
    let
      model =
      { currentLine = Nothing
      , messages = initDummyMessages
      , pet = Just initDummyPet 
      } 
    in
      (model, Cmd.none)


initDummyMessages: List Message
initDummyMessages = 
    [
        { text= "Bokbok!"
        , self= False
        },
        { text= "You want a berry?"
        , self= True
        },
        { text= "bok"
        , self= False
        },
        { text= "Hi Princess!"
        , self= True
        }
    ]

initDummyPet: Pet
initDummyPet =
    { id = 1
    , name = "Princess"
    , distance = 20
    , text = "dummy"
    , photoUrl = "http://localhost:3000/profiles/princess.jpg"
    }

type Event = TextTyped String
           | MessageSent
           | MessageReceived String

update: Event -> EventHandler
update event model = 
    case event of
        TextTyped text -> textTyped text model
        MessageSent -> messageSent model
        MessageReceived text -> messageReceived text model
        
type alias EventHandler = Model -> (Model, Cmd Event)

textTyped: String -> EventHandler
textTyped text model = (\input -> ({model | currentLine = Just input }, Cmd.none)) text


messageSent: EventHandler
messageSent model =
    let
      newMessages = {text=Maybe.withDefault "" model.currentLine, self=True} :: model.messages
    in
      ({model | messages = newMessages }, sendText model.pet model.currentLine)

sendText: Maybe Pet -> Maybe String -> Cmd Event
sendText pet line =
    case (pet, line) of
        (Just pet, Just line)  ->
            let
                url = "ws://localhost:3000/api/chat/"++ toString pet.id
            in
                WebSocket.send url line
        (_, _) -> Cmd.none
      
messageReceived: String -> EventHandler
messageReceived text model = 
    let
      messages = {text=text, self=False} :: model.messages
    in
      ((\input -> {model | messages = input }) messages, Cmd.none)


subscriptions : Model -> Sub Event
subscriptions model =
    WebSocket.listen "ws://localhost:3000/api/chat/1" MessageReceived


view: Model -> Html Event
view model = 
    let
        photoUrl = Maybe.map .photoUrl model.pet |> Maybe.withDefault "http://localhost:3000/profiles/self-profile.png"
    in
        div [ class "body" ]
            [ header []
                [ button [ class "icon-left back-icon" ]
                    []
                , span [ class "header-title" ]
                    [ text "Princess" ]
                , img [ class "header-profile-image", src photoUrl ]
                    []
                ]
            , div [ class "container chat-container" ] <| List.map mapTextMessage <| List.reverse model.messages
            , div [ class "new-message" ]
                [ input [ type_ "text", placeholder "enter message", onInput TextTyped ] []
                , button [ class "button-round button-primary", onClick MessageSent ] [ text "Send" ]
                ]
            ]

mapTextMessage: Message -> Html.Html msg
mapTextMessage message =
    let
        classSelf = if message.self then "chat-item-self" else "chat-item-other"
    in
        div [ class classSelf ]
            [ div [ class (classSelf++"-text") ]
                [ text message.text ]
            ]
                
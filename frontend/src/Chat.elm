-- import Tasks exposing(..)
-- import Date exposing (Date)
-- now: Cmd Event
-- now = 
--   Task.perform (always (SetDate Nothing)) (Just >> SetDate) Date.now

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pets exposing (..)

main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

subscriptions : Model -> Sub Event
subscriptions model = Sub.none

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
      , pet = Nothing 
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

type Event = TextTyped String
           | TextSent

update: Event -> EventHandler
update event model = 
    case event of
        TextTyped text -> textTyped text model
        TextSent -> textSent model
        
type alias EventHandler = Model -> (Model, Cmd Event)

textTyped: String -> EventHandler
textTyped text model = (\input -> ({model | currentLine = Just input }, Cmd.none)) text

textSent: EventHandler
textSent model =
    let
      newMessages = {text=Maybe.withDefault "" model.currentLine, self=True} :: model.messages
    in
        ({model | messages = newMessages }, Cmd.none)


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
            , div [ class "container chat-container" ] <| List.map mapTextMessage model.messages
            , div [ class "new-message" ]
                [ input [ type_ "text", placeholder "enter message", onInput TextTyped ]
                    []
                , button [ class "button-round button-primary", onClick TextSent ]
                    [ text "Send" ]
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
                
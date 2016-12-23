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
      , messages = []
      , pet = Nothing 
      } 
    in
      (model, Cmd.none)


type Event = TextTyped String

update: Event -> EventHandler
update event model = 
    case event of
        TextTyped text -> textTyped text model
        
type alias EventHandler = Model -> (Model, Cmd Event)

textTyped: String -> EventHandler
textTyped text model = (\input -> ({model | currentLine = Just input }, Cmd.none)) text




view: Model -> Html Event
view model = 
    let
        photoUrl = Maybe.withDefault "" (Maybe.map .photoUrl model.pet)
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
            , div [ class "container chat-container" ]
                [ div [ class "chat-item-self" ]
                    [ div [ class "chat-item-self-text" ]
                        [ text "hi Princess!" ]
                    ]
                , div [ class "chat-item-other" ]
                    [ div [ class "chat-item-other-text" ]
                        [ text "bok" ]
                    ]
                , div [ class "chat-item-self" ]
                    [ div [ class "chat-item-self-text" ]
                        [ text "do you want a berry?" ]
                    ]
                , div [ class "chat-item-other" ]
                    [ div [ class "chat-item-other-text" ]
                        [ text "bokbok!" ]
                    ]
                ]
            , div [ class "new-message" ]
                [ input [ type_ "text", placeholder "enter message", onInput TextTyped ]
                    []
                , button [ class "button-round button-primary" ]
                    [ text "Send" ]
                ]
            ]
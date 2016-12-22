import Html exposing (..)
import Html.Attributes exposing (..)

main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

subscriptions : Model -> Sub Event
subscriptions model = Sub.none

type alias Model = {id: Int}




init: (Model, Cmd Event)
init = ({id=0}, Cmd.none)




type Event = TextEntered

update: Event -> EventHandler
update event model = 
    case event of
        TextEntered -> textEntered model
        
type alias EventHandler = Model -> (Model, Cmd Event)

textEntered: EventHandler
textEntered model = (model, Cmd.none)




view: Model -> Html Event
view model = 
    div [ class "body" ]
        [ header []
            [ button [ class "icon-left back-icon" ]
                []
            , span [ class "header-title" ]
                [ text "Princess" ]
            , img [ class "header-profile-image", src "http://localhost:3000/profiles/princess.jpg" ]
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
            [ input [ type_ "text", placeholder "enter message" ]
                []
            , button [ class "button-round button-primary" ]
                [ text "Send" ]
            ]
        ]
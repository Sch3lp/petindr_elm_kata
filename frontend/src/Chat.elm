module Chat exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pets exposing (..)
import WebSocket
import Json.Decode as Json
import Navigation exposing(..)
import UrlParser exposing (..)


main =
  Navigation.program LocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Model = 
    { currentLine: String
    , messages: List Message
    , pet: Maybe Pet
    , currentRoute: Route
    }

type alias Message = 
    { text: String
    , self: Bool
    }


init: Location -> (Model, Cmd Event)
init location = 
    locationChanged location  
        { currentLine = ""
        , messages = []
        , pet = Nothing
        , currentRoute = NotFoundRoute 
        }


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

routes: Parser (Route -> a) a 
routes = 
    UrlParser.oneOf
        [ UrlParser.map ChatRoute (UrlParser.s "chat" </> UrlParser.int) ]

parseLocation: Location -> Route
parseLocation location =
    case (UrlParser.parseHash routes location) of
      Just route -> route
      Nothing -> NotFoundRoute

type Route = ChatRoute Int --Refactor to PetId -- Navigation.newUrl "#chat/" ++ petId
           | NotFoundRoute

type Event = TextTyped String
           | SendMessageClicked
           | MessageReceived String
           | EnterPressed
           | NonEvent
           | LocationChange Navigation.Location

type alias EventHandler = Model -> (Model, Cmd Event)

update: Event -> EventHandler
update event model = 
    case event of
        TextTyped text -> textTyped text model
        SendMessageClicked -> sendMessage model
        MessageReceived text -> messageReceived text model  
        EnterPressed -> sendMessage model
        LocationChange location -> locationChanged location model
        _ -> (model, Cmd.none)

locationChanged: Location -> EventHandler
locationChanged location model =
    let
        newRoute = parseLocation location
        fetchedPet = 
            case newRoute of
                ChatRoute petId -> fetchPet petId
                NotFoundRoute -> Nothing
    in ({ model 
        | currentRoute = newRoute
        , pet = fetchedPet
        , messages = []
        } 
        , Cmd.none)

-- TODO: replace with http call?
fetchPet: Int -> Maybe Pet
fetchPet petId = 
    List.filter (\pet -> pet.id == petId) Pets.nextPets |> List.head

textTyped: String -> EventHandler
textTyped text model = (\input -> ({ model | currentLine = input }, Cmd.none)) text

sendMessage: EventHandler
sendMessage model =
    let
      currentLine = model.currentLine
      newMessages = { text = currentLine, self = True } :: model.messages
    in
      ({ model | messages = newMessages, currentLine = "" }, sendText model.pet currentLine)

sendText: Maybe Pet -> String -> Cmd Event
sendText pet line =
    case (pet, line) of
        (Just pet, "") -> Cmd.none
        (Just pet, line) ->
            let
                url = "ws://localhost:3000/api/chat/" ++ toString pet.id
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
    WebSocket.listen ( (++) "ws://localhost:3000/api/chat/" <| toString <| Maybe.withDefault 0 <| Maybe.map .id model.pet )  MessageReceived

onKeyDown: (Int -> Event) -> Attribute Event
onKeyDown tagger =
  on "keydown" (Json.map tagger keyCode) --TODO: replace Enter keydown with form submit

checkEnter: Int -> Event
checkEnter keyCode =
    case keyCode of
        13 -> EnterPressed
        _  -> NonEvent

view: Model -> Html Event
view model = 
    case model.pet of
        Just pet ->
            let
                photoUrl = pet.photoUrl
                petName = pet.name
            in
                div [ class "body" ]
                    [ header []
                        [ button [ class "icon-left back-icon" ]
                            []
                        , span [ class "header-title" ]
                            [ text petName ]
                        , img [ class "header-profile-image", src photoUrl ]
                            []
                        ]
                    , div [ class "container chat-container" ] <| List.map mapTextMessage <| List.reverse model.messages
                    , div [ class "new-message" ]
                        [ input [ type_ "text", value model.currentLine, placeholder "enter message", onInput TextTyped, onKeyDown checkEnter] []
                        , button [ class "button-round button-primary", onClick SendMessageClicked ] [ text "Send" ]
                        ]
                    ]
        Nothing -> 
            div [ class "body" ]
                [ header []
                    [ button [ class "icon-left back-icon" ]
                        []
                    , span [ class "header-title" ]
                        [ text "No pet found T_T" ]
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
                
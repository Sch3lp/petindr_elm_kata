module Home exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode exposing (..)

import Pets exposing (..)


subscriptions : Model -> Sub Event
subscriptions model = Sub.none

main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Model =
    { showProfileText : Bool
    , matched : Bool
    , nextPets : List Pet
    , currentPet : Maybe 
        { id : Int
        , name : String
        , distance : Int
        , text : String
        , photoUrl : String
        }
    }




init: (Model, Cmd Event)
init = (initialize Pets.nextPets, Cmd.none)

initialize: List Pet -> Model
initialize pets = 
    case pets of
      h::t ->
        let
          initialModel = 
            { showProfileText = False
            , matched = False
            , nextPets = t
            , currentPet = Just h
            }
        in initialModel
      [] ->
        let
          initialModel = 
            { showProfileText = False
            , matched = False
            , nextPets = []
            , currentPet = Nothing
            }
        in initialModel



type Event = ShowInfoWasClicked
        | BackWasClickedFromMatched
        | LikeWasClicked
        | DislikeWasClicked
        | Matched (Result Http.Error Bool)

update: Event -> EventHandler
update event model = 
    case event of
        ShowInfoWasClicked -> showInfo model
        BackWasClickedFromMatched -> nextPet model
        LikeWasClicked -> liked model
        DislikeWasClicked -> nextPet model
        Matched (Ok True) -> matched model
        Matched (Ok False) -> nextPet model
        Matched (Err _) -> nextPet model

type alias EventHandler = Model -> (Model, Cmd Event)

showInfo: EventHandler
showInfo model = ({ model | showProfileText = not model.showProfileText }, Cmd.none)

nextPet: EventHandler
nextPet model = (nextPetAndReset model, Cmd.none)

liked: EventHandler
liked model = 
    case model.currentPet of
        Just pet -> (model, like (toString pet.id))
        Nothing -> (model, Cmd.none)

matched: EventHandler
matched model = ({ model | matched = True }, Cmd.none)




like: String -> Cmd Event
like petId =
    let
        url = "http://localhost:3000/api/pets/"++petId
        request = Http.post url Http.emptyBody Json.Decode.bool
    in
        Http.send Matched request

nextPetAndReset: Model -> Model
nextPetAndReset model = 
    case model.nextPets of
        h :: t -> reset { model | currentPet = Just h, nextPets = t }
        [] -> reset { model | currentPet = Nothing, nextPets = [] }

reset: Model -> Model
reset model = 
    { model | matched = False, showProfileText = False }





view: Model -> Html Event
view model = 
    let
        currentPet = 
            case model.currentPet of 
                Just pet -> pet
                Nothing ->  { id = 0
                            , name = ""
                            , distance = 0
                            , text = ""
                            , photoUrl = ""
                            }
    in
        div [ class "body" ]
        [ Html.header []
            [ span [ class "header-title" ]
                [ text "Petindr" ]
            , button [ class "icon-right chat-icon" ]
                []
            ]
        , div [ class "container main-container" ]
            [ div [ class "profiles" ]
                [ div [ class "profile" ]
                    [ div []
                        [ img [ src currentPet.photoUrl ]
                            []
                        , if model.showProfileText then
                            div [ class "profile-text" ]
                                [ text currentPet.text ]
                        else
                            div [] []
                        ]
                    , div [ class "identification" ]
                        [ span [ class "identification-name" ]
                            [ text currentPet.name ]
                        , span [ class "identification-distance" ]
                            [ text 
                                (currentPet.distance
                                    |>toString
                                    |>(++) "km") 
                            ]
                        ]
                    ]
                ]
            , div [ class "button-group" ]
                [ button [ class "button-round button-primary button-big icon-x", onClick DislikeWasClicked ]
                    [ img [ src "/styling/images/x-icon.png" ]
                        []
                    ]
                , button [ class "button-round button-primary button-small button-front", onClick ShowInfoWasClicked ]
                    [ img [ src "/styling/images/i-icon.png" ]
                        []
                    ]
                , button [ class "button-round button-primary button-big", onClick LikeWasClicked ]
                    [ img [ src "/styling/images/like-icon.png" ]
                        []
                    ]
                ]
            ]
        , if model.matched then 
        div [ class "overlay" ]
            [ div []
                [ div [ class "match-title" ]
                    [ text "It's a match!" ]
                , div [ class "match-details" ]
                    [ text (("You and "++currentPet.name)++" have like each other") ]
                ]
            , div [ class "match-profiles" ]
                [ img [ class "match-profile", src "http://localhost:3000/profiles/self-profile.png" ]
                    []
                , img [ class "match-profile", src currentPet.photoUrl ]
                    []
                ]
            , button [ class "button-square button-primary" ]
                [ span [ class "button-chat" ]
                    [ text "Send message" ]
                ]
            , button [ class "button-square button-secundary", onClick BackWasClickedFromMatched ]
                [ span [ class "button-goback" ]
                    [ text "Go back" ]
                ]
            ]
        else
            div [] []
        ]
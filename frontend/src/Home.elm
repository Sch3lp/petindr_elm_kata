module Home exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode exposing (..)

import Pets exposing (..)


subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type Msg = ShowInfo
        | BackFromMatched
        | Like
        | Dislike
        | Matched (Result Http.Error Bool)

type alias Model =
    { showProfileText : Bool
    , matched : Bool
    , nextPets : List Pet
    , currentPet :
        { id : Int
        , name : String
        , distance : Int
        , text : String
        , photoUrl : String
        }
    }




init: (Model, Cmd Msg)
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
            , currentPet = h
            }
        in initialModel
      [] ->
        let
          initialModel = 
            { showProfileText = False
            , matched = False
            , nextPets = []
            , currentPet = 
                { id = 0
                , name = ""
                , distance = 0
                , text = ""
                , photoUrl = ""
                } 
            }
        in initialModel




update: Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        ShowInfo -> ({ model | showProfileText = not model.showProfileText }, Cmd.none)
        BackFromMatched -> (nextPet model, Cmd.none)
        Like -> (model, like (toString model.currentPet.id))
        Dislike -> (nextPet model, Cmd.none)
        Matched (Ok True) -> ({ model | matched = True }, Cmd.none)
        Matched (Ok False) -> (nextPet model, Cmd.none)
        Matched (Err _) -> (nextPet model, Cmd.none)

like: String -> Cmd Msg
like petId =
    let
        url = "http://localhost:3000/api/pets/"++petId
        request = Http.post url Http.emptyBody Json.Decode.bool
    in
        Http.send Matched request

nextPet: Model -> Model
nextPet model = 
    case model.nextPets of
        h :: t -> reset { model | currentPet = h, nextPets = t }
        [] -> reset { model | nextPets = [] }

reset: Model -> Model
reset model = 
    { model | matched = False, showProfileText = False }

view: Model -> Html Msg
view model = 
    div []
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
                    [ img [ src model.currentPet.photoUrl ]
                        []
                    , if model.showProfileText then
                        div [ class "profile-text" ]
                            [ text model.currentPet.text ]
                      else
                        div [] []
                    ]
                , div [ class "identification" ]
                    [ span [ class "identification-name" ]
                        [ text model.currentPet.name ]
                    , span [ class "identification-distance" ]
                        [ text ((toString model.currentPet.distance)++"km") ]
                    ]
                ]
            ]
        , div [ class "button-group" ]
            [ button [ class "button-round button-primary button-big icon-x", onClick Dislike ]
                [ img [ src "/styling/images/x-icon.png" ]
                    []
                ]
            , button [ class "button-round button-primary button-small button-front", onClick ShowInfo ]
                [ img [ src "/styling/images/i-icon.png" ]
                    []
                ]
            , button [ class "button-round button-primary button-big", onClick Like ]
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
                    [ text (("You and "++model.currentPet.name)++" have liked each other") ]
                ]
            , div [ class "match-profiles" ]
                [ img [ class "match-profile", src "http://localhost:3000/profiles/self-profile.png" ]
                    []
                , img [ class "match-profile", src model.currentPet.photoUrl ]
                    []
                ]
            , button [ class "button-square button-primary" ]
                [ span [ class "button-chat" ]
                    [ text "Send message" ]
                ]
            , button [ class "button-square button-secundary", onClick BackFromMatched ]
                [ span [ class "button-goback" ]
                    [ text "Go back" ]
                ]
            ]
        else
            div [] []
    ]
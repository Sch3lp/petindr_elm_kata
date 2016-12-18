module Home exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Pets exposing (..)

-- Define your messages
type Msg = 
    ShowInfo
  | Like
  | Dislike  
-- Setup your model
type alias Model =
    { showProfileText : Bool
    , nextPets : List Pet
    , currentPet :
        { id : Int
        , name : String
        , distance : Int
        , text : String
        , photoUrl : String
        }
    }

-- initialModel =
initialModel: Model 
initialModel =
    { showProfileText = False
    , nextPets = nextPets
    , currentPet =
        { id = 1
        , name = "BellyBell"
        , distance = 24
        , text = "Look at this face. Is this not the derpiest face you have ever seen? This is Bluebell, the chicken who acts like a dog and thinks she’s people. Bluebell’s you’re in the backyard relaxing in your lounge chair, you can count on Bluebell to be nearby, napping in the grass at your feet."
        , photoUrl = "http://localhost:3000/profiles/bluebell1.jpg"
        }
    }

-- Define an update function
update: Msg -> Model -> Model
update msg model = 
    case msg of
        ShowInfo -> { model | showProfileText = not model.showProfileText }
        Like -> nextPet model
        Dislike -> nextPet model

nextPet: Model -> Model
nextPet model = 
    case model.nextPets of
        h :: t -> { model | currentPet = h, nextPets = t}
        [] -> { model | nextPets = []}

-- Define a view function
view: Model -> Html Msg
view model = 
    div []
    [ header []
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
    ]


-- main
main =
    beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }

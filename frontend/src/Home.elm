module Home exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- Define your messages
-- Setup your model


type alias Model =
    { showProfileText : Bool
    , currentPet :
        { id : Int
        , name : String
        , distance : Int
        , text : String
        , photoUrl : String
        }
    }


initialModel =
    { showProfileText = False
    , currentPet =
        { id = 2
        , name = "Baby Chicken"
        , distance = 42
        , text = "Meet Baby Chicken. She’s got a whole lot of personality packed into a mere 500 feathered grams. When she lays an egg, she’s putting out a whopping 10% of her body weight. That’s hardcore! Because she’s so tiny and round as a meatball, Baby Chicken has got that whole ‘I’m so little and cute’ thing going on. But don’t let that fool you. You see that evil glint in her eye? You might mistake it for RBF – Resting Bok Face, but it’s really just the cruel indifference of nature packaged into a chirping fluffball. That’s right. Baby Chicken would peck the eyeballs out of your skull if she liked the way it felt. Don’t wait; set up your date with Baby today!"
        , photoUrl = "http://localhost:3000/profiles/babychicken.jpg"
        }
    }



-- Define an update function


type Msg
    = ProfileButtonWasClicked


update : Msg -> Model -> Model
update msg model =
    case msg of
        ProfileButtonWasClicked ->
            if model.showProfileText then
                { model | showProfileText = False }
            else
                { model | showProfileText = True }


-- Define a view function


view : Model -> Html Msg
view model =
    let
        profileTextDiv =
            if model.showProfileText then
                div [ class "profile-text" ]
                    [ text model.currentPet.text ]
            else
                div [] []
    in
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
                            , profileTextDiv
                            ]
                        , div [ class "identification" ]
                            [ span [ class "identification-name" ]
                                [ text model.currentPet.name ]
                            , span [ class "identification-distance" ]
                                [ text <| toString model.currentPet.distance ]
                            ]
                        ]
                    ]
                , div [ class "button-group" ]
                    [ button [ class "button-round button-primary button-big icon-x" ]
                        [ img [ src "/styling/images/x-icon.png" ]
                            []
                        ]
                    , button
                        [ class "button-round button-primary button-small button-front"
                        , onClick ProfileButtonWasClicked
                        ]
                        [ img [ src "/styling/images/i-icon.png" ]
                            []
                        ]
                    , button [ class "button-round button-primary button-big" ]
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

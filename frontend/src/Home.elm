module Home exposing (..)

import Pets exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type alias Model =
    { showProfileText : Bool
    , currentPet : Pet
    }


babyChicken : Pet
babyChicken =
    { id = 2
    , name = "Baby Chicken"
    , distance = 23
    , text = "Meet Baby Chicken. She’s got a whole lot of personality packed into a mere 500 feathered grams. When she lays an egg, she’s putting out a whopping 10% of her body weight. That’s hardcore! Because she’s so tiny and round as a meatball, Baby Chicken has got that whole ‘I’m so little and cute’ thing going on. But don’t let that fool you. You see that evil glint in her eye? You might mistake it for RBF – Resting Bok Face, but it’s really just the cruel indifference of nature packaged into a chirping fluffball. That’s right. Baby Chicken would peck the eyeballs out of your skull if she liked the way it felt. Don’t wait; set up your date with Baby today!"
    , photoUrl = "http://localhost:3000/profiles/babychicken.jpg"
    }


initialModel : Model
initialModel =
    Model False babyChicken


view : Model -> Html Msg
view model =
    let
        profilePet =
            model.currentPet

        profileText =
            if model.showProfileText then
                div [ class "profile-text" ]
                    [ text profilePet.text ]
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
                            [ img [ src profilePet.photoUrl ]
                                []
                            , profileText
                            ]
                        , div [ class "identification" ]
                            [ span [ class "identification-name" ]
                                [ text profilePet.name ]
                            , span [ class "identification-distance" ]
                                [ text <| toString profilePet.distance ]
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
                        , onClick InfoButtonClicked
                        ]
                        [ img [ src "/styling/images/i-icon.png" ]
                            []
                        ]
                    , button
                        [ class "button-round button-primary button-big"
                        , onClick LikeButtonClicked
                        ]
                        [ img [ src "/styling/images/like-icon.png" ]
                            []
                        ]
                    ]
                ]
            ]


type Msg
    = InfoButtonClicked
    | LikeButtonClicked


update : Msg -> Model -> Model
update msg model =
    case msg of
        InfoButtonClicked ->
            if model.showProfileText == False then
                { model | showProfileText = True }
            else
                { model | showProfileText = False }

        LikeButtonClicked ->
            model


main : Program Never Model Msg
main =
    beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }

module Home exposing (..)

import Pets exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    { showProfileText : Bool
    , currentPet : Pet
    , nextPets : List Pet
    }


initialModel : Model
initialModel =
    { showProfileText = False
    , currentPet = princess
    , nextPets = nextPets
    }



-- Define an update function


type Msg
    = ProfileButtonWasClicked
    | DislikeButtonWasClicked
    | LikeButtonWasClicked


update : Msg -> Model -> Model
update msg model =
    case msg of
        ProfileButtonWasClicked ->
            { model | showProfileText = not model.showProfileText }

        DislikeButtonWasClicked ->
            advancePet model

        LikeButtonWasClicked ->
            advancePet model


advancePet : Model -> Model
advancePet model =
    let
        ( nextPet, remainingPets ) =
            case model.nextPets of
                h :: t ->
                    ( h, t )

                [] ->
                    ( princess, [] )
    in
        { model | currentPet = nextPet, nextPets = remainingPets }



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
                    [ button
                        [ class "button-round button-primary button-big icon-x"
                        , onClick DislikeButtonWasClicked
                        ]
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
                    , button
                        [ class "button-round button-primary button-big"
                        , onClick LikeButtonWasClicked
                        ]
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

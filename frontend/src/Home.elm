module Home exposing (..)

import Pets exposing (..)
import Http exposing (Request)
import Json.Decode exposing (bool)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    { showProfileText : Bool
    , currentPet : Pet
    , nextPets : List Pet
    , showMatchOverlay : Bool
    , possibleMatchedPet : Pet
    }


initialModel : Model
initialModel =
    { showProfileText = False
    , currentPet = princess
    , nextPets = nextPets
    , showMatchOverlay = False
    , possibleMatchedPet = princess
    }


type Msg
    = ProfileButtonWasClicked
    | DislikeButtonWasClicked
    | LikeButtonWasClicked
    | MatchmakeWasSuccessful
    | MatchmakeWasUnsuccessful
    | GoBackFromMatchOverlayButtonWasClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ProfileButtonWasClicked ->
            ( { model | showProfileText = not model.showProfileText }, Cmd.none )

        DislikeButtonWasClicked ->
            ( advancePet model, Cmd.none )

        LikeButtonWasClicked ->
            ( { model | possibleMatchedPet = model.currentPet }, checkMatch model )

        MatchmakeWasSuccessful ->
            ( advancePet { model | showMatchOverlay = True }, Cmd.none )

        MatchmakeWasUnsuccessful ->
            ( advancePet model, Cmd.none )

        GoBackFromMatchOverlayButtonWasClicked ->
            ( { model | showMatchOverlay = False }, Cmd.none )


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


checkMatch : Model -> Cmd Msg
checkMatch model =
    Http.send evaluateMatchMakingResponse performMatchmaking


evaluateMatchMakingResponse : Result Http.Error Bool -> Msg
evaluateMatchMakingResponse result =
    case result of
        Ok True ->
            MatchmakeWasSuccessful

        Ok False ->
            MatchmakeWasUnsuccessful

        Err _ ->
            MatchmakeWasUnsuccessful



-- HTTP calls


performMatchmaking : Http.Request Bool
performMatchmaking =
    Http.post "http://localhost:3000/api/pets/1" Http.emptyBody (Json.Decode.bool)


view : Model -> Html Msg
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
                        , conditionallyRender model.showProfileText <|
                            div [ class "profile-text" ] [ text model.currentPet.text ]
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
            , renderMatchOverlay model.showMatchOverlay model.possibleMatchedPet
            ]
        ]


conditionallyRender : Bool -> Html Msg -> Html Msg
conditionallyRender shouldRender divToBeRendered =
    if shouldRender then
        divToBeRendered
    else
        div [] []


renderProfileText : Bool -> String -> Html Msg
renderProfileText shouldRender profileText =
    conditionallyRender shouldRender <|
        div [ class "profile-text" ] [ text profileText ]


renderMatchOverlay : Bool -> Pet -> Html Msg
renderMatchOverlay shouldRender possibleMatchedPet =
    conditionallyRender shouldRender <|
        div [ class "overlay" ]
            [ div []
                [ div [ class "match-title" ]
                    [ text "It's a match!" ]
                , div [ class "match-details" ]
                    [ text ("You and " ++ possibleMatchedPet.name ++ " have liked each other") ]
                ]
            , div [ class "match-profiles" ]
                [ img [ class "match-profile", src "http://localhost:3000/profiles/self-profile.png" ]
                    []
                , img [ class "match-profile", src possibleMatchedPet.photoUrl ]
                    []
                ]
            , button [ class "button-square button-primary" ]
                [ span [ class "button-chat" ]
                    [ text "Send message" ]
                ]
            , button
                [ class "button-square button-secundary"
                , onClick GoBackFromMatchOverlayButtonWasClicked
                ]
                [ span [ class "button-goback" ]
                    [ text "Go back" ]
                ]
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main =
    program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

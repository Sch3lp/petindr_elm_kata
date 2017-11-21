module Pages.Home exposing (..)

import Data.Pets exposing (..)
import Views.Header exposing(..)

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
    Http.send evaluateMatchMakingResponse <| performMatchmaking <| toString model.currentPet.id


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


performMatchmaking : String -> Http.Request Bool
performMatchmaking petId =
    Http.post ("http://localhost:3000/api/pets/"++petId) Http.emptyBody (Json.Decode.bool)


view : Model -> Html Msg
view model =
    div []
        [ Views.Header.header
        , div [ class "container main-container" ]
            [ viewProfile model
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
            , conditionallyRender (matchOverlayDiv model.possibleMatchedPet) model.showMatchOverlay
            ]
        ]

viewProfile: Model -> Html Msg
viewProfile model = div [ class "profiles" ]
                [ div [ class "profile" ]
                    [ div []
                        [ img [ src model.currentPet.photoUrl ]
                            []
                        , conditionallyRender 
                             (profileTextDiv model.currentPet.text )
                             model.showProfileText
                        ]
                    , div [ class "identification" ]
                        [ span [ class "identification-name" ]
                            [ text model.currentPet.name ]
                        , span [ class "identification-distance" ]
                            [ text <| toString model.currentPet.distance ]
                        ]
                    ]
                ]

profileTextDiv : String -> Html Msg
profileTextDiv profileText = div [ class "profile-text" ] [ text profileText ]

matchOverlayDiv: Pet -> Html Msg
matchOverlayDiv possibleMatchedPet = div [ class "overlay" ]
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

conditionallyRender : Html Msg -> Bool -> Html Msg
conditionallyRender divToBeRendered shouldRender =
    if shouldRender then
        divToBeRendered
    else
        div [] []


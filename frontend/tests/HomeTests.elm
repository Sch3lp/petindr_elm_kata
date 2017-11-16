module HomeTests exposing (..)

import Pages.Home exposing (..)
import Data.Pets exposing (princess, cricket, babyChicken)
import Expect exposing (Expectation)
import Test exposing (..)
import Html exposing (..)
import Test.Html.Query as Query


conditionallyRenderTest : Test
conditionallyRenderTest =
    describe "conditionallyRender"
        [ test "should return empty div when argument is False" <|
            \() ->
                let
                    toBeRenderedDiv =
                        div [] [ text "test" ]
                in
                    conditionallyRender toBeRenderedDiv False
                        |> Query.fromHtml
                        |> Query.children []
                        |> Query.count (Expect.equal 0)
        , test "should return to be rendered div when argument is True" <|
            \() ->
                let
                    toBeRenderedDiv =
                        div [] [ text "test" ]
                in
                    conditionallyRender toBeRenderedDiv True
                        |> Query.fromHtml
                        |> Query.contains [ toBeRenderedDiv ]
        ]


advancePetTest : Test
advancePetTest =
    describe "advancePetTest"
        [ describe "model.nextPets tests"
            [ test "nextPets is empty, nextPets remains empty" <|
                \_ ->
                    let
                        nextPets =
                            []

                        model =
                            Model False princess nextPets False princess

                        actual =
                            advancePet model
                    in
                        Expect.equal [] actual.nextPets
            , test "nextPets contains one value, nextPets becomes empty" <|
                \_ ->
                    let
                        nextPets =
                            [ cricket ]

                        model =
                            Model False princess nextPets False princess

                        actual =
                            advancePet model
                    in
                        Expect.equal [] actual.nextPets
            , test "nextPets contains multiple values, nextPets is trimmed from the top" <|
                \_ ->
                    let
                        nextPets =
                            [ cricket, princess ]

                        model =
                            Model False princess nextPets False princess

                        actual =
                            advancePet model
                    in
                        Expect.equal [ princess ] actual.nextPets
            ]
        , describe "model.currentPet tests"
            [ test "nextPets is empty, currentPet is defaulted to Princess" <|
                \_ ->
                    let
                        nextPets =
                            []

                        model =
                            Model False cricket nextPets False cricket

                        actual =
                            advancePet model
                    in
                        Expect.equal princess actual.currentPet
            , test "nextPets contains a pet, currentPet is set to this last pet" <|
                \_ ->
                    let
                        nextPets =
                            [babyChicken]

                        model =
                            Model False cricket nextPets False cricket

                        actual =
                            advancePet model
                    in
                        Expect.equal babyChicken actual.currentPet
            ]
        ]

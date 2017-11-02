module HomeTests exposing (..)

import Home exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)
import Html exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Select


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

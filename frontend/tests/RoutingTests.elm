module RoutingTests exposing (..)

import Main exposing (route)
import Expect exposing (Expectation)
import Test exposing (..)
import UrlParser exposing ((</>), s, int, parseHash, Parser, oneOf, map, top)
import Navigation as Nav


routingTest : Test
routingTest =
    describe "route"
        [ test "#chat/1 routes to Chat with Int 1" <|
            \_ ->
                parseHash route (buildLocation "#chat/1")
                    |> Expect.equal (Just (Main.Chat 1))
        , test "#chat/1 routes to Chat with Int 65408" <|
            \_ ->
                parseHash route (buildLocation "#chat/65408")
                    |> Expect.equal (Just (Main.Chat 65408))
        , test "#, or nothing routes to Home" <|
            \_ ->
                parseHash route (buildLocation "#")
                    |> Expect.equal (Just Main.Home)
        ]


buildLocation : String -> Nav.Location
buildLocation hash =
    { hash = hash
    , host = ""
    , hostname = ""
    , href = ""
    , origin = ""
    , password = ""
    , pathname = ""
    , port_ = ""
    , protocol = ""
    , search = ""
    , username = ""
    }
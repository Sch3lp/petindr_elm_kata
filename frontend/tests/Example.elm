module Example exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

notASuite = String.reverse "hannah"

suite : Test
suite =
    describe "The String module"
        [ describe "String.reverse"
            [ test "has no effect on a palindrome" <|
                \() ->
                    Expect.equal "hannah" (String.reverse "hannah")
            , test "reverses a known string" <|
                \_ ->
                    "ABCDEFG"
                        |> String.reverse
                        |> Expect.equal "GFEDCBA"
            ]
        ]

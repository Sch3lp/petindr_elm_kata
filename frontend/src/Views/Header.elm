module Views.Header exposing(header)

import Html exposing (..)
import Html.Attributes exposing (..)

header : Html msg
header = Html.header []
            [ span [ class "header-title" ]
                [ text "Petindr" ]
            , button [ class "icon-right chat-icon" ]
                []
            ]
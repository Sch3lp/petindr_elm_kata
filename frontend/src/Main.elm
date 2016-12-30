import Html exposing(..)
import Html.Attributes exposing (..)
import Navigation exposing(..)
import UrlParser exposing (..)


main =
  Navigation.program LocationChange
    { init = init
    , view = view
    , update = update
    , subscriptions = (\_ -> Sub.none)
    }

init: Navigation.Location -> ( Model, Cmd Msg )
init location = 
    ( Model HomeRoute
    , Cmd.none )

type alias Model =
    { currentRoute: Route }

type Msg = 
    LocationChange Navigation.Location

type Route = HomeRoute -- Navigation.newUrl "#"
           | ChatRoute Int --Refactor to PetId -- Navigation.newUrl "#chat/" ++ petId
           | NotFoundRoute

routes: Parser (Route -> a) a 
routes = 
    UrlParser.oneOf
        [ UrlParser.map HomeRoute UrlParser.top
        , UrlParser.map ChatRoute (UrlParser.s "chat" </> UrlParser.int)
--        , UrlParser.map ChatListRoute (UrlParser.s "chat") -- must come after /chat/:petid because /chat will match first
        ]

parseLocation: Location -> Route
parseLocation location =
    case (UrlParser.parseHash routes location) of
      Just route -> route
      Nothing -> NotFoundRoute



update: Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
          LocationChange location -> 
                    let
                        newRoute = parseLocation location
                    in ({ model | currentRoute = newRoute } , Cmd.none)

view: Model -> Html msg
view model = 
    div [ class "body" ]
        <| currentPage model

currentPage: Model -> List (Html msg)
currentPage model =
    case model.currentRoute of
      HomeRoute -> Home.view model
      ChatRoute petId -> Chat.view petId
      NotFoundRoute -> notFoundView

{-
    [ header []
        [ span [ class "header-title" ]
            [ text "Petindr" ]
        , button [ class "icon-right chat-icon" ]
            []
        ]
    , div [ class "container main-container" ] 
        [
            -- profiles
            -- button group (dislike, info, like)
        ]
    -- matched overlay
    ]
-}
notFoundView: Model -> Html msg
notFoundView = 
    div [ class "container main-container" ] 
        [ text "Page not found yo" ]
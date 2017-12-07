module Main exposing (..)

import Navigation
import Html exposing (..)
import Dict exposing (Dict)
import Util exposing (..)
import WebSocket
import Docker.Types exposing (..)
import Docker exposing (fromJson)
import Components as UI
import Messages exposing (Msg(..))
import UrlParser exposing (..)
import Dialog exposing (view)
import Debug exposing (log)


localWebsocket : Navigation.Location -> String
localWebsocket location =
    if location.protocol == "https:" then
        "wss://" ++ location.host ++ "/stream"
    else
        "ws://" ++ location.host ++ "/stream"


type alias Model =
    { webSocketUrl : String
    , swarm : Docker
    , tasks : TaskIndex
    , errors : List String
    , pageFilter : String
    , showImage: Maybe ContainerSpec
    }


type Route = FilterRoute (Maybe String)


route : Parser (Route -> a) a
route =
      UrlParser.map FilterRoute (UrlParser.s "" <?> stringParam "filter")


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        pageFilter =
            case (UrlParser.parsePath route location) of
                Just (FilterRoute (Just s)) ->
                    s
                _ ->
                    ""
    in
        ( { webSocketUrl = localWebsocket location
          , swarm = Docker.empty
          , tasks = Dict.empty
          , errors = []
          , pageFilter = pageFilter
          , showImage = Nothing
          }
        , Cmd.none
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Receive serverJson ->
            case fromJson serverJson of
                Ok serverData ->
                    ( { model | swarm = serverData, tasks = groupBy taskIndexKey serverData.assignedTasks }, Cmd.none )

                Err error ->
                    ( { model | errors = error :: model.errors }, Cmd.none )

        UrlChange location ->
            ( model, Cmd.none )
        PageFilter pageFilter ->
            ( { model | pageFilter = pageFilter }, Cmd.none )
        ShowImageDialog image ->
            ( { model | showImage = image }, Cmd.none )
        AcknowledgeImageDialog ->
            ( { model | showImage = Nothing }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen model.webSocketUrl Receive


view : Model -> Html Msg
view { swarm, tasks, errors, pageFilter, showImage } =
    let
        { services, nodes, networks } =
            swarm
    in
        div []
            [ UI.swarmGrid services nodes networks tasks pageFilter
            , ul [] (List.map (\e -> li [] [ text e ]) errors)
            , bootstrap, Dialog.view
              (case showImage of
                Just a ->
                    Just { closeMessage = Just AcknowledgeImageDialog
                         , containerClass = Nothing
                         , header = Just (text "Image details")
                         , body = Just (p [] [text (toString a)])
                         , footer = Nothing
                         }
                Nothing ->
                  Nothing
              )
            ]


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

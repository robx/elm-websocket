module Echo exposing (..)

import Html
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Process
import Task
import Time
import WebSocket.Explicit as WebSocket exposing (WebSocket)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


type alias Model =
    { websocket : Maybe WebSocket
    , value : String
    , events : List String
    }


type Msg
    = Input String
    | Echo
    | Connect
    | WSOpen (Result String WebSocket)
    | WSMessage String
    | WSClose String
    | WSSendingError String


init : ( Model, Cmd Msg )
init =
    ( { websocket = Nothing
      , value = ""
      , events = []
      }
    , connect
    )


connect : Cmd Msg
connect =
    WebSocket.open "wss://echo.websocket.org"
        { onOpen = WSOpen
        , onMessage = WSMessage
        , onClose = WSClose
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        log event details model =
            { model | events = (event ++ ": " ++ details) :: model.events }
    in
    case msg of
        Input value ->
            ( { model | value = value }, Cmd.none )

        Echo ->
            case model.websocket of
                Just ws ->
                    ( model, WebSocket.send ws model.value WSSendingError )

                Nothing ->
                    ( model |> log "echo" "no connection", Cmd.none )

        Connect ->
            ( model, connect )

        WSOpen (Ok ws) ->
            ( { model | websocket = Just ws }
                |> log "open" "success"
            , Cmd.none
            )

        WSOpen (Err err) ->
            ( model |> log "open" err
            , Task.perform (always Connect) <| Process.sleep (5 * Time.second)
            )

        WSMessage msg ->
            ( model |> log "reply" msg, Cmd.none )

        WSClose reason ->
            ( { model | websocket = Nothing } |> log "close" reason, Cmd.none )

        WSSendingError err ->
            ( model |> log "send" err, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.p []
            [ Html.text <| "Connected: " ++ toString (model.websocket /= Nothing) ]
        , Html.input [ HtmlA.type_ "text", HtmlE.onInput Input ] []
        , Html.button [ HtmlE.onClick Echo ] [ Html.text "Send" ]
        , Html.ul []
            (List.map (\event -> Html.li [] [ Html.text event ]) model.events)
        ]

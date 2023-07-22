module Main exposing (..)

import Browser
import Json.Decode as J
import Set as S

import Html
import Http
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type alias Talk =
    { day : String
    , time : String
    , speaker : Maybe String
    , room : String
    , title : String
    , session : String
    , abstract : Maybe String
    }

decodeTalk : J.Decoder Talk
decodeTalk = J.map7 Talk
    (J.field "Date" J.string)
    (J.field "Timespan" J.string)
    (J.field "Speaker" (J.nullable J.string))
    (J.field "Room" J.string)
    (J.field "Title" J.string)
    (J.field "Session" J.string)
    (J.field "Abstract" (J.nullable J.string))

type Model =
    Loading
    | LoadFailed String
    | ShowAll (List Talk)

main = let
        getTalks : Cmd Msg
        getTalks = Http.get
            { url = "ISMB_ECCB_2023_All_sessions.json"
            , expect = Http.expectJson GotData (J.list decodeTalk)
            }
    in Browser.element
        { init = \() -> (Loading, getTalks)
        , update = update
        , subscriptions = \_ ->  Sub.none
        , view = view }

type Msg =
        GotData (Result Http.Error (List Talk))

update msg model = (updateM msg model, Cmd.none)

updateM msg model =
  case msg of
      GotData r -> case r of
          Ok d -> ShowAll d
          Err err -> LoadFailed <| case err of
            Http.BadUrl e -> "BadURL: " ++ e
            Http.Timeout -> "TimeOut"
            Http.NetworkError -> "NetworkError"
            Http.BadStatus c -> "BadStatus: " ++ String.fromInt c
            Http.BadBody e -> "BadBody: " ++ e

view model = case model of
    Loading -> Html.text "Loading..."
    LoadFailed err -> Html.text ("Load error: " ++ err)
    ShowAll m ->
        let
            n = m
                    |> List.length
                    |> String.fromInt
        in Html.div []
            [ Html.text n]

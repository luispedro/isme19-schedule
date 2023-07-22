module Main exposing (..)

import Browser
import Json.Decode as J
import Set as S

import Html
import Http
import Html exposing (Html)
import Html.Attributes as HtmlAttr
import Html.Events as HE

import Bootstrap.CDN as CDN
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Table as Table

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

type alias FilterSet =
    { days : S.Set String
    , sessions : S.Set String
    , speaker : String
    , title : String
    , abstract : String
    , talks : List Talk
    }

initFilters talks =
    { days = talks
                |> List.map .day
                |> S.fromList
    , sessions = talks
                |> List.map .session
                |> S.fromList
    , speaker = ""
    , title = ""
    , abstract = ""
    , talks = talks
    }

type Model =
    Loading
    | LoadFailed String
    | ShowTalks FilterSet

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
        , view = view
        }

type Msg =
    GotData (Result Http.Error (List Talk))
    | ToggleDayFilter String
    | ToggleSessionFilter String
    | UpdateTitleFilter String
    | UpdateSpeakerFilter String
    | UpdateAbstractFilter String

update msg model = (updateM msg model, Cmd.none)

updateM msg model =
    case msg of
        GotData r -> case r of
          Ok d -> ShowTalks <| initFilters d
          Err err -> LoadFailed <| case err of
            Http.BadUrl e -> "BadURL: " ++ e
            Http.Timeout -> "TimeOut"
            Http.NetworkError -> "NetworkError"
            Http.BadStatus c -> "BadStatus: " ++ String.fromInt c
            Http.BadBody e -> "BadBody: " ++ e
        ToggleDayFilter d -> case model of
            Loading -> model
            LoadFailed err -> model
            ShowTalks m ->
                let
                    newSet =
                        if S.member d m.days
                        then S.remove d m.days
                        else S.insert d m.days
                in ShowTalks { m | days = newSet }
        ToggleSessionFilter d -> case model of
            Loading -> model
            LoadFailed err -> model
            ShowTalks m ->
                let
                    newSet =
                        if S.member d m.sessions
                        then S.remove d m.sessions
                        else S.insert d m.sessions
                in ShowTalks { m | sessions = newSet }
        UpdateTitleFilter t -> case model of
            Loading -> model
            LoadFailed err -> model
            ShowTalks m -> ShowTalks { m | title = t }
        UpdateSpeakerFilter t -> case model of
            Loading -> model
            LoadFailed err -> model
            ShowTalks m -> ShowTalks { m | speaker = t }
        UpdateAbstractFilter t -> case model of
            Loading -> model
            LoadFailed err -> model
            ShowTalks m -> ShowTalks { m | abstract = t }

view m =
    Html.div []
        [ CDN.stylesheet
        , CDN.fontAwesome
        , viewModel m ]


viewModel model = case model of
    Loading -> Html.text "Loading..."
    LoadFailed err -> Html.text ("Load error: " ++ err)
    ShowTalks m ->
        let
            filterDays = List.filter (\t -> S.member t.day m.days)
            filterSpeakers = List.filter (\t -> String.contains (String.toLower m.speaker) (String.toLower (Maybe.withDefault "" t.speaker)))
            filterAbstracts = List.filter (\t -> String.contains (String.toLower m.abstract) (String.toLower (Maybe.withDefault "" t.abstract)))
            sel = m.talks
                    |> filterDays
                    |> filterSpeakers
                    |> filterAbstracts
            allDays = List.map (\t -> t.day) m.talks
                        |> S.fromList
                        |> S.toList
                        |> List.sort
            allSessions = m.talks
                        |> List.map .session
                        |> S.fromList
                        |> S.toList
                        |> List.sort
        in Grid.containerFluid []
            [ Grid.simpleRow
                [ Grid.col []
                    [ Html.h1 [] [ Html.text "ISMB/ECCB 2023" ]
                    , Html.h2 [] [ Html.text "All sessions" ]
                    , Html.p [] [ Html.text "This is a list of all sessions at ISMB/ECCB 2023, based on a table from Lars Juhl Jensen" ]
                    , Html.p [] [ Html.text ("Showing " ++ String.fromInt (List.length sel) ++ " talks") ]
                    ]
                ]
            , Grid.simpleRow
                    [ Grid.col [ ]
                        ((Html.h4 [] [Html.text "Filter by day" ])::
                        List.map (\d ->
                            Grid.simpleRow
                            [ Grid.col [ ]
                                [ Button.button
                                        [ (if S.member d m.days then Button.primary else Button.outlineSecondary)
                                        , Button.onClick (ToggleDayFilter d)
                                        ]
                                        [ Html.text d ]
                                ]]
                            ) allDays)
                    , Grid.col [ ]
                        ((Html.h4 [] [Html.text "Filter by session" ])::
                        List.map (\s ->
                            Grid.simpleRow
                            [ Grid.col [ ]
                                [ Button.button
                                        [ (if S.member s m.sessions then Button.primary else Button.outlineSecondary)
                                        , Button.onClick (ToggleSessionFilter s)
                                        ]
                                        [ Html.text s ]
                                ]]
                            ) allSessions)
                    , Grid.col [ ]
                        [Html.h4 [] [Html.text "Filter by speaker" ]
                        ,Html.input [ HtmlAttr.type_ "text", HtmlAttr.value m.speaker, HE.onInput UpdateSpeakerFilter ] []
                        ]
                    , Grid.col [ ]
                        [Html.h4 [] [Html.text "Filter by title" ]
                        ,Html.input [ HtmlAttr.type_ "text", HtmlAttr.value m.title, HE.onInput UpdateTitleFilter ] []
                        ]
                    , Grid.col [ ]
                        [Html.h4 [] [Html.text "Filter by abstract" ]
                        ,Html.input [ HtmlAttr.type_ "text", HtmlAttr.value m.abstract, HE.onInput UpdateAbstractFilter ] []
                        ]
                    ]

            , Grid.simpleRow
                    [ Grid.col [ ]
                [Table.table
                    { options = [ Table.striped, Table.hover ]
                    , thead =  Table.simpleThead
                        [ Table.th [] [ Html.text "Day" ]
                        , Table.th [] [ Html.text "Time" ]
                        , Table.th [] [ Html.text "Speaker" ]
                        , Table.th [] [ Html.text "Room" ]
                        , Table.th [] [ Html.text "Title" ]
                        , Table.th [] [ Html.text "Session" ]
                        , Table.th [] [ Html.text "Abstract" ]
                        ]
                    , tbody =
                        sel
                            |> List.map (\t ->
                                Table.tr []
                                    [ Table.td [] [ Html.text t.day ]
                                    , Table.td [] [ Html.text t.time ]
                                    , Table.td [] [ Html.text (Maybe.withDefault "" t.speaker) ]
                                    , Table.td [] [ Html.text t.room ]
                                    , Table.td [] [ Html.text t.title ]
                                    , Table.td [] [ Html.text t.session ]
                                    , Table.td [] [ Html.text (Maybe.withDefault "" t.abstract) ]
                                    ])
                            |> Table.tbody []
                    }
                ]
            ]
        ]


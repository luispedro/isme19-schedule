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
import Bootstrap.Dropdown as Dropdown
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
    , session : String
    , sessionFilterState : Dropdown.State
    , speaker : String
    , title : String
    , abstract : String
    , showFullAbstract : Bool
    , talks : List Talk
    }

initFilters talks =
    { days = talks
                |> List.map .day
                |> S.fromList
    , session = ""
    , sessionFilterState = Dropdown.initialState
    , speaker = ""
    , title = ""
    , abstract = ""
    , showFullAbstract = False
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
        , subscriptions = \model -> case model of
            Loading -> Sub.none
            LoadFailed _ -> Sub.none
            ShowTalks m -> Dropdown.subscriptions m.sessionFilterState SessionFilterChanged
        , view = view
        }

type Msg =
    GotData (Result Http.Error (List Talk))
    | ToggleDayFilter String
    | SetSessionFilter String
    | SessionFilterChanged Dropdown.State
    | ToggleShowFullAbstract
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
        _ -> case model of
            Loading -> model
            LoadFailed err -> model
            ShowTalks m -> case msg of
                GotData _ -> model -- impossible, but needed to satisfy the compiler
                ToggleDayFilter d ->
                    let
                        newSet =
                            if S.member d m.days
                            then S.remove d m.days
                            else S.insert d m.days
                    in ShowTalks { m | days = newSet }
                SetSessionFilter d -> ShowTalks { m | session = d }
                SessionFilterChanged s -> ShowTalks { m | sessionFilterState = s }
                ToggleShowFullAbstract -> ShowTalks { m | showFullAbstract = not m.showFullAbstract }
                UpdateTitleFilter t -> ShowTalks { m | title = t }
                UpdateSpeakerFilter t -> ShowTalks { m | speaker = t }
                UpdateAbstractFilter t ->  ShowTalks { m | abstract = t }

view m =
    Html.div []
        [ CDN.stylesheet
        , CDN.fontAwesome
        , Html.node "link"
            [ HtmlAttr.rel "stylesheet"
            , HtmlAttr.href "style.css"
            ]
            []
        , viewModel m ]


viewModel model = case model of
    Loading -> Html.text "Loading..."
    LoadFailed err -> Html.text ("Load error: " ++ err)
    ShowTalks m ->
        let
            filterDays = List.filter (\t -> S.member t.day m.days)
            filterSpeakers = List.filter (\t -> String.contains (String.toLower m.speaker) (String.toLower (Maybe.withDefault "" t.speaker)))
            filterTitles = List.filter (\t -> String.contains (String.toLower m.title) (String.toLower t.title))
            filterAbstracts = List.filter (\t -> String.contains (String.toLower m.abstract) (String.toLower (Maybe.withDefault "" t.abstract)))
            filterSessions = List.filter (\t -> m.session == "" || m.session == t.session)
            sel = m.talks
                    |> filterDays
                    |> filterTitles
                    |> filterSpeakers
                    |> filterAbstracts
                    |> filterSessions
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
                    , Html.p [] [ Html.text "This is a list of all sessions at ISMB/ECCB 2023."]
                    , Html.p [] [ Html.text "Created by "
                                , Html.a [ HtmlAttr.href "https://luispedro.org/"]
                                    [ Html.text "Luis Pedro Coelho" ]
                                , Html.text " based on a table from "
                                , Html.a [ HtmlAttr.href "https://twitter.com/larsjuhljensen"]
                                    [ Html.text "Lars Juhl Jensen" ]
                                , Html.text ". Code is available on "
                                , Html.a [ HtmlAttr.href "https://github.com/luispedro/ismb-schedule" ]
                                    [ Html.text "GitHub" ]
                                ]
                    ]
                ]
            , Grid.simpleRow
                    [ Grid.col [ ]
                        ((Html.h4 [] [Html.text "Filter by day" ])::
                        List.map (\d ->
                                Button.button
                                        [ (if S.member d m.days then Button.primary else Button.outlineSecondary)
                                        , Button.onClick (ToggleDayFilter d)
                                        ]
                                        [ Html.text d ]
                            ) allDays)
                    , Grid.col [ ]
                        (let
                            filter =
                                allSessions
                                        |> List.map (\s ->
                                                Dropdown.buttonItem
                                                    [ HE.onClick (SetSessionFilter s)
                                                    ]
                                                    [ Html.text s ]
                                                )
                        in [Html.h4 [] [Html.text "Filter by session" ]
                            , Dropdown.dropdown
                                m.sessionFilterState
                                { options = [ Dropdown.alignMenuRight ]
                                , toggleMsg = SessionFilterChanged
                                , toggleButton =
                                    Dropdown.toggle [ ] [ Html.text "Select session" ]
                                , items =
                                    ([ Dropdown.buttonItem [ HE.onClick (SetSessionFilter "") ] [ Html.text "All sessions" ]
                                    , Dropdown.divider ] ++ filter)
                                }
                            ])
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
                [ Html.p [] [ Html.text ("Showing " ++ String.fromInt (List.length sel) ++ " talks") ]
                , Button.button
                    [ (if m.showFullAbstract then Button.primary else Button.outlineSecondary)
                    , Button.onClick ToggleShowFullAbstract
                    ]
                    [ Html.text (if m.showFullAbstract then "Trim abstracts" else "Expand abstracts") ]
                , Table.table
                    { options = [ Table.striped, Table.hover, Table.responsive ]
                    , thead =  Table.simpleThead
                        [ Table.th [] [ Html.text "When?" ]
                        , Table.th [] [ Html.text "Room" ]
                        , Table.th [] [ Html.text "Speaker" ]
                        , Table.th [] [ Html.text "Title" ]
                        , Table.th [] [ Html.text "Session" ]
                        , Table.th [] [ Html.text "Abstract" ]
                        ]
                    , tbody =
                        sel
                            |> List.map (\t ->
                                Table.tr []
                                    [ Table.td [] [ Html.text <| t.day ++ " " ++ t.time ]
                                    , Table.td [] [ Html.text t.room ]
                                    , Table.td [] [ showHits m.speaker (Maybe.withDefault "" t.speaker) ]
                                    , Table.td [] [ showHits m.title t.title ]
                                    , Table.td [] [ Html.text t.session ]
                                    , Table.td [] [ showHits m.abstract <| trimAbstract m.showFullAbstract (Maybe.withDefault "" t.abstract) ]
                                    ])
                            |> Table.tbody []
                    }
                ]
            ]
        ]


trimAbstract : Bool -> String -> String
trimAbstract showFull abstract =
    if showFull
    then abstract
    else
        let
            maxLen = 150
            trimmed = String.left maxLen abstract
        in
        if String.length abstract <= maxLen
        then abstract
        else trimmed ++ "..."
showHits filter abstract =
    if filter == ""
    then Html.text abstract
    else
    let
        matches : List Int
        matches = String.indexes (String.toLower filter) (String.toLower abstract)
        showMatch : Int -> Int -> Html msg
        showMatch start ix =
            let
                before = String.slice start ix abstract
            in
                Html.span []
                    [Html.text before
                    ,Html.strong [] [Html.text <| String.slice ix (ix + String.length filter) abstract]]
        showMatches : Int -> List Int -> List (Html msg)
        showMatches start ixs = case ixs of
            [] -> [Html.text <| (String.slice start (String.length abstract) abstract)]
            ix::rest -> showMatch start ix :: showMatches (ix + String.length filter) rest
    in
    Html.p []
        (showMatches 0 matches)


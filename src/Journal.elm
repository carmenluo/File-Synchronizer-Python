module Main exposing (..)

import Html exposing (text, div, beginnerProgram, h1, h2, textarea, input, form, node, Html)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, id, type_, value, style, attribute, cols, rows)
import GraphicSVG exposing (..)
import Svg
import Svg.Attributes
import Char
import String


type alias Gmsg =
    GraphicSVG.Msg Msg

type alias SvgWithArgs = 
    List ( Svg.Attribute Gmsg ) -> Html.Html Gmsg

onClick msg =
    Html.Events.onClick (Graphics msg)


text =
    Html.text


main =
    customLayoutApp Tick { svgView = svgView, htmlView = htmlView, init = ( Menu, Cmd.none ), update = update, subscriptions = subscriptions }


subscriptions model =
    Sub.none


type Msg
    = ShowActivity JournalScreen
    | Tick Float GetKeyState
    | Nothing


type JournalScreen
    = Menu
    | Mood
    | Goal
    | Achievement
    | Note


type alias Model =
    JournalScreen




update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowActivity act ->
            ( act, Cmd.none )

        Nothing ->
            ( model, Cmd.none )

        otherwise ->
            ( model, Cmd.none )


htmlView collage model =
    div [ style [ ( "height", "100%" ) ] ]
        [ node "style" [] [ text "" ]
        , linkStyleSheet "css/journal.css"

        , (case model of
            Menu ->
                menuHtml

            Mood ->
                moodHtml

            Goal ->
                goalHtml

            Achievement ->
                achievementHtml

            Note ->
                noteHtml
          )
        ]


linkStyleSheet : String -> Html Gmsg
linkStyleSheet filename =
    node "link"
        [ attribute "rel" "stylesheet"
        , attribute "href" filename
        ]
        []


svgView model =
    collage 0 0 []


-- menuBar : Html Gmsg
menuBar =
  let backArrow = String.fromChar (Char.fromCode 8701) in
   div [ id "menuBar", onClick (ShowActivity Menu) ] [ text backArrow ]


menuHtml =
    div [ id "menuBG"]
        [ h1 [] [ text "Add Journal Entry" ]
        , h2 [] [ text "What would you like to record?" ]
        , div [ class "button", id "moodButton", onClick (ShowActivity Mood) ] [ text "Mood" ]
        , div [ class "button", id "goalButton", onClick (ShowActivity Goal) ] [ text "Goal" ]
        , div [ class "button", id "achievementButton", onClick (ShowActivity Achievement) ] [ text "Achievement" ]
        , div [ class "button", id "noteButton", onClick (ShowActivity Note) ] [ text "Note" ]
        ]


moodHtml =
    div [ id "moodBG" ]
        [ h1 [] [ text "Record Mood" ]
        , myForm
        , menuBar
        ]


goalHtml =
    div [ id "goalBG"]
        [ h1 [] [ text "Record Goal" ]
        , myForm
        , menuBar
        ] 


achievementHtml =
    div [ id "achievementBG" ]
        [ h1 [] [ text "Record Achievement" ]
        , myForm
        , menuBar
        ]


noteHtml =
    div [ id "noteBG" ]
        [ h1 [] [ text "Record Note" ]
        , myForm
        , menuBar
        ]


myForm =
    form []
        [ div [] [ textarea [ class "textarea", cols 40, rows 20 ] [] ]
        , input [ type_ "submit", value "Save" ] []
        ]

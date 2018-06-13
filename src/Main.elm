module Main exposing (..)

import Calendar exposing (..)
import GraphicSVG exposing (..)
import GraphicSVG as G
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, id, class, attribute)
import Date exposing (..)
import Time
import Task
import List as L
import Array
import CircleOfSix as C6
import Svg
import Svg.Attributes


main =
    GraphicSVG.customLayoutApp
        Tick
        { init = ( initModel, Task.perform GetDate Date.now )
        , update = update
        , svgView = svgView
        , htmlView = htmlView
        , subscriptions = subscriptions
        }


-- MISC

-- Make HTML Event Handlers compatible with GraphicSVG.Msg

onClick msg =
    Html.Events.onClick (Graphics msg)


text =
    Html.text


emptyCollage =
    collage 0 0 []

type alias SvgWithArgs = 
    List ( Svg.Attribute Gmsg ) -> Html.Html Gmsg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none


-- MODEL


type Activity
    = CalendarAct
    | CircleOfSixAct
    | AvatarCreatorAct
    | AddJournalAct
    | PlayGamesAct
    | ResourcesAct


type alias CalendarState = 
  { calendar : Calendar.Calendar
  , myPair : (Month, Year) 
  }

type alias Model =
  { activity : Activity
  , calendarState : CalendarState
  , sixModel : C6.Model
  }

initModel : Model
initModel =
  { activity = CalendarAct
  , calendarState = { myPair = ( Jan, 0 ), calendar = [] }
  , sixModel = C6.init
  }


-- UPDATE

type Direction = Forward | Backward | Still

type Msg
    = Reset
    | Tick Float GetKeyState
    | GetDate Date
    | ChangeMonth Direction
    | ShowActivity Activity
    | CircleSixMsg C6.Msg



-- For improved readability on type signatures
-- Gmsg is our message type wrapped by GraphicSVG.Msg


type alias Gmsg =
    G.Msg Msg


updateMonthYear : Int -> ( Month, Year ) -> ( Month, Year )
updateMonthYear n month =
    case n of
        1 ->
            Calendar.nextMonthYear month

        (-1) ->
            Calendar.prevMonthYear month

        otherwise ->
          month


updateCalendarState : Direction -> (Month, Year) -> CalendarState
updateCalendarState dir pair = 
  let newPair = 
    case dir of
       Forward -> Calendar.nextMonthYear pair
       Backward -> Calendar.prevMonthYear pair
       Still -> pair
  in
  { myPair = newPair
  , calendar = Calendar.calendar newPair
  }


isTick : Msg -> Bool
isTick msg =
  case msg of 
    Tick _ _ -> True
    otherwise -> False


monthYearFromDate : Date -> ( Month, Year )
monthYearFromDate d =
  ( (Date.month d), (Date.year d) )

daysOfTheWeek : List Day
daysOfTheWeek =
  [ Sun, Mon, Tue, Wed, Thu, Fri, Sat ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let 
      _ = if isTick msg then (msg,model) else Debug.log "msg,model" (msg,model)
  in
    case ( msg, model ) of
        ( GetDate date, model ) ->
            ( { model | calendarState = updateCalendarState Still (monthYearFromDate date) }
            , Cmd.none
            )

        ( ChangeMonth by, model ) ->
            ( { model | calendarState = updateCalendarState by model.calendarState.myPair }
            , Cmd.none
            )

        ( ShowActivity act, model ) ->
            ( { model | activity = act }
            , Cmd.none
            )

        ( Tick t state, model ) ->
          case model.activity of
            CircleOfSixAct -> ( { model | sixModel = (C6.update (C6.Tick t state) model.sixModel) } , Cmd.none)
            otherwise -> (model, Cmd.none)

        ( CircleSixMsg msg, model ) ->
            ( { model | sixModel = (C6.update msg model.sixModel) }
            , Cmd.none
            )

        otherwise ->
            ( model, Cmd.none )



-- VIEW


sliceCalendar : Calendar -> Int -> Int -> Html Gmsg
sliceCalendar week start end =
  week
    |> Array.fromList
    |> Array.slice start end
    |> Array.map toString
    |> Array.map (\s -> td [] [ text s ])
    |> Array.toList
    |> tr []


calendarToRows : Calendar -> List (Html Gmsg)
calendarToRows cal =
    [ sliceCalendar cal 0 7
    , sliceCalendar cal 7 14
    , sliceCalendar cal 14 21
    , sliceCalendar cal 21 28
    , sliceCalendar cal 28 35
    , sliceCalendar cal 35 42
    ]


daysOfWeekRow : Html Gmsg
daysOfWeekRow =
    tr [id "weekRow"] <|
        L.map (\day -> th [] [ text day ]) <|
            L.map toString daysOfTheWeek


calendarTable : Calendar -> Html Gmsg
calendarTable cal =
    table [ id "calendar" ] <| daysOfWeekRow :: (calendarToRows cal)


monthYearToString : ( Month, Year ) -> String
monthYearToString ( m, y ) =
    (Calendar.monthToString m) ++ " " ++ (toString y)


-- Making our calendar data 5x7 simplifies conversion into into table rows, so
-- we pad pad it to make it fit these dimensions


calendarViewToHtml : CalendarState -> Html Gmsg
calendarViewToHtml state =
    div [ id "calendarContainer" ]
        [ div [ id "monthYearContainer" ]
            [ span [ onClick (ChangeMonth Backward), id "calPrev" ] [ text "<" ]
            , span [ id "selectedMonthYear" ] [ text <| monthYearToString state.myPair ]
            , span [ onClick (ChangeMonth Forward), id "calNext" ] [ text ">" ]
            ]
        , calendarTable state.calendar
        , menuHtml
        ]


menuHtml : Html Gmsg
menuHtml =
    div [ id "menuContainer" ]
        [ span [ id "menuButton1", onClick (ShowActivity AddJournalAct) ] [ text "[Add Journal Entry]" ]
        , span [ id "menuButton2", onClick (ShowActivity PlayGamesAct) ] [ text "[Play Games]" ]
        , span [ id "menuButton3", onClick (ShowActivity CircleOfSixAct) ] [ text "[Circle Of Six]" ]
        , span [ id "menuButton4", onClick (ShowActivity AvatarCreatorAct) ] [ text "[Avatar/Self]" ]
        , span [ id "menuButton5", onClick (ShowActivity ResourcesAct) ] [ text "[Resources]" ]
        ]


backNavHtml : Html Gmsg
backNavHtml =
    div [ id "menuContainer" ]
        [ span
            [ id "menuButton1"
            , onClick (ShowActivity CalendarAct)
            ]
            [ text "[Go Back]" ]
        ]


linkStyleSheet : String -> Html Gmsg
linkStyleSheet filename =
    node "link"
        [ attribute "rel" "stylesheet"
        , attribute "href" filename
        ]
        []


calendarActivityHtml : CalendarState -> Html Gmsg
calendarActivityHtml state =
    div [ id "main" ]
        [ calendarViewToHtml state
        , linkStyleSheet "css/calendar.css"
        ]


circleOfSixHtml : Html Gmsg -> Html Gmsg
circleOfSixHtml svg =
    div [] [ backNavHtml, svg ]


htmlViewMain svg model =
    case model.activity of
        CalendarAct ->
            calendarActivityHtml model.calendarState

        CircleOfSixAct ->
            circleOfSixHtml svg

        otherwise ->
            div [ style [ ( "width", "100%" ) ] ] [ svg ]


htmlView : SvgWithArgs -> Model -> Html Gmsg
htmlView svg model =
    div [ id "main" ]
        [ htmlViewMain (svg [Svg.Attributes.width "50%"]) model
        , linkStyleSheet "css/calendar.css"
        ]


placeHolderSvg model =
    collage 500 500
        [ group
            [ circle 200 |> filled green
            , G.text "Click me!" |> filled black
            , G.text ("state = " ++ (toString model.activity)) |> filled black |> move ( 0, -20 )
            ]
            |> (notifyTap (ShowActivity CalendarAct))
        ]


svgView model =
    case model.activity of
        CircleOfSixAct ->
            collageMap CircleSixMsg (C6.view model.sixModel)

        otherwise ->
            placeHolderSvg model

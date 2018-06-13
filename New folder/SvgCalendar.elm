{-
   Shape Creator for GraphicSVG and elm 0.17.
   Based on the original ShapeCreator by Levin Noronha.
   Presents the major options for creating shapes on one screen, with visualization and graph paper.
-}


module SvgCalendar exposing (..)


{-
   Copyright 2017 Christopher Kumar Anand
   Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
   1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
   2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
   3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

import GraphicSVG exposing (..)
import String exposing (..)
import Ease exposing (..)
import List as L
import Debug exposing (log)
import AvatarCreator as A1
import Basics exposing (..)
import Round as R exposing (..)
import Date exposing (..)
import Time
import Array
import Dict
import AngryFace exposing (..)
import FrustratedFace exposing (..)
import HopefulFace exposing (..)
import SadFace exposing (..)
import HappyFace exposing (..)
import VibrantFace exposing (..)





options =
    { debug = False }


type State
    = Start
    | ExpandingCell Bool Float
    | ClosingCell Float
    | OpenedCell Bool -- Bool for whether it is opening today



type JEntry = JText String
            | JHappy Float
            | JAchievement Achievement

type Achievement = ClimbMountain
                 | ReadBook

type alias Model =
    { time : Float
    , timeToShow : Float
    , faceModel : A1.Model
    , xcoord : Float
    , ycoord : Float
    , expandX : Int
    , expandY : Int
    , currentText : String
    , timeForCurrentEntry : Time.Time
    , previousDailyEntries : List (Time.Time, JEntry)
    , expandTime : Float
    , scaledSize : Float
    , state : State
    , xChange : Float
    , yChange : Float
    , yStep : Float
    , yStart : Float
    , xStep : Float
    , xStart : Float
    , height : Float
    , width : Float
    , firstDayVisible : Time.Time
    , firstMonthVisible : Time.Time
    , journalEntries : Dict.Dict Int (List (Time.Time,JEntry))
    , angryModel: AngryFace.Model
    , frustratedModel: FrustratedFace.Model
    , hopefulModel: HopefulFace.Model
    , sadModel: SadFace.Model
    , happyModel: HappyFace.Model
    , vibrantModel: VibrantFace.Model
  --  , nextmonth : bool
  --  , previousmonth : bool

    }

initJ = Dict.fromList [(dayFromTime 0, [ (0, JText "On Aug 28th I jumped a magic bean.") ])
                      ]

init : Model
init =
    { time = 0
    , timeToShow = 0
    , faceModel = A1.init
    , xcoord = 0
    , ycoord = 0
    , expandX = 1000
    , expandY = 1000
    , currentText = ""
    , timeForCurrentEntry = 0
    , previousDailyEntries = []
    , expandTime = 1
    , scaledSize = 0
    , state = Start
    , xChange = 0
    , yChange = 0
    , yStep = -(const.screen.height / 7)
    , yStart = const.screen.height / 2 - (const.screen.height / 7)
    , xStep = (const.screen.width / (7))
    , xStart = -const.screen.width / 2 + 5
    , height = const.screen.height
    , width = const.screen.width
    , firstDayVisible = 0
    , firstMonthVisible = 0
    , journalEntries = Dict.empty
    , angryModel = AngryFace.init
    , frustratedModel = FrustratedFace.init
    , hopefulModel = HopefulFace.init
    , sadModel = SadFace.init
    , happyModel = HappyFace.init
    , vibrantModel = VibrantFace.init
   -- , nextmonth = false
   -- , previousmonth = false
    }

windowResize : Float -> Float -> Model -> Model
windowResize w h model =
    { model | width = w
            , height = h
            , yStep = -(h / 7)
            , yStart = h / 2 - (h / 7)
            , xStep = (w / (7))
            , xStart = -w / 2 + 5
    }

update : Msg -> Model -> (Model, Maybe (String -> Msg))
update msg model =
    let m1 = case msg of
        Tick t screen _ ->
            { model
                | time = t
                , firstDayVisible = if model.firstDayVisible == 0
                                      then t 
                                      else model.firstDayVisible
                , expandTime = updateExpandModel model
                , state = case model.state of
                              ExpandingCell today t ->
                                  if t < 0.95 then ExpandingCell today (t + 0.05) else OpenedCell today

                              ClosingCell t ->
                                  if t < 0.95 then ClosingCell (t + 0.05) else Start

                              state ->
                                  state
                , height = screen.height
                , width = screen.width
                , xChange = updateXChangeModel model
                , yChange = updateYChangeModel model
                -- these should change based on updated screen geometry
                , yStep = -(screen.height / 7)
                , yStart = screen.height / 2 - (screen.height / 7)
                , xStep = (screen.width / (7))
                , xStart = -screen.width / 2 + 5

            }

        FullPage ( x, y ) ->
            { model
                | xcoord = getCellX x
                , ycoord = getCellY y
            }

        Today ->
            { model
                | state = case model.state of
                              OpenedCell True -> ClosingCell 0
                              Start           -> ExpandingCell True 0
                              transition -> transition
                , timeToShow = model.time
                , timeForCurrentEntry = model.time
                , currentText = ""
                , previousDailyEntries = getJEntries model model.time
            }

        GetTextEntry str ->
            setJEntry (JText str) model

        --face messages
        HappyMsg msg ->
            { model
            | happyModel = HappyFace.update msg model.happyModel

        }
        AngryMsg msg ->
            { model
            | angryModel =  AngryFace.update msg model.angryModel

        }

        FrustratedMsg msg ->
            { model
            | frustratedModel = FrustratedFace.update msg model.frustratedModel

        }

        HopefulMsg msg ->
            { model
            | hopefulModel = HopefulFace.update msg model.hopefulModel

        }

        SadMsg msg ->
            { model
            | sadModel= SadFace.update msg model.sadModel

        }
        VibrantMsg msg ->
            { model
            | vibrantModel= VibrantFace.update msg model.vibrantModel

        }


        TapCalendar ( x, y) ->
            case model.state of
              Start ->
                  { model
                      | expandX = filterX <| getCellX x
                      , expandY = filterY <| getCellY y
                      , state = case model.state of
                                    Start ->
                                        ExpandingCell False 0
                                    OpenedCell _ ->
                                        ClosingCell 0
                                    state ->
                                        state
                  }
              _ ->
                  model
    in
        (m1, case m1.state of
                 ExpandingCell True _ -> Just GetTextEntry
                 OpenedCell True -> Just GetTextEntry
                 _ -> Nothing
        )
filterX x = if x < 1 then 0 else if x > 6 then 6 else Basics.floor x
filterY y = if y < 1 then 0 else if y > 5 then 5 else Basics.floor y

type Msg
    = Tick Float { width : Float, height : Float } GetKeyState
    | FullPage ( Float, Float )
    | TapCalendar ( Float, Float )
    | GetTextEntry String
    | Today
    | AngryMsg AngryFace.Msg
    | FrustratedMsg FrustratedFace.Msg
    | HopefulMsg HopefulFace.Msg
    | SadMsg SadFace.Msg
    | HappyMsg HappyFace.Msg
    | VibrantMsg VibrantFace.Msg




updateXChangeModel model =
    case model.state of
        Start ->
            0

        ExpandingCell _ t ->
            xFormula model

        state ->
            model.xChange


updateYChangeModel model =
    case model.state of
        Start ->
            0

        ExpandingCell _ t ->
            yFormula model

        state ->
            model.yChange


xFormula model =
    model.xChange - ((Basics.toFloat model.expandX) - 3) * 0.75


yFormula model =
    (model.yChange + ((Basics.toFloat model.expandY) - 2.5) * 1.15) + 0.15


expandChange =
    0.1


expandFull =
    7


updateExpandModel model =
    case model.state of
        Start ->
            1

        ExpandingCell _ t ->
            model.expandTime + expandChange

        ClosingCell t ->
            model.expandTime - expandChange

        OpenedCell _ ->
            expandFull


weeksList =
    (List.range 0 5)


daysList =
    [ [ 0, 1, 2, 3, 4, 5, 6 ], [ 0, 1, 2, 3, 4, 5, 6 ], [ 0, 1, 2, 3, 4, 5, 6 ], [ 0, 1, 2, 3, 4, 5, 6 ], [ 0, 1, 2, 3, 4, 5, 6 ], [ 0, 1, 2, 3, 4, 5, 6 ] ]


circleOfSixRadius =
    25


getCellX x =
    (x + 128) / 36


getCellY y =
    -(y - 250) / 54


view : Model -> List (Shape (GraphicSVG.Msg Msg))
view model =
    [ group
        [ calendarGrid model
   --     , circle 10 |> filled red |> move (-model.width/2,model.height/2)
   --     , circle 10 |> filled red |> move (model.width/2,model.height/2)
   --     , circle 30 |> filled red |> move (-model.width/2,-model.height/2)
   --     , circle 50 |> filled red |> move (model.width/2,-model.height/2)
        ]
       |> scale 0.4
        |> move ( 0, 35 )
    , backgroundDebugScreen model
    ]++ (
        case model.state of
            -- editable day
            OpenedCell _ -> [ dayCell model
                            ]
            _ -> []
    )


--animation

-- IMPORTANT FOR SPEED OF THE ANIMATION
-- in seconds*10

myCircleOutline =
    circle 11
        |> outlined (solid 1) orange
animationTimeLength =
    20

enlargedSize =
    1.05
scaledSize =
    1

timeShift =
    0.1

circleMultiplyer =
    246


theta =
    1.59


startingY =
    -43

--DONT TOUCH


eightyPercent =
    animationTimeLength * (0.8)


eightySevenPercent =
    animationTimeLength * (0.877777777)


twentyPercent =
    animationTimeLength * (0.2)


thirteenPercent =
    animationTimeLength * (0.133333)


fiftyPercent =
    animationTimeLength / 2


circleX x ani =
    if ani - x * thirteenPercent < 0 then
        -1 * circleOfSixRadius
    else
        (sin ((ani / circleOfSixRadius) / circleOfSixRadius * circleMultiplyer - (x * (delay ani)) - theta)) * circleOfSixRadius


circleY y ani =
    if ani - y * thirteenPercent < -1 * thirteenPercent then
        startingY
    else if ani - y * thirteenPercent < 0 then
        (ani - y * thirteenPercent) * ((-1) * startingY / thirteenPercent)
    else
        (cos ((ani / circleOfSixRadius) / circleOfSixRadius * circleMultiplyer - (y * (delay ani)) - theta)) * circleOfSixRadius


middleTransparency aniTimeForMidTransparency =
    if aniTimeForMidTransparency <= eightyPercent then
        0
    else
        (aniTimeForMidTransparency - (eightyPercent)) / (twentyPercent - thirteenPercent)


delay ani =
    if ani < 0 then
        Basics.sqrt ((circleOfSixRadius ^ 2) + (circleOfSixRadius ^ 2) - (2 * (circleOfSixRadius ^ 2) * cos ((2 * Basics.pi) / 6)))
    else
        (2 * Basics.pi) / 6


scaledCircleOfSixContactIconSize =
    0.8


scaleContacts mod num =
    if (mod.aniTimeExpand + thirteenPercent) - ((5 - num) * thirteenPercent) < thirteenPercent / 2 then
        0.5
    else if (mod.aniTimeExpand + thirteenPercent) - ((5 - num) * thirteenPercent) < thirteenPercent / 2 + eightySevenPercent / 4 then
        0.5 + ((mod.aniTimeExpand + thirteenPercent) - ((5 - num) * thirteenPercent) - (thirteenPercent / 2)) / (thirteenPercent / 2 + eightySevenPercent / 4) / 2 * scaledCircleOfSixContactIconSize
    else if (mod.aniTimeExpand + thirteenPercent) - ((0) * thirteenPercent) < thirteenPercent / 2 + eightySevenPercent / 4 then
        1
    else
        (max (sin (mod.time - timeShift * num) * enlargedSize) scaledSize) * scaledCircleOfSixContactIconSize

makeButton model data =
    group
        [ group
            [ circle 11
                |> filled backgroundPerson
            , data.face
            --, if data.emergency == True then
              --  emergency
              --else
               -- group []
            , myCircleOutline
                |> makeTransparent data.noteAttributes.transparency
                |> move ( data.noteAttributes.x, data.noteAttributes.y )
            ]
           -- |> notifyTap (ButtonPress data.position)
        ]
        |> move ( circleX data.n model.aniTimeExpand, (circleY data.n model.aniTimeExpand) - 20 )
        |> scale (scaleContacts model (5 - data.n))

--function goes

dayCell model =
    let
        w = 250
        h = 300
        charsPerLine = 40
        fontSize = 6
        s = 0.35 * min (model.width/w) (model.height/h) --0.35
        date = Date.fromTime model.time
        month = Date.month date
        day = Date.day date
   --   entries = (model.time,JText model.currentText) :: (getJEntries model model.time)
        entries = (model.time,JText <| toString model.time ++ model.currentText) :: (getJEntries model model.time)  -- FIXME:  only for current day
    --    entries = ((model.firstDayVisible*1000),JText model.currentText) :: (getJEntries model (model.firstDayVisible*1000))
        textAndSizes = List.map (someText (w-10) charsPerLine fontSize) <| List.filterMap justText entries
        (shapes,_) = List.foldr (\ (txtShape,numLines) (shapes,totalLines) -> ( [txtShape, group shapes |> move (0,-10*Basics.toFloat numLines)]
                                                                              , totalLines + Basics.toFloat numLines)
                                )
                                ([], 0)
                                textAndSizes
    in
        group [ rect w h |> filled (rgba 144 238 144 0.2)
              , rect w h |> outlined (solid 2) (rgb 144 238 144)
           {-   , (toString (Date.fromTime <| 1.0072778 * model.time))
                  |> text
                  |> size 10
                  |> centered
                  |> filled darkBlue
                  |> move (0,120)-}
              , (group shapes |> move (130,95))

              --Angry Slider
             , GraphicSVG.map AngryMsg (AngryFace.slider model.angryModel)
             |>move (-30,-30)
             --Angry Face
             , GraphicSVG.map AngryMsg (AngryFace.myShapes model.angryModel)
             |>move (30,-95)
             |> scale 0.45

             --Frustrated Slider
             , GraphicSVG.map FrustratedMsg (FrustratedFace.slider model.frustratedModel)
             |> move (-30, 0)
             --Frustrated Face
             , GraphicSVG.map FrustratedMsg (FrustratedFace.myShapes model.frustratedModel)
             |>move (60,-70)
             |> scale 0.45

            --Sad Slider
              , GraphicSVG.map SadMsg (SadFace.slider model.sadModel)
             |> move (-30, 25)
            --Sad Face
             , GraphicSVG.map SadMsg (SadFace.myShapes model.sadModel)
             |>move (30,-45)
             |> scale 0.5

             --Hopeful Slider
             , GraphicSVG.map HopefulMsg (HopefulFace.slider model.hopefulModel)
             |> move (-30,55)
             --Hopeful Face
             , GraphicSVG.map HopefulMsg (HopefulFace.myShapes model.hopefulModel)
             |>move (60,-15)
             |> scale 0.5

            --Happy Slider
              , GraphicSVG.map HappyMsg (HappyFace.slider model.happyModel)
             |> move (-30, 60)
             --Happy Face
             , GraphicSVG.map HappyMsg (HappyFace.myShapes model.happyModel)
             |>move (30,8)
             |> scale 0.5

             --Vibrant Slider
              , GraphicSVG.map VibrantMsg (VibrantFace.slider model.vibrantModel)
             |> move (-30, 90)
             --Vibrant Face
             , GraphicSVG.map VibrantMsg (VibrantFace.myShapes model.vibrantModel)
             |>move (60,17)
             |> scale 0.5

            ]
                    |> scale s
                    |> move (0,0.1*model.height)

stringOfAniTime x =
    Basics.toString x

justText (t,je) = case je of
                      JText string -> Just (t,string)
                      _            -> Nothing

backgroundDebugScreen model =
    if options.debug then
        group
            [ -- Test
              GraphicSVG.text ("x: " ++ toString model.xcoord ++ " y: " ++ toString model.ycoord)
                |> filled black
            , GraphicSVG.text ("x: " ++ ((toString model.expandX)) ++ " y: " ++ ((toString model.expandY)))
                |> filled black
                |> move ( 0, -10 )
            , GraphicSVG.text ("Expand: " ++ ((toString model.expandTime)))
                |> filled black
                |> move ( 0, -20 )
            , GraphicSVG.text ("Expand: " ++ ((toString model.state)))
                |> filled black
                |> move ( 0, -30 )
            , GraphicSVG.text ("x: " ++ ((R.round 0 model.xChange)) ++ " y: " ++ ((R.round 0 model.yChange)))
                |> filled black
                |> move ( 0, -40 )
            , GraphicSVG.text ("x: " ++ (toString (xFormula model)) ++ " y: " ++ (toString (yFormula model)))
                |> filled black
                |> move ( 0, -50 )
            , GraphicSVG.text ("28: " ++ (toString (getJEntries model model.time)) )
                |> filled black
                |> move ( 0, -60 )
            , GraphicSVG.text ("28: " ++ (toString (model.journalEntries)) )
                |> filled black
                |> move ( 0, -70 )
            , GraphicSVG.text ("28: " ++ (toString (dayFromTime model.time,model.time) ))
                |> filled black
                |> move ( 0, -80 )
            , GraphicSVG.text ("28: " ++ (toString ((List.map (\x -> 24*60*60*1000 * Basics.toFloat x + model.firstDayVisible) <| List.range 0 (7*6)))) )
                |> filled black
                |> move ( 0, -90 )
            , GraphicSVG.text ("28: " ++ (toString ((List.map (\x -> dayFromTime <| 24*60*60*1000 * Basics.toFloat x + model.firstDayVisible) <| List.range 0 (7*6)))) )
                |> filled black
                |> move ( 0, -100 )
            , GraphicSVG.text ("currentText: " ++ (model.currentText) )
                |> filled black
                |> move ( 0, -110 )
            ]
            |> scale 0.5
    else
        group []


const =
    let
        calendar =
            { cellWidth = 91
            , cellHeight = 159
            , cellColor = rgb 250 253 165
            }

        screen =
            { width = 640
            , height = 960
            }

        misc =
            { backgroundColor = black }
    in
        { cal = calendar
        , screen = screen
        , misc = misc
        }


type alias Cell =
    { x : Float
    , y : Float
    }



months =
    (List.map (toString >> text >> size 18.5 >> filled (rgb 180 180 180)) <| List.range 1 12)



xs =
    [ 0, 1, 2, 3, 4, 5, 6]


ys =
    [ 0, 1, 2, 3, 4, 5 ]


xys : List ( Float, Float )
xys =
    List.concat <| List.map (\y -> List.map (\x -> ( x, y )) xs) ys







-- Numbered days of the month


mappedMonthDays model =

  
  let
  
    findSunday t = if Sun == (Date.dayOfWeek <| Date.fromTime <| 1000*t)
                     then t
                     else findSunday <| t - 24*60*60 

    firstSundayVisible = findSunday model.firstDayVisible*1000


    timesOfVisibleDays = List.map (\x -> 24*60*60 * Basics.toFloat x*1000 + firstSundayVisible) <| List.range 0 (7*6)

  in

     (List.map2 (\( x, y ) time  -> (dayMaker model (time |> Date.fromTime |> Date.day |> toString |> text |> size 18.5 |> filled charcoal) time x y))

               xys
               timesOfVisibleDays)
                ++    [text(toString <| Date.month  <| Date.fromTime <| firstSundayVisible) |>size 40|> filled blue |> move (-780,350)]                 
                ++    [text(toString <| Date.year  <| Date.fromTime <| firstSundayVisible) |>size 40|> filled blue |> move (-700,350)]


                ++    [text (toString <| Date.fromTime <| firstSundayVisible) |> filled red |> move (0,20)]
                ++    [text (toString <|24*60*60 + firstSundayVisible/1000) |> filled red |> move (0,40)] 

  {-
  if (nextmonth == false && previousmonth ==false){
  
  let
  
    findSunday t = if Sun == (Date.dayOfWeek <| Date.fromTime <| 1000*t)
                     then t
                     else findSunday <| t - 24*60*60 

    firstSundayVisible = findSunday model.firstDayVisible*1000


    timesOfVisibleDays = List.map (\x -> 24*60*60 * Basics.toFloat x*1000 + firstSundayVisible) <| List.range 0 (7*6)

    }
                
  else if (previousmonth){
    let

        findSunday t = if Sun == (Date.dayOfWeek <| Date.fromTime <| 1000*t)
                     then t
                     else findSunday <| t - 24*60*60 

        firstSundayVisible = findSunday (model.firstDayVisible-20)*1000


        timesOfVisibleDays = List.map (\x -> 24*60*60 * Basics.toFloat x*1000 + (firstSundayVisible-20) <| List.range 0 (7*6)
      }
    

  else if (nextmonth) {

     let

        findSunday t = if Sun == (Date.dayOfWeek <| Date.fromTime <| 1000*t)
                     then t
                     else findSunday <| t - 24*60*60 

        firstSundayVisible = findSunday (model.firstDayVisible+20)*1000


        timesOfVisibleDays = List.map (\x -> 24*60*60 * Basics.toFloat x*1000 + (firstSundayVisible+20)) <| List.range 0 (7*6)
        }

      in

     (List.map2 (\( x, y ) time  -> (dayMaker model (time |> Date.fromTime |> Date.day |> toString |> text |> size 18.5 |> filled charcoal) time x y))

               xys
               timesOfVisibleDays)
                ++    [text(toString <| Date.month  <| Date.fromTime <| firstSundayVisible) |>size 40|> filled blue |> move (-780,350)]                 
                ++    [text(toString <| Date.year  <| Date.fromTime <| firstSundayVisible) |>size 40|> filled blue |> move (-700,350)]


                ++    [text (toString <| Date.fromTime <| firstSundayVisible) |> filled red |> move (0,20)]
                ++    [text (toString <|24*60*60 + firstSundayVisible/1000) |> filled red |> move (0,40)]

          -}

mappedMonth model =
  let
    timesOfVisibleMonths = List.map (\x -> 30*24*60*60 * Basics.toFloat x + model.firstMonthVisible) <| List.range 0 (7*6)
  in
    (List.map2 (\( x, y ) time  -> (dayMaker model (time |> Date.fromTime |> Date.month |> toString |> text |> size 18.5 |> filled charcoal) time x y))
              xys
     --         months
              timesOfVisibleMonths)
              ++    [text (toString <| Date.fromTime <| model.firstMonthVisible) |> filled green |> move (0,60)]
                ++    [text (toString <|24*60*60*30 + model.firstMonthVisible*1000*30) |> filled green |> move (0,80)]
{-
mappedMonth model =
  let
    timesOfVisibleMonths = List.map (\x -> 30*24*60*60 * Basics.toFloat x + model.firstMonthVisible) <| List.range 0 (7*6)
  in
    (List.map2 (\( x, y ) time  -> (dayMaker model (time |> Date.fromTime |> Date.month |> toString |> text |> size 18.5 |> filled charcoal) time x y))
              xys
     --         months
              timesOfVisibleMonths)
              ++    [text (toString <| Date.fromTime <| model.firstMonthVisible) |> filled green |> move (0,60)]
                ++    [text (toString <|24*60*60*30 + model.firstMonthVisible*1000*30) |> filled green |> move (0,80)]
-}
dayMaker model day time x y =
    group
        [ group
            [ rect ((model.width / 24.8)) ((model.height / 24.6))
                |> outlined (solid 0.5) green
            , rect (model.width / 24.8) (model.height / 24.6)
                |> filled white
            ]
            |> move ( 0, -30 )
            |> move ( model.xStart + x * model.xStep + 64, model.yStart + y * model.yStep - 20 )
            |> scale 3.5
        , day
            |> move ( model.xStart + x * model.xStep, model.yStart + y * model.yStep )
            |> move ( -25, -10 )
   --     , text (toString time) |> filled red-- debug
    --        |> move ( model.xStart + x * model.xStep, model.yStart + y * model.yStep )
    --        |> move ( 0, -12 )
        , text (toString (getJEntries model time |> List.map (\ (_,y) -> y )) ) |> filled green-- debug
            |> move ( model.xStart + x * model.xStep, model.yStart + y * model.yStep )
            |> move ( 0, -22 ) 
        , group (getJEntries model time |> List.map (\ (_,y) -> y ) |> List.map drawAcheivement )-- debug
            |> move ( model.xStart + (x-0.1) * model.xStep, model.yStart + (y) * model.yStep )
            |> move ( 0, -32 )
    {-    , group ( List.map2 ( \ s yOff -> s |> move ( model.xStart + x * model.xStep, model.yStart + y * model.yStep + yOff) )
                            ( (getJEntries model time)
                                 |> List.map (\ (_,y) -> y )
                                 |> List.map drawAcheivement
                            )
                            [-10,-17,-24,-31]
                )-}
        ]


drawAcheivement : JEntry -> Shape (GraphicSVG.Msg Msg)
drawAcheivement je =
    case je of
        JText string   -> text string |> size 10 |> filled darkGreen
        JHappy h       -> circle h |> filled red
        JAchievement _ -> circle 3 |> filled blue

-- we'll count days from Jan 1st
fstJan2017Daybreak = 1480550400000
msPerDay = 24 * 60 * 60 * 1000

-- store information by day, so we can look it up easily
-- calculate the approximate day in this way, some times near midnight may be in the wrong day
dayFromTime : Time.Time -> Int
dayFromTime t = (t - fstJan2017Daybreak) / msPerDay
                   |> Basics.floor


getJEntries : Model -> Time.Time -> List (Time.Time,JEntry)
getJEntries model time =
    case Dict.get (dayFromTime (time*1000)) model.journalEntries of
        Just l -> List.sortBy (\ (x,_) -> x ) l
        Nothing -> []
{-
setJEntry : JEntry -> Model -> Model
setJEntry new model =
    { model | journalEntries = Dict.insert (Basics.floor(model.firstDayVisible*1000))
                                           ( (model.firstDayVisible, new) :: model.previousDailyEntries )
                                           model.journalEntries
    }
-}
setJEntry : JEntry -> Model -> Model
setJEntry new model =
    { model | journalEntries = Dict.insert (Basics.floor(model.timeForCurrentEntry))
                                           ( (model.timeForCurrentEntry, new) :: model.previousDailyEntries )
                                           model.journalEntries
    }

-- Text days of the week


mappedWeekTitles model =
    (List.map2 (\x day -> text day |> size 20 |> filled black |> move ( model.xStart + (x-0.1) * model.xStep - 5, -model.yStep * 3 - 40 ))
   -- move ( model.xStart + (x + 0.5) * model.xStep - 5, -model.yStep * 3 - 40 ))
        [ 0, 1, 2, 3, 4, 5, 6 ]
        [ "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday" ]
    )

mappedmonthsTitles model =
    (List.map2 (\x month -> text month |> size 20 |> filled black |> move ( model.xStart + (x-0.1) * model.xStep - 5, -model.yStep * 3 - 40 ))
   -- move ( model.xStart + (x + 0.5) * model.xStep - 5, -model.yStep * 3 - 40 ))
        [ 0, 1, 2, 3, 4, 5, 6 , 7, 8, 9, 10, 11]
        [ "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December" ]
    )

giantRectangle model =
    [ rectangle model.width (model.height - (model.height / 7))
        |> outlined (solid 3) red
        |> makeTransparent 0 -- change for debug
        |> move ( 0, -model.height / 17.5 )
        |> notifyMouseMoveAt FullPage
      --  |> notifyTapAt TapCalendar
    ]



--++ part3

calendarGrid model =
    group (mappedWeekTitles model
        ++ mappedMonthDays model
        ++ [text ("model.firstDayVisible "++toString model.firstDayVisible ++toString (model.firstDayVisible *1000 |> Date.fromTime)) |>filled black]
        ++ [text ("current "++toString model.timeForCurrentEntry ) |>filled black |> move (100,-50)]

   --     ++ mappedMonth model
        ++ giantRectangle model)
 {-       ++ expandedCell model) -}


background model =
    rect model.width model.height
        |> filled const.misc.backgroundColor


noteColour =
    red


noteColour2 model =
    rgb 50 166 model


noteColour3 =
    rgb 250 66 106


noteColour4 =
    rgb 50 0 250


backgroundPerson =
    rgb 200 80 75


faceScale =
    0.08




changeAllFace model =
    group
        [ --THIS IS THE WHOLE FACE
          A1.myHairBack model.hairBack model
        , rect 30 155
            |> filled backgroundPerson
            |> move ( 0, -55 )

        --EARS
        , group
            [ oval 24 44 |> filled black
            , oval 20 40 |> filled (A1.mySkinColour model.skinColour)
            ]
            |> move ( -70, 0 )
        , group
            [ oval 24 44 |> filled black
            , oval 20 40 |> filled (A1.mySkinColour model.skinColour)
            ]
            |> move ( 70, 0 )

        --The oval that makes up the face
        , oval 143 156
            |> filled black
            |> move ( 0, -7 )
        , oval 140 153
            |> filled (A1.mySkinColour model.skinColour)
            |> move ( 0, -7 )

        --, blush |> move (-40,-20)
        --, blush |> move (40,-20)
        , group
            [ filled black (circle 20) |> move ( -27, 0 )

            --Left eye
            , filled white (circle 18) |> move ( -27, 0 )
            , circle 15
                |> filled (A1.myEyeColour model.eyeColour)
                |> move ( -24, 0 )
            , filled black (circle 12) |> move ( -21, 2 )
            , filled white (circle 6) |> move ( -18, 3 )
            , filled black (circle 20) |> move ( 27, 0 )

            --Right eye
            , filled white (circle 18) |> move ( 27, 0 )
            , circle 15
                |> filled (A1.myEyeColour model.eyeColour)
                |> move ( 30, 0 )
            , filled black (circle 12) |> move ( 32, 2 )
            , filled white (circle 6) |> move ( 35, 3 )
            ]
        , A1.eyebrows model
        , A1.myHairFront model.hairFront model
            |> move ( 0, 60 )
        , curve ( -50, -20 ) [ Pull ( 0, -40 ) ( 50, -20 ) ]
            |> outlined (solid 3) red
            |> move ( 0, -40 )
            |> scale 0.4
        ]
        |> scale faceScale


-- dividing up strings

takeNum : Int -> List String -> (String,List String)
takeNum num wrds = case wrds of
                     head :: tail -> if String.length head <= num
                                     then
                                       let (firstLine, rest) = takeNum (num - String.length head - 1) tail
                                       in (String.concat [head," ",firstLine],rest)
                                     else ("",wrds)
                     []           -> ("",[])
mkLines size wrds
  = case takeNum size wrds of
      (line,[]) -> [line]
      ("",_)    -> []
      (line,moreWrds) -> line :: (mkLines size moreWrds)

someText rectWidth charsPerLine fontSize (time,str)
  = let
      fullLines = List.take 4 <| mkLines charsPerLine  <| words str

      truncatedLines = List.map (String.slice 0 charsPerLine) fullLines

      date = Date.fromTime time
      hour = Date.hour date
      minute = Date.minute date
      timeStamp = (toString hour)++":"++(right 2 <| "0" ++ toString minute)
                     |> text |> size (fontSize-2) |> bold |> filled red --(rgb 100 100 100)
                     |> move (-rectWidth,23+fontSize)
    in
      (group (timeStamp ::
                 (List.map2 (\ line idx -> text line |> size fontSize |> filled red |> move (-(rectWidth),-idx * (fontSize-1) + 24))
                  truncatedLines [0,1,2,3,4,5,6,7,8,9,10]
                 )
             )
        |> move (0,fontSize-1)
      ,List.length truncatedLines
      )

nextmonthIcon =
    group
        [ GraphicSVG.text "NEXT MONTH"
            |> filled darkGreen
            |> move ( 0, 0 )
            |> scale 1
        ]
previousmonthIcon =
    group
        [ GraphicSVG.text "PREVIOUS MONTH"
            |> filled darkGreen
            |> move ( 0, 0 )
            |> scale 1
        ]


-- Merging Jasper's Code:
{-
   monthYearFromDate d =
       ( (Date.month d), (Date.year d) )
   sliceCalendar week start end =
       week
           |> Array.fromList
           |> Array.slice start end
           |> Array.map toString
           |> Array.map (\s -> text s)
           |> Array.toList
   calendarToRows cal =
       [ sliceCalendar cal 0 7
       , sliceCalendar cal 7 14
       , sliceCalendar cal 14 21
       , sliceCalendar cal 21 28
       , sliceCalendar cal 28 35
       , sliceCalendar cal 35 42
       ]
   calendarTable cal =
       daysOfWeekRow :: (calendarToRows cal)
   monthYearToString : ( Month, Year ) -> String
   monthYearToString ( m, y ) =
       (Calendar.monthToString m) ++ " " ++ (toString y)
-}
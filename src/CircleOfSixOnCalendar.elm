{-
   Shape Creator for GraphicSVG and elm 0.17.
   Based on the original ShapeCreator by Levin Noronha.

   Presents the major options for creating shapes on one screen, with visualization and graph paper.
-}


module CircleOfSixOnCalendar exposing (..)

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


options =
    { debug = False }



-- MODEL


type State
    = Start
    | ExpandingContacts
    | ClosingContacts
    | CircleOfSixOpen
    | StationaryTopNote
    | TopNoteFadingIn
    | TopNoteFadingOut
    | StationaryRightTopNote
    | RightTopNoteFadingIn
    | RightTopNoteFadingOut
    | StationaryRightBottomNote
    | RightBottomNoteFadingIn
    | RightBottomNoteFadingOut
    | StationaryBottomNote
    | BottomNoteFadingIn
    | BottomNoteFadingOut
    | StationaryLeftBottomNote
    | LeftBottomNoteFadingIn
    | LeftBottomNoteFadingOut
    | StationaryLeftTopNote
    | LeftTopNoteFadingIn
    | LeftTopNoteFadingOut


init =
    { time = 0
    , state = Start
    , topNoteTransparency = 0
    , rightTopNoteTransparency = 0
    , rightBottomNoteTransparency = 0
    , bottomNoteTransparency = 0
    , leftBottomNoteTransparency = 0
    , leftTopNoteTransparency = 0
    , aniTimeFade = 0
    , spiralTheta = 0
    , circleOfSixX = 0
    , circleOfSixY = 0
    , topNoteX = 1000
    , topNoteY = 1000
    , rightTopNoteX = 1000
    , rightTopNoteY = 1000
    , rightBottomNoteX = 1000
    , rightBottomNoteY = 1000
    , bottomNoteX = 1000
    , bottomNoteY = 1000
    , leftBottomNoteX = 1000
    , leftBottomNoteY = 1000
    , leftTopNoteX = 1000
    , leftTopNoteY = 1000
    , aniTime = 0
    , aniTimeExpand = (-1) * thirteenPercent
    }


update msg model =
    case msg of
        Tick t _ ->
            { model
                | time = t
                , aniTimeFade =
                    case model.state of
                        TopNoteFadingIn ->
                            model.aniTimeFade + 0.05

                        TopNoteFadingOut ->
                            model.aniTimeFade + 0.05

                        RightTopNoteFadingIn ->
                            model.aniTimeFade + 0.05

                        RightTopNoteFadingOut ->
                            model.aniTimeFade + 0.05

                        RightBottomNoteFadingIn ->
                            model.aniTimeFade + 0.05

                        RightBottomNoteFadingOut ->
                            model.aniTimeFade + 0.05

                        BottomNoteFadingIn ->
                            model.aniTimeFade + 0.05

                        BottomNoteFadingOut ->
                            model.aniTimeFade + 0.05

                        LeftBottomNoteFadingIn ->
                            model.aniTimeFade + 0.05

                        LeftBottomNoteFadingOut ->
                            model.aniTimeFade + 0.05

                        LeftTopNoteFadingIn ->
                            model.aniTimeFade + 0.05

                        LeftTopNoteFadingOut ->
                            model.aniTimeFade + 0.05

                        _ ->
                            0
                , aniTime =
                    case model.state of
                        Start ->
                            0

                        ExpandingContacts ->
                            model.aniTime + 0.5

                        ClosingContacts ->
                            model.aniTime - 0.5

                        _ ->
                            animationTimeLength
                , aniTimeExpand =
                    case model.state of
                        Start ->
                            (-1) * thirteenPercent

                        ExpandingContacts ->
                            model.aniTimeExpand + (movementSpeed model.aniTime)

                        ClosingContacts ->
                            model.aniTimeExpand - (movementSpeed model.aniTime)

                        _ ->
                            animationTimeLength - thirteenPercent
                , state = updateState model
                , topNoteTransparency = updateTopNoteTransparency model
                , rightTopNoteTransparency = updateRightTopNoteTransparency model
                , rightBottomNoteTransparency = updateRightBottomNoteTransparency model
                , bottomNoteTransparency = updateBottomNoteTransparency model
                , leftBottomNoteTransparency = updateLeftBottomNoteTransparency model
                , leftTopNoteTransparency = updateLeftTopNoteTransparency model
                , topNoteX =
                    case model.state of
                        TopNoteFadingIn ->
                            0

                        TopNoteFadingOut ->
                            0

                        StationaryTopNote ->
                            0

                        _ ->
                            1000
                , topNoteY =
                    case model.state of
                        TopNoteFadingIn ->
                            0

                        TopNoteFadingOut ->
                            0

                        StationaryTopNote ->
                            0

                        _ ->
                            1000
                , rightTopNoteX =
                    case model.state of
                        RightTopNoteFadingIn ->
                            0

                        RightTopNoteFadingOut ->
                            0

                        StationaryRightTopNote ->
                            0

                        _ ->
                            1000
                , rightTopNoteY =
                    case model.state of
                        RightTopNoteFadingIn ->
                            0

                        RightTopNoteFadingOut ->
                            0

                        StationaryRightTopNote ->
                            0

                        _ ->
                            1000
                , rightBottomNoteX =
                    case model.state of
                        RightBottomNoteFadingIn ->
                            0

                        RightBottomNoteFadingOut ->
                            0

                        StationaryRightBottomNote ->
                            0

                        _ ->
                            1000
                , rightBottomNoteY =
                    case model.state of
                        RightBottomNoteFadingIn ->
                            0

                        RightBottomNoteFadingOut ->
                            0

                        StationaryRightBottomNote ->
                            0

                        _ ->
                            1000
                , bottomNoteX =
                    case model.state of
                        BottomNoteFadingIn ->
                            0

                        BottomNoteFadingOut ->
                            0

                        StationaryBottomNote ->
                            0

                        _ ->
                            1000
                , bottomNoteY =
                    case model.state of
                        BottomNoteFadingIn ->
                            0

                        BottomNoteFadingOut ->
                            0

                        StationaryBottomNote ->
                            0

                        _ ->
                            1000
                , leftBottomNoteX =
                    case model.state of
                        LeftBottomNoteFadingIn ->
                            0

                        LeftBottomNoteFadingOut ->
                            0

                        StationaryLeftBottomNote ->
                            0

                        _ ->
                            1000
                , leftBottomNoteY =
                    case model.state of
                        LeftBottomNoteFadingIn ->
                            0

                        LeftBottomNoteFadingOut ->
                            0

                        StationaryLeftBottomNote ->
                            0

                        _ ->
                            1000
                , leftTopNoteX =
                    case model.state of
                        LeftTopNoteFadingIn ->
                            0

                        LeftTopNoteFadingOut ->
                            0

                        StationaryLeftTopNote ->
                            0

                        _ ->
                            1000
                , leftTopNoteY =
                    case model.state of
                        LeftTopNoteFadingIn ->
                            0

                        LeftTopNoteFadingOut ->
                            0

                        StationaryLeftTopNote ->
                            0

                        _ ->
                            1000

                {- UNECESSARY FOR NOW
                   --, circleOfSixX = updateExpandingContactsX model
                   --, circleOfSixY = updateExpandingContactsY model
                -}
            }

        ContactsPress ->
            { model
                | state = contactsPress model.state
            }

        ButtonPressTop ->
            { model
                | state = buttonPressTop model.state
            }

        ButtonPressRightTop ->
            { model
                | state = buttonPressRightTop model.state
            }

        ButtonPressRightBottom ->
            { model
                | state = buttonPressRightBottom model.state
            }

        ButtonPressBottom ->
            { model
                | state = buttonPressBottom model.state
            }

        ButtonPressLeftBottom ->
            { model
                | state = buttonPressLeftBottom model.state
            }

        ButtonPressLeftTop ->
            { model
                | state = buttonPressLeftTop model.state
            }

        XPressTop ->
            { model
                | state = xPressTop model.state
            }

        XPressRightTop ->
            { model
                | state = xPressRightTop model.state
            }

        XPressRightBottom ->
            { model
                | state = xPressRightBottom model.state
            }

        XPressBottom ->
            { model
                | state = xPressBottom model.state
            }

        XPressLeftBottom ->
            { model
                | state = xPressLeftBottom model.state
            }

        XPressLeftTop ->
            { model
                | state = xPressLeftTop model.state
            }


updateState model =
    case model.state of
        ExpandingContacts ->
            if model.aniTimeExpand >= (animationTimeLength - thirteenPercent) then
                CircleOfSixOpen
            else
                ExpandingContacts

        ClosingContacts ->
            if model.aniTimeExpand <= (0 - thirteenPercent) then
                Start
            else
                ClosingContacts

        TopNoteFadingIn ->
            if model.aniTimeFade >= 0.4 then
                StationaryTopNote
            else
                TopNoteFadingIn

        TopNoteFadingOut ->
            if model.aniTimeFade >= 0.4 then
                CircleOfSixOpen
            else
                TopNoteFadingOut

        RightTopNoteFadingIn ->
            if model.aniTimeFade >= 0.4 then
                StationaryRightTopNote
            else
                RightTopNoteFadingIn

        RightTopNoteFadingOut ->
            if model.aniTimeFade >= 0.4 then
                CircleOfSixOpen
            else
                RightTopNoteFadingOut

        RightBottomNoteFadingIn ->
            if model.aniTimeFade >= 0.4 then
                StationaryRightBottomNote
            else
                RightBottomNoteFadingIn

        RightBottomNoteFadingOut ->
            if model.aniTimeFade >= 0.4 then
                CircleOfSixOpen
            else
                RightBottomNoteFadingOut

        BottomNoteFadingIn ->
            if model.aniTimeFade >= 0.4 then
                StationaryBottomNote
            else
                BottomNoteFadingIn

        BottomNoteFadingOut ->
            if model.aniTimeFade >= 0.4 then
                CircleOfSixOpen
            else
                BottomNoteFadingOut

        LeftBottomNoteFadingIn ->
            if model.aniTimeFade >= 0.4 then
                StationaryLeftBottomNote
            else
                LeftBottomNoteFadingIn

        LeftBottomNoteFadingOut ->
            if model.aniTimeFade >= 0.4 then
                CircleOfSixOpen
            else
                LeftBottomNoteFadingOut

        LeftTopNoteFadingIn ->
            if model.aniTimeFade >= 0.4 then
                StationaryLeftTopNote
            else
                LeftTopNoteFadingIn

        LeftTopNoteFadingOut ->
            if model.aniTimeFade >= 0.4 then
                CircleOfSixOpen
            else
                LeftTopNoteFadingOut

        state ->
            state



-- UPDATE


type Msg m
    = Tick Float GetKeyState
    | ContactsPress
    | ButtonPressTop
    | XPressTop
    | ButtonPressRightTop
    | XPressRightTop
    | ButtonPressRightBottom
    | XPressRightBottom
    | ButtonPressBottom
    | XPressBottom
    | ButtonPressLeftBottom
    | XPressLeftBottom
    | ButtonPressLeftTop
    | XPressLeftTop


contactsPress oldState =
    case oldState of
        Start ->
            ExpandingContacts

        CircleOfSixOpen ->
            ClosingContacts

        state ->
            state


updateTopNoteTransparency model =
    case model.state of
        TopNoteFadingIn ->
            model.topNoteTransparency + 0.1

        TopNoteFadingOut ->
            model.topNoteTransparency - 0.1

        _ ->
            model.topNoteTransparency


updateRightTopNoteTransparency model =
    case model.state of
        RightTopNoteFadingIn ->
            model.rightTopNoteTransparency + 0.1

        RightTopNoteFadingOut ->
            model.rightTopNoteTransparency - 0.1

        _ ->
            model.rightTopNoteTransparency


updateRightBottomNoteTransparency model =
    case model.state of
        RightBottomNoteFadingIn ->
            model.rightBottomNoteTransparency + 0.1

        RightBottomNoteFadingOut ->
            model.rightBottomNoteTransparency - 0.1

        _ ->
            model.rightBottomNoteTransparency


updateBottomNoteTransparency model =
    case model.state of
        BottomNoteFadingIn ->
            model.bottomNoteTransparency + 0.1

        BottomNoteFadingOut ->
            model.bottomNoteTransparency - 0.1

        _ ->
            model.bottomNoteTransparency


updateLeftBottomNoteTransparency model =
    case model.state of
        LeftBottomNoteFadingIn ->
            model.leftBottomNoteTransparency + 0.1

        LeftBottomNoteFadingOut ->
            model.leftBottomNoteTransparency - 0.1

        _ ->
            model.leftBottomNoteTransparency


updateLeftTopNoteTransparency model =
    case model.state of
        LeftTopNoteFadingIn ->
            model.leftTopNoteTransparency + 0.1

        LeftTopNoteFadingOut ->
            model.leftTopNoteTransparency - 0.1

        _ ->
            model.leftTopNoteTransparency


buttonPressTop oldState =
    case oldState of
        CircleOfSixOpen ->
            TopNoteFadingIn

        StationaryTopNote ->
            TopNoteFadingOut

        state ->
            state


buttonPressRightTop oldState =
    case oldState of
        CircleOfSixOpen ->
            RightTopNoteFadingIn

        StationaryRightTopNote ->
            RightTopNoteFadingOut

        state ->
            state


buttonPressRightBottom oldState =
    case oldState of
        CircleOfSixOpen ->
            RightBottomNoteFadingIn

        StationaryRightBottomNote ->
            RightBottomNoteFadingOut

        state ->
            state


buttonPressBottom oldState =
    case oldState of
        CircleOfSixOpen ->
            BottomNoteFadingIn

        StationaryBottomNote ->
            BottomNoteFadingOut

        state ->
            state


buttonPressLeftBottom oldState =
    case oldState of
        CircleOfSixOpen ->
            LeftBottomNoteFadingIn

        StationaryLeftBottomNote ->
            LeftBottomNoteFadingOut

        state ->
            state


buttonPressLeftTop oldState =
    case oldState of
        CircleOfSixOpen ->
            LeftTopNoteFadingIn

        StationaryLeftTopNote ->
            LeftTopNoteFadingOut

        state ->
            state


xPressTop oldState =
    case oldState of
        StationaryTopNote ->
            TopNoteFadingOut

        state ->
            state


xPressRightTop oldState =
    case oldState of
        StationaryRightTopNote ->
            RightTopNoteFadingOut

        state ->
            state


xPressRightBottom oldState =
    case oldState of
        StationaryRightBottomNote ->
            RightBottomNoteFadingOut

        state ->
            state


xPressBottom oldState =
    case oldState of
        StationaryBottomNote ->
            BottomNoteFadingOut

        state ->
            state


xPressLeftBottom oldState =
    case oldState of
        StationaryLeftBottomNote ->
            LeftBottomNoteFadingOut

        state ->
            state


xPressLeftTop oldState =
    case oldState of
        StationaryLeftTopNote ->
            LeftTopNoteFadingOut

        state ->
            state



-- VIEW
-- CONTACT ANIMATION CONTROLS
--    Allows control of the contact bubbles animation
-- Original scaled size of each


scaledSize =
    1



-- Time between each piece popping out


timeShift =
    0.1



-- Size to pop out to


enlargedSize =
    1.05


stringOfAniTime x =
    Basics.toString x



--Functions to animate the movement
-- NEW


contactPoint1 =
    3


movementSpeed ani =
    if ani < 0.425 * animationTimeLength then
        (Ease.inOutSine (ani / (fiftyPercent)))
    else
        (Ease.inOutSine (1 - (ani - fiftyPercent) / fiftyPercent))


theta =
    1.59



-- IMPORTANT FOR SPEED OF THE ANIMATION
-- in seconds*10


animationTimeLength =
    20


circleMultiplyer =
    246


circleOfSixRadius =
    25


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


backgroundDebugScreen model =
    if options.debug then
        group
            [ -- Test
              text ("Change in time step (Speed) " ++ (stringOfAniTime (movementSpeed model.aniTime)))
                |> filled black
                |> move ( 30 - 150, -10 + 60 )
            , text ("Time " ++ (stringOfAniTime model.time))
                |> filled black
                |> move ( 30 - 150, -70 + 60 )
            , text ("Expand Time " ++ (stringOfAniTime model.aniTimeExpand))
                |> filled black
                |> move ( 30 - 150, -50 + 60 )
            , text ("Constant Time " ++ (stringOfAniTime model.aniTime))
                |> filled black
                |> move ( 30 - 150, -30 + 60 )
            , text ("Scale Time " ++ (stringOfAniTime (max (sin (model.time) * enlargedSize) scaledSize)))
                |> filled black
                |> move ( 30 - 150, -90 + 60 )
            , text ("Scale Time " ++ toString (model.state))
                |> filled black
                |> move ( 30 - 150, -110 + 60 )
            , text ("Animation Time " ++ (stringOfAniTime model.aniTimeFade))
                |> filled black
                |> move ( 30, -50 )
            ]
    else
        group []


view model =
    [ --backgroundDebugScreen model
      --,
      group
        [ calendarGrid

        -- Center Face
        , group
            [ group
                [ circle 11
                    |> filled backgroundPerson
                , face4
                , myCircleOutline
                    |> makeTransparent 0
                ]
                |> move ( 0, -20 )
                |> makeTransparent (middleTransparency model.aniTimeExpand)

            -- Top
            , group
                [ circle 11
                    |> filled backgroundPerson
                , face1
                , myCircleOutline
                    |> makeTransparent model.topNoteTransparency
                ]
                |> notifyTap ButtonPressTop
                |> move ( circleX 0 model.aniTimeExpand, (circleY 0 model.aniTimeExpand) - 20 )
                --speed, % of the wheel
                |> scale (scaleContacts model 5)

            -- Right Top
            , group
                [ circle 11
                    |> filled backgroundPerson
                , face2
                , myCircleOutline
                    |> makeTransparent model.rightTopNoteTransparency
                ]
                |> notifyTap ButtonPressRightTop
                |> move ( circleX 1 model.aniTimeExpand, (circleY 1 model.aniTimeExpand) - 20 )
                |> scale (scaleContacts model 4)

            -- Right Bottom
            , group
                [ circle 11
                    |> filled backgroundPerson
                , face3
                , myCircleOutline
                    |> makeTransparent model.rightBottomNoteTransparency
                ]
                |> notifyTap ButtonPressRightBottom
                |> move ( circleX 2 model.aniTimeExpand, (circleY 2 model.aniTimeExpand) - 20 )
                |> scale (scaleContacts model 3)

            -- Bottom
            , group
                [ circle 11
                    |> filled backgroundPerson
                , face4
                , myCircleOutline
                    |> makeTransparent model.bottomNoteTransparency
                ]
                |> notifyTap ButtonPressBottom
                |> move ( circleX 3 model.aniTimeExpand, (circleY 3 model.aniTimeExpand) - 20 )
                |> scale (scaleContacts model 2)

            -- Left Bottom
            , group
                [ circle 11
                    |> filled backgroundPerson
                , face5
                , myCircleOutline
                    |> makeTransparent model.leftBottomNoteTransparency
                ]
                |> notifyTap ButtonPressLeftBottom
                |> move ( circleX 4 model.aniTimeExpand, (circleY 4 model.aniTimeExpand) - 20 )
                |> scale (scaleContacts model 1)

            -- Left Top
            , group
                [ circle 11
                    |> filled backgroundPerson
                , face6
                , myCircleOutline
                    |> makeTransparent model.leftTopNoteTransparency
                ]
                |> notifyTap ButtonPressLeftTop
                |> move ( circleX 5 model.aniTimeExpand, (circleY 5 model.aniTimeExpand) - 20 )
                |> scale (scaleContacts model 0)

            -- Top Note
            , group
                [ roundedRect 80 20 5
                    |> filled noteColour
                    |> move ( 0, 35 )
                , text text1
                    |> filled black
                    |> scale 0.45
                    |> move ( -35, 38 )
                , closeBoxSymbol
                    |> rotate (degrees 45)
                    |> move ( 35, 40 )
                    |> notifyTap XPressTop
                ]
                |> makeTransparent model.topNoteTransparency
                |> move ( model.topNoteX, model.topNoteY )

            -- Right Top Note
            , group
                [ roundedRect 80 20 5
                    |> filled noteColour
                    |> move ( 0, 35 )
                , text text2
                    |> filled black
                    |> scale 0.45
                    |> move ( -35, 38 )
                , closeBoxSymbol
                    |> rotate (degrees 45)
                    |> move ( 35, 40 )
                    |> notifyTap XPressRightTop
                ]
                |> makeTransparent model.rightTopNoteTransparency
                |> move ( model.rightTopNoteX, model.rightTopNoteY )

            -- Right Bottom Note
            , group
                [ roundedRect 80 20 5
                    |> filled noteColour
                    |> move ( 0, 35 )
                , text text3
                    |> filled black
                    |> scale 0.45
                    |> move ( -35, 38 )
                , closeBoxSymbol
                    |> rotate (degrees 45)
                    |> move ( 35, 40 )
                    |> notifyTap XPressRightBottom
                ]
                |> makeTransparent model.rightBottomNoteTransparency
                |> move ( model.rightBottomNoteX, model.rightBottomNoteY )

            -- Bottom Note
            , group
                [ roundedRect 80 20 5
                    |> filled noteColour
                    |> move ( 0, 35 )
                , text text4
                    |> filled black
                    |> scale 0.45
                    |> move ( -35, 38 )
                , closeBoxSymbol
                    |> rotate (degrees 45)
                    |> move ( 35, 40 )
                    |> notifyTap XPressBottom
                ]
                |> makeTransparent model.bottomNoteTransparency
                |> move ( model.bottomNoteX, model.bottomNoteY )

            -- Left Bottom Note
            , group
                [ roundedRect 80 20 5
                    |> filled noteColour
                    |> move ( 0, 35 )
                , text text5
                    |> filled black
                    |> scale 0.45
                    |> move ( -35, 38 )
                , closeBoxSymbol
                    |> rotate (degrees 45)
                    |> move ( 35, 40 )
                    |> notifyTap XPressLeftBottom
                ]
                |> makeTransparent model.leftBottomNoteTransparency
                |> move ( model.leftBottomNoteX, model.leftBottomNoteY )

            -- Left Top Note
            , group
                [ roundedRect 80 20 5
                    |> filled noteColour
                    |> move ( 0, 35 )
                , text text6
                    |> filled black
                    |> scale 0.45
                    |> move ( -35, 38 )
                , group
                    [ circle 10 |> filled white |> move ( 0, 5 )
                    , curve ( 0, 0 ) [ Pull ( 4, 8.5 ) ( -5, 10 ) ] |> outlined (solid 4) darkRed |> rotate (degrees -10)
                    , circle 2.5 |> filled red |> scaleX 0.8 |> move ( -1.25, 9.75 )
                    , circle 3 |> filled red |> scaleX 0.8
                    , circle 3 |> filled red |> scaleX 0.8 |> move ( -2, 10 )
                    , circle 2.25 |> filled darkRed |> scaleX 0.8
                    , circle 2.25 |> filled darkRed |> scaleX 0.8 |> move ( -2, 10 )
                    , circle 0.5 |> filled black |> scaleX 0.8 |> move ( -1, 0 )
                    , circle 0.5 |> filled black |> scaleX 0.8 |> move ( -0.25, 1 )
                    , circle 0.5 |> filled black |> scaleX 0.8 |> move ( -0.25, -1 )
                    , group
                        [ circle 0.5 |> filled black |> scaleX 0.8 |> move ( -1, 0 )
                        , circle 0.5 |> filled black |> scaleX 0.8 |> move ( -0.25, 1 )
                        , circle 0.5 |> filled black |> scaleX 0.8 |> move ( -0.25, -1 )
                        ]
                        |> move ( -2, 10 )
                    ]
                    |> scale 2
                    |> rotate (degrees -5)
                    --, roundedRect 10 3 2 |> filled black |> rotate (degrees -20) |> move (-14,23)
                    --, roundedRect 9 2.5 2 |> filled black |> move (-13,18)
                    --, roundedRect 8 2.25 2 |> filled black |> rotate (degrees 20) |> move (-12,14)
                    |> scale 0.2
                    |> move ( 0, 35 )
                    |> GraphicSVG.addHyperlink "tel:905-933-2082"
                , closeBoxSymbol
                    |> rotate (degrees 45)
                    |> move ( 35, 40 )
                    |> notifyTap XPressLeftTop
                ]
                |> makeTransparent model.leftTopNoteTransparency
                |> move ( model.leftTopNoteX, model.leftTopNoteY )

            -- Contacts Button Face
            , group
                [ circle 11
                    |> filled backgroundPerson
                , face4
                , myCircleOutline
                    |> makeTransparent (middleTransparency model.aniTimeExpand)
                ]
                |> move ( -25 - circleOfSixRadius, -75 + startingY - 8 )
                |> notifyTap ContactsPress
                |> scale 0.5
            ]
            |> scale 6.5
            |> move ( 0, 8 )
        ]
        |> scale 0.4

    --, graphPaper 10
    ]



-- END OF MODEL
-- DEFINING VARIABLES


positive x =
    if x > 0 then
        x
    else
        0


text1 =
    "Julie"


text2 =
    "Sam"


text3 =
    "Jane"


text4 =
    "Jo"


text5 =
    "Grandpa"


text6 =
    "Grandma"


profilePerson =
    myCircle


trapezoid =
    polygon [ ( 0, 0 ), ( 2, 5 ), ( 6, 5 ), ( 8, 0 ) ]


settingsIcon =
    group
        [ circle 10 |> filled charcoal
        , circle 4.5 |> filled white
        , trapezoid |> filled charcoal |> move ( -5, 6.5 ) |> scale 1.25
        , trapezoid |> filled charcoal |> move ( -6.5, -5 ) |> scale 1.25 |> rotate (degrees 90)
        , trapezoid |> filled charcoal |> move ( 5, -6.5 ) |> scale 1.25 |> rotate (degrees 180)
        , trapezoid |> filled charcoal |> move ( 6.5, 5 ) |> scale 1.25 |> rotate (degrees -90)
        , trapezoid |> filled charcoal |> move ( -7.5, 1 ) |> scale 1.25 |> rotate (degrees 45)
        , trapezoid |> filled charcoal |> move ( 1, 7.5 ) |> scale 1.25 |> rotate (degrees -45)
        , trapezoid |> filled charcoal |> move ( 7.5, -1 ) |> scale 1.25 |> rotate (degrees -135)
        , trapezoid |> filled charcoal |> move ( -1, -7.5 ) |> scale 1.25 |> rotate (degrees 135)
        ]
        |> scale (faceScale * 6)
        |> rotate (degrees 45)


textBox1 =
    group
        [ roundedRect 80 20 5
            |> filled yellow
        , text "Journal Entry:"
            |> filled black
            |> scale 0.45
            |> move ( -35, 0 )
        , closeBoxSymbol
            |> rotate (degrees 45)
            |> move ( 35, 5 )
        ]


closeBoxSymbol =
    group
        [ circle 3
            |> filled noteColour
        , rect 1 5
            |> filled black
        , rect 5 1
            |> filled black
        ]


backgroundPerson =
    rgb 200 80 75


contactPersonColour =
    rgb 67 157 145


noteColour =
    rgb 250 166 106


myCircleOutline =
    circle 11
        |> outlined (solid 1) orange


myCircle =
    group
        [ circle 10
            |> filled backgroundPerson

        -- Main body
        , curve ( 0, 0 ) [ Pull ( 5, 10 ) ( 10, 0 ) ]
            |> filled contactPersonColour
            |> move ( -5, -8 )
        , -- Head
          circle 4
            |> filled contactPersonColour
            |> move ( 0, 0 )
        , -- Left Side
          circle 2
            |> filled contactPersonColour
            |> move ( 0, -8.1 )
        , circle 2
            |> filled contactPersonColour
            |> move ( -1, -7.9 )
        , circle 2
            |> filled contactPersonColour
            |> move ( -2, -7.8 )
        , circle 1
            |> filled contactPersonColour
            |> move ( -3.8, -8.1 )
        , circle 0.5
            |> filled contactPersonColour
            |> move ( -4.5, -8.1 )
        , circle 0.3
            |> filled contactPersonColour
            |> move ( -4.9, -8.4 )
        , circle 0.3
            |> filled contactPersonColour
            |> move ( -4.7, -8.6 )
        , circle 0.5
            |> filled contactPersonColour
            |> move ( -3.5, -8.8 )
        , -- Right Side
          circle 2
            |> filled contactPersonColour
            |> move ( 0, -8.1 )
        , circle 2
            |> filled contactPersonColour
            |> move ( 1, -7.9 )
        , circle 2
            |> filled contactPersonColour
            |> move ( 2, -7.8 )
        , circle 1
            |> filled contactPersonColour
            |> move ( 3.8, -8.1 )
        , circle 0.5
            |> filled contactPersonColour
            |> move ( 4.5, -8.1 )
        , circle 0.3
            |> filled contactPersonColour
            |> move ( 4.9, -8.4 )
        , circle 0.3
            |> filled contactPersonColour
            |> move ( 4.7, -8.6 )
        , circle 0.5
            |> filled contactPersonColour
            |> move ( 3.5, -8.8 )
        ]



--BIYA'S WONDERFUL FACES


faceScale =
    0.08



--CIRCLE OF 6 FACES


face1 =
    group
        [ --THIS IS THE WHOLE FACE
          hair4b

        --EARS
        , group
            [ oval 24 44 |> filled black
            , oval 20 40 |> filled skin8
            ]
            |> move ( -70, 0 )
        , group
            [ oval 24 44 |> filled black
            , oval 20 40 |> filled skin8
            ]
            |> move ( 70, 0 )

        --The oval that makes up the face
        , oval 143 156
            |> filled black
            |> move ( 0, -7 )
        , oval 140 153
            |> filled skin8
            |> move ( 0, -7 )
        , freckles |> move ( -40, -20 )
        , freckles |> move ( 40, -20 )

        -- , blush |> move (-40,-20)
        --, blush |> move (40,-20)
        , group
            [ filled black (circle 20) |> move ( -27, 0 ) --Left eye
            , filled white (circle 18) |> move ( -27, 0 )
            , circle 15
                |> filled (rgb 75 114 72)
                |> move ( -24, 0 )
            , filled black (circle 12) |> move ( -21, 2 )
            , filled white (circle 6) |> move ( -18, 3 )
            , filled black (circle 20) |> move ( 27, 0 ) --Right eye
            , filled white (circle 18) |> move ( 27, 0 )
            , circle 15
                |> filled (rgb 75 114 72)
                |> move ( 30, 0 )
            , filled black (circle 12) |> move ( 32, 2 )
            , filled white (circle 6) |> move ( 35, 3 )
            ]
        , hair4f
            |> move ( 0, 60 )
        , curve ( -50, -20 ) [ Pull ( 0, -40 ) ( 50, -20 ) ]
            |> outlined (solid 3) red
            |> move ( 0, -40 )
            |> scale 0.4
        ]
        |> scale faceScale
        --Scale the whole face
        |> move ( 0, 1 )


face2 =
    group
        [ --THIS IS THE WHOLE FACE
          hair3b

        --EARS
        , group
            [ oval 24 44 |> filled black
            , oval 20 40 |> filled skin3
            ]
            |> move ( -70, 0 )
        , group
            [ oval 24 44 |> filled black
            , oval 20 40 |> filled skin3
            ]
            |> move ( 70, 0 )

        --The oval that makes up the face
        , oval 143 156
            |> filled black
            |> move ( 0, -7 )
        , oval 140 153
            |> filled skin3
            |> move ( 0, -7 )
        , freckles |> move ( -40, -20 )
        , freckles |> move ( 40, -20 )

        --, blush |> move (-40,-20)
        --, blush |> move (40,-20)
        , group
            [ filled black (circle 20) |> move ( -27, 0 ) --Left eye
            , filled white (circle 18) |> move ( -27, 0 )
            , circle 15
                |> filled (rgb 161 202 241)
                |> move ( -24, 0 )
            , filled black (circle 12) |> move ( -21, 2 )
            , filled white (circle 6) |> move ( -18, 3 )
            , filled black (circle 20) |> move ( 27, 0 ) --Right eye
            , filled white (circle 18) |> move ( 27, 0 )
            , circle 15
                |> filled (rgb 161 202 241)
                |> move ( 30, 0 )
            , filled black (circle 12) |> move ( 32, 2 )
            , filled white (circle 6) |> move ( 35, 3 )
            ]
        , hair3f
            |> move ( 0, 60 )
        , curve ( -50, -20 ) [ Pull ( 0, -40 ) ( 50, -20 ) ]
            |> outlined (solid 3) red
            |> move ( 0, -40 )
            |> scale 0.4
        ]
        |> scale faceScale
        --Scale the whole face
        |> move ( 0, 1 )


face3 =
    group
        [ --THIS IS THE WHOLE FACE
          hair2b

        --EARS
        , group
            [ oval 24 44 |> filled black
            , oval 20 40 |> filled skin9
            ]
            |> move ( -70, 0 )
        , group
            [ oval 24 44 |> filled black
            , oval 20 40 |> filled skin9
            ]
            |> move ( 70, 0 )

        --The oval that makes up the face
        , oval 143 156
            |> filled black
            |> move ( 0, -7 )
        , oval 140 153
            |> filled skin9
            |> move ( 0, -7 )
        , freckles |> move ( -40, -20 )
        , freckles |> move ( 40, -20 )

        --, blush |> move (-40,-20)
        --, blush |> move (40,-20)
        , group
            [ filled black (circle 20) |> move ( -27, 0 ) --Left eye
            , filled white (circle 18) |> move ( -27, 0 )
            , circle 15
                |> filled (rgb 75 114 72)
                |> move ( -24, 0 )
            , filled black (circle 12) |> move ( -21, 2 )
            , filled white (circle 6) |> move ( -18, 3 )
            , filled black (circle 20) |> move ( 27, 0 ) --Right eye
            , filled white (circle 18) |> move ( 27, 0 )
            , circle 15
                |> filled (rgb 75 114 72)
                |> move ( 30, 0 )
            , filled black (circle 12) |> move ( 32, 2 )
            , filled white (circle 6) |> move ( 35, 3 )
            ]
        , hair2f
            |> move ( 0, 60 )
        , curve ( -50, -20 ) [ Pull ( 0, -40 ) ( 50, -20 ) ]
            |> outlined (solid 3) red
            |> move ( 0, -40 )
            |> scale 0.4
        ]
        |> scale faceScale
        --Scale the whole face
        |> move ( 0, 1 )



{- face4 =
   group
       [ --THIS IS THE WHOLE FACE
         hair1b

       --EARS
       , group
           [ oval 24 44 |> filled black
           , oval 20 40 |> filled skin7
           ]
           |> move ( -70, 0 )
       , group
           [ oval 24 44 |> filled black
           , oval 20 40 |> filled skin7
           ]
           |> move ( 70, 0 )

       --The oval that makes up the face
       , oval 143 156
           |> filled black
           |> move ( 0, -7 )
       , oval 140 153
           |> filled skin7
           |> move ( 0, -7 )
       , freckles |> move ( -40, -20 )
       , freckles |> move ( 40, -20 )

       --, blush |> move (-40,-20)
       --, blush |> move (40,-20)
       , group
           [ filled black (circle 20) |> move ( -27, 0 ) --Left eye
           , filled white (circle 18) |> move ( -27, 0 )
           , circle 15
               |> filled (rgb 79 56 3)
               |> move ( -24, 0 )
           , filled black (circle 12) |> move ( -21, 2 )
           , filled white (circle 6) |> move ( -18, 3 )
           , filled black (circle 20) |> move ( 27, 0 ) --Right eye
           , filled white (circle 18) |> move ( 27, 0 )
           , circle 15
               |> filled (rgb 79 56 3)
               |> move ( 30, 0 )
           , filled black (circle 12) |> move ( 32, 2 )
           , filled white (circle 6) |> move ( 35, 3 )
           ]
       , hair1f
           |> move ( 0, 60 )
       , curve ( -50, -20 ) [ Pull ( 0, -40 ) ( 50, -20 ) ]
           |> outlined (solid 3) red
           |> move ( 0, -40 )
           |> scale 0.4
       ]
       |> scale faceScale
-}


face4 =
    AvatarCreator.view



--Scale the whole face


face6 =
    group
        [ --THIS IS THE WHOLE FACE
          hair6b

        --EARS
        , group
            [ oval 24 44 |> filled black
            , oval 20 40 |> filled skin6
            ]
            |> move ( -70, 0 )
        , group
            [ oval 24 44 |> filled black
            , oval 20 40 |> filled skin6
            ]
            |> move ( 70, 0 )

        --The oval that makes up the face
        , oval 143 156
            |> filled black
            |> move ( 0, -7 )
        , oval 140 153
            |> filled skin6
            |> move ( 0, -7 )
        , freckles |> move ( -40, -20 )
        , freckles |> move ( 40, -20 )

        -- , blush |> move (-40,-20)
        --, blush |> move (40,-20)
        , group
            [ filled black (circle 20) |> move ( -27, 0 ) --Left eye
            , filled white (circle 18) |> move ( -27, 0 )
            , circle 15
                |> filled (rgb 75 114 72)
                |> move ( -24, 0 )
            , filled black (circle 12) |> move ( -21, 2 )
            , filled white (circle 6) |> move ( -18, 3 )
            , filled black (circle 20) |> move ( 27, 0 ) --Right eye
            , filled white (circle 18) |> move ( 27, 0 )
            , circle 15
                |> filled (rgb 75 114 72)
                |> move ( 30, 0 )
            , filled black (circle 12) |> move ( 32, 2 )
            , filled white (circle 6) |> move ( 35, 3 )
            ]
        , hair6f
            |> move ( 0, 60 )
        , curve ( -50, -20 ) [ Pull ( 0, -40 ) ( 50, -20 ) ]
            |> outlined (solid 3) red
            |> move ( 0, -40 )
            |> scale 0.4
        ]
        |> scale faceScale
        --Scale the whole face
        |> move ( 0, 0 )


face5 =
    group
        [ --THIS IS THE WHOLE FACE
          hair5b

        --EARS
        , group
            [ oval 24 44 |> filled black
            , oval 20 40 |> filled skin9
            ]
            |> move ( -70, 0 )
        , group
            [ oval 24 44 |> filled black
            , oval 20 40 |> filled skin9
            ]
            |> move ( 70, 0 )

        --The oval that makes up the face
        , oval 143 156
            |> filled black
            |> move ( 0, -7 )
        , oval 140 153
            |> filled skin9
            |> move ( 0, -7 )
        , freckles |> move ( -40, -20 )
        , freckles |> move ( 40, -20 )

        -- , blush |> move (-40,-20)
        --, blush |> move (40,-20)
        , group
            [ filled black (circle 20) |> move ( -27, 0 ) --Left eye
            , filled white (circle 18) |> move ( -27, 0 )
            , circle 15
                |> filled (rgb 75 114 72)
                |> move ( -24, 0 )
            , filled black (circle 12) |> move ( -21, 2 )
            , filled white (circle 6) |> move ( -18, 3 )
            , filled black (circle 20) |> move ( 27, 0 ) --Right eye
            , filled white (circle 18) |> move ( 27, 0 )
            , circle 15
                |> filled (rgb 75 114 72)
                |> move ( 30, 0 )
            , filled black (circle 12) |> move ( 32, 2 )
            , filled white (circle 6) |> move ( 35, 3 )
            ]
        , hair5f
            |> move ( 0, 60 )
        , curve ( -50, -20 ) [ Pull ( 0, -40 ) ( 50, -20 ) ]
            |> outlined (solid 3) red
            |> move ( 0, -40 )
            |> scale 0.4
        ]
        |> scale faceScale
        --Scale the whole face
        |> move ( 0, 0 )



--GRANDPA HAIR


hair5f =
    group
        [ circle 19
            |> filled black
            |> move ( 55, -20 )
        , circle 14
            |> filled black
            |> move ( 62, -33 )
        , circle 14
            |> filled black
            |> move ( 42, -10 )
        , circle 19
            |> filled black
            |> move ( -55, -20 )
        , circle 14
            |> filled black
            |> move ( -62, -33 )
        , circle 14
            |> filled black
            |> move ( -42, -10 )
        , circle 17
            |> filled grey
            |> move ( 55, -20 )
        , circle 12
            |> filled grey
            |> move ( 62, -33 )
        , circle 12
            |> filled grey
            |> move ( 42, -10 )
        , circle 17
            |> filled grey
            |> move ( -55, -20 )
        , circle 12
            |> filled grey
            |> move ( -62, -33 )
        , circle 12
            |> filled grey
            |> move ( -42, -10 )
        ]


hair5b =
    group []


hair6f =
    group
        [ circle 19
            |> filled black
            |> move ( 55, -20 )
        , circle 14
            |> filled black
            |> move ( 62, -33 )
        , circle 18
            |> filled black
            |> move ( 42, -10 )
        , circle 20
            |> filled black
            |> move ( 30, -3 )
        , circle 18
            |> filled black
            |> move ( 15, 5 )
        , circle 20
            |> filled black
            |> move ( 0, 5 )
        , circle 19
            |> filled black
            |> move ( -55, -20 )
        , circle 14
            |> filled black
            |> move ( -62, -33 )
        , circle 18
            |> filled black
            |> move ( -42, -10 )
        , circle 20
            |> filled black
            |> move ( -30, -3 )
        , circle 18
            |> filled black
            |> move ( -15, 5 )
        , circle 17
            |> filled grey
            |> move ( 55, -20 )
        , circle 12
            |> filled grey
            |> move ( 62, -33 )
        , circle 16
            |> filled grey
            |> move ( 42, -10 )
        , circle 18
            |> filled grey
            |> move ( 30, -3 )
        , circle 16
            |> filled grey
            |> move ( 15, 5 )
        , circle 18
            |> filled grey
            |> move ( 0, 5 )
        , circle 17
            |> filled grey
            |> move ( -55, -20 )
        , circle 12
            |> filled grey
            |> move ( -62, -33 )
        , circle 16
            |> filled grey
            |> move ( -42, -10 )
        , circle 18
            |> filled grey
            |> move ( -30, -3 )
        , circle 16
            |> filled grey
            |> move ( -15, 5 )
        ]


hair6b =
    group
        [ circle 32 |> filled black |> move ( 0, 80 )
        , circle 30 |> filled grey |> move ( 0, 80 )
        ]



--All of this code is the stuff that makes the face. Play around and make new faces
--Use the Face1-4 templates to get started instead of building it from scratch
--Colours are at the very bottom
-----------------------------------------------------------------------------------


ear =
    group
        [ oval 24 44 |> filled black
        , oval 20 40 |> filled skin
        ]


eyebrow =
    group [ oval 37 4 |> filled black ]


blush =
    group [ circle 15 |> filled pinkie ]



--Curly dark hair


hair1f =
    group
        [ oval 60 40 |> filled black |> move ( 0, 5 )
        , oval 60 40 |> filled black |> move ( -30, 0 ) |> rotate (degrees 20)
        , oval 60 40 |> filled black |> move ( 30, 0 ) |> rotate (degrees 160)
        , oval 55 35 |> filled black |> move ( 55, -15 ) |> rotate (degrees 130)
        , oval 55 35 |> filled black |> move ( -55, -15 ) |> rotate (degrees 50)
        ]


hair1b =
    group []



--dreadlocks front


hair2f =
    group
        [ oval 17 27 |> filled black |> move ( -70, -35 ) |> rotate (degrees 60)
        , oval 17 27 |> filled black |> move ( 70, -35 ) |> rotate (degrees -60)
        , oval 12 22 |> filled hair2c |> move ( -70, -35 ) |> rotate (degrees 60)
        , oval 12 22 |> filled hair2c |> move ( 70, -35 ) |> rotate (degrees -60)
        , oval 20 30 |> filled black |> move ( 65, -25 ) |> rotate (degrees -60)
        , oval 15 25 |> filled hair2c |> move ( 65, -25 ) |> rotate (degrees -60)
        , oval 20 30 |> filled black |> move ( -65, -25 ) |> rotate (degrees 60)
        , oval 15 25 |> filled hair2c |> move ( -65, -25 ) |> rotate (degrees 60)
        , oval 20 35 |> filled black |> move ( -55, -15 ) |> rotate (degrees 50)
        , oval 15 30 |> filled hair2c |> move ( -55, -15 ) |> rotate (degrees 50)
        , oval 20 35 |> filled black |> move ( -45, -5 ) |> rotate (degrees 40)
        , oval 15 30 |> filled hair2c |> move ( -45, -5 ) |> rotate (degrees 40)
        , oval 20 35 |> filled black |> move ( 55, -15 ) |> rotate (degrees 130)
        , oval 15 30 |> filled hair2c |> move ( 55, -15 ) |> rotate (degrees 130)
        , oval 20 35 |> filled black |> move ( 45, -5 ) |> rotate (degrees -40)
        , oval 15 30 |> filled hair2c |> move ( 45, -5 ) |> rotate (degrees -40)
        , oval 20 40 |> filled black |> move ( 30, 0 ) |> rotate (degrees 160)
        , oval 15 35 |> filled hair2c |> move ( 30, 0 ) |> rotate (degrees 160)
        , oval 20 40 |> filled black |> move ( -30, 0 ) |> rotate (degrees 20)
        , oval 15 35 |> filled hair2c |> move ( -30, 0 ) |> rotate (degrees 20)
        , oval 20 40 |> filled black |> move ( -15, 5 ) |> rotate (degrees 15)
        , oval 15 35 |> filled hair2c |> move ( -15, 5 ) |> rotate (degrees 15)
        , oval 20 40 |> filled black |> move ( 15, 5 ) |> rotate (degrees -15)
        , oval 15 35 |> filled hair2c |> move ( 15, 5 ) |> rotate (degrees -15)
        , oval 23 43 |> filled black |> move ( 0, 7 )
        , oval 18 38 |> filled hair2c |> move ( 0, 7 )
        ]



--dreadlocks back


hair2b =
    group
        [ --Long hair
          roundedRect 18 75 10 |> filled black |> move ( -70, -50 )
        , roundedRect 13 70 10 |> filled hair2c |> move ( -70, -50 )
        , roundedRect 18 75 10 |> filled black |> move ( -55, -65 )
        , roundedRect 13 70 10 |> filled hair2c |> move ( -55, -65 )
        , roundedRect 18 75 10 |> filled black |> move ( 70, -50 )
        , roundedRect 13 70 10 |> filled hair2c |> move ( 70, -50 )
        , roundedRect 18 75 10 |> filled black |> move ( 55, -65 )
        , roundedRect 13 70 10 |> filled hair2c |> move ( 55, -65 )
        , roundedRect 18 75 10 |> filled black |> move ( -38, -75 ) |> rotate (degrees -5)
        , roundedRect 13 70 10 |> filled hair2c |> move ( -38, -75 ) |> rotate (degrees -5)
        , roundedRect 18 75 10 |> filled black |> move ( 38, -75 ) |> rotate (degrees 5)
        , roundedRect 13 70 10 |> filled hair2c |> move ( 38, -75 ) |> rotate (degrees 5)
        ]



--bangs front


hair3f =
    group
        [ bangsl
            |> scale 1.2
            |> move ( 11, 10 )
        , bangsl
            |> scale 1.2
            |> move ( -12, 10 )
            |> mirrorX
        , circle 5
            |> filled brown1
            |> move ( 0, 23.2 )

        --highlights
        , curve ( 0, 0 ) [ Pull ( 50, 10 ) ( 40, 50 ) ]
            |> outlined (solid 1) highlights
            |> move ( -75, -65 )
            |> scale 1.6
        , curve ( 0, 0 ) [ Pull ( 40, 10 ) ( 40, 50 ) ]
            |> outlined (solid 1) highlights
            |> move ( -72, -55 )
            |> scale 1.3
        , curve ( 0, 0 ) [ Pull ( 50, 10 ) ( 40, 50 ) ]
            |> outlined (solid 1) highlights
            |> move ( 75, -65 )
            |> scale 1.6
            |> mirrorX
        , curve ( 0, 0 ) [ Pull ( 40, 10 ) ( 40, 50 ) ]
            |> outlined (solid 1) highlights
            |> move ( 72, -55 )
            |> scale 1.3
            |> mirrorX
        ]



--Bangs back // just long brown hair


hair3b =
    group
        [ roundedRect 145 140 30
            |> filled brown1
            |> move ( 0, -50 )
        , rect 2 140
            |> filled highlights
            |> move ( -60, -30 )
        , rect 2 150
            |> filled highlights
            |> move ( -50, -40 )
        , rect 2 150
            |> filled highlights
            |> move ( -30, -40 )
        , rect 2 140
            |> filled highlights
            |> move ( 60, -30 )
        , rect 2 150
            |> filled highlights
            |> move ( 50, -40 )
        , rect 2 150
            |> filled highlights
            |> move ( 30, -40 )
        , rect 30 150
            |> filled backgroundPerson
            |> move ( 0, -50 )
        ]



--Part of hair3b


bangsl =
    group
        [ curve ( 0, 0 ) [ Pull ( 10, 50 ) ( 40, 50 ) ]
            |> filled brown1
            |> move ( -75, -65 )
            |> scale 1.6
        , curve ( 0, 0 ) [ Pull ( 50, 10 ) ( 40, 50 ) ]
            |> filled brown1
            |> move ( -75, -65 )
            |> scale 1.6
        ]



--Curly red hair front


hair4f =
    group
        [ oval 60 95
            |> filled black
            |> rotate (degrees -65)
            |> move ( -20, -15 )
        , oval 20 30
            |> filled black
            |> rotate (degrees 60)
            |> move ( 27, 0 )
        , oval 60 95
            |> filled redhair
            |> rotate (degrees -65)
            |> move ( -20, -13 )
        , oval 20 30
            |> filled redhair
            |> rotate (degrees 60)
            |> move ( 27, 2 )
        ]



--Curly red hair back


hair4b =
    group
        [ -- top of the head
          oval 154 74
            |> filled black
            |> move ( 0, 55 )
        , oval 54 74
            |> filled black
            |> move ( 55, 40 )
            |> rotate (degrees -40)
        , oval 54 74
            |> filled black
            |> move ( -55, 40 )
            |> rotate (degrees 40)
        , oval 74 184
            |> filled black
            |> rotate (degrees 5)
            |> move ( 40, -15 )
        , oval 44 74
            |> filled black
            |> move ( 80, 10 )
            |> rotate (degrees 5)
        , oval 74 184
            |> filled black
            |> rotate (degrees -5)
            |> move ( -40, -15 )
        , oval 44 74
            |> filled black
            |> move ( -80, 10 )
            |> rotate (degrees -5)
        , oval 74 184
            |> filled black
            |> rotate (degrees -25)
            |> move ( -50, 0 )

        --long
        , oval 74 184
            |> filled black
            |> rotate (degrees 25)
            |> move ( 50, 0 )
        , oval 150 70
            |> filled redhair
            |> move ( 0, 55 )
        , oval 70 180
            |> filled redhair
            |> rotate (degrees 25)
            |> move ( 50, 0 )

        --long
        , oval 70 180
            |> filled redhair
            |> rotate (degrees 5)
            |> move ( 40, -15 )

        --long
        , oval 40 70
            |> filled redhair
            |> move ( -80, 10 )
            |> rotate (degrees -5)
        , oval 70 180
            |> filled redhair
            |> rotate (degrees -25)
            |> move ( -50, 0 )

        --long
        , oval 70 180
            |> filled redhair
            |> rotate (degrees -5)
            |> move ( -40, -15 )

        --side head
        , oval 50 70
            |> filled redhair
            |> move ( 55, 40 )
            |> rotate (degrees -40)

        --side head
        , oval 50 70
            |> filled redhair
            |> move ( -55, 40 )
            |> rotate (degrees 40)
        , oval 40 70
            |> filled redhair
            |> move ( 80, 10 )
            |> rotate (degrees 5)
        ]


freckles =
    group
        [ circle 1.3 |> filled brown1
        , circle 1.3 |> filled brown1 |> move ( -4, -4 )
        , circle 1.3 |> filled brown1 |> move ( 4, -4 )
        ]



-- COLOURS


skin =
    skin10


skin1 =
    rgb 255 223 196


skin2 =
    rgb 238 206 179


skin3 =
    rgb 225 170 100


skin4 =
    rgb 229 184 143


skin5 =
    rgb 231 158 109


skin6 =
    rgb 219 144 101


skin7 =
    rgb 186 108 73


skin8 =
    rgb 198 120 86


skin9 =
    rgb 130 72 24



--


skin10 =
    rgb 146 106 45


skin11 =
    rgb 135 97 39


skin12 =
    rgb 124 80 26


skin13 =
    rgb 126 77 28


skin14 =
    rgb 137 78 26


skin15 =
    rgb 88 53 18


skin16 =
    rgb 62 43 19


lips =
    rgb 150 18 18


lbrown =
    rgb 79 56 3


redhair =
    rgb 67 0 0


redhair2 =
    rgb 60 0 0


brown1 =
    rgb 61 27 0


highlights =
    rgb 89 40 0


pinkie =
    rgba 255 158 158 0.2


hair2c =
    rgb 40 40 40


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


screen =
    let
        width =
            const.screen.width

        height =
            const.screen.height
    in
        { width = width
        , height = height
        , left = width / -2
        , bottom = height / -2
        , right = width / 2
        , top = height / 2
        , center = ( 0, 0 )
        }


type alias Cell =
    { x : Float
    , y : Float
    }



-- Calendar
{-
        July 2017
   Su Mo Tu We Th Fr Sa
                      1
    2  3  4  5  6  7  8
    9 10 11 12 13 14 15
   16 17 18 19 20 21 22
   23 24 25 26 27 28 29
   30 31
-}


cells : List (Shape a)
cells =
    let
        days =
            (List.map (toString >> text >> size 14 >> filled (rgb 180 180 180)) <| List.range 25 30)
                ++ (List.map (toString >> text >> size 15 >> filled charcoal) <| List.range 1 31)
                ++ (List.map (toString >> text >> size 14 >> filled (rgb 180 180 180)) <| List.range 1 5)

        xs =
            [ 0, 1, 2, 3, 4, 5, 6 ]

        ys =
            [ 0, 1, 2, 3, 4, 5 ]

        xys : List ( Float, Float )
        xys =
            List.concat <| List.map (\y -> List.map (\x -> ( x, y )) xs) ys

        -- _step is the distance between squares, _start is the coordinate of the first square
        -- _step is multiplied by an index value to automate placement of cells
        yStep =
            -(screen.height / 7)

        yStart =
            screen.height / 2 + yStep

        xStep =
            (screen.width / (7))

        xStart =
            -screen.width / 2 + 5
    in
        (List.map2 (\( x, y ) day -> day |> move ( xStart + x * xStep, yStart + y * yStep )) xys days)
            ++ (List.map2 (\x day -> text day |> size 9 |> centered |> filled black |> move ( xStart + (x + 0.5) * xStep - 5, -yStep * 3 - 40 ))
                    [ 0, 1, 2, 3, 4, 5, 6 ]
                    [ "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday" ]
               )
            ++ (List.map (\x -> rect 0.5 screen.height |> filled black |> move ( xStart + x * xStep - 5, 0 )) [ 1, 2, 3, 4, 5, 6 ])
            ++ (List.map (\y -> rect screen.width 0.5 |> filled black |> move ( 0, yStart + y * yStep + 15 )) [ 0, 1, 2, 3, 4, 5 ])


calendarGrid =
    group cells


background =
    rect screen.width screen.height
        |> filled const.misc.backgroundColor

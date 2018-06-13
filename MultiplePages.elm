module MultiplePages exposing (..)

{-
   Copyright 2017 Christopher Kumar Anand
   Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
   1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
   2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
   3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

import GraphicSVG exposing (..)
import SvgCalendar
import AvatarAssets exposing (..)
--import TetrisGame
import Graph
import AvatarCreator as A1
import Tetris
import Ease exposing (inOutSine)
import Html
import Html.Events
import Svg
import Svg.Attributes as SvgAtts
import Dict
import Date


{- main function -}


subscriptions model =
    Sub.none


options =
    { debug = False
    , debugTime = True }


main : CmdProgram Model Msg
--main : CmdProgram Model (Msg SvgCalendar.Msg Tetris.Msg A1.Msg)

main =
    cmdApp Tick windowResize
        { init = ( init, Cmd.none )
        , update = update
        , view = svgView
        , subscriptions = (\m -> Sub.none)
        }

type alias NoteData =
    { x : Float, y : Float, transparency : Float }

-- proof obligation:  must make sure this has enough elements
initAvatars = Dict.fromList [(1,A1.init),(2,A1.init),(3,A1.init),(4,A1.init),(5,A1.init),(6,A1.init),(0,A1.init)]  -- 0 is "me"

type alias Model =
    { page : Pages
    , width : Float
    , height : Float
    , calModel : SvgCalendar.Model
    , tetrisModel : Tetris.Model
    , graphModel : Graph.Model
    , avatars : Dict.Dict Int A1.Model
    , oneSat : Float
    , twoSat : Float
    , threeSat : Float
    , fourSat : Float
    , fiveSat : Float
    , sixSat : Float
    , sevenSat : Float
    , eightSat : Float
    , nineSat : Float
    , time : Float
    , debugTime : Float -- add some time so it is a different day
    , state : State
    , aniTimeFade : Float
    , spiralTheta : Float
    , topNote : { x : Float, y : Float, transparency : Float }
    , topRightNote : { x : Float, y : Float, transparency : Float }
    , bottomRightNote : { x : Float, y : Float, transparency : Float }
    , bottomNote : { x : Float, y : Float, transparency : Float }
    , bottomLeftNote : { x : Float, y : Float, transparency : Float }
    , topLeftNote : { x : Float, y : Float, transparency : Float }
    , verytopNote : { x : Float, y : Float, transparency : Float }
    , aniTime : Float
    , aniTimeExpand : Float
    , middleFaceX : Float
    , middleFaceY : Float
    , debug : String
    }

init : Model
init =
    { page = SvgCalendar
    , width = 512
    , height = 380
    , calModel = SvgCalendar.init
    , tetrisModel = Tetris.init
    , graphModel = Graph.init
    , avatars = initAvatars
    , oneSat = 1
    , twoSat = 0
    , threeSat = 0
    , fourSat = 0
    , fiveSat = 0
    , sixSat = 0
    , sevenSat = 0
    , eightSat = 0
    , nineSat = 0
    , time = 0
    , debugTime = 0
    , state = Start
    , aniTimeFade = 0
    , spiralTheta = 0
    , topNote = { x = 1000, y = 1000, transparency = 0 }
    , topRightNote = { x = 1000, y = 1000, transparency = 0 }
    , bottomRightNote = { x = 1000, y = 1000, transparency = 0 }
    , bottomNote = { x = 1000, y = 1000, transparency = 0 }
    , bottomLeftNote = { x = 1000, y = 1000, transparency = 0 }
    , topLeftNote = { x = 1000, y = 1000, transparency = 0 }
    , verytopNote = { x = 1000, y = 1000, transparency = 0 }
    , aniTime = 0
    , aniTimeExpand = (-1) * thirteenPercent
    , middleFaceX = 1000
    , middleFaceY = 1000
    , debug = ""
    }

windowResize : Float -> Float -> Model -> Model
windowResize w h model =
    { model | width = w
            , height = h
            , calModel = SvgCalendar.windowResize w h model.calModel
            , tetrisModel = Tetris.windowResize w h model.tetrisModel
    }

updateAvatar msg avatar = case avatar of
                                  Just mdl -> Just (A1.update msg mdl)
                                  Nothing -> Nothing


oneColour model =
    hsl (degrees 30) model.oneSat 0.85


oneAccent model =
    hsl (degrees 22) model.oneSat 0.6


twoColour model =
    hsl (degrees 285) model.twoSat 0.85


twoAccent model =
    hsl (degrees 280) model.twoSat 0.6


threeColour model =
    hsl (degrees 250) model.threeSat 0.9


threeAccent model =
    hsl (degrees 245) model.threeSat 0.6


type EnumCircle = C1 | C2 | C3 | C4 | C5 | C6 | CMe

type Pages
    = SvgCalendar
    | TetrisGame
    | Avatar Int -- proof obligation:  only [0..6] are allowed
    | GraphPage


type Position
    = Top
    | TopRight
    | TopLeft
    | Bottom
    | BottomLeft
    | BottomRight
    | Verytop


type State
    = Start
    | ExpandingContacts
    | ClosingContacts
    | CircleOfSixOpen
    | FadingIn Position
    | FadingOut Position
    | Stationary Position
    | DummyState


type Msg
    = Tick Float { width : Float, height : Float } GetKeyState
    | NewTextboxConent String
    | Msg1 SvgCalendar.Msg
    | Msg2 Tetris.Msg
    | Msg3 Graph.Msg
    | Msg Int A1.Msg -- proof obligation:  only [0..6] are allowed
    | Goto1
    | Goto2
    | Goto3
    | DebugAddHour
    | DebugAddDay
    | Goto Int -- proof obligation:  only [0..6]
    | MoveInRect ( Float, Float )
    | ContactsPress
    | ButtonPress Position
    | XPress Position


updateStateButtonPress oldState button =
    case oldState of
        CircleOfSixOpen ->
            FadingIn button

        Stationary x ->
            if x == button then
                FadingOut button
            else
                oldState

        state ->
            state


updateStateXButtonPress oldState button =
    case oldState of
        Stationary x ->
            if x == button then
                FadingOut button
            else
                oldState

        state ->
            state


updateTransparency model value =
    case model.state of
        FadingIn position ->
            value + 0.1

        FadingOut position ->
            value - 0.1

        _ ->
            value


updateNoteCoord model t =
    case model.state of
        FadingIn x ->
            if x == t then
                0
            else
                1000

        FadingOut x ->
            if x == t then
                0
            else
                1000

        Stationary x ->
            if x == t then
                0
            else
                1000

        otherwise ->
            1000


wrapCmdNone model =
    ( model, Cmd.none )


update : Msg  -> Model -> ( Model, Maybe (String -> Msg ), Cmd (Msg) )


--update : Msg SvgCalendar.Msg Tetris.Msg A1.Msg -> Model -> ( Model, Maybe (String -> Msg SvgCalendar.Msg Tetris.Msg A1.Msg), Cmd (Msg SvgCalendar.Msg Tetris.Msg A1.Msg) )
update msg model =
    let (newModel,mMsg) =
                case model.page of
                    SvgCalendar ->
                        case msg of
                            DebugAddHour ->
                                ({ model | debugTime = model.debugTime + 60*60*1000}, Nothing) -- note that this may cause text field to disappear

                            DebugAddDay ->
                                ({ model | debugTime = model.debugTime + 24*60*60*1000}, Nothing) -- note that this may cause text field to disappear

                            Tick f screen g ->
                              let (m1,maybeMsg) = SvgCalendar.update (SvgCalendar.Tick (f+model.debugTime) screen g) model.calModel
                              in
                                ({ model
                                    | calModel = m1
                                    , time = f
                                    , aniTimeFade =
                                        case model.state of
                                            FadingIn note ->
                                                model.aniTimeFade + 0.05

                                            FadingOut note ->
                                                model.aniTimeFade + 0.05

                                            otherwise ->
                                                0
                                    , aniTime =
                                        case model.state of
                                            Start ->
                                                0

                                            ExpandingContacts ->
                                                model.aniTime + 0.5

                                            ClosingContacts ->
                                                model.aniTime - 0.5

                                            otherwise ->
                                                animationTimeLength
                                    , aniTimeExpand =
                                        case model.state of
                                            Start ->
                                                (-1) * thirteenPercent

                                            ExpandingContacts ->
                                                model.aniTimeExpand + (movementSpeed model.aniTime)

                                            ClosingContacts ->
                                                model.aniTimeExpand - (movementSpeed model.aniTime)

                                            otherwise ->
                                                animationTimeLength - thirteenPercent
                                    , state = updateState model
                                    , topNote =
                                        { x = updateNoteCoord model Top
                                        , y = updateNoteCoord model Top
                                        , transparency = updateTransparency model model.topNote.transparency
                                        }
                                    , topRightNote =
                                        { x = updateNoteCoord model TopRight
                                        , y = updateNoteCoord model TopRight
                                        , transparency = updateTransparency model model.topRightNote.transparency
                                        }
                                    , topLeftNote =
                                        { x = updateNoteCoord model TopLeft
                                        , y = updateNoteCoord model TopLeft
                                        , transparency = updateTransparency model model.topLeftNote.transparency
                                        }
                                    ,   verytopNote =
                                        { x = updateNoteCoord model Verytop
                                        , y = updateNoteCoord model Verytop
                                        , transparency = updateTransparency model model.verytopNote.transparency
                                        }
                                    , bottomNote =
                                        { x = updateNoteCoord model Bottom
                                        , y = updateNoteCoord model Bottom
                                        , transparency = updateTransparency model model.bottomNote.transparency
                                        }
                                    , bottomRightNote =
                                        { x = updateNoteCoord model BottomRight
                                        , y = updateNoteCoord model BottomRight
                                        , transparency = updateTransparency model model.bottomRightNote.transparency
                                        }
                                    , bottomLeftNote =
                                        { x = updateNoteCoord model BottomLeft
                                        , y = updateNoteCoord model BottomLeft
                                        , transparency = updateTransparency model model.bottomLeftNote.transparency
                                        }
                                    , middleFaceX =
                                        case model.state of
                                            Start ->
                                                1000

                                            _ ->
                                                0
                                    , middleFaceY =
                                        case model.state of
                                            Start ->
                                                1000

                                            _ ->
                                                0
                                }
                                , Maybe.map ( (<<) Msg1 ) maybeMsg
                                )

                            Msg1 m1 ->
                              let (m2,maybeMsg) = SvgCalendar.update m1 model.calModel
                              in
                                ({ model | calModel = m2, debug = toString m1 }, Maybe.map ( (<<) Msg1 ) maybeMsg)

                            Msg2 _ ->
                                noTextBox model

                            Msg3 _ ->
                                noTextBox model

                            Msg _ _ ->
                                noTextBox model

                            Goto1 ->
                                noTextBox { model
                                    | page = SvgCalendar
                                    , twoSat = 0
                                    , threeSat = 0
                                    , fourSat = 0
                                    , fiveSat = 0
                                    , sixSat = 0
                                    , sevenSat = 0
                                    , eightSat = 0
                                    , nineSat = 0
                                    , state = ClosingContacts
                                    , topNote =
                                        { x = updateNoteCoord model Top
                                        , y = updateNoteCoord model Top
                                        , transparency = updateTransparency model 0
                                        }
                                    , topRightNote =
                                        { x = updateNoteCoord model TopRight
                                        , y = updateNoteCoord model TopRight
                                        , transparency = updateTransparency model 0
                                        }
                                    , topLeftNote =
                                        { x = updateNoteCoord model TopLeft
                                        , y = updateNoteCoord model TopLeft
                                        , transparency = updateTransparency model 0
                                        }
                                    ,   verytopNote =
                                        { x = updateNoteCoord model Verytop
                                        , y = updateNoteCoord model Verytop
                                        , transparency = updateTransparency model model.verytopNote.transparency
                                        }
                                    , bottomNote =
                                        { x = updateNoteCoord model Bottom
                                        , y = updateNoteCoord model Bottom
                                        , transparency = updateTransparency model 0
                                        }
                                    , bottomRightNote =
                                        { x = updateNoteCoord model BottomRight
                                        , y = updateNoteCoord model BottomRight
                                        , transparency = updateTransparency model 0
                                        }
                                    , bottomLeftNote =
                                        { x = updateNoteCoord model BottomLeft
                                        , y = updateNoteCoord model BottomLeft
                                        , transparency = updateTransparency model 0
                                        }
                                }

                            Goto2 ->
                                noTextBox { model
                                    | page = TetrisGame
                                    , oneSat = 0
                                    , threeSat = 0
                                    , fourSat = 0
                                    , fiveSat = 0
                                    , sixSat = 0
                                    , sevenSat = 0
                                    , eightSat = 0
                                    , nineSat = 0
                                    , state = ClosingContacts
                                    , topNote =
                                        { x = updateNoteCoord model Top
                                        , y = updateNoteCoord model Top
                                        , transparency = updateTransparency model 0
                                        }
                                    , topRightNote =
                                        { x = updateNoteCoord model TopRight
                                        , y = updateNoteCoord model TopRight
                                        , transparency = updateTransparency model 0
                                        }
                                    ,   verytopNote =
                                        { x = updateNoteCoord model Verytop
                                        , y = updateNoteCoord model Verytop
                                        , transparency = updateTransparency model model.verytopNote.transparency
                                        }
                                    , topLeftNote =
                                        { x = updateNoteCoord model TopLeft
                                        , y = updateNoteCoord model TopLeft
                                        , transparency = updateTransparency model 0
                                        }
                                    , bottomNote =
                                        { x = updateNoteCoord model Bottom
                                        , y = updateNoteCoord model Bottom
                                        , transparency = updateTransparency model 0
                                        }
                                    , bottomRightNote =
                                        { x = updateNoteCoord model BottomRight
                                        , y = updateNoteCoord model BottomRight
                                        , transparency = updateTransparency model 0
                                        }
                                    , bottomLeftNote =
                                        { x = updateNoteCoord model BottomLeft
                                        , y = updateNoteCoord model BottomLeft
                                        , transparency = updateTransparency model 0
                                        }
                                }

                            Goto3 ->
                                noTextBox { model
                                    | page = GraphPage
                                    , oneSat = 0
                                    , twoSat = 0
                                    , threeSat = 0
                                    , fiveSat = 0
                                    , sixSat = 0
                                    , sevenSat = 0
                                    , eightSat = 0
                                    , nineSat = 0
                                    , state = ClosingContacts
                                    , topNote =
                                        { x = updateNoteCoord model Top
                                        , y = updateNoteCoord model Top
                                        , transparency = updateTransparency model 0
                                        }
                                    , topRightNote =
                                        { x = updateNoteCoord model TopRight
                                        , y = updateNoteCoord model TopRight
                                        , transparency = updateTransparency model 0
                                        }
                                    , topLeftNote =
                                        { x = updateNoteCoord model TopLeft
                                        , y = updateNoteCoord model TopLeft
                                        , transparency = updateTransparency model 0
                                        }
                                    ,   verytopNote =
                                        { x = updateNoteCoord model Verytop
                                        , y = updateNoteCoord model Verytop
                                        , transparency = updateTransparency model model.verytopNote.transparency
                                        }
                                    , bottomNote =
                                        { x = updateNoteCoord model Bottom
                                        , y = updateNoteCoord model Bottom
                                        , transparency = updateTransparency model 0
                                        }
                                    , bottomRightNote =
                                        { x = updateNoteCoord model BottomRight
                                        , y = updateNoteCoord model BottomRight
                                        , transparency = updateTransparency model 0
                                        }
                                    , bottomLeftNote =
                                        { x = updateNoteCoord model BottomLeft
                                        , y = updateNoteCoord model BottomLeft
                                        , transparency = updateTransparency model 0
                                        }
                                }


                            Goto n ->
                                noTextBox { model
                                    | page = Avatar n
                                    , twoSat = 0
                                    , oneSat = 0
                                    , fourSat = 0
                                    , fiveSat = 0
                                    , sixSat = 0
                                    , sevenSat = 0
                                    , eightSat = 0
                                    , nineSat = 0
                                    , state = ClosingContacts
                                    , topNote =
                                        { x = updateNoteCoord model Top
                                        , y = updateNoteCoord model Top
                                        , transparency = updateTransparency model 0
                                        }
                                    , topRightNote =
                                        { x = updateNoteCoord model TopRight
                                        , y = updateNoteCoord model TopRight
                                        , transparency = updateTransparency model 0
                                        }
                                    , topLeftNote =
                                        { x = updateNoteCoord model TopLeft
                                        , y = updateNoteCoord model TopLeft
                                        , transparency = updateTransparency model 0
                                        }
                                    ,   verytopNote =
                                        { x = updateNoteCoord model Verytop
                                        , y = updateNoteCoord model Verytop
                                        , transparency = updateTransparency model model.verytopNote.transparency
                                        }
                                    , bottomNote =
                                        { x = updateNoteCoord model Bottom
                                        , y = updateNoteCoord model Bottom
                                        , transparency = updateTransparency model 0
                                        }
                                    , bottomRightNote =
                                        { x = updateNoteCoord model BottomRight
                                        , y = updateNoteCoord model BottomRight
                                        , transparency = updateTransparency model 0
                                        }
                                    , bottomLeftNote =
                                        { x = updateNoteCoord model BottomLeft
                                        , y = updateNoteCoord model BottomLeft
                                        , transparency = updateTransparency model 0
                                        }
                                }

                            ContactsPress ->
                                noTextBox { model
                                    | state = contactsPress model.state
                                }

                            ButtonPress button ->
                                noTextBox { model
                                    | state = updateStateButtonPress model.state button
                                }

                            XPress button ->
                                noTextBox { model
                                    | state = updateStateXButtonPress model.state button
                                }

                            _ ->
                                noTextBox model


                    TetrisGame ->
                        case msg of
                            Tick f screen g ->
                                noTextBox { model
                                    | tetrisModel = Tetris.update (Tetris.Tick f screen g) model.tetrisModel
                                    , time = f
                                    , aniTimeFade =
                                        case model.state of
                                            FadingIn note ->
                                                model.aniTimeFade + 0.05

                                            FadingOut note ->
                                                model.aniTimeFade + 0.05

                                            otherwise ->
                                                0
                                    , aniTime =
                                        case model.state of
                                            Start ->
                                                0

                                            ExpandingContacts ->
                                                model.aniTime + 0.5

                                            ClosingContacts ->
                                                model.aniTime - 0.5

                                            otherwise ->
                                                animationTimeLength
                                    , aniTimeExpand =
                                        case model.state of
                                            Start ->
                                                (-1) * thirteenPercent

                                            ExpandingContacts ->
                                                model.aniTimeExpand + (movementSpeed model.aniTime)

                                            ClosingContacts ->
                                                model.aniTimeExpand - (movementSpeed model.aniTime)

                                            otherwise ->
                                                animationTimeLength - thirteenPercent
                                    , state = updateState model
                                    , topNote =
                                        { x = updateNoteCoord model Top
                                        , y = updateNoteCoord model Top
                                        , transparency = updateTransparency model model.topNote.transparency
                                        }
                                    , topRightNote =
                                        { x = updateNoteCoord model TopRight
                                        , y = updateNoteCoord model TopRight
                                        , transparency = updateTransparency model model.topRightNote.transparency
                                        }
                                    , topLeftNote =
                                        { x = updateNoteCoord model TopLeft
                                        , y = updateNoteCoord model TopLeft
                                        , transparency = updateTransparency model model.topLeftNote.transparency
                                        }
                                    ,   verytopNote =
                                        { x = updateNoteCoord model Verytop
                                        , y = updateNoteCoord model Verytop
                                        , transparency = updateTransparency model model.verytopNote.transparency
                                        }
                                    , bottomNote =
                                        { x = updateNoteCoord model Bottom
                                        , y = updateNoteCoord model Bottom
                                        , transparency = updateTransparency model model.bottomNote.transparency
                                        }
                                    , bottomRightNote =
                                        { x = updateNoteCoord model BottomRight
                                        , y = updateNoteCoord model BottomRight
                                        , transparency = updateTransparency model model.bottomRightNote.transparency
                                        }
                                    , bottomLeftNote =
                                        { x = updateNoteCoord model BottomLeft
                                        , y = updateNoteCoord model BottomLeft
                                        , transparency = updateTransparency model model.bottomLeftNote.transparency
                                        }
                                    , middleFaceX =
                                        case model.state of
                                            Start ->
                                                1000

                                            _ ->
                                                0
                                    , middleFaceY =
                                        case model.state of
                                            Start ->
                                                1000

                                            _ ->
                                                0
                                }

                            Msg1 _ ->
                                noTextBox model

                            Msg2 m2 ->
                                noTextBox { model | tetrisModel = Tetris.update m2 model.tetrisModel }

                            Msg3 _ ->
                                noTextBox model

                            Msg _ _ ->
                                noTextBox model

                            Goto1 ->
                                noTextBox { model
                                    | page = SvgCalendar
                                    , twoSat = 0
                                    , threeSat = 0
                                    , fourSat = 0
                                    , fiveSat = 0
                                    , sixSat = 0
                                    , sevenSat = 0
                                    , eightSat = 0
                                    , nineSat = 0
                                    , state = ClosingContacts
                                    , topNote =
                                        { x = updateNoteCoord model Top
                                        , y = updateNoteCoord model Top
                                        , transparency = updateTransparency model 0
                                        }
                                    , topRightNote =
                                        { x = updateNoteCoord model TopRight
                                        , y = updateNoteCoord model TopRight
                                        , transparency = updateTransparency model 0
                                        }
                                    , topLeftNote =
                                        { x = updateNoteCoord model TopLeft
                                        , y = updateNoteCoord model TopLeft
                                        , transparency = updateTransparency model 0
                                        }
                                    ,   verytopNote =
                                        { x = updateNoteCoord model Verytop
                                        , y = updateNoteCoord model Verytop
                                        , transparency = updateTransparency model model.verytopNote.transparency
                                        }
                                    , bottomNote =
                                        { x = updateNoteCoord model Bottom
                                        , y = updateNoteCoord model Bottom
                                        , transparency = updateTransparency model 0
                                        }
                                    , bottomRightNote =
                                        { x = updateNoteCoord model BottomRight
                                        , y = updateNoteCoord model BottomRight
                                        , transparency = updateTransparency model 0
                                        }
                                    , bottomLeftNote =
                                        { x = updateNoteCoord model BottomLeft
                                        , y = updateNoteCoord model BottomLeft
                                        , transparency = updateTransparency model 0
                                        }
                                }

                            Goto2 ->
                                noTextBox { model
                                    | page = TetrisGame
                                    , oneSat = 0
                                    , threeSat = 0
                                    , fourSat = 0
                                    , fiveSat = 0
                                    , sixSat = 0
                                    , sevenSat = 0
                                    , eightSat = 0
                                    , nineSat = 0
                                    , state = ClosingContacts
                                    , topNote =
                                        { x = updateNoteCoord model Top
                                        , y = updateNoteCoord model Top
                                        , transparency = updateTransparency model 0
                                        }
                                    , topRightNote =
                                        { x = updateNoteCoord model TopRight
                                        , y = updateNoteCoord model TopRight
                                        , transparency = updateTransparency model 0
                                        }
                                    , topLeftNote =
                                        { x = updateNoteCoord model TopLeft
                                        , y = updateNoteCoord model TopLeft
                                        , transparency = updateTransparency model 0
                                        }
                                    ,   verytopNote =
                                        { x = updateNoteCoord model Verytop
                                        , y = updateNoteCoord model Verytop
                                        , transparency = updateTransparency model model.verytopNote.transparency
                                        }
                                    , bottomNote =
                                        { x = updateNoteCoord model Bottom
                                        , y = updateNoteCoord model Bottom
                                        , transparency = updateTransparency model 0
                                        }
                                    , bottomRightNote =
                                        { x = updateNoteCoord model BottomRight
                                        , y = updateNoteCoord model BottomRight
                                        , transparency = updateTransparency model 0
                                        }
                                    , bottomLeftNote =
                                        { x = updateNoteCoord model BottomLeft
                                        , y = updateNoteCoord model BottomLeft
                                        , transparency = updateTransparency model 0
                                        }
                                }

                            Goto3 ->
                                noTextBox { model
                                    | page = GraphPage
                                    , oneSat = 0
                                    , threeSat = 0
                                    , twoSat = 0
                                    , fiveSat = 0
                                    , sixSat = 0
                                    , sevenSat = 0
                                    , eightSat = 0
                                    , nineSat = 0
                                    , state = ClosingContacts
                                    , topNote =
                                        { x = updateNoteCoord model Top
                                        , y = updateNoteCoord model Top
                                        , transparency = updateTransparency model 0
                                        }
                                    , topRightNote =
                                        { x = updateNoteCoord model TopRight
                                        , y = updateNoteCoord model TopRight
                                        , transparency = updateTransparency model 0
                                        }
                                    , topLeftNote =
                                        { x = updateNoteCoord model TopLeft
                                        , y = updateNoteCoord model TopLeft
                                        , transparency = updateTransparency model 0
                                        }
                                    ,   verytopNote =
                                        { x = updateNoteCoord model Verytop
                                        , y = updateNoteCoord model Verytop
                                        , transparency = updateTransparency model model.verytopNote.transparency
                                        }
                                    , bottomNote =
                                        { x = updateNoteCoord model Bottom
                                        , y = updateNoteCoord model Bottom
                                        , transparency = updateTransparency model 0
                                        }
                                    , bottomRightNote =
                                        { x = updateNoteCoord model BottomRight
                                        , y = updateNoteCoord model BottomRight
                                        , transparency = updateTransparency model 0
                                        }
                                    , bottomLeftNote =
                                        { x = updateNoteCoord model BottomLeft
                                        , y = updateNoteCoord model BottomLeft
                                        , transparency = updateTransparency model 0
                                        }
                                }

                            Goto n ->
                                noTextBox { model
                                    | page = Avatar n
                                    , twoSat = 0
                                    , oneSat = 0
                                    , fourSat = 0
                                    , fiveSat = 0
                                    , sixSat = 0
                                    , sevenSat = 0
                                    , eightSat = 0
                                    , nineSat = 0
                                    , state = ClosingContacts
                                    , topNote =
                                        { x = updateNoteCoord model Top
                                        , y = updateNoteCoord model Top
                                        , transparency = updateTransparency model 0
                                        }
                                    , topRightNote =
                                        { x = updateNoteCoord model TopRight
                                        , y = updateNoteCoord model TopRight
                                        , transparency = updateTransparency model 0
                                        }
                                    , topLeftNote =
                                        { x = updateNoteCoord model TopLeft
                                        , y = updateNoteCoord model TopLeft
                                        , transparency = updateTransparency model 0
                                        }
                                    ,   verytopNote =
                                        { x = updateNoteCoord model Verytop
                                        , y = updateNoteCoord model Verytop
                                        , transparency = updateTransparency model model.verytopNote.transparency
                                        }
                                    , bottomNote =
                                        { x = updateNoteCoord model Bottom
                                        , y = updateNoteCoord model Bottom
                                        , transparency = updateTransparency model 0
                                        }
                                    , bottomRightNote =
                                        { x = updateNoteCoord model BottomRight
                                        , y = updateNoteCoord model BottomRight
                                        , transparency = updateTransparency model 0
                                        }
                                    , bottomLeftNote =
                                        { x = updateNoteCoord model BottomLeft
                                        , y = updateNoteCoord model BottomLeft
                                        , transparency = updateTransparency model 0
                                        }
                                }


                            ContactsPress ->
                                noTextBox { model
                                    | state = contactsPress model.state
                                }

                            ButtonPress button ->
                                noTextBox { model
                                    | state = updateStateButtonPress model.state button
                                }

                            XPress button ->
                                noTextBox { model
                                    | state = updateStateXButtonPress model.state button
                                }

                            _ ->
                                noTextBox model

                    GraphPage ->
                        case msg of
                            Tick f screen g ->
                                noTextBox { model
                                    | graphModel = Graph.update (Graph.Tick f g) model.graphModel
                                    , time = f
                                    , aniTimeFade =
                                        case model.state of
                                            FadingIn note ->
                                                model.aniTimeFade + 0.05

                                            FadingOut note ->
                                                model.aniTimeFade + 0.05

                                            otherwise ->
                                                0
                                    , aniTime =
                                        case model.state of
                                            Start ->
                                                0

                                            ExpandingContacts ->
                                                model.aniTime + 0.5

                                            ClosingContacts ->
                                                model.aniTime - 0.5

                                            otherwise ->
                                                animationTimeLength
                                    , aniTimeExpand =
                                        case model.state of
                                            Start ->
                                                (-1) * thirteenPercent

                                            ExpandingContacts ->
                                                model.aniTimeExpand + (movementSpeed model.aniTime)

                                            ClosingContacts ->
                                                model.aniTimeExpand - (movementSpeed model.aniTime)

                                            otherwise ->
                                                animationTimeLength - thirteenPercent
                                    , state = updateState model
                                    , topNote =
                                        { x = updateNoteCoord model Top
                                        , y = updateNoteCoord model Top
                                        , transparency = updateTransparency model model.topNote.transparency
                                        }
                                    , topRightNote =
                                        { x = updateNoteCoord model TopRight
                                        , y = updateNoteCoord model TopRight
                                        , transparency = updateTransparency model model.topRightNote.transparency
                                        }
                                    , topLeftNote =
                                        { x = updateNoteCoord model TopLeft
                                        , y = updateNoteCoord model TopLeft
                                        , transparency = updateTransparency model model.topLeftNote.transparency
                                        }
                                    ,   verytopNote =
                                        { x = updateNoteCoord model Verytop
                                        , y = updateNoteCoord model Verytop
                                        , transparency = updateTransparency model model.verytopNote.transparency
                                        }
                                    , bottomNote =
                                        { x = updateNoteCoord model Bottom
                                        , y = updateNoteCoord model Bottom
                                        , transparency = updateTransparency model model.bottomNote.transparency
                                        }
                                    , bottomRightNote =
                                        { x = updateNoteCoord model BottomRight
                                        , y = updateNoteCoord model BottomRight
                                        , transparency = updateTransparency model model.bottomRightNote.transparency
                                        }
                                    , bottomLeftNote =
                                        { x = updateNoteCoord model BottomLeft
                                        , y = updateNoteCoord model BottomLeft
                                        , transparency = updateTransparency model model.bottomLeftNote.transparency
                                        }
                                    , middleFaceX =
                                        case model.state of
                                            Start ->
                                                1000

                                            _ ->
                                                0
                                    , middleFaceY =
                                        case model.state of
                                            Start ->
                                                1000

                                            _ ->
                                                0
                                }

                            Msg1 _ ->
                                noTextBox model


                            Msg2 _ ->
                                noTextBox model

                            Msg3 m3 ->
                                noTextBox { model | graphModel = Graph.update m3 model.graphModel }

                            Msg _ _ ->
                                noTextBox model

                            Goto1 ->
                                noTextBox { model
                                    | page = SvgCalendar
                                    , twoSat = 0
                                    , threeSat = 0
                                    , fourSat = 0
                                    , fiveSat = 0
                                    , sixSat = 0
                                    , sevenSat = 0
                                    , eightSat = 0
                                    , nineSat = 0
                                    , state = ClosingContacts
                                    , topNote =
                                        { x = updateNoteCoord model Top
                                        , y = updateNoteCoord model Top
                                        , transparency = updateTransparency model 0
                                        }
                                    , topRightNote =
                                        { x = updateNoteCoord model TopRight
                                        , y = updateNoteCoord model TopRight
                                        , transparency = updateTransparency model 0
                                        }
                                    , topLeftNote =
                                        { x = updateNoteCoord model TopLeft
                                        , y = updateNoteCoord model TopLeft
                                        , transparency = updateTransparency model 0
                                        }
                                    ,   verytopNote =
                                        { x = updateNoteCoord model Verytop
                                        , y = updateNoteCoord model Verytop
                                        , transparency = updateTransparency model model.verytopNote.transparency
                                        }
                                    , bottomNote =
                                        { x = updateNoteCoord model Bottom
                                        , y = updateNoteCoord model Bottom
                                        , transparency = updateTransparency model 0
                                        }
                                    , bottomRightNote =
                                        { x = updateNoteCoord model BottomRight
                                        , y = updateNoteCoord model BottomRight
                                        , transparency = updateTransparency model 0
                                        }
                                    , bottomLeftNote =
                                        { x = updateNoteCoord model BottomLeft
                                        , y = updateNoteCoord model BottomLeft
                                        , transparency = updateTransparency model 0
                                        }
                                }

                            Goto2 ->
                                noTextBox { model
                                    | page = TetrisGame
                                    , oneSat = 0
                                    , threeSat = 0
                                    , fourSat = 0
                                    , fiveSat = 0
                                    , sixSat = 0
                                    , sevenSat = 0
                                    , eightSat = 0
                                    , nineSat = 0
                                    , state = ClosingContacts
                                    , topNote =
                                        { x = updateNoteCoord model Top
                                        , y = updateNoteCoord model Top
                                        , transparency = updateTransparency model 0
                                        }
                                    , topRightNote =
                                        { x = updateNoteCoord model TopRight
                                        , y = updateNoteCoord model TopRight
                                        , transparency = updateTransparency model 0
                                        }
                                    , topLeftNote =
                                        { x = updateNoteCoord model TopLeft
                                        , y = updateNoteCoord model TopLeft
                                        , transparency = updateTransparency model 0
                                        }
                                    ,   verytopNote =
                                        { x = updateNoteCoord model Verytop
                                        , y = updateNoteCoord model Verytop
                                        , transparency = updateTransparency model model.verytopNote.transparency
                                        }
                                    , bottomNote =
                                        { x = updateNoteCoord model Bottom
                                        , y = updateNoteCoord model Bottom
                                        , transparency = updateTransparency model 0
                                        }
                                    , bottomRightNote =
                                        { x = updateNoteCoord model BottomRight
                                        , y = updateNoteCoord model BottomRight
                                        , transparency = updateTransparency model 0
                                        }
                                    , bottomLeftNote =
                                        { x = updateNoteCoord model BottomLeft
                                        , y = updateNoteCoord model BottomLeft
                                        , transparency = updateTransparency model 0
                                        }
                                }

                            Goto3 ->
                                noTextBox { model
                                    | page = GraphPage
                                    , oneSat = 0
                                    , threeSat = 0
                                    , twoSat = 0
                                    , fiveSat = 0
                                    , sixSat = 0
                                    , sevenSat = 0
                                    , eightSat = 0
                                    , nineSat = 0
                                    , state = ClosingContacts
                                    , topNote =
                                        { x = updateNoteCoord model Top
                                        , y = updateNoteCoord model Top
                                        , transparency = updateTransparency model 0
                                        }
                                    , topRightNote =
                                        { x = updateNoteCoord model TopRight
                                        , y = updateNoteCoord model TopRight
                                        , transparency = updateTransparency model 0
                                        }
                                    , topLeftNote =
                                        { x = updateNoteCoord model TopLeft
                                        , y = updateNoteCoord model TopLeft
                                        , transparency = updateTransparency model 0
                                        }
                                    ,   verytopNote =
                                        { x = updateNoteCoord model Verytop
                                        , y = updateNoteCoord model Verytop
                                        , transparency = updateTransparency model model.verytopNote.transparency
                                        }
                                    , bottomNote =
                                        { x = updateNoteCoord model Bottom
                                        , y = updateNoteCoord model Bottom
                                        , transparency = updateTransparency model 0
                                        }
                                    , bottomRightNote =
                                        { x = updateNoteCoord model BottomRight
                                        , y = updateNoteCoord model BottomRight
                                        , transparency = updateTransparency model 0
                                        }
                                    , bottomLeftNote =
                                        { x = updateNoteCoord model BottomLeft
                                        , y = updateNoteCoord model BottomLeft
                                        , transparency = updateTransparency model 0
                                        }
                                }

                            Goto n ->
                                noTextBox { model
                                    | page = Avatar n
                                    , twoSat = 0
                                    , oneSat = 0
                                    , fourSat = 0
                                    , fiveSat = 0
                                    , sixSat = 0
                                    , sevenSat = 0
                                    , eightSat = 0
                                    , nineSat = 0
                                    , state = ClosingContacts
                                    , topNote =
                                        { x = updateNoteCoord model Top
                                        , y = updateNoteCoord model Top
                                        , transparency = updateTransparency model 0
                                        }
                                    , topRightNote =
                                        { x = updateNoteCoord model TopRight
                                        , y = updateNoteCoord model TopRight
                                        , transparency = updateTransparency model 0
                                        }
                                    , topLeftNote =
                                        { x = updateNoteCoord model TopLeft
                                        , y = updateNoteCoord model TopLeft
                                        , transparency = updateTransparency model 0
                                        }
                                    ,   verytopNote =
                                        { x = updateNoteCoord model Verytop
                                        , y = updateNoteCoord model Verytop
                                        , transparency = updateTransparency model model.verytopNote.transparency
                                        }
                                    , bottomNote =
                                        { x = updateNoteCoord model Bottom
                                        , y = updateNoteCoord model Bottom
                                        , transparency = updateTransparency model 0
                                        }
                                    , bottomRightNote =
                                        { x = updateNoteCoord model BottomRight
                                        , y = updateNoteCoord model BottomRight
                                        , transparency = updateTransparency model 0
                                        }
                                    , bottomLeftNote =
                                        { x = updateNoteCoord model BottomLeft
                                        , y = updateNoteCoord model BottomLeft
                                        , transparency = updateTransparency model 0
                                        }
                                }


                            ContactsPress ->
                                noTextBox { model
                                    | state = contactsPress model.state
                                }

                            ButtonPress button ->
                                noTextBox { model
                                    | state = updateStateButtonPress model.state button
                                }

                            XPress button ->
                                noTextBox { model
                                    | state = updateStateXButtonPress model.state button
                                }

                            _ ->
                                noTextBox model

                    Avatar aIdx ->
                        case msg of
                            Tick f screen g ->
                                noTextBox { model
                                    | avatars = Dict.update aIdx (updateAvatar (A1.Tick f g)) model.avatars
                                    , time = f
                                    , aniTimeFade =
                                        case model.state of
                                            FadingIn note ->
                                                model.aniTimeFade + 0.05

                                            FadingOut note ->
                                                model.aniTimeFade + 0.05

                                            otherwise ->
                                                0
                                    , aniTime =
                                        case model.state of
                                            Start ->
                                                0

                                            ExpandingContacts ->
                                                model.aniTime + 0.5

                                            ClosingContacts ->
                                                model.aniTime - 0.5

                                            otherwise ->
                                                animationTimeLength
                                    , aniTimeExpand =
                                        case model.state of
                                            Start ->
                                                (-1) * thirteenPercent

                                            ExpandingContacts ->
                                                model.aniTimeExpand + (movementSpeed model.aniTime)

                                            ClosingContacts ->
                                                model.aniTimeExpand - (movementSpeed model.aniTime)

                                            otherwise ->
                                                animationTimeLength - thirteenPercent
                                    , state = updateState model
                                    , topNote =
                                        { x = updateNoteCoord model Top
                                        , y = updateNoteCoord model Top
                                        , transparency = updateTransparency model model.topNote.transparency
                                        }
                                    , topRightNote =
                                        { x = updateNoteCoord model TopRight
                                        , y = updateNoteCoord model TopRight
                                        , transparency = updateTransparency model model.topRightNote.transparency
                                        }
                                    , topLeftNote =
                                        { x = updateNoteCoord model TopLeft
                                        , y = updateNoteCoord model TopLeft
                                        , transparency = updateTransparency model model.topLeftNote.transparency
                                        }
                                    ,   verytopNote =
                                        { x = updateNoteCoord model Verytop
                                        , y = updateNoteCoord model Verytop
                                        , transparency = updateTransparency model model.verytopNote.transparency
                                        }
                                    , bottomNote =
                                        { x = updateNoteCoord model Bottom
                                        , y = updateNoteCoord model Bottom
                                        , transparency = updateTransparency model model.bottomNote.transparency
                                        }
                                    , bottomRightNote =
                                        { x = updateNoteCoord model BottomRight
                                        , y = updateNoteCoord model BottomRight
                                        , transparency = updateTransparency model model.bottomRightNote.transparency
                                        }
                                    , bottomLeftNote =
                                        { x = updateNoteCoord model BottomLeft
                                        , y = updateNoteCoord model BottomLeft
                                        , transparency = updateTransparency model model.bottomLeftNote.transparency
                                        }
                                    , middleFaceX =
                                        case model.state of
                                            Start ->
                                                1000

                                            _ ->
                                                0
                                    , middleFaceY =
                                        case model.state of
                                            Start ->
                                                1000

                                            _ ->
                                                0
                                }

                            Msg1 _ ->
                                noTextBox model

                            Msg2 _ ->
                                noTextBox model

                            Msg3 _ ->
                                noTextBox model

                            Msg mIdx m ->
                                if aIdx == mIdx
                                    then
                                        noTextBox { model | avatars = Dict.update aIdx (updateAvatar m) model.avatars }
                                    else
                                        noTextBox model

                            Goto1 ->
                                noTextBox { model
                                    | page = SvgCalendar
                                    , twoSat = 0
                                    , threeSat = 0
                                    , fourSat = 0
                                    , fiveSat = 0
                                    , sixSat = 0
                                    , sevenSat = 0
                                    , eightSat = 0
                                    , nineSat = 0
                                    , state = ClosingContacts
                                    , topNote =
                                        { x = updateNoteCoord model Top
                                        , y = updateNoteCoord model Top
                                        , transparency = updateTransparency model 0
                                        }
                                    , topRightNote =
                                        { x = updateNoteCoord model TopRight
                                        , y = updateNoteCoord model TopRight
                                        , transparency = updateTransparency model 0
                                        }
                                    , topLeftNote =
                                        { x = updateNoteCoord model TopLeft
                                        , y = updateNoteCoord model TopLeft
                                        , transparency = updateTransparency model 0
                                        }
                                    ,   verytopNote =
                                        { x = updateNoteCoord model Verytop
                                        , y = updateNoteCoord model Verytop
                                        , transparency = updateTransparency model model.verytopNote.transparency
                                        }
                                    , bottomNote =
                                        { x = updateNoteCoord model Bottom
                                        , y = updateNoteCoord model Bottom
                                        , transparency = updateTransparency model 0
                                        }
                                    , bottomRightNote =
                                        { x = updateNoteCoord model BottomRight
                                        , y = updateNoteCoord model BottomRight
                                        , transparency = updateTransparency model 0
                                        }
                                    , bottomLeftNote =
                                        { x = updateNoteCoord model BottomLeft
                                        , y = updateNoteCoord model BottomLeft
                                        , transparency = updateTransparency model 0
                                        }
                                }

                            Goto2 ->
                                noTextBox { model
                                    | page = TetrisGame
                                    , oneSat = 0
                                    , threeSat = 0
                                    , fourSat = 0
                                    , fiveSat = 0
                                    , sixSat = 0
                                    , sevenSat = 0
                                    , eightSat = 0
                                    , nineSat = 0
                                    , state = ClosingContacts
                                    , topNote =
                                        { x = updateNoteCoord model Top
                                        , y = updateNoteCoord model Top
                                        , transparency = updateTransparency model 0
                                        }
                                    , topRightNote =
                                        { x = updateNoteCoord model TopRight
                                        , y = updateNoteCoord model TopRight
                                        , transparency = updateTransparency model 0
                                        }
                                    , topLeftNote =
                                        { x = updateNoteCoord model TopLeft
                                        , y = updateNoteCoord model TopLeft
                                        , transparency = updateTransparency model 0
                                        }
                                    ,   verytopNote =
                                        { x = updateNoteCoord model Verytop
                                        , y = updateNoteCoord model Verytop
                                        , transparency = updateTransparency model model.verytopNote.transparency
                                        }
                                    , bottomNote =
                                        { x = updateNoteCoord model Bottom
                                        , y = updateNoteCoord model Bottom
                                        , transparency = updateTransparency model 0
                                        }
                                    , bottomRightNote =
                                        { x = updateNoteCoord model BottomRight
                                        , y = updateNoteCoord model BottomRight
                                        , transparency = updateTransparency model 0
                                        }
                                    , bottomLeftNote =
                                        { x = updateNoteCoord model BottomLeft
                                        , y = updateNoteCoord model BottomLeft
                                        , transparency = updateTransparency model 0
                                        }
                                }

                            Goto3 ->
                                noTextBox { model
                                    | page = GraphPage
                                    , oneSat = 0
                                    , threeSat = 0
                                    , twoSat = 0
                                    , fiveSat = 0
                                    , sixSat = 0
                                    , sevenSat = 0
                                    , eightSat = 0
                                    , nineSat = 0
                                    , state = ClosingContacts
                                    , topNote =
                                        { x = updateNoteCoord model Top
                                        , y = updateNoteCoord model Top
                                        , transparency = updateTransparency model 0
                                        }
                                    , topRightNote =
                                        { x = updateNoteCoord model TopRight
                                        , y = updateNoteCoord model TopRight
                                        , transparency = updateTransparency model 0
                                        }
                                    , topLeftNote =
                                        { x = updateNoteCoord model TopLeft
                                        , y = updateNoteCoord model TopLeft
                                        , transparency = updateTransparency model 0
                                        }
                                    ,   verytopNote =
                                        { x = updateNoteCoord model Verytop
                                        , y = updateNoteCoord model Verytop
                                        , transparency = updateTransparency model model.verytopNote.transparency
                                        }
                                    , bottomNote =
                                        { x = updateNoteCoord model Bottom
                                        , y = updateNoteCoord model Bottom
                                        , transparency = updateTransparency model 0
                                        }
                                    , bottomRightNote =
                                        { x = updateNoteCoord model BottomRight
                                        , y = updateNoteCoord model BottomRight
                                        , transparency = updateTransparency model 0
                                        }
                                    , bottomLeftNote =
                                        { x = updateNoteCoord model BottomLeft
                                        , y = updateNoteCoord model BottomLeft
                                        , transparency = updateTransparency model 0
                                        }
                                }


                            Goto n ->
                                noTextBox { model
                                    | page = Avatar n
                                    , twoSat = 0
                                    , oneSat = 0
                                    , fourSat = 0
                                    , fiveSat = 0
                                    , sixSat = 0
                                    , sevenSat = 0
                                    , eightSat = 0
                                    , nineSat = 0
                                    , state = ClosingContacts
                                    , topNote =
                                        { x = updateNoteCoord model Top
                                        , y = updateNoteCoord model Top
                                        , transparency = updateTransparency model 0
                                        }
                                    , topRightNote =
                                        { x = updateNoteCoord model TopRight
                                        , y = updateNoteCoord model TopRight
                                        , transparency = updateTransparency model 0
                                        }
                                    , topLeftNote =
                                        { x = updateNoteCoord model TopLeft
                                        , y = updateNoteCoord model TopLeft
                                        , transparency = updateTransparency model 0
                                        }
                                    ,   verytopNote =
                                        { x = updateNoteCoord model Verytop
                                        , y = updateNoteCoord model Verytop
                                        , transparency = updateTransparency model model.verytopNote.transparency
                                        }
                                    , bottomNote =
                                        { x = updateNoteCoord model Bottom
                                        , y = updateNoteCoord model Bottom
                                        , transparency = updateTransparency model 0
                                        }
                                    , bottomRightNote =
                                        { x = updateNoteCoord model BottomRight
                                        , y = updateNoteCoord model BottomRight
                                        , transparency = updateTransparency model 0
                                        }
                                    , bottomLeftNote =
                                        { x = updateNoteCoord model BottomLeft
                                        , y = updateNoteCoord model BottomLeft
                                        , transparency = updateTransparency model 0
                                        }
                                }

                            ContactsPress ->
                                noTextBox { model
                                    | state = contactsPress model.state
                                }

                            ButtonPress button ->
                                noTextBox { model
                                    | state = updateStateButtonPress model.state button
                                }

                            XPress button ->
                                noTextBox { model
                                    | state = updateStateXButtonPress model.state button
                                }

                            _ ->
                                noTextBox model


    in (newModel
       , mMsg
       , Cmd.none
       )

noTextBox x = (x,Nothing)

updateStateFadingIn model button =
    if model.aniTimeFade >= 0.4 then
        Stationary button
    else
        FadingIn button


updateStateFadingOut model button =
    if model.aniTimeFade >= 0.4 then
        CircleOfSixOpen
    else
        FadingOut button


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

        FadingIn button ->
            updateStateFadingIn model button

        FadingOut button ->
            updateStateFadingOut model button

        state ->
            state


contactsPress oldState =
    case oldState of
        Start ->
            ExpandingContacts

        CircleOfSixOpen ->
            ClosingContacts

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


pencil =     group
        [ group
            [ circle 40
                |> filled (rgb 144 238 144)
            , circle 40
                |> outlined (solid 1) black
            ]
            |> move ( 5, 3 )
        , rect 2 40
            |> filled orange
        , rect 5 40
            |> filled orange
            |> move ( 4, 0 )
        , rect 2 40
            |> filled orange
            |> move ( 8, 0 )
        , rect 10.5 5
            |> filled yellow
            |> move ( 3.9, 22 )
        , rect 10 8
            |> filled pink
            |> move ( 4, 28 )
        , triangle 6
            |> filled lightBrown
            |> rotate (degrees 30)
            |> move ( 4, -23 )
        , triangle 2
            |> filled charcoal
            |> rotate (degrees 30)
            |> move ( 4, -27 )
        ]



makeEditAvatar model =
    pencil
        |> scale (faceScale * 1)
        |> move ( -1, -2 )
        |> move ( 9, 9 )
        |> rotate (degrees 45)
        |> makeTransparent (middleTransparency model.aniTimeExpand)

makecall model data =
    group
        [ group
            [ circle 11
                |> filled (rgb 144 238 144) --backgroundPerson
            , data.face
            , if data.emergency == True then
                emergency
              else
                group []
            , myCircleOutline
                |> makeTransparent data.noteAttributes.transparency
             --   |> move ( data.noteAttributes.x, data.noteAttributes.y )
            ]
           |> notifyTap (ButtonPress data.position)
        ]
     --   |> move ( circleX data.n model.aniTimeExpand, (circleY data.n model.aniTimeExpand) - 20 )
        |> scale (1)

makeButton model data =
    group
        [ group
            [ circle 11
                |> filled (rgb 144 238 144) --backgroundPerson
            , data.face
            , if data.emergency == True then
                emergency
              else
                group []
            , myCircleOutline
                |> makeTransparent data.noteAttributes.transparency
                |> move ( data.noteAttributes.x, data.noteAttributes.y )
            ]
           |> notifyTap (ButtonPress data.position)
        ]
        |> move ( circleX data.n model.aniTimeExpand, (circleY data.n model.aniTimeExpand) - 20 )
        |> scale (scaleContacts model (5 - data.n))



-- FORM STUFF

makethecall model data =
    group
        [ GraphicSVG.text "Call Help!"
            |> filled black
            |> scale 1.5
            |> move ( -38, -6 )
            ]
            |> scale 1
         --   |> rotate (degrees -5)
            --, roundedRect 10 3 2 |> filled black |> rotate (degrees -20) |> move (-14,23)
            --, roundedRect 9 2.5 2 |> filled black |> move (-13,18)
            --, roundedRect 8 2.25 2 |> filled black |> rotate (degrees 20) |> move (-12,14)
       --     |> scale 0.2
        --    |> move ( -30, 29 )
            |> GraphicSVG.addHyperlink ("tel:" ++ data.phoneNumber)

makeNote model data =
    group
        [ roundedRect 80 20 5
            |> filled noteColour
            |> move ( 0, 35 )
        , GraphicSVG.text data.text
            |> filled red --black
            |> scale 0.45
            |> move ( -35, 38 )
        , closeBoxSymbol
            |> rotate (degrees 45)
            |> move ( 35, 40 )
            |> notifyTap (XPress data.position)
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
            |> move ( -30, 29 )
            |> GraphicSVG.addHyperlink ("tel:" ++ data.phoneNumber)
        , makeEditAvatar model
            |> notifyTap data.editPosition
            |> move ( 20, 22 )
            |> scale 1.2
        ]
        |> makeTransparent data.d.transparency
        |> move ( data.d.x, data.d.y )


backgroundDebugScreen model =
    if options.debug then
        group
            [ -- Test
              GraphicSVG.text ("Change in time step (Speed) " ++ (stringOfAniTime (movementSpeed model.aniTime)))
                |> filled black
                |> move ( 30 - 150, -10 + 60 )
            , GraphicSVG.text ("Time " ++ (stringOfAniTime model.time))
                |> filled black
                |> move ( 30 - 150, -70 + 60 )
            , GraphicSVG.text ("Expand Time " ++ (stringOfAniTime model.aniTimeExpand))
                |> filled black
                |> move ( 30 - 150, -50 + 60 )
            , GraphicSVG.text ("Constant Time " ++ (stringOfAniTime model.aniTime))
                |> filled black
                |> move ( 30 - 150, -30 + 60 )
            , GraphicSVG.text ("Scale Time " ++ (stringOfAniTime (max (sin (model.time) * enlargedSize) scaledSize)))
                |> filled black
                |> move ( 30 - 150, -90 + 60 )
            , GraphicSVG.text ("Scale Time " ++ toString (model.state))
                |> filled black
                |> move ( 30 - 150, -110 + 60 )
            , GraphicSVG.text ("Animation Time " ++ (stringOfAniTime model.topLeftNote.transparency))
                |> filled black
                |> move ( 30, -50 )
            , text (toString (model.width,model.height))
                |> filled black
                |> move ( 30, -60)
            ]
            |> scale 0.5
    else
        group []


monthText =
    "Feb"
  --  GraphicSVG.text(toString <| Date.month  <| Date.fromTime <| model.)  

dayText =
    "5"


calendarIcon =
    group
        [ circle 40
            |> filled (rgb 144 238 144)
        , roundedRect 50 50 5
            |> filled white
        , roundedRect 50 30 5
            |> filled red
            |> move ( 0, 10 )
        , rect 50 20
            |> filled white
        , roundedRect 5 10 2.5
            |> filled grey
            |> move ( -14, 25 )
        , roundedRect 5 10 2.5
            |> filled grey
            |> move ( 14, 25 )
        , GraphicSVG.text monthText            
            |> filled white
            |> move ( -12, 13 )
            |> scale 0.8
        , GraphicSVG.text dayText
            |> filled black
            |> move ( -18, -18 )
            |> scale 3
        ]
        |> scale 0.38


--Replace 1 with other numbers upto 6 so far


svgView : Model -> Collage (GraphicSVG.Msg (Msg))
svgView model =
    collage model.width model.height <|
        (case model.page of
            SvgCalendar ->
                List.map (map Msg1) (SvgCalendar.view model.calModel)

            TetrisGame ->
                [group (List.map (map Msg2) (Tetris.view model.tetrisModel)) |> scale 0.8]

            GraphPage ->
                [group (List.map (map Msg3) (Graph.myShapes model.graphModel)) |> scale 1.2]

            Avatar n ->
                let
                    m = case Dict.get n model.avatars of
                          Just mm -> mm
                          Nothing -> A1.init
                in
                    List.map (map (Msg n)) (A1.view m)
        )
            ++ [ rect 200 70 |> filled blank |> move ( 0, -190 ) |> notifyMouseMoveAt MoveInRect ]
            ++ [ case model.page of
                     SvgCalendar ->
                         pencil
                             |> rotate (degrees -45)
                             |> scale (15/40)
                             |> move ( -25 - SvgCalendar.circleOfSixRadius + 24, -75 + SvgCalendar.startingY - 43 )
                             |> notifyTap (Msg1 SvgCalendar.Today)
                     _ ->
                         group
                             [ calendarIcon
                             , circle 15
                                 |> filled (blank)
                                 |> addOutline (lineThicknessSvgCalendar model)
                                     (if model.page == SvgCalendar then
                                         orange
                                      else if model.oneSat == 0 then
                                         black
                                      else
                                         orange
                                     )
                             ]
                             |> move ( -25 - SvgCalendar.circleOfSixRadius + 29, -75 + SvgCalendar.startingY - 43 )
                             |> notifyTap Goto1
               , group
                    [ circle 15
                        |> filled (rgb 144 238 144) --(twoColour model)
                        |> addOutline (lineThicknessTetris model)
                            (if model.page == TetrisGame then
                                blue
                             else if model.twoSat == 0 then
                                black
                             else
                                blue
                            )
                    , tetrisIcon
                    ]
                    |> move ( -25 - SvgCalendar.circleOfSixRadius + 72, -75 + SvgCalendar.startingY - 43 )
                    |> notifyTap Goto2
                , group
                    [ SvgCalendar.nextmonthIcon
                    ]
                    |> move ( 180,-150)
                --     |> SvgCalendar.nextmonth = true
               --     |> SvgCalendar.mappedMonthDays model
                , group
                    [ SvgCalendar.previousmonthIcon
                    ]
                    |> move ( -280,-150)
                 --   |> SvgCalendar.previousmonth = true
                --    |> SvgCalendar.mappedMonthDays model
               , group
                    [ rectangle 80 20
                        |> filled orange
                   -- ,phoneIcon
                 --   ,makecall model { face = changeAllFace (getAvatar 1 model), noteAttributes = model.verytopNote, n = 0, position = Verytop, emergency = False }
                    , makethecall model {phoneNumber = "647-458-3630" }

                    ]

                    |> move ( 270,180 )
                --   |> notifyTap (ButtonPress data.position)
                , group
                    [ circle 15
                        |> filled (rgb 144 238 144) --(threeColour model)
                        |> addOutline (lineThicknessMiddleAvatarCreator model)
                            (if model.page == Avatar 1 then
                                darkBlue
                             else if model.threeSat == 0 then
                                black
                             else
                                darkBlue
                            )
                    , settingsIcon
                    ]
                    |> move ( -25 - SvgCalendar.circleOfSixRadius + 115, -75 + SvgCalendar.startingY - 43 )
                    |> notifyTap (Goto 0)
                , group
                    [ circle 15
                        |> filled (rgb 144 238 144) --(threeColour model)
                        |> addOutline (lineThicknessMiddleGraph model)
                            (if model.page == GraphPage then
                                yellow
                             else if model.fourSat == 0 then
                                black
                             else
                                yellow
                            )
                    , graphIcon
                    ]
                    |> move ( -25 - SvgCalendar.circleOfSixRadius + 158, -75 + SvgCalendar.startingY - 43 )
                    |> notifyTap (Goto3)
               , group
                    [ backgroundDebugScreen model
                    , makeButton model { face = changeAllFace (getAvatar 6 model), noteAttributes = model.topLeftNote, n = 5, position = TopLeft, emergency = True }
                    , makeButton model { face = changeAllFace (getAvatar 5 model), noteAttributes = model.bottomLeftNote, n = 4, position = BottomLeft, emergency = False }
                    , makeButton model { face = changeAllFace (getAvatar 4 model), noteAttributes = model.bottomNote, n = 3, position = Bottom, emergency = False }
                    , makeButton model { face = changeAllFace (getAvatar 3 model), noteAttributes = model.bottomRightNote, n = 2, position = BottomRight, emergency = False }
                    , makeButton model { face = changeAllFace (getAvatar 2 model), noteAttributes = model.topRightNote, n = 1, position = TopRight, emergency = True }
                    , makeButton model { face = changeAllFace (getAvatar 1 model), noteAttributes = model.topNote, n = 0, position = Top, emergency = False }
                    , makeNote model { d = model.topNote, text = text1, position = Top, phoneNumber = "647-458-3630", editPosition = Goto 1 }
                    , makeNote model { d = model.topRightNote, text = text2, position = TopRight, phoneNumber = "647-458-3630", editPosition = Goto 2 }
                    , makeNote model { d = model.bottomRightNote, text = text3, position = BottomRight, phoneNumber = "647-458-3630", editPosition = Goto 3 }
                    , makeNote model { d = model.bottomNote, text = text4, position = Bottom, phoneNumber = "647-458-3630", editPosition = Goto 4 }
                    , makeNote model { d = model.bottomLeftNote, text = text5, position = BottomLeft, phoneNumber = "647-458-3630", editPosition = Goto 5 }
                    , makeNote model { d = model.topLeftNote, text = text6, position = TopLeft, phoneNumber = "647-458-3630", editPosition = Goto 6 }

                    -- Center Face
                    , group
                        [ circle 11
                            |> filled (rgb 144 238 144) --backgroundPerson
                        , changeAllFace (getAvatar 0 model)
                        , myCircleOutline
                            |> makeTransparent 0
                        , makeEditAvatar model
                            |> notifyTap (Goto 0)
                        ]
                        |> move ( 0 + model.middleFaceX, -20 + model.middleFaceY )
                        |> makeTransparent (middleTransparency model.aniTimeExpand)

                    -- Button Face
                    , group
                        [ circle 11
                            |> filled (rgb 144 238 144)  --backgroundPerson
                        , changeAllFace (getAvatar 0 model)
                        , myCircleOutline
                            |> makeTransparent (middleTransparency model.aniTimeExpand)
                        ]
                        |> move ( -25 - circleOfSixRadius, -83 + startingY )
                        |> notifyTap ContactsPress
                        |> scale 0.5

                    ]
                    |> scale 2.8
                    |> move ( 0, 15 )
               ]
     ++ [ text (toString (Date.fromTime <| 1000 * model.time)) |> filled orange |> move (-110,180)]
          --     ++ [ text (toString model.time ++ toString (Date.fromTime <| 1000 * model.time)) |> filled orange |> move (-110,0)]
               {-
               ++ ( if options.debugTime then   [ group
                                                    [ roundedRect 20 10 5
                                                        |> filled purple
                                                    , text "+hour" |> centered |> size 5 |> filled pink
                                                    ]
                                                    |> move ( 95, -75 + SvgCalendar.startingY - 37 )
                                                    |> notifyTap DebugAddHour
                                              , group
                                                    [ roundedRect 20 10 5
                                                        |> filled purple
                                                    , text "+day" |> centered |> size 5 |> filled pink
                                                    ]
                                                    |> move ( 95, -75 + SvgCalendar.startingY - 48 )
                                                    |> notifyTap DebugAddDay
                                              ]
                   else []
                   )-}

getAvatar idx model = case Dict.get idx model.avatars of
                        Just a -> a
                        Nothing -> A1.init -- FIXME make error face

distTo pointA pointB =
    sqrt ((Tuple.first pointB - Tuple.first pointA) ^ 2 + (Tuple.second pointB - Tuple.second pointA) ^ 2)


lineThicknessMiddleAvatarCreator m =
    (if m.page == Avatar 1 then
        solid 2
     else if m.threeSat == 0 then
        solid 0.3
     else
        solid 2
    )


lineThicknessMiddleGraph m =
    (if m.page == GraphPage then
        solid 2
     else if m.fourSat == 0 then
        solid 0.3
     else
        solid 2
    )

lineThicknessTetris m =
    (if m.page == TetrisGame then
        solid 2
     else if m.twoSat == 0 then
        solid 0.3
     else
        solid 2
    )


lineThicknessSvgCalendar m =
    (if m.page == SvgCalendar then
        solid 2
     else if m.oneSat == 0 then
        solid 0.3
     else
        solid 2
    )


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

phoneIcon =
    group
        [ GraphicSVG.text "647-458-3630"
            |> filled black

            ]
        |> scale (1)
        |> move ( -33, -3.5 )
tetrisIcon =
    group
        [ GraphicSVG.text "TETRIS"
            |> filled black
            ]
       {- rect 30 10
            |> filled orange
        , rect 10 20
            |> filled orange
            |> move ( 0, 5 )
        , rect 20 20
            |> filled blue
            |> move ( 25, 5 )
        , rect 10 20
            |> filled green
            |> move ( 10, 15 )
        , rect 30 10
            |> filled green
            |> move ( 10, 20 )
        , rect 20 10
            |> filled yellow
            |> move ( -5, 30 )
        , rect 10 40
            |> filled yellow
            |> move ( 40, 40 )
        ]-}
        |> scale (0.08 * 8)
        |> move ( -13, -3 )
graphIcon =     group
        [ group [

        rect 2 19 -- vertical line
            |> filled orange
            |> move (-8,2)
     , rect 2 20 -- horizontal line
            |> filled orange
            |> move ( 0, -8 )
            |> rotate (degrees 90)


       , rect 2 10.5
            |> filled darkGreen
            |> move (-3,4)
            |> rotate (degrees -45)


       , rect 2 8.5
            |> filled darkGreen
            |> move (3,4)
            |> rotate (degrees 35)

        , rect 2 7.5
            |> filled darkGreen
            |> move (8,4)
            |> rotate (degrees -35)
    ]
       ]


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


textBox1 =
    group
        [ roundedRect 80 20 5
            |> filled yellow
        , GraphicSVG.text "Journal Entry:"
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


emergency =
    group
        [ circle 40
            |> outlined (solid (12 * scaledValue)) emergencyBorderColour
        , emergencyWords
        , emergencyWords
            |> rotate (degrees (difference * 1))
        , emergencyWords
            |> rotate (degrees (difference * 2))
        ]
        |> scale (faceScale * 3.1)


lambda =
    -12


scaledValue =
    0.8


difference =
    120


xValue =
    -2


yValue =
    37


emergencyBorderColour =
    rgb 0 0 255


emergencyWords =
    group
        [ group
            [ GraphicSVG.text "E"
                |> filled white
                |> move ( xValue, yValue )
                |> scale scaledValue
            ]
            |> rotate (degrees (0 * lambda + 2))
        , group
            [ GraphicSVG.text "M"
                |> filled white
                |> move ( xValue, yValue )
                |> scale scaledValue
            ]
            |> rotate (degrees (1 * lambda + 2))
        , group
            [ GraphicSVG.text "E"
                |> filled white
                |> move ( xValue, yValue )
                |> scale scaledValue
            ]
            |> rotate (degrees 2 * lambda)
        , group
            [ GraphicSVG.text "R"
                |> filled white
                |> move ( xValue, yValue )
                |> scale scaledValue
            ]
            |> rotate (degrees 3 * lambda)
        , group
            [ GraphicSVG.text "G"
                |> filled white
                |> move ( xValue, yValue )
                |> scale scaledValue
            ]
            |> rotate (degrees 4 * lambda)
        , group
            [ GraphicSVG.text "E"
                |> filled white
                |> move ( xValue, yValue )
                |> scale scaledValue
            ]
            |> rotate (degrees 5 * lambda)
        , group
            [ GraphicSVG.text "N"
                |> filled white
                |> move ( xValue, yValue )
                |> scale scaledValue
            ]
            |> rotate (degrees 6 * lambda)
        , group
            [ GraphicSVG.text "C"
                |> filled white
                |> move ( xValue, yValue )
                |> scale scaledValue
            ]
            |> rotate (degrees 7 * lambda)
        , group
            [ GraphicSVG.text "Y"
                |> filled white
                |> move ( xValue, yValue )
                |> scale scaledValue
            ]
            |> rotate (degrees 8 * lambda)
        ]


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
        , freckles |> move ( -40, -20 )
        , freckles |> move ( 40, -20 )

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
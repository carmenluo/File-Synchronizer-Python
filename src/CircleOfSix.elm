module CircleOfSix exposing (..)

import GraphicSVG exposing (..)
import AvatarAssets exposing (..)
import Ease exposing (..)


options =
    { debug = False }


main =
    gameApp Tick { model = init, view = view, update = update }



-- MODEL


type Position
    = Top
    | TopRight
    | TopLeft
    | Bottom
    | BottomLeft
    | BottomRight


type State
    = Start
    | ExpandingContacts
    | ClosingContacts
    | CircleOfSixOpen
    | FadingIn Position
    | FadingOut Position
    | Stationary Position


type alias NoteData =
    { x : Float, y : Float, transparency : Float }


type alias Model =
    { time : Float
    , state : State
    , aniTimeFade : Float
    , spiralTheta : Float
    , circleOfSix : { x : Float, y : Float }
    , topNote : NoteData
    , topRightNote : NoteData
    , bottomRightNote : NoteData
    , bottomNote : NoteData
    , bottomLeftNote : NoteData
    , topLeftNote : NoteData
    , aniTime : Float
    , aniTimeExpand : Float
    }


init : Model
init =
    { time = 0
    , state = Start
    , aniTimeFade = 0
    , spiralTheta = 0
    , circleOfSix = { x = 0, y = 0 }
    , topNote = { x = 1000, y = 1000, transparency = 0 }
    , topRightNote = { x = 1000, y = 1000, transparency = 0 }
    , bottomRightNote = { x = 1000, y = 1000, transparency = 0 }
    , bottomNote = { x = 1000, y = 1000, transparency = 0 }
    , bottomLeftNote = { x = 1000, y = 1000, transparency = 0 }
    , topLeftNote = { x = 1000, y = 1000, transparency = 0 }
    , aniTime = 0
    , aniTimeExpand = 0
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick t _ ->
            { model
                | time = t
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
            }

        ContactsPress ->
            { model
                | state = contactsPress model.state
            }

        ButtonPress button ->
            { model
                | state = updateStateButtonPress model.state button
            }

        XPress button ->
            { model
                | state = updateStateXButtonPress model.state button
            }


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



-- UPDATE


type Msg
    = Tick Float GetKeyState
    | ContactsPress
    | ButtonPress Position
    | XPress Position


contactsPress oldState =
    case oldState of
        Start ->
            ExpandingContacts

        CircleOfSixOpen ->
            ClosingContacts

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



-- VIEW


view model =
    collage 250 128 (myShapes model)



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
    -35



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


scaleContacts mod num =
    let
        scaledIconSize =
            0.8
    in
        if (mod.aniTimeExpand + thirteenPercent) - ((5 - num) * thirteenPercent) < thirteenPercent / 2 then
            0.5
        else if (mod.aniTimeExpand + thirteenPercent) - ((5 - num) * thirteenPercent) < thirteenPercent / 2 + eightySevenPercent / 4 then
            0.5 + ((mod.aniTimeExpand + thirteenPercent) - ((5 - num) * thirteenPercent) - (thirteenPercent / 2)) / (thirteenPercent / 2 + eightySevenPercent / 4) / 2 * scaledIconSize
        else if (mod.aniTimeExpand + thirteenPercent) - ((0) * thirteenPercent) < thirteenPercent / 2 + eightySevenPercent / 4 then
            1
        else
            (max (sin (mod.time - timeShift * num) * enlargedSize) scaledSize) * scaledIconSize


makeButton model data =
    group
        [ circle 11
            |> filled backgroundPerson
        , data.face
        , myCircleOutline
            |> makeTransparent data.transparency
        ]
        |> notifyTap (ButtonPress data.position)
        |> move ( circleX data.n model.aniTimeExpand, (circleY data.n model.aniTimeExpand) - 20 )
        --speed, % of the wheel
        |> scale (scaleContacts model (5 - data.n))


makeNote data =
    group
        [ roundedRect 80 20 5
            |> filled noteColour
            |> move ( 0, 35 )
        , text data.text
            |> filled black
            |> scale 0.45
            |> move ( -35, 38 )
        , closeBoxSymbol
            |> rotate (degrees 45)
            |> move ( 35, 40 )
            |> notifyTap (XPress data.position)
        ]
        |> makeTransparent data.d.transparency
        |> move ( data.d.x, data.d.y )


backgroundDebugScreen model =
    if options.debug then
        group
            [ rect 250 128 |> filled black
            , rect 248 126 |> filled (rgb 253 255 219)
            , graphPaper 10 |> makeTransparent 0.4

            -- Test
            , text ("Change in time step (Speed) " ++ (stringOfAniTime (movementSpeed model.aniTime)))
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
            ]
    else
        group []


myShapes model =
    [ backgroundDebugScreen model
    , makeButton model { face = face1, transparency = model.topNote.transparency, n = 0, position = Top }
    , makeButton model { face = face2, transparency = model.topRightNote.transparency, n = 1, position = TopRight }
    , makeButton model { face = face3, transparency = model.bottomRightNote.transparency, n = 2, position = BottomRight }
    , makeButton model { face = face4, transparency = model.bottomNote.transparency, n = 3, position = Bottom }
    , makeButton model { face = face5, transparency = model.bottomLeftNote.transparency, n = 4, position = BottomLeft }
    , makeButton model { face = face6, transparency = model.topLeftNote.transparency, n = 5, position = TopLeft }
    , makeNote { d = model.topNote, text = ".", position = Top }
    , makeNote { d = model.topRightNote, text = ".", position = TopRight }
    , makeNote { d = model.bottomRightNote, text = ".", position = BottomRight }
    , makeNote { d = model.bottomNote, text = ".", position = Bottom }
    , makeNote { d = model.bottomLeftNote, text = ".", position = BottomLeft }
    , makeNote { d = model.topNote, text = ".", position = TopLeft }

    -- Center Face
    , group
        [ circle 11
            |> filled backgroundPerson
        , face4
        , myCircleOutline
            |> makeTransparent (middleTransparency model.aniTimeExpand)
        ]
        |> move ( -25 - circleOfSixRadius, -75 + startingY )
        |> notifyTap ContactsPress
        |> scale 0.5

    -- Center Face
    , group
        [ circle 11
            |> filled backgroundPerson
        , face4
        , myCircleOutline
            |> makeTransparent 0
        ]
        |> move ( 0, -20 )
        |> makeTransparent (middleTransparency model.aniTimeExpand)
    ]



-- END OF MODEL
-- DEFINING VARIABLES


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
    let
        circleAt r to =
            circle r
                |> filled contactPersonColour
                |> move to
    in
        [ circle 10 |> filled backgroundPerson

        -- Main body
        , curve ( 0, 0 ) [ Pull ( 5, 10 ) ( 10, 0 ) ]
            |> filled contactPersonColour
            |> move ( -5, -8 )

        -- Head
        , circleAt 4 ( 0, 0 )

        -- Left Side
        , circleAt 2 ( 0, -8.1 )
        , circleAt 2 ( -1, -7.9 )
        , circleAt 2 ( -2, -7.8 )
        , circleAt 1 ( -3.8, -8.1 )
        , circleAt 0.5 ( -4.5, -8.1 )
        , circleAt 0.3 ( -4.9, -8.4 )
        , circleAt 0.3 ( -4.7, -8.6 )
        , circleAt 0.5 ( -3.5, -8.8 )

        -- Right Side
        , circleAt 2 ( 0, -8.1 )
        , circleAt 2 ( 1, -7.9 )
        , circleAt 2 ( 2, -7.8 )
        , circleAt 1 ( 3.8, -8.1 )
        , circleAt 0.5 ( 4.5, -8.1 )
        , circleAt 0.3 ( 4.9, -8.4 )
        , circleAt 0.3 ( 4.7, -8.6 )
        , circleAt 0.5 ( 3.5, -8.8 )
        ]

module AvatarAssets exposing (..)

import GraphicSVG exposing (..)


backgroundPerson =
    rgb 200 80 75


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


face4 =
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

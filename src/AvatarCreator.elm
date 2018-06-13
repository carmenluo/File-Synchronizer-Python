module AvatarCreator exposing (..)

import GraphicSVG exposing (..)


{-

-}
type Affect = Sad Float
            | Happy Float
            | Angry Float
            | Frustratated Float
            | Vibrant Float
            | Hopefull Float

theFace : Affect -> Model -> Shape (GraphicSVG.Msg Msg)
theFace affect model =
    let
        mouth = case affect of
            Sad x ->
                curve (-50,-20) [Pull (0,(x-10)/3) (50, -20)]
            Happy x -> 
                curve (-50,-30) [Pull (0,(-190-x*2)/2) (50, -30)]
            _ ->
                curve (-50,-20) [Pull (0,-30) (50, -20)]
                
    in
      group [
              --THIS IS THE WHOLE FACE
                    --The portion of the hair that shows behind the head
                    (myHairBack model.hairBack model)
                        |> notifyTap ClickHair

                   ,ears model

                   --The oval that makes up the face
                   ,oval 143 156
                        |> filled black
                        |> move (0,-7)
                   ,oval 140 153
                        |> filled (mySkinColour model.skinColour)
                        |> move (0,-7)
                        |> notifyTap ClickSkin

                   ,freckles |> move (-40,-20)
                   ,freckles |> move (40,-20)


                   ,myEyeShape model.eyeShape model

{- This is where other accessories go that can be accessed by
   removing the uncommenting the code.
-}

                   --,blush |> move (-40,-20)
                   --,blush |> move (40,-20)
                   --,hipsterGlasses |> move (0,10)
                   --,phone
                 ,myExtra model.extra


                 --This is the curve for the mouth
                 ,mouth 
                      |> outlined (solid 3) red
                      |> move (0,-40)
                      |> scale 0.4

                --The portion of the hair that shows infront of the face
                ,(myHairFront model.hairFront model)
                        |> notifyTap ClickHair
                        |> move(0,60)
                
                         ] |> scale 0.4 --Scale the whole face


view : Model -> List (Shape (GraphicSVG.Msg Msg))
view model =
    [
                theFace (Happy 0) model
                ,group [   roundedRect 35 17 5 |> filled black
                            , roundedRect 33 15 4 |> filled (rgb 56 173 180)
                            , text "Hair Colour"
                                |> filled black
                                |> scale 0.5
                                |> move (-14,-2)
                          ] |> move (-20,-55)
                            |> scale 0.9
                            |> notifyTap ClickHairColour

                ,group [   roundedRect 35 17 5 |> filled black
                            , roundedRect 33 15 4 |> filled (rgb 56 173 180)
                            , text "Highlight"
                                |> filled black
                                |> scale 0.5
                                |> move (-11,2)
                             , text "Colour"
                                |> filled black
                                |> scale 0.5
                                |> move (-8,-5)
                          ] |> move (20,-55)
                            |> scale 0.9
                            |> notifyTap ClickHighlightColour

                  ,group [   roundedRect 22 17 5 |> filled black
                                  |> move (-8,0)
                            , roundedRect 20 15 4 |> filled (rgb 56 173 180)
                                  |> move (-8,0)
                            , text "Eyes"
                                |> filled black
                                |> scale 0.5
                                |> move (-14,-2)
                          ] |> move (60,-55)
                            |> scale 0.9
                            |> notifyTap ClickEye

                ,group [roundedRect 22 17 5 |> filled black
                                  |> move (-8,0)
                            , roundedRect 20 15 4 |> filled (rgb 56 173 180)
                                  |> move (-8,0)
                            , text "Extras"
                                |> filled black
                                |> scale 0.5
                                |> move (-16,-2)
                          ] |> move (-45,-55)
                            |> scale 0.9
                            |> notifyTap ClickExtra

                   ,group [ text "Tap on the face"
                                  |> filled black
                                  |> move (-5,0)
                          , text "to change skin"
                                  |> filled black
                                  |> move (-3,-10)
                          , text "colour"
                                  |> filled black
                                  |> move (10,-22)
                          , curve (0,-10) [Pull (0,-40) (40,-40)]
                                  |> outlined (solid 1) black
                                  |> move (20,-20)
                          , rect 1 10
                                  |> filled black
                                  |> rotate (degrees 35)
                                  |> move (62,-55.5)
                          , rect 1 10
                                  |> filled black
                                  |> rotate (degrees -35)
                                  |> move (62,-64.5)

                          ] |>  scale 0.5 |> move (-91,10)

                    ,group [ text "Tap on the eyes"
                                  |> filled black
                          , text "to change eye colour"
                                  |> filled black
                                  |> move (-10,-10)
                          , curve (0,0) [Pull (0,-40) (40,-40)]
                                  |> outlined (solid 1) black
                                  |> move (40,-20)
                                  |> mirrorX
                          , rect 1 10
                                  |> filled black
                                  |> rotate (degrees 35)
                                  |> move (0,-65)
                          , rect 1 10
                                  |> filled black
                                  |> rotate (degrees -35)
                                  |> move (0,-56)

                          ] |>  scale 0.5 |> move (50,35)

                  , group [ text "Tap on the hair"
                                  |> filled black
                          , text "to change hairstyles"
                                  |> filled black
                                  |> move (-10,-10)
                          ,group[
                           curve (0,0) [Pull (0,-40) (40,-40)]
                                  |> outlined (solid 1) black
                                  |> move (20,-20)
                          , rect 1 10
                                  |> filled black
                                  |> rotate (degrees 35)
                                  |> move (62,-55.5)
                          , rect 1 10
                                  |> filled black
                                  |> rotate (degrees -35)
                                  |> move (62,-64.5) ] |>  scale 0.8
                                              |> move (20,0)

                            ] |> scale 0.5
                              |> move (-90,50)

                   ]

eyes1 model = group [     eyebrows model
                          ,filled black (circle 20)|> move (-27,0) --Left eye
                          ,filled white (circle 18) |> move (-27,0)
                          ,circle 15
                              |> filled (myEyeColour model.eyeColour)
                              |> move (-24,0)
                          ,filled black (circle 12) |> move (-21,2)
                          ,filled white (circle 6) |> move (-18,3)
                          ,filled black (circle 20)|> move (27,0) --Right eye
                          ,filled white (circle 18) |> move (27,0)
                          ,circle 15
                              |> filled (myEyeColour model.eyeColour)
                              |> move (30,0)
                          ,filled black (circle 12) |> move (32,2)
                          ,filled white (circle 6) |> move (35,3)
                        ] |> notifyTap Click

eyes2 model = group [   eyebrows model
                                |> move (0,-10)
                       ,wedge 21 0.5 |> filled black |> rotate (degrees -90) |> move(-27,1)
                       , wedge 18 0.5 |> filled white |> rotate (degrees -90)|> move(-27,0)
                       , wedge 15 0.5 |> filled (myEyeColour model.eyeColour)
                                      |> rotate (degrees -90)|> move(-24,0)
                       , wedge 12 0.5 |> filled black |> rotate (degrees -90)|> move(-22,0)
                       , wedge 4 0.5|> filled white |> rotate (degrees -90)|> move (-28,0)

                       , wedge 21 0.5 |> filled black |> rotate (degrees -90) |> move(27,1)
                       , wedge 18 0.5 |> filled white |> rotate (degrees -90)|> move(27,0)
                       , wedge 15 0.5 |> filled (myEyeColour model.eyeColour) |> rotate (degrees -90)|> move(30,0)
                       , wedge 12 0.5 |> filled black |> rotate (degrees -90)|> move(31,0)
                       , wedge 4 0.5|> filled white |> rotate (degrees -90)|> move (25,0)
                       ] |> notifyTap Click

eyes3 model = group [ wedge 21 0.5 |> filled black |> rotate (degrees 90) |> move(-27,-1)
                        ,eyebrows model

                       , wedge 18 0.5 |> filled white |> rotate (degrees 90)|> move(-27,0)
                       , wedge 15 0.5 |> filled (myEyeColour model.eyeColour) |> rotate (degrees 90)|> move(-24,0)
                       , wedge 12 0.5 |> filled black |> rotate (degrees 90)|> move(-22,-1)
                       , wedge 4 0.5  |> filled white |> rotate (degrees 90)|> move (-31,0)

                       , wedge 21 0.5 |> filled black |> rotate (degrees 90) |> move(27,-1)
                       , wedge 18 0.5 |> filled white |> rotate (degrees 90)|> move(27,0)
                       , wedge 15 0.5 |> filled (myEyeColour model.eyeColour)
                                      |> rotate (degrees 90)
                                      |> move(30,0)
                       , wedge 12 0.5 |> filled black |> rotate (degrees 90)|> move(31,-1)
                       , wedge 4 0.5|> filled white |> rotate (degrees 90)|> move (22,0)
                       ] |> move (0,-15) |> notifyTap Click


almond colour = group[ curve (15,-15) [Pull (-17,-13) (-15,15)] |> filled colour
                     , curve (15,-15) [Pull (15,20) (-15,15)] |> filled colour]

hollowAlmond thickness colour = group[ curve (15,-15) [Pull (-17,-13) (-15,15)] |> outlined (solid thickness) colour
                     , curve (15,-15) [Pull (15,20) (-15,15)] |> outlined (solid thickness) colour]

eyes4 model = group [ almond white |> move (-30,-5) |> rotate (degrees 20)
                     , eyebrows model
                            |> move (0,-5)
                     , almond white |> move (30,-5) |> rotate (degrees -20) |> scaleX(-1)
                     , circle 11 |> filled (myEyeColour model.eyeColour) |> move (-29,-3)
                     , circle 11 |> filled  (myEyeColour model.eyeColour) |> move (34,-3)
                     , circle 8 |> filled black |> move (-28,-3)
                     , circle 8 |> filled black |> move (35,-3)
                     , circle 2 |> filled white |> move (38,0)
                     , circle 2 |> filled white |> move (-25,0)
                     , hollowAlmond 1.85 black |> move(-30,-5) |> rotate (degrees 20)
                     , hollowAlmond 1.85 black |> move(30,-5) |> rotate (degrees -20) |> scaleX(-1)
                     ] |> notifyTap Click

ears model = group [
                   group[
                       oval 24 44 |> filled black
                      ,oval 20 40 |> filled (mySkinColour model.skinColour)
                         ] |> move (-70,0)
                           |> notifyTap ClickSkin
                   ,group[
                       oval 24 44 |> filled black
                      ,oval 20 40 |> filled (mySkinColour model.skinColour)
                         ] |> move (70,0)
                           |> notifyTap ClickSkin

                    ]

eyebrows model = group [
                    curve (0,0) [Pull (20,10) (40,0)]
                                |> filled black
                                |> move (-47,23)
                    ,curve (0,0) [Pull (20,10) (40,0)]
                                |> filled black
                                |> move (7,23)

                    ,curve (4,0) [Pull (20,5) (36,0)]
                                |> filled (myHairColour model.hairColour)
                                |> move (-47,24)
                    ,curve (4,0) [Pull (20,5) (36,0)]
                                |> filled (myHairColour model.hairColour)
                                |> move (7,24)
                          ]

phone = group [ polygon [(0,0), (20,-5), (20,-55), (0,-50)] |> filled black
               ,polygon [(0,0), (20,-5), (20,-55), (0,-50)] |> filled grey
                    |> scale 0.9
                    |> move (2,-2)
              ,polygon [(0,0), (20,-5), (20,-20), (0,-15)] |> filled green
                    |> scale 0.9
                    |> move (2,-2)
              ,polygon [(0,-25), (20,-30), (20,-55), (0,-50)] |> filled white
                    |> scale 0.9
                    |> move (2,-2)

             ,text "CALLING"
                      |> filled white
                      |> rotate (degrees -13)
                      |> scale 0.3
                      |> move (3.5,-10)


              ,text "555-0122"
                      |> filled black
                      |> rotate (degrees -13)
                      |> scale 0.3
                      |> move (4,-22)
            ,group [
              text "1"
                      |> filled black
                      |> rotate (degrees -13)
                      |> scale 0.3
                      |> move (4,-31)
              ,text "2"
                      |> filled black
                      |> rotate (degrees -13)
                      |> scale 0.3
                      |> move (9,-32.25)
              ,text "3"
                      |> filled black
                      |> rotate (degrees -13)
                      |> scale 0.3
                      |> move (14,-33.5)

              ,text "4"
                      |> filled black
                      |> rotate (degrees -13)
                      |> scale 0.3
                      |> move (4,-35)
              ,text "5"
                      |> filled black
                      |> rotate (degrees -13)
                      |> scale 0.3
                      |> move (9,-36.25)
              ,text "6"
                      |> filled black
                      |> rotate (degrees -13)
                      |> scale 0.3
                      |> move (14,-37.5)

              ,text "7"
                      |> filled black
                      |> rotate (degrees -13)
                      |> scale 0.3
                      |> move (4,-39)
              ,text "8"
                      |> filled black
                      |> rotate (degrees -13)
                      |> scale 0.3
                      |> move (9,-40.25)
              ,text "9"
                      |> filled black
                      |> rotate (degrees -13)
                      |> scale 0.3
                      |> move (14,-41.5)

              ,text "*"
                      |> filled black
                      |> rotate (degrees -13)
                      |> scale 0.3
                      |> move (4,-43)
              ,text "0"
                      |> filled black
                      |> rotate (degrees -13)
                      |> scale 0.3
                      |> move (9,-44.25)
              ,text "#"
                      |> filled black
                      |> rotate (degrees -13)
                      |> scale 0.3
                      |> move (14,-45.5)
                        ] |> move (1,0)


                ] |> move (-115,20)
                  |> rotate (degrees 5)
                  |> scale (1.5)

blush = group [circle 15 |> filled pinkie]

hipsterGlasses = group[ roundedRect 40 33 4 |> outlined (solid 4) black |> move(-29,-10)
                             , roundedRect 40 33 4 |> outlined (solid 4) black |> move(29,-10)
                             , curve (-2,0) [Pull (6,5) (14,0)] |> outlined (solid 3) black |> move (-6,-3)
                             , curve (-2,0) [Pull (10,-5) (22,0)] |> outlined (solid 3) black |> move (-10,7)
                             , rect 15 6 |> filled black |> move(0,2)
                             , rect 6 5 |> filled black |> move (48,6)
                             , rect 6 5 |> filled black |> move (-48,6)
                             , oval 3 2 |> filled lightGrey |> move (-48.5,6)
                             , oval 3 2 |> filled lightGrey |> move (48.5,6)]
                                    |> move (0,8)
--Basic Curly hair
hair1f model = group [

             oval 63 43 |> filled black |> move(0,5)
              ,oval 63 43 |> filled black
                      |> move (-30,0) |> rotate (degrees 20)
              ,oval 63 43 |> filled black
              |> move (30,0) |> rotate (degrees 160)
              ,oval 58 38 |> filled black
              |> move (55,-15) |> rotate (degrees 130)
              ,oval 58 38 |> filled black
              |> move (-55,-15) |> rotate (degrees 50)


              ,oval 60 40 |> filled (myHairColour model.hairColour) |> move(0,5)
              ,oval 60 40 |> filled (myHairColour model.hairColour)
                      |> move (-30,0) |> rotate (degrees 20)
              ,oval 60 40 |> filled (myHairColour model.hairColour)
              |> move (30,0) |> rotate (degrees 160)
              ,oval 55 35 |> filled (myHairColour model.hairColour)
              |> move (55,-15) |> rotate (degrees 130)
              ,oval 55 35 |> filled (myHairColour model.hairColour)
              |> move (-55,-15) |> rotate (degrees 50)
              ]


hair1b model = group []

--Dreadlocks
hair2f model= group [
              oval 17 27 |> filled black|> move (-70,-35) |> rotate (degrees 60)
              ,oval 17 27 |> filled black |> move (70,-35) |> rotate (degrees -60)

              ,oval 12 22 |> filled (myHairColour model.hairColour)|> move (-70,-35) |> rotate (degrees 60)
              ,oval 12 22 |> filled (myHairColour model.hairColour) |> move (70,-35) |> rotate (degrees -60)

              ,oval 20 30 |> filled black |> move (65,-25) |> rotate (degrees -60)
              ,oval 15 25 |> filled (myHairColour model.hairColour) |> move (65,-25) |> rotate (degrees -60)

              ,oval 20 30 |> filled black |> move (-65,-25) |> rotate (degrees 60)
              ,oval 15 25 |> filled (myHairColour model.hairColour) |> move (-65,-25) |> rotate (degrees 60)

              ,oval 20 35 |> filled black |> move (-55,-15) |> rotate (degrees 50)
              ,oval 15 30 |> filled (myHairColour model.hairColour) |> move (-55,-15) |> rotate (degrees 50)

              ,oval 20 35 |> filled black |> move (-45,-5) |> rotate (degrees 40)
              ,oval 15 30 |> filled (myHairColour model.hairColour) |> move (-45,-5) |> rotate (degrees 40)

              ,oval 20 35 |> filled black |> move (55,-15) |> rotate (degrees 130)
              ,oval 15 30 |> filled (myHairColour model.hairColour) |> move (55,-15) |> rotate (degrees 130)

              ,oval 20 35 |> filled black |> move (45,-5) |> rotate (degrees -40)
              ,oval 15 30 |> filled (myHairColour model.hairColour) |> move (45,-5) |> rotate (degrees -40)

              ,oval 20 40 |> filled black |> move (30,0) |> rotate (degrees 160)
              ,oval 15 35 |> filled (myHairColour model.hairColour) |> move (30,0) |> rotate (degrees 160)

              ,oval 20 40 |> filled black |> move (-30,0) |> rotate (degrees 20)
              ,oval 15 35 |> filled (myHairColour model.hairColour) |> move (-30,0) |> rotate (degrees 20)

              ,oval 20 40 |> filled black |> move(-15,5) |> rotate (degrees 15)
              ,oval 15 35 |> filled (myHairColour model.hairColour) |> move(-15,5) |> rotate (degrees 15)

             ,oval 20 40 |> filled black |> move(15,5) |> rotate (degrees -15)
             ,oval 15 35 |> filled (myHairColour model.hairColour) |> move(15,5) |> rotate (degrees -15)
             ,oval 23 43 |> filled black |> move(0,7)
             ,oval 18 38 |> filled (myHairColour model.hairColour) |> move(0,7)
                ]


hair2b model = group[--Long hair
          roundedRect 18 75 10 |> filled black |> move (-70,-50)
          ,roundedRect 13 70 10 |> filled (myHairColour model.hairColour) |> move (-70,-50)

          ,roundedRect 18 75 10 |> filled black |> move (-55,-65)
          ,roundedRect 13 70 10 |> filled (myHairColour model.hairColour) |> move (-55,-65)

          ,roundedRect 18 75 10 |> filled black |> move (70,-50)
          ,roundedRect 13 70 10 |> filled (myHairColour model.hairColour) |> move (70,-50)

          ,roundedRect 18 75 10 |> filled black |> move (55,-65)
          ,roundedRect 13 70 10 |> filled (myHairColour model.hairColour) |> move (55,-65)

          ,roundedRect 18 75 10 |> filled black |> move (-38,-75) |> rotate (degrees -5)
          ,roundedRect 13 70 10 |> filled (myHairColour model.hairColour) |> move (-38,-75) |> rotate (degrees -5)

          ,roundedRect 18 75 10 |> filled black |> move (38,-75) |> rotate (degrees 5)
          ,roundedRect 13 70 10 |> filled (myHairColour model.hairColour) |> move (38,-75) |> rotate (degrees 5)


            --HIGHLIGHTS

          ,rect 0.8 60
                |> filled (highlights model.highlightColour model)
                |> move (-74.5,-50)
          ,rect 0.8 60
                |> filled (highlights model.highlightColour model)
                |> move (-70,-55)
          ,rect 0.8 60
                |> filled (highlights model.highlightColour model)
                |> move (-59,-65)
          ,rect 0.8 60
                |> filled (highlights model.highlightColour model)
                |> move (-54,-69)
          ,rect 0.8 60
                |> filled (highlights model.highlightColour model)
                |> move (-42,-75)
                |> rotate (degrees -5)
          ,rect 0.8 60
                |> filled (highlights model.highlightColour model)
                |> move (-37,-79)
                |> rotate (degrees -5)

          ,rect 0.8 60
                |> filled (highlights model.highlightColour model)
                |> move (74.5,-50)
          ,rect 0.8 60
                |> filled (highlights model.highlightColour model)
                |> move (70,-55)
          ,rect 0.8 60
                |> filled (highlights model.highlightColour model)
                |> move (59,-65)
          ,rect 0.8 60
                |> filled (highlights model.highlightColour model)
                |> move (54,-69)
          ,rect 0.8 60
                |> filled (highlights model.highlightColour model)
                |> move (42,-75)
                |> rotate (degrees 5)
          ,rect 0.8 60
                |> filled (highlights model.highlightColour model)
                |> move (37,-79)
                |> rotate (degrees 5)
          ]

--Long hair with bangs
hair3f model = group [
                    bangs model
                    |> scale 1.2
                    |> move (11,10)
                   ,bangs model
                    |> scale 1.2
                    |> move (-12,10)
                    |> mirrorX

                   ,circle 5
                    |> filled (myHairColour model.hairColour)
                    |> move(0,23.2)

                     --highlights
                   ,curve (0,0) [Pull (50,10) (40,50)]
                    |> outlined (solid 0.8) (highlights model.highlightColour model)
                    |> move (-75,-65)
                    |> scale 1.6
                    ,curve (0,0) [Pull (40,10) (40,50)]
                    |> outlined (solid 0.8) (highlights model.highlightColour model)
                    |> move (-72,-55)
                    |> scale 1.3

                    ,curve (0,0) [Pull (50,10) (40,50)]
                    |> outlined (solid 0.8) (highlights model.highlightColour model)
                    |> move (75,-65)
                    |> scale 1.6
                    |> mirrorX
                    ,curve (0,0) [Pull (40,10) (40,50)]
                    |> outlined (solid 0.8) (highlights model.highlightColour model)
                    |> move (72,-55)
                    |> scale 1.3
                    |> mirrorX
               ]

hair3b model = group [

               roundedRect 148 143 30
                    |> filled black
                    |> move (0,-50)

               ,roundedRect 145 140 30
                    |> filled (myHairColour model.hairColour)
                    |> move (0,-50)
                , rect 1 140
                    |> filled (highlights model.highlightColour model)
                    |> move (-60,-30)
                , rect 1 150
                    |> filled (highlights model.highlightColour model)
                    |> move (-50,-40)
                , rect 1 150
                    |> filled (highlights model.highlightColour model)
                    |> move (-30,-40)
                , rect 1 140
                    |> filled (highlights model.highlightColour model)
                    |> move (60,-30)
                , rect 1 150
                    |> filled (highlights model.highlightColour model)
                    |> move (50,-40)
                , rect 1 150
                    |> filled (highlights model.highlightColour model)
                    |> move (30,-40)
                , rect 33 153
                    |> filled black
                    |> move (0,-45)

                , rect 30 150
                    |> filled white
                    |> move (0,-50)


               ]

bangs model = group [
                curve (0,0) [Pull (10,50) (40,50)]
                    |> filled black
                    |> move (-78,-68)
                    |> scale 1.7
                ,curve (0,0) [Pull (50,10) (40,50)]
                    |> filled black
                    |> move (-78,-68)
                    |> scale 1.7

                ,curve (0,0) [Pull (10,50) (40,50)]
                    |> filled (myHairColour model.hairColour)
                    |> move (-75,-64.5)
                    |> scale 1.6
                ,curve (0,0) [Pull (50,10) (40,50)]
                    |> filled (myHairColour model.hairColour)
                    |> move (-76,-64.5)
                    |> scale 1.6
                    ]

--Long curly hair
hair4f model = group [
                oval 60 95
                      |> filled black
                      |> rotate (degrees -65)
                      |> move (-20,-15)
                ,oval 20 30
                      |> filled black
                      |> rotate (degrees 60)
                      |> move (27,0)
                ,oval 60 95
                      |> filled (myHairColour model.hairColour)
                      |> rotate (degrees -65)
                      |> move (-20,-13)
                ,oval 20 30
                      |> filled (myHairColour model.hairColour)
                      |> rotate (degrees 60)
                      |> move (27,2)

                --HIGHLIGHTS
                ,curve (14,-10) [Pull (-10,-50) (-54,-38)]
                      |> outlined (solid 1) (highlights model.highlightColour model)
                ,curve (14,-10) [Pull (-10,-50) (-54,-38)]
                      |> outlined (solid 1) (highlights model.highlightColour model)
                      |> scale 0.8
                      |> move (-10,0)

                  ]

hair4b model = group [
            -- top of the head
            oval 154 74
                |> filled black
                |> move (0,55)
            ,oval 54 74
                |> filled black
                |> move (55,40)
                |> rotate (degrees -40)
            ,oval 54 74
                |> filled black
                |> move (-55,40)
                |> rotate (degrees 40)
            ,oval 74 184
                |> filled black
                |> rotate (degrees 5)
                |> move (40,-15)
            ,oval 44 74
                |> filled black
                |> move (80,10)
                |> rotate (degrees 5)
            ,oval 74 184
                |> filled black
                |> rotate (degrees -5)
                |> move (-40,-15)
            ,oval 44 74
                |> filled black
                |> move (-80,10)
                |> rotate (degrees -5)
            ,oval 74 184
                |> filled black
                |> rotate (degrees -25)
                |> move (-50,0)
            --long
           ,oval 74 184
                |> filled black
                |> rotate (degrees 25)
                |> move (50,0)
           ,oval 150 70
                |> filled (myHairColour model.hairColour)
                |> move (0,55)
           ,oval 70 180
                |> filled (myHairColour model.hairColour)
                |> rotate (degrees 25)
                |> move (50,0)
            --long
            ,oval 70 180
                |> filled (myHairColour model.hairColour)
                |> rotate (degrees 5)
                |> move (40,-15)
            --long
            ,oval 40 70
                |> filled (myHairColour model.hairColour)
                |> move (-80,10)
                |> rotate (degrees -5)
           ,oval 70 180
                |> filled (myHairColour model.hairColour)
                |> rotate (degrees -25)
                |> move (-50,0)
            --long
            ,oval 70 180
                |> filled (myHairColour model.hairColour)
                |> rotate (degrees -5)
                |> move (-40,-15)
            --side head
            ,oval 50 70
                |> filled (myHairColour model.hairColour)
                |> move (55,40)
                |> rotate (degrees -40)
            --side head
            ,oval 50 70
                |> filled (myHairColour model.hairColour)
                |> move (-55,40)
                |> rotate (degrees 40)
            ,oval 40 70
                |> filled (myHairColour model.hairColour)
                |> move (80,10)
                |> rotate (degrees 5)

            --HIGHLIGHTS
            ,curve (0,0) [Pull (-10,-20) (0,-40)]
                  |> outlined (solid 1) (highlights model.highlightColour model)
                  |> move (-90,32)
            ,curve (0,0) [Pull (-10,-20) (0,-40)]
                  |> outlined (solid 1) (highlights model.highlightColour model)
                  |> move (90,32)
                  |> mirrorX
            ,curve (-2,0) [Pull (-10,-55) (20,-45)]
                  |> outlined (solid 1) (highlights model.highlightColour model)
                  |> move (-90,-30)
            ,curve (-2,0) [Pull (-10,-55) (20,-45)]
                  |> outlined (solid 1) (highlights model.highlightColour model)
                  |> move (90,-30)
                  |> mirrorX

                ]
--Two buns
hair5f model = group [

                      curve (0,5) [Pull (-20,-50) (0,-105)]
                      |> filled black
                      |> move (-68,-28)
                      |> rotate (degrees 5)

                     ,curve (0,5) [Pull (-20,-50) (0,-105)]
                      |> filled black
                      |> move (68,-28)
                      |> rotate (degrees -5)
                      |> mirrorX

                      , oval 105 45
                      |> filled black
                      |> rotate (degrees 35)
                      |> move (-30,-20)

                      , oval 105 45
                      |> filled black
                      |> rotate (degrees -35)
                      |> move (30,-20)





                      ,curve (0,0) [Pull (-15,-50) (0,-100)]
                      |> filled (myHairColour model.hairColour)
                      |> move (-69,-28)
                      |> rotate (degrees 5)
                      ,curve (0,0) [Pull (-15,-50) (0,-100)]
                      |> filled (myHairColour model.hairColour)
                      |> move (69,-28)
                      |> rotate (degrees -5)
                      |>mirrorX






                      , oval 102 42
                      |> filled (myHairColour model.hairColour)
                      |> rotate (degrees 35)
                      |> move (-30,-20)

                      , oval 102 42
                      |> filled (myHairColour model.hairColour)
                      |> rotate (degrees -35)
                      |> move (30,-20)
                    ]

hair5b model = group [
                  circle 32 |> filled black |> move (-55,60)
                  ,circle 30 |> filled (myHairColour model.hairColour)  |> move (-55,60)
                  ,circle 32 |> filled black |> move (55,60)
                  ,circle 30 |> filled (myHairColour model.hairColour)  |> move (55,60)
                     ]
--Short side part
hair6f model = group [ oval 42 102
                            |> filled black
                            |> rotate (degrees -55)
                            |> move (-20,-10)


                       ,group [curve (0,0) [Pull (10,-10) (-20,-30)]
                                   |> filled black
                               ,curve (0,0) [Pull (-30,-35) (-20,-30)]
                                    |> filled black
                              ] |> move (26,13)
                                |> rotate (degrees 13)
                                |> scale 1.3


                        ,group [curve (0,0) [Pull (10,-10) (-20,-30)]
                                   |> filled (myHairColour model.hairColour)
                                ,curve (0,0) [Pull (-30,-35) (-20,-30)]
                                    |> filled (myHairColour model.hairColour)
                              ] |> move (26,10)
                                |> rotate (degrees 13)

                        ,oval 40 100
                            |> filled (myHairColour model.hairColour)
                            |> rotate (degrees -55)
                            |> move (-20,-8)

                        ,oval 10 10
                              |> filled (myHairColour model.hairColour)
                              |> move (28.5,9.5)
                       ,oval 5 11
                              |> filled (myHairColour model.hairColour)
                              |> move (23,13.5)


                         ]

hair6b model = group [oval 73 123
                            |> filled black
                            |> rotate (degrees -55)
                            |> move (-25,50)
                      ,oval 53 83
                            |> filled black
                            |> rotate (degrees 40)
                            |> move (50,45)


                      ,oval 70 120
                            |> filled (myHairColour model.hairColour)
                            |> rotate (degrees -55)
                            |> move (-25,50)
                      ,oval 50 80
                            |> filled (myHairColour model.hairColour)
                            |> rotate (degrees 40)
                            |> move (50,45)


                            ]

--Short bob
hair7f model = group [ oval 60 122
                      |> filled black
                      |> rotate (degrees -65)
                      |> move (-20,-15)
                ,oval 20 30
                      |> filled black
                      |> rotate (degrees 60)
                      |> move (35,0)
                ,oval 60 120
                      |> filled (myHairColour model.hairColour)
                      |> rotate (degrees -65)
                      |> move (-20,-13)
                ,oval 20 30
                      |> filled (myHairColour model.hairColour)
                      |> rotate (degrees 60)
                      |> move (35,2)

                      ] |> scale 0.8
hair7b model = group [
                      oval 73 123
                            |> filled (black)
                            |> rotate (degrees -55)
                            |> move (-25,50)
                      ,oval 73 123
                            |> filled black
                            |> rotate (degrees 55)
                            |> move (25,50)
                      ,oval 73 123
                            |> filled black
                            |> move (-50,0)
                      ,oval 73 123
                            |> filled black
                            |> move (50,0)


                      ,oval 70 120
                            |> filled (myHairColour model.hairColour)
                            |> rotate (degrees -55)
                            |> move (-25,50)
                      ,oval 70 120
                            |> filled (myHairColour model.hairColour)
                            |> rotate (degrees 55)
                            |> move (25,50)
                      ,oval 70 120
                            |> filled (myHairColour model.hairColour)
                            |> move (-50,0)
                      ,oval 70 120
                            |> filled (myHairColour model.hairColour)
                            |> move (50,0)

                      ]
--Close army cut
hair8f model = group [
                      oval 100 30 |> filled black |> move (0,-2)
                      ,oval 100 30 |> filled (myHairColour model.hairColour)
                      , circle 5
                            |> filled (myHairColour model.hairColour)
                            |> move (-46,-2)
                      , circle 5
                            |> filled (myHairColour model.hairColour)
                            |> move (46,-2)

                      --HIGHLIGHTS
                      ,curve (-15,0) [Pull (-30,-10) (-45,-40)]
                            |> outlined (solid 1) (highlights model.highlightColour model)
                            |> move (-25,5)
                      ,curve (-15,0) [Pull (-30,-10) (-45,-40)]
                            |> outlined (solid 1) (highlights model.highlightColour model)
                            |> move (25,5)
                            |> mirrorX



                      ]
hair8b model = group [oval 73 123
                            |> filled (black)
                            |> rotate (degrees -55)
                            |> move (-25,50)
                      ,oval 73 123
                            |> filled black
                            |> rotate (degrees 55)
                            |> move (25,50)



                      ,oval 70 120
                            |> filled (myHairColour model.hairColour)
                            |> rotate (degrees -55)
                            |> move (-25,50)
                      ,oval 70 120
                            |> filled (myHairColour model.hairColour)
                            |> rotate (degrees 55)
                            |> move (25,50)

                        --HIGHLIGHTS
                      ,curve (0,0) [Pull (-30,-10) (-45,-50)]
                            |> outlined (solid 1) (highlights model.highlightColour model)
                            |> move (-30,80)

                      ,curve (0,0) [Pull (-30,-10) (-45,-50)]
                            |> outlined (solid 1) (highlights model.highlightColour model)
                            |> move (30,80)
                            |> mirrorX

                      ]
--Tall hair
hair9f model = group [oval 130 40 |> filled black |> move (0,-8)
                      ,oval 130 40
                            |> filled (myHairColour model.hairColour)
                            |> move (0,-6)


                      , circle 7
                            |> filled (myHairColour model.hairColour)
                            |> move (-60,-12)
                      , circle 7
                            |> filled (myHairColour model.hairColour)
                            |> move (60,-12)
                      ]
hair9b model = group [ oval 153 103 |> filled black
                            |> move (0,40)

                      ,oval 150 100 |> filled (myHairColour model.hairColour)
                            |> move (0,40)



                          ]
--Pony tail
hair10f model = group [oval 120 40
                            |> filled black
                            |> move (0,-7.5)
                      ,oval 120 40
                            |> filled (myHairColour model.hairColour)
                            |> move (0,-6)
                      , circle 5
                            |> filled (myHairColour model.hairColour)
                            |> move (-58,-12)
                      , circle 5
                            |> filled (myHairColour model.hairColour)
                            |> move (58,-12)
                      ,rect 1.4 69 |> filled black
                                |> move (-70,-123)

                      ,curve (0,1) [Pull (-10,15) (-31,10)]
                            |> outlined (solid 1) black
                            |> move (-15,32)

                          --HIGHLIGHTS
                      ,curve (-15,0) [Pull (-30,-10) (-45,-40)]
                            |> outlined (solid 1) (highlights model.highlightColour model)
                            |> move (-25,5)
                      ,curve (-15,0) [Pull (-30,-10) (-45,-40)]
                            |> outlined (solid 1) (highlights model.highlightColour model)
                            |> move (25,5)
                            |> mirrorX


                            ]
hair10b model = group [

                      oval 113 153
                            |> filled black
                            |> move (-35,35)
                            |> rotate (degrees -10)

                      ,curve (0,0) [Pull (-40,-10) (0,-120) ]
                            |> filled black
                            |> move (-70,35)
                            |> scale 1.1



                      ,oval 110 150
                            |> filled (myHairColour model.hairColour)
                            |> move (-35,35)
                            |> rotate (degrees -10)

                      ,curve (0,0) [Pull (-40,-10) (0,-120) ]
                            |> filled (myHairColour model.hairColour)
                            |> move (-70,30)


                      ,oval 73 123
                            |> filled (black)
                            |> rotate (degrees -55)
                            |> move (-25,50)
                      ,oval 73 123
                            |> filled black
                            |> rotate (degrees 55)
                            |> move (25,50)



                      ,oval 70 120
                            |> filled (myHairColour model.hairColour)
                            |> rotate (degrees -55)
                            |> move (-25,50)
                      ,oval 70 120
                            |> filled (myHairColour model.hairColour)
                            |> rotate (degrees 55)
                            |> move (25,50)

                        --HIGHLIGHTS
                      ,curve (0,0) [Pull (-30,-10) (-45,-50)]
                            |> outlined (solid 1) (highlights model.highlightColour model)
                            |> move (-30,80)

                      ,curve (0,0) [Pull (-30,-10) (-45,-50)]
                            |> outlined (solid 1) (highlights model.highlightColour model)
                            |> move (30,80)
                            |> mirrorX

                      ]
--Mid length
hair11f model = group [
                oval 55 95
                      |> filled black
                      |> rotate (degrees -45)
                      |> move (-30,-15)
                ,oval 45 90
                      |> filled black
                      |> rotate (degrees 50)
                      |> move (27,-8)
                ,oval 55 95
                      |> filled (myHairColour model.hairColour)
                      |> rotate (degrees -45)
                      |> move (-30,-13)
                ,oval 45 90
                      |> filled (myHairColour model.hairColour)
                      |> rotate (degrees 50)
                      |> move (26,-6)

                  ]

hair11b model = group [
           oval 58 25
               |> filled black
               |> move (0,80)

           ,oval 74 184
                |> filled black
                |> rotate (degrees 25)
                |> move (50,5)

           ,oval 74 184
                |> filled black
                |> rotate (degrees 5)
                |> move (40,-10)

           ,oval 74 184
                |> filled black
                |> rotate (degrees -25)
                |> move (-50,5)

           ,oval 74 184
                |> filled black
                |> rotate (degrees -5)
                |> move (-40,-10)

           ,oval 58 25
               |> filled (myHairColour model.hairColour)
               |> move (0,78)

           ,oval 70 180
                |> filled (myHairColour model.hairColour)
                |> rotate (degrees 25)
                |> move (50,5)
            ,oval 70 180
                |> filled (myHairColour model.hairColour)
                |> rotate (degrees 5)
                |> move (40,-10)


           ,oval 70 180
                |> filled (myHairColour model.hairColour)
                |> rotate (degrees -25)
                |> move (-50,5)
            ,oval 70 180
                |> filled (myHairColour model.hairColour)
                |> rotate (degrees -5)
                |> move (-40,-10)


                ]
--Side tufts
hair12f model = group [circle 19
                      |> filled black
                      |> move (55,-20)
                ,circle 14
                      |> filled black
                      |> move (62,-33)
                ,circle 14
                      |> filled black
                      |> move (42,-10)

                ,circle 19
                      |> filled black
                      |> move (-55,-20)
                ,circle 14
                      |> filled black
                      |> move (-62,-33)
                ,circle 14
                      |> filled black
                      |> move (-42,-10)



                ,circle 17
                      |> filled (myHairColour model.hairColour)
                      |> move (55,-20)
                ,circle 12
                      |> filled (myHairColour model.hairColour)
                      |> move (62,-33)
                ,circle 12
                      |> filled (myHairColour model.hairColour)
                      |> move (42,-10)

                ,circle 17
                      |> filled (myHairColour model.hairColour)
                      |> move (-55,-20)
                ,circle 12
                      |> filled (myHairColour model.hairColour)
                      |> move (-62,-33)
                ,circle 12
                      |> filled (myHairColour model.hairColour)
                      |> move (-42,-10)

                ]

hair12b model = group []

--One bun
hair13f model = group [ circle 19
                      |> filled black
                      |> move (55,-20)
                ,circle 14
                      |> filled black
                      |> move (62,-33)
                ,circle 18
                      |> filled black
                      |> move (42,-10)
                ,circle 20
                      |> filled black
                      |> move (30,-3)
                ,circle 18
                      |> filled black
                      |> move (15,5)
                ,circle 20
                      |> filled black
                      |> move (0,5)


                ,circle 19
                      |> filled black
                      |> move (-55,-20)
                ,circle 14
                      |> filled black
                      |> move (-62,-33)
                ,circle 18
                      |> filled black
                      |> move (-42,-10)
                ,circle 20
                      |> filled black
                      |> move (-30,-3)
                ,circle 18
                      |> filled black
                      |> move (-15,5)






                ,circle 17
                      |> filled (myHairColour model.hairColour)
                      |> move (55,-20)
                ,circle 12
                      |> filled (myHairColour model.hairColour)
                      |> move (62,-33)
                ,circle 16
                      |> filled (myHairColour model.hairColour)
                      |> move (42,-10)
                ,circle 18
                      |> filled (myHairColour model.hairColour)
                      |> move (30,-3)
                ,circle 16
                      |> filled (myHairColour model.hairColour)
                      |> move (15,5)
                ,circle 18
                      |> filled (myHairColour model.hairColour)
                      |> move (0,5)


                ,circle 17
                      |> filled (myHairColour model.hairColour)
                      |> move (-55,-20)
                ,circle 12
                      |> filled (myHairColour model.hairColour)
                      |> move (-62,-33)
                ,circle 16
                      |> filled (myHairColour model.hairColour)
                      |> move (-42,-10)
                ,circle 18
                      |> filled (myHairColour model.hairColour)
                      |> move (-30,-3)
                ,circle 16
                      |> filled (myHairColour model.hairColour)
                      |> move (-15,5)

                    ]

hair13b model = group [   circle 32 |> filled black |> move (0,80)
                  ,circle 30 |> filled (myHairColour model.hairColour) |> move (0,80)]



freckles = group [
            circle 1.3 |> filled brown1
            ,circle 1.3 |> filled brown1 |> move (-4,-4)
            ,circle 1.3 |> filled brown1 |> move (4,-4)]

nothing = group[]

-- COLOURS
skin = skin9

skin1 = rgb 255 223 196

skin2 = rgb 238 206 179

skin3 = rgb 225 170 100

skin4 = rgb 229 184 143

skin5 = rgb 231 158 109

skin6 = rgb 219 144 101

skin7 = rgb 186 108 73

skin8 = rgb 198 120 86

skin9 = rgb 130 72 24

--skin10 = rgb 146 106 45 -- weird

--skin11 = rgb 135 97 39

skin12 = rgb 124 80 26

skin13 = rgb 126 77 28

skin14 = rgb 137 78 26

skin15 = rgb 88 53 18

skin16 = rgb 62 43 19

--HAIR COLOURS
hair1  = rgb 68  25  0 -- Brown
hair2  = rgb 044 034 043 -- off black
hair3  = rgb 255 245 225 --white blone
hair4  = rgb 229 200 168 -- golden blonde
hair5  = rgb 216 192 120 -- v blonde
hair6  = rgb 242 218 145 -- Honey blonde
hair7  = rgb 181 082 057 -- light red
hair8  = rgb 141 074 067 -- dark red
hair9  = rgb 145 085 061 -- light auburn
hair10 = rgb 083 061 050 -- dark auburn
hair11 = rgb 059 048 036 -- Dark brown --EEH
hair12 = rgb 106 078 066 -- brown
hair13 = rgb 102 79 60 -- light brown
hair14 = rgb 151 121 097 -- Ash brown --ew


lips =
  rgb 150 18 18


lbrown = rgb 79 56 3

redhair = rgb 67 0 0
redhair2 = rgb 60 0 0

brown1 = rgb 61 27 0
--highlights = rgb 89 40 0

pinkie = rgba 255 158 158 0.2

hair2c = rgb 40 40 40


--ClickHighlightColour
--(highlights model.highlightColour model)
--highlights changing function

type HighlightColour = Hl1 | Hl2 | Hl3 | Hl4 | Hl5 | Hl6 | Hl7

changeHighlightColour old = case old of
                            Hl1 -> Hl2
                            Hl2 -> Hl3
                            Hl3 -> Hl4
                            Hl4 -> Hl5
                            Hl5 -> Hl6
                            Hl6 -> Hl7
                            Hl7 -> Hl1

highlights colour model = case colour of
                      Hl1 -> myHairColour model.hairColour
                      Hl2 -> black
                      Hl3 -> red
                      Hl4 -> blue
                      Hl5 -> green
                      Hl6 -> purple
                      Hl7 -> rgb 229 200 168


--|> filled (myHairColour model.hairColour)
--ClickHairColour
--HAIR COLOUR CHANGING FUNCTION
type HairColour = Hair1 | Hair2 | Hair3 | Hair4 | Hair5 | Hair6 | Hair7
                |  Hair8 | Hair9 | Hair10 | Hair11 | Hair12 | Hair13 | Hair14

changeHairColour old = case old of
              Hair1     -> Hair2
              Hair2     -> Hair3
              Hair3     -> Hair4
              Hair4     -> Hair5
              Hair5     -> Hair6
              Hair6     -> Hair7
              Hair7     -> Hair8
              Hair8     -> Hair9
              Hair9     -> Hair10
              Hair10    -> Hair11
              Hair11    -> Hair12
              Hair12    -> Hair13
              Hair13    -> Hair14
              Hair14    -> Hair1

myHairColour colour = case colour of
              Hair1     -> hair1
              Hair2     -> hair2
              Hair3     -> hair3
              Hair4     -> hair4
              Hair5     -> hair5
              Hair6     -> hair6
              Hair7     -> hair7
              Hair8     -> hair8
              Hair9     -> hair9
              Hair10    -> hair10
              Hair11    -> hair11
              Hair12    -> hair12
              Hair13    -> hair13
              Hair14    -> hair14

--HAIR CHAINGING FUNCTION

type HairBack = HairB1 | HairB2 | HairB3 |HairB4 | HairB5
                | HairB6 | HairB7 | HairB8 |HairB9 | HairB10
                | HairB11 | HairB12 | HairB13

changeHairBack old = case old of
                  HairB1 -> HairB2
                  HairB2 -> HairB3
                  HairB3 -> HairB4
                  HairB4 -> HairB5
                  HairB5 -> HairB6
                  HairB6 -> HairB7
                  HairB7 -> HairB8
                  HairB8 -> HairB9
                  HairB9 -> HairB10
                  HairB10 -> HairB11
                  HairB11 -> HairB12
                  HairB12 -> HairB13
                  HairB13 -> HairB1


myHairBack hair = case hair of
                  HairB1 -> hair1b
                  HairB2 -> hair2b
                  HairB3 -> hair3b
                  HairB4 -> hair4b
                  HairB5 -> hair5b
                  HairB6 -> hair6b
                  HairB7 -> hair7b
                  HairB8 -> hair8b
                  HairB9 -> hair9b
                  HairB10 -> hair10b
                  HairB11 -> hair11b
                  HairB12 -> hair12b
                  HairB13 -> hair13b

type HairFront = HairF1 | HairF2 | HairF3 | HairF4 | HairF5
                  | HairF6 | HairF7 | HairF8 | HairF9 | HairF10
                  | HairF11 | HairF12 | HairF13

changeHairFront old = case old of
                  HairF1 -> HairF2
                  HairF2 -> HairF3
                  HairF3 -> HairF4
                  HairF4 -> HairF5
                  HairF5 -> HairF6
                  HairF6 -> HairF7
                  HairF7 -> HairF8
                  HairF8 -> HairF9
                  HairF9 -> HairF10
                  HairF10 -> HairF11
                  HairF11 -> HairF12
                  HairF12 -> HairF13
                  HairF13 -> HairF1


myHairFront hair = case hair of
                  HairF1 -> hair1f
                  HairF2 -> hair2f
                  HairF3 -> hair3f
                  HairF4 -> hair4f
                  HairF5 -> hair5f
                  HairF6 -> hair6f
                  HairF7 -> hair7f
                  HairF8 -> hair8f
                  HairF9 -> hair9f
                  HairF10 -> hair10f
                  HairF11 -> hair11f
                  HairF12 -> hair12f
                  HairF13 -> hair13f

type SkinColours = S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9
                    | S12 | S13 | S14 | S15 | S16

changeSkin old = case old of
              S1     -> S2
              S2     -> S3
              S3     -> S4
              S4     -> S5
              S5     -> S6
              S6     -> S7
              S7     -> S8
              S8     -> S9
              S9     -> S12
            --  S10    -> S11
             -- S11    -> S12
              S12    -> S13
              S13    -> S14
              S14    -> S15
              S15    ->S16
              S16    -> S1


mySkinColour c = case c of
              S1  -> skin1
              S2  -> skin2
              S3  -> skin3
              S4  -> skin4
              S5     -> skin5
              S6     -> skin6
              S7     -> skin7
              S8     -> skin8
              S9     -> skin9
              --S10    -> skin10
             -- S11    -> skin11
              S12    -> skin12
              S13    -> skin13
              S14    -> skin14
              S15    -> skin15
              S16    -> skin16


type Colours = Brown | Blue | Green | Grey

change old = case old of
              Brown    -> Blue
              Blue     -> Green
              Green    -> Grey
              Grey      -> Brown

myEyeColour c = case c of
             Brown   -> rgb 79 56 3
             Blue    -> rgb 161 202 241
             Green   -> rgb 75 114 72
             Grey     -> rgb 61 69 80

--ClickEye  -> {model | eyeShape = changeEyeShape model.eyeShape}

type EyeShapes = Eye1 | Eye2 | Eye3 | Eye4

changeEyeShape old = case old of
                        Eye1 -> Eye2
                        Eye2 -> Eye3
                        Eye3 -> Eye4
                        Eye4 -> Eye1

myEyeShape c model = case c of
                Eye1 -> eyes1 model
                Eye2 -> eyes2 model
                Eye3 -> eyes3 model
                Eye4 -> eyes4 model

type Extra = Extra1 | Extra2 | Extra3

changeExtra old = case old of
                    Extra1 -> Extra2
                    Extra2 -> Extra3
                    Extra3 -> Extra1

myExtra c = case c of
            Extra1 -> nothing
            Extra2 -> phone
            Extra3 -> hipsterGlasses

type Msg = Tick Float GetKeyState
                | Click | ClickSkin | ClickHair
                | ClickHairColour | ClickHighlightColour
                | ClickEye | ClickExtra

update : Msg -> Model -> Model
update msg model = case msg of

                    Tick t _  -> { model | time = t }

                    Click     -> { model | eyeColour = change model.eyeColour}

                    ClickExtra -> { model | extra = changeExtra model.extra}

                    ClickEye  -> {model | eyeShape = changeEyeShape model.eyeShape}

                    ClickSkin -> { model | skinColour = changeSkin model.skinColour}

                    ClickHair -> { model | hairFront = changeHairFront model.hairFront
                                          ,hairBack = changeHairBack model.hairBack}

                    ClickHairColour -> {model | hairColour = changeHairColour model.hairColour}

                    ClickHighlightColour -> {model | highlightColour = changeHighlightColour model.highlightColour}

type alias Model =
       { time : Float,
         eyeColour : Colours,
         skinColour : SkinColours,
         hairFront : HairFront,
         hairBack : HairBack,
         hairColour : HairColour,
         highlightColour : HighlightColour,
         eyeShape : EyeShapes,
         extra : Extra
        }

init : Model
init = { time = 0,
         eyeColour = Green,
         skinColour = S6,
         hairFront = HairF4,
         hairBack = HairB4,
         hairColour = Hair1,
         highlightColour = Hl1,
         eyeShape = Eye1,
         extra = Extra1
        }


import GraphicSVG exposing (..)

main = gameApp Tick { model = init, view = view, update = update }

view model = collage 110 180 (myShapes model)
myShapes model = [ 
                -- BACKGROUND
                rect 110 180 |> filled black
                ,rect 105 175 |> filled (rgb 253 255 219)

                --everything but background 
                ,group [  
                
                group [
                    face model
                      |> scale 0.5
                      |> move (-50,0)




                    -- GROUP EYEBROWS FOR MOVEMENT
                    ,group [
                         eyebrow 
                          |> move (-69,13) 
                          |> rotate (degrees 10)
                          |> rotate (degrees 5 * (model.happiness/20))
                          |> scale 0.55 
                        -- |> rotate (degrees (clamp 0 50 (0.4*model.happiness)) )
                        ,eyebrow 
                          |> move (-31,13) 
                          |> rotate (degrees 170)
                          |> rotate -(degrees 5 * (model.happiness/20))
                          |> scale 0.55
                         -- |> rotate (degrees (clamp 0 -50 (-0.4*model.happiness))) 
                           ] |> move (0, (model.happiness) * -0.05)


                    ,rect 10 0.2
                      |> filled black
                      |> move (-59,12.5)
                      |> rotate (degrees -30)
                      |> makeTransparent (model.happiness/50)
                    ,rect 10 0.2
                      |> filled black
                      |> move (-41,12.5)
                      |> rotate (degrees 30)
                      |> makeTransparent (model.happiness/50)

                    ,group [    
                          rect 3 0.5
                            |> filled black
                            |> rotate (degrees -30)
                          ,rect 3 0.5
                           |> filled black
                           |> rotate (degrees 30)
                           |> move (0,-2)
                           ] |> move (-54,9)
                             |> scale 0.75
                             |> makeTransparent (model.happiness/100)

                      ,group [    
                          rect 3 0.5
                            |> filled black
                            |> rotate (degrees -30)
                          ,rect 3 0.5
                           |> filled black
                           |> rotate (degrees 30)
                           |> move (0,-2)
                           ] 
                            |> move (-46,7.5)
                             |> scale 0.75
                             |> mirrorY
                             |> makeTransparent (model.happiness/100)

               ,curve (-50,-20) [Pull (0,(model.happiness-20)/2) (50, -20)] 
                   
                     |> outlined (solid 5) lips
                     |> move (-50,-15)
                     |> scale 0.3
{-
                      ,wedge 18 0.5
                       |> filled (rgb 168 77 77)
                        |> rotate (degrees -90)
                        |> move (-50,-25)
                        |> scale 0.5 
                        -}
                   
                        ]
                        |> move (35,10) 
                          |> scale 0.75
                   
               {-    ,group [ -- Text under head  
                         rect 80 15 
                          |> filled white
                          |> move (-52, -50)
                        ,rect 80 0.6 
                          |> filled (rgb 204 204 255)
                          |> move (-52, -53.25)
                        ,rect 80 0.6 
                          |> filled (rgb 204 204 255)
                          |> move (-52, -48)  
                        ,text "Journal Entry:"
                          |> filled black
                          |> scale 0.45
                          |> move (-66, -48)  -}
                        {-,text "I do/don't feel this because"
                          |> filled black
                          |> scale 0.4
                          |> move (-77, -53)
                        ,rect 0.35 3.3
                         |> filled black 
                         |> move (-25.5, -51.6)
                         |> makeTransparent (cos (model.time * 7)) ] -}
                          --|> move (50,-10)
              , text "How do you feel?"
                      |> filled (rgb 231 124 149) --outlined (solid 0.2) (rgb 231 124 149)
                      |> move (-42,55)
             
                 -- SLIDER
            ,group[
                  group [ 
                    circle 10 
                      |> filled (rgb 211 162 184)
                      |> move (model.happiness-20,0)
                  ,roundedRect 100 10 5 --100 10 5
                      |> filled (rgb 187 130 161)
                 -- ,roundedRect 105 25 5
                   --   |> filled red
                    --  |> makeTransparent 0
                      --|> move (5,0)
                     |> notifyMouseMoveAt Slider

                        ] |> scale 0.7 |> move (40,-30) 
             
             --Back button
              , group [ circle 6 |> filled black
                        , circle 5.9 |> filled white
                        ,triangle 4.5
                              |> filled (rgb 231 124 149)
                              |> rotate (degrees -60)

                        ]|> move (-2,82)

              , group [
                  roundedRect 25 12 5 
                      |> filled (rgb 249 184 161)
                      |> move (40,-50)
                 ,text "SUBMIT" 
                      |> filled black
                      |> scale 0.4
                      |> move (31,-52)
                     ] |> notifyTap ButtonPress
                        |> move (0,-20)
                ] |>  move (-42.5,-10)


                    ] |> move (0,8)
            {---Happiness Counter
               , text (toString <| round <| model.happiness)
                    |> filled black
                    |> move (0,50)-}

                ]
                -- END OF MODEL

{- ALL OF THE CODE FOR THE FACE -}
face model = group [
           ear |> move (-70,0) 
          ,ear |> move (70,0)
          ,oval 143 156
            |> filled black 
            |> move (0,-7) 
          ,oval 140 153
            |> filled skin
            |> move (0,-7)
        -- , blush |> move (-40,-20)
        --, blush |> move (40,-20)
         , eyes model
           |> scale (sadness model)
         , hair |> move (0,58)
             ]
vein = group [
                        rect 5 30 |> filled lbrown
                        ,rect 30 5 |> filled lbrown
                            |> move (-12.5,-12.5)

                        ]

eyes model = group [
        filled black (circle 17)|> move (-20,0) --Left eye 27,0
        ,filled white (circle 15) |> move (-20,0) --27,0
        ,filled lbrown (circle 9) |> move (-20,-5) --24,0
        ,filled black (circle 5) |> move (-20,-7) --21,2
        ,filled white (circle 2) |> move (-20,-7) --18,3
        ,filled black (circle 17)|> move (20,0) --Right eye
        ,filled white (circle 15) |> move (20,0)
        ,filled lbrown (circle 9) |> move (20,-5)
        ,filled black (circle 5) |> move (20,-7)
        ,filled white (circle 2) |> move (20,-7)

  {-      ,curve (0,0) [Pull (35,-22) (70,0)]
            |> filled lightBlue
            |> scale 0.4
            |> move (-41,-13)
            |> makeTransparent (tears model)
        ,curve (0,0) [Pull (35,-22) (70,0)]
            |> filled lightBlue
            |> scale 0.4
            |> move (13,-13)
            |> makeTransparent (tears model)

        ,wedge 18 0.5
            |> filled lightBlue
            |> rotate (degrees -90)
            |> move (-27.5,0)
            |> makeTransparent (tears model)

        ,wedge 18 0.5
            |> filled lightBlue
            |> rotate (degrees -90)
            |> move (27.5,0)
            |> makeTransparent (tears model)-}
      ]

ear = group[
        oval 24 44 |> filled black
       ,oval 20 40 |> filled skin
           ]

eyebrow = group  [oval 37 4 |> filled black ]   

     
blush = group [circle 15 |> filled pinkie]

hair = group [ 
               oval 60 40 |> filled black |> move(0,5)
              ,oval 60 40 |> filled black |> move (-30,0) |> rotate (degrees 20)
              ,oval 60 40 |> filled black |> move (30,0) |> rotate (degrees 160)
              ,oval 55 35 |> filled black |> move (55,-15) |> rotate (degrees 130)
              ,oval 55 35 |> filled black |> move (-55,-15) |> rotate (degrees 50)
              ]

--COLOURS 
skin    = rgb 214 170 83
skin2   = rgb 225 170 100
lips    = rgb 150 18 18
lbrown  = rgb 79 56 3
pinkie  = rgba 255 158 158 0.45

{- ALL THE COD-}

sadness model = 
    if model.happiness > 1 then
      1 + (model.happiness/1000) --500
    else 
      1

mouth model =
    if model.happiness > 1 then
      model.happiness /100
    else
      0

--1) make a type for the states (unless the state is a number or existing type)
-- MOVING INTO CALENDAR
type State = Moving | Start | Stop


-- (2) implement transition function
--SUBMIT BUTTON
buttonPress oldState = case oldState of
                         Start -> Moving
                         Moving -> Moving
                         Stop -> Stop

-- THE TYPE OF MESSAGE, 
--EITHER MOVE THE SLIDER OR PRESS THE SUBMIT BUTTON
type Msg = Tick Float GetKeyState
        | Slider (Float,Float)
        | ButtonPress
        
update msg model = case msg of

                    Tick t _     -> { model | time = t 
                                         , xcoord = updatex model
                                         , ycoord = updatey model
                                         , aniTime = case model.state of
                                                      Moving -> model.aniTime + 0.05
                                                      _ -> 0
                                         , state = updateState model
                                         , size = updateSize model}

                    Slider (x,y) -> { model | happiness = case model.state of
                                                        Start -> ((x)*1.5)+20  
                                                                             
                                                        _     -> model.happiness 
                                          

                                    }

                    ButtonPress -> { model | state = buttonPress model.state
                                          
                                    }
                 

init = { time = 0, happiness = -20, state = Start, xcoord = 0, ycoord = 0 , aniTime = 0, size = 1 }
updatex model = case model.state of
                      Moving -> model.xcoord - model.aniTime - 1.2
                      _ -> model.xcoord
updatey model = case model.state of
                      Moving -> model.ycoord - model.aniTime + 0.01
                      _ -> model.ycoord
updateState model = case model.state of
                       Moving -> if model.aniTime >= 2
                                    then Stop
                                    else Moving
                       Stop -> Stop
                       Start -> Start
updateSize model = case model.state of
                      Moving -> model.size - 0.02
                      x -> model.size 


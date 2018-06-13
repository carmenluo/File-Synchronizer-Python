module FrustratedFace exposing (..)

import GraphicSVG exposing (..)

myShapes model = 
              group [  
                
                --FACE
                group [
                    face model
                      |> scale 0.5 
                      |> move (-50,0)

                --Throbbing vein
              ,group[
                   vein
                   ,vein 
                      |> move (0,-35)
                      |> mirrorY
                  ,vein 
                      |> move (10,0)
                      |> mirrorX
                  ,vein 
                      |> move (10,-35)
                      |> mirrorX
                      |> mirrorY

                  ] |> scale 0.1
                    |> move (-45,20)
                    |> makeTransparent (model.happiness/60)



                    -- GROUP EYEBROWS FOR MOVEMENT
                    ,group [
                         eyebrow 
                          |> move (-65,12) 
                          |> rotate -(degrees 5 * (model.happiness/40))
                          |> scale 0.55 

                          --|> rotate (degrees (clamp 0 50 (0.4*model.happiness)) )
                        ,eyebrow 
                          |> move (-35,12) 
                          |> rotate -(degrees -5 * (model.happiness/40))
                          |> scale 0.55
                          --|> rotate (degrees (clamp 0 -50 (-0.4*model.happiness))) 
                           ] --|> move (0, (model.happiness) * -0.05)
                    
                    --Creases for anger
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
                           ] |> move (-46,7.5)
                             |> scale 0.75
                             |> mirrorY
                             |> makeTransparent (model.happiness/100)

                   , text "Frustrated?"
                      |> filled (rgb 255 140 0) 
                      |> move (-270,-7)
                      |> scale 3

                   ,curve (-50,-20) [Pull (0,(model.happiness+20)/2) (50, -20)]
                      |> outlined (solid 3) red
                      |> move (-50,-15)
                      |> scale 0.4
                      |> rotate (degrees (rotation model))
                   
                        ] |> scale 1
                          |> move (60,-70)

                          |> move (model.xcoord,model.ycoord)
                          |> scale model.size
                   
                  
          

                     |> move (0,8)
                     |> scale (sizes model)
                     {-
            --Happiness Counter
               , text (toString <| round <| model.happiness/(-40))
                    |> filled black
                    |> move (0,20) -}
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
         --, blush |> move (-40,-20)
        -- , blush |> move (40,-20)
         , eyes model
         , hair |> move (0,58)
             ]
vein = group [
                        rect 5 30 |> filled black
                        ,rect 30 5 |> filled black
                            |> move (-12.5,-12.5)

                        ]

-- SLIDER
slider model =
            group[
                  group [ 
                    circle 10 
                      |> filled (rgb 255 140 0)
                      |> move (model.happiness+25,0)
                  ,roundedRect 100 10 5
                      |> filled (rgba 255 140 0 0.4)
                  ,roundedRect 105 25 5
                      |> filled white
                      |> makeTransparent 0
                      |> move (5,0)
                      |> notifyMouseMoveAt Slider
                        ] |> scale 0.7 |> move (0,-87)

            --  , group [
                  ,roundedRect 25 12 5 
                      |> filled (rgb 255 140 0)
                      |> move (180,-85)
                 ,text "SUBMIT" 
                      |> filled black
                      |> scale 0.4
                      |> move (172,-67)
                      --] 

                      |> notifyTap ButtonPress
                        |> move (0,-20)


              ] |>  move (-42.5,-10)

eyes model = group [
        filled black (circle 20)|> move (-27,0) --Left eye
        ,filled white (circle 18) |> move (-27,0)
        ,filled lbrown (circle 15) |> move (-24,0)
        ,filled black (circle 12) |> move (-21,2)
        ,filled white (circle 6) |> move (-18,3)
        ,filled black (circle 20)|> move (27,0) --Right eye
        ,filled white (circle 18) |> move (27,0)
        ,filled lbrown (circle 15) |> move (30,0)
        ,filled black (circle 12) |> move (32,2)
        ,filled white (circle 6) |> move (35,3)
      ]

ear = group[
        oval 24 44 |> filled black
       ,oval 20 40 |> filled skin
           ]

eyebrow = group [ oval 37 4 |> filled black]     
     
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

{- ALL THE FUNCTIONS-}
--1) make a type for the states (unless the state is a number or existing type)
-- MOVING INTO CALENDAR
type State = Moving | Start | Stop


-- (2) implement transition function
--SUBMIT BUTTON
buttonPress oldState = case oldState of
                         Start -> Moving
                         Moving -> Moving
                         Stop -> Stop

{-Function to rotate the mouth only if happiness is greater than 0-}
rotation model = 
    if model.happiness > 0 then
      model.happiness /1.5
    else 
      0

sizes model = 
    if model.happiness > 1 then
      1 + (model.happiness/350)
    else 
      1

-- THE TYPE OF MESSAGE, 
--EITHER MOVE THE SLIDER OF PRESS THE SUBMIT BUTTON
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
                                                        Start -> (x+45)*1.3 -- Keeps the user from being able to
                                                                               -- move the slider after submitting
                                                        _     -> model.happiness


                                    }

                    ButtonPress -> { model | state = buttonPress model.state
                                          
                                    }
type alias Model =
      {
      time: Float
      ,happiness: Float
      ,state: State
      ,xcoord: Float
      ,ycoord: Float
      ,aniTime: Float
      ,size: Float
    }         

init = { 
      time = 0
    , happiness = -65
    , state = Start
    , xcoord = 0
    , ycoord = 0 
    , aniTime = 0
    , size = 1 }


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
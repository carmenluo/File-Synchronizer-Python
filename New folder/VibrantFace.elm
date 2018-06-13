module VibrantFace exposing (..)
import GraphicSVG exposing (..)

--main = gameApp Tick { model = init, view = view, update = update }

--view model = collage 110 180 (myShapes model)
myShapes model = group [  
                --Face
                group [
                    face model
                      |> scale 0.5 
                      |> move (-50,15)
                    -- GROUP EYEBROWS FOR MOVEMENT
                     ,curve (-60,18) [Pull (-35,((model.happiness+60)/3)+10)(-10, 18)]
                     |> outlined  (solid 6) black
                     |> move (-50,18)
                     |> scale 0.4
                     ,curve (10,18) [Pull (35,((model.happiness+60)/3)+10)(60, 18)]
                     |> outlined  (solid 6) black
                     |> move (-50,18)
                     |> scale 0.4
                     ,group [
                     line (-70,-7) (-30, -7)
                     |> outlined (solid 1) red
                      ,curve (-50,-5) [Pull (0,(-(model.happiness+60)/2)+5) (50, -5)]
                     |> outlined (solid 3) red
                     |> move (-50,-5)
                     |> scale 0.4
                     ]
                   
                        ] |> move (60,10)
                          |> scale 0.95
                          |> move (0, (model.elevation+20)*0.3)
                   
                     |> move (0,8)
                   --  |> scale (sizes model/1.5)
 
              , text "Vibrant?"
                      |> filled (rgb 40 188 29) 
                      |> move (-195,17)
                      |> scale 3
             
{-
            --Happiness Counter
               , text (toString <| round <| model.happiness/40)
                    |> filled black
                    |> move (0,50) -}
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
           |> scale (sadness model)
         , hair |> move (0,58)
             ]
slider model =  group[
                  group [ 
                    circle 10 
                      |> filled (rgb 255 140 0)
                      |> move (model.happiness-20,0)
                  ,roundedRect 100 10 5
                      |> filled (rgba 255 140 0 0.4)
                  ,roundedRect 105 25 5
                      |> filled white
                      |> makeTransparent 0
                      |> move (5,0)
                      |> notifyMouseMoveAt Slider

                        ] |> scale 0.7 |> move (0,-50) 

             -- , group [
                  ,roundedRect 25 12 5 
                      |> filled (rgb 255 140 0)
                      |> move (180,-50)
                 ,text "SUBMIT" 
                      |> filled black
                      |> scale 0.4
                      |> move (172,-32)
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
mouth model = group [
         line (-70,-7) (-30, -7)
         |> outlined (solid 1) red
         ,curve (-50,-5) [Pull (0,(-(model.happiness+60)/2)+5) (50, -5)]
         |> outlined (solid 3) red
         |> move (-50,-5)
         |> scale 0.4
         ]
sizes model = 
    if model.happiness > 1 then
      1 + (model.happiness/350)
    else 
      1
--COLOURS 
skin    = rgb 214 170 83
skin2   = rgb 225 170 100
lips    = rgb 150 18 18
lbrown  = rgb 79 56 3
pinkie  = rgba 255 158 158 0.45

{- ALL THE COD-}

sadness model = 
    if model.happiness > 1 then
      1 + (model.happiness/500)
    else 
      1

tears model =
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
                                         , size = updateSize model
                                         , elevation = updateElevation model}

                    Slider (x,y) -> { model | happiness = case model.state of
                                                        Start -> ((x+45)*1.3)+43
                                                                               
                                                        _     -> model.happiness 


                                    }

                    ButtonPress -> { model | state = buttonPress model.state
                                          
                                    }
                 
type alias Model =
        {
        time: Float,
        happiness: Float,
        state: State,
        xcoord: Float,
        ycoord: Float,
        aniTime: Float,
        size: Float,
        elevation: Float
      }  
init = { time = 0, happiness = -20, state = Start, xcoord = 0, ycoord = 0 , aniTime = 0, size = 1, elevation = 0 }
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

updateElevation model = if model.happiness <(-17)
                        then 0*(sin(model.time*(model.happiness+110/8)))
                        else
                        0.3*(model.happiness+110/8)*(sin(model.time*0.1*(model.happiness+110/8)))

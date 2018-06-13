module HopefulFace exposing (..)
import GraphicSVG exposing (..)


myShapes model = 
              group [  
                
                group [
                    face model
                      |> scale 0.5 
                      |> move (-50,0)
                    -- GROUP EYEBROWS FOR MOVEMENT
                    ,group [
                         eyebrow 
                          |> move (-69,13) 
                          |> rotate (degrees 10)
                          |> rotate (degrees 5 * (model.happiness/40))
                          |> scale 0.55 
                       --   |> rotate (degrees (clamp 0 50 (0.4*model.happiness)) )
                        ,eyebrow 
                          |> move (-31,13) 
                          |> rotate (degrees 170)
                          |> rotate -(degrees 5 * (model.happiness/40))
                          |> scale 0.55
                         -- |> rotate (degrees (clamp 0 -50 (-0.4*model.happiness))) 
                           ] |> move (0, (model.happiness) * -0.05)

                      , text "Hopeful?"
                      |> filled (rgb 40 188 29) 
                      |> move (-265,-27)
                      |> scale 3

                   ,curve (-50,-20) [Pull (0,(-model.happiness-100)/2) (50, -20)] --moves mouth
                      |> outlined (solid 3) red
                      |> move (-50,-15)
                      |> scale 0.4
                   
                        ] |> move (60,-30)
                          |> scale 0.95


                     |> move (0,8)
                     |> scale (sizes model)
{-
                  --Happiness Counter
               , text (toString <| round <| model.happiness/(40))
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
         , blush model|> move (-40,-20)
         , blush model|> move (40,-20)
         , eyesOuter model
           |> scale (sadness model)
        ,eyesInner model
          |> scale (sadness model)
          |> move(0, (model.happiness+18)/15)
        , shine model
          |>move (-8,-93)
          |> scale 0.6
         ,hands
          |> move (-8, -93)
          |> scale 0.6
         , hair |> move (0,58)
             ]
--SLIDER
slider model = group[
                  group [ 
                    circle 10 
                      |> filled (rgb 255 140 0)
                      |> move (model.happiness+25,0)
                  ,roundedRect 100 10 5
                      |> filled (rgba 255 140 0 0.4)
                      |> notifyMouseMoveAt Slider

                        ] |> scale 0.7

               |> move (0,-75) 
             

           --  {- , group [
                  ,roundedRect 25 12 5 
                      |> filled (rgb 255 140 0 )
                      |> move (180,-75)
                 ,text "SUBMIT" 
                      |> filled black
                      |> scale 0.4
                      |> move (172,-57)
                   --  ] -}

                      |> notifyTap ButtonPress
                        |> move (0,-20)
                ] |>  move (-42.5,-13)


eyesOuter model = group [
        --left eye
        filled black (circle 20)|> move (-27,0) --outline
        ,filled white (circle 18) |> move (-27,0) --inside white
        --right eye
        ,filled black (circle 20)|> move (27,0) 
        ,filled white (circle 18) |> move (27,0)
{-
        ,curve (0,0) [Pull (35,-22) (70,0)]
            |> filled lightBlue
            |> scale 0.4
            |> move (-41,-13)
            |> makeTransparent (tears model)
        ,curve (0,0) [Pull (35,-22) (70,0)]
            |> filled lightBlue
            |> scale 0.4
            |> move (13,-13)
            |> makeTransparent (tears model) -}

{-        ,wedge 18 0.5
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
eyesInner model = group [
  --left eye
        filled lbrown (circle 15) |> move (-24,3) --brown mid
        ,filled black (circle 13) |> move (-21,5) -- pupil
        ,filled white (circle 6) |> move (-18,8)-- reflection in pupil
    --right eye
        ,filled lbrown (circle 15) |> move (30,3)
        ,filled black (circle 13) |> move (32,5)
        ,filled white (circle 6) |> move (35,8)
       ]

hands = group [
      filled skin (oval 25 10) |>move(10,40) |> rotate (degrees -6) --top finger skin
      ,outlined (solid 2) black (oval 27 12) |> move (10,40)|> rotate (degrees -6) --outline of top finger
      , outlined (solid 2)black (oval 42 82) |> move (25,0)|> rotate (degrees 6) --
      , filled skin (oval 40 80) |> move (25,0)|> rotate (degrees 6)
      , outlined (solid 2) black (oval 42 82)|> move (0,0) |> rotate (degrees -2)
      , filled skin (oval 40 80) |> move (0,0)|> rotate (degrees -2) --left hand
      , filled skin (oval 40 20)|> move(15,20)|>rotate(degrees 20) -- bottom finger
      , outlined (solid 2)black (oval 42 22)|> move(15,20) |>rotate(degrees 20)
      , filled skin (oval 21 31) |> move(29,18) |>rotate(degrees 5) -- cover for bottom finger
      , filled skin (oval 9 11) |>move (16,39) |>rotate(degrees 55)   -- cover for top finger
 ]

shine model = group [
   
      filled shines (rect 80 10)|> move (-50,-30) |>makeTransparent (shining model*10)
      , filled shines (rect 80 10)|> move (70,-30) |>makeTransparent (shining model*20)
      , filled shines (rect 80 10)|> move (-40,45)|> rotate(degrees -40) |>makeTransparent (shining model*10)
      , filled shines ( rect 80 10)|> move (-50,5)|> rotate(degrees -20) |>makeTransparent (shining model*10)
      , filled shines(rect 80 10) |> move (70,10) |> rotate(degrees 20) |>makeTransparent (shining model*10)
      , filled shines (rect 80 10) |> move (55,50) |> rotate(degrees 40) |>makeTransparent (shining model*10)
      , filled shines (rect 80 10)|> move (10,70)|> rotate(degrees 90) |>makeTransparent (shining model*10)

           
  ]



ear = group[
        oval 24 44 |> filled black
       ,oval 20 40 |> filled skin
           ]

eyebrow = group [ oval 37 4 |> filled black]     
     
blush model = group [circle 15 
                |> filled pinkie 
                |>makeTransparent (blushing model)
            ]

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
shines  = rgba 255 255 77 1

{- ALL THE COD-}

sadness model = 
    if model.happiness > 1 then
      1 + (model.happiness/500) -- this makes eyes bigger
    else 
      1

tears model =
    if model.happiness > 1 then
      model.happiness /100
    else
      0

blushing model =
    if model.happiness > 1 then
      model.happiness /20
    else
      0

shining model =
    if model.happiness > 1 then
      model.happiness /20
    else
      0
sizes model = 
    if model.happiness > 1 then
      1 + (model.happiness/350)
    else 
      1
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
                                         , size = updateSize model}

                    Slider (x,y) -> { model | happiness = case model.state of
                                                        Start -> (x+45)*1.3 --((x)*1.5)+20
                                                                               
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
        size: Float
      }           

init = { 
    time = 0, 
    happiness = -65, 
    state = Start, 
    xcoord = 0, 
    ycoord = 0 , 
    aniTime = 0, 
    size = 1 
  }

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

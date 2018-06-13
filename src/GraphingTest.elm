import GraphicSVG exposing (..)

main = gameApp Tick { model = init, view = view, update = update }

view model = collage 350 350 (myShapes model)

myShapes model = [  graphPaper 10 
           |> makeTransparent 0.4, 
           drawGraph [[(0, 0), (50,70) , (100,20), (150, 70), (200, 35)], [(0, 0), (50,-40) , (100,-20), (150, -40), (200, -30)]]     ] 


type Msg = Tick Float GetKeyState

update msg model = case msg of
                     Tick t _ -> { time = t }

init = { time = 0 }

sortByX : List(Float, Float) -> List(Float,Float)
sortByX list = 

    List.sortBy Tuple.first list 

{-| Sorts points by x-coordinate for each line. Uses sortByX function.
-}
sortLines : List (List (Float,Float) ) -> List ( List(Float,Float) )
sortLines lists = List.map sortByX lists



maxX : List(List(Float,Float)) ->Float
maxX lists =
    let 
        (x,y) = List.unzip (List.concat lists)

    in
      if List.isEmpty x then 0.0 else Maybe.withDefault 0.0 (List.maximum x)

maxY : List(List(Float,Float)) ->Float 
maxY lists =
    let 
        (x,y) = List.unzip (List.concat lists)

    in
      if List.isEmpty y then 0.0 else Maybe.withDefault 0.0 (List.maximum y)

minY : List(List(Float,Float)) ->Float 
minY lists =
    let 
        (x,y) = List.unzip (List.concat lists)

    in
      if List.isEmpty y then 0.0 else Maybe.withDefault 0.0 (List.minimum y)






happyFace = group [face |> scale 0.5,
                  eyebrow |> move (-15,14 ) 
                          |> rotate (degrees -5)
                          |> scale 0.5,
                  eyebrow |> move (15,14 ) 
                          |> rotate (degrees 5)
                          |> scale 0.5,
                  curve (-50,-20) [Pull (0,-90) (50, -20)]
                      |> outlined (solid 3) lips
                      |> move (0,-15)
                      |> scale (0.35),
                    filled red (circle 20)|> move (-27,0)
           -- |> filled 
            |> scale 0.4
            |> move (4,-17)
            |> makeTransparent 0.4
        ,filled red (circle 20)|> move (-27,0)
            |> scale 0.4
            |> move (50,-17)
            |> makeTransparent 0.4
                   ]|> scale(0.45)



sadFace = group [face |> scale 0.5,
                  eyebrow |> move (-15,14 ) 
                          |> rotate (degrees 10)
                          |> scale 0.5,
                  eyebrow |> move (15,14 ) 
                          |> rotate (degrees 170)
                          |> scale 0.5,
                  curve (-50,-20) [Pull (0,15) (50, -20)]
                      |> outlined (solid 3) lips
                      |> move (0,-15)
                      |> scale (0.35),
                  curve (0,0) [Pull (35,-22) (70,0)]
            |> filled lightBlue
            |> scale 0.19
            |> move (-20,-6.5)
            |> makeTransparent (0.7)
        ,curve (0,0) [Pull (35,-22) (70,0)]
            |> filled lightBlue
            |> scale 0.2
            |> move (7,-6.5)
            |> makeTransparent (0.7)]|> scale(0.46)
                   
                          





face = group [
           ear |> move (-70,0) 
          ,ear |> move (70,0)
          ,oval 143 156
            |> filled black 
            |> move (0,-7) 
          ,oval 140 153
            |> filled skin
            |> move (0,-7)
        
         , eyes 
           |> scale 1.0
         , hair |> move (0,58)
             ]


eyes  = group [
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
              
              
skin    = rgb 214 170 83
skin2   = rgb 225 170 100
lips    = rgb 150 18 18
lbrown  = rgb 79 56 3
pinkie  = rgba 255 158 158 0.45







last : List a -> Maybe a
last list =
    case list of
        [] -> Nothing
        [x] ->  Just x
        (x::xs) -> last xs






average : List Float -> Float
average numbers = List.sum numbers / Basics.toFloat(List.length numbers)


avgY : List(List(Float,Float)) ->Float 
avgY lists =
    let 
        (x,y) = List.unzip (List.concat lists)

    in
      if List.isEmpty y then 0.0 else (average y)
       

drawGraph : List (List (Float,Float) ) -> Shape userMsg
drawGraph lists =
    let
        sortedLists = sortLines lists
             
    in
    group[
    line (0,0) (maxX sortedLists,0) |> outlined (solid 3) black
    ,line (0,maxY sortedLists) (0,minY sortedLists) |> outlined (solid 3) black
    ,line (0,avgY sortedLists) (maxX sortedLists,avgY sortedLists) |> outlined (dashed 3) green |> makeTransparent 0.6
    ,(drawLines sortedLists [])
    ] |> move(-150,-20)

drawLine xP yP linePts shapes = 
    case linePts of 
        [] -> group shapes 
        [(xN,yN)] -> if yN >= 0 then drawLine xN yN [] (shapes ++ [outlined (solid 3) red (line (xP,yP) (xN,yN))] ) else drawLine xN yN [] (shapes ++ [outlined (solid 3) blue (line (xP,yP) (xN,yN))] )
        ((xN,yN)::xs) -> 
              let
                 (x,y) = List.unzip xs
              in
                 if (List.sum y) + yN>= 0 then drawLine xN yN xs (shapes ++  [outlined (solid 3) red (line (xP,yP) (xN,yN))] ) else drawLine xN yN xs (shapes ++  [outlined (solid 3) blue (line (xP,yP) (xN,yN))] )

--drawLines : List(?)-> List(?) -> Shape userMsg
drawLines lines shapes = 
    case lines of 
        [] -> group shapes 
        [x] -> --drawLines [] (shapes ++ [drawLine 0 0 x shapes])
             let
               (a,b) = List.unzip x
            in
              if (average b) >= 0 then group [drawLines [] (shapes ++ [drawLine 0 0 x shapes]), happyFace |> move(Maybe.withDefault (0,0) (last x))  ] else group [drawLines [] (shapes ++ [drawLine 0 0 x shapes]), sadFace |> move(Maybe.withDefault (0,0) (last x)) ]
        (x::xs) ->                                                                    --drawLines xs (shapes ++ [drawLine 0 0 x shapes]) 
                 let
                    (a,b) = List.unzip x
                in
                  if (average b) >= 0 then group [drawLines xs (shapes ++ [drawLine 0 0 x shapes]), happyFace |> move(Maybe.withDefault (0,0) (last x))] else group [drawLines xs (shapes ++ [drawLine 0 0 x shapes]), sadFace |> move(Maybe.withDefault (0,0) (last x))]                                                              


              --drawLines xs (shapes ++ [drawLine 0 0 x shapes]) 



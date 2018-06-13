module ActivityGraph exposing (..)
import GraphicSVG exposing (..)

graphAct = 
    group [
        line (-150,-50) (150,-50)
            |> outlined (solid 2) black
        , graphHelper [(3,1,2),(3,3,3),(1,1,1),(3,4,6),(0,2,3),(4,3,2)]
        ]

graphHelper actList = group (graphMulti actList 1 (toFloat (List.length actList)))

graphMulti actList number length=
        case actList of
            [] -> [line (0,0) (0,0)
                    |> outlined (solid 1) white]

            [t] -> [graphActTime t (number) length |> move (-150 + (300/(length+1))*number,0)]

            t::xs ->  graphMulti xs (number + 1) length ++ [graphActTime t (number) length |> move (-150 + (300/(length+1))*number,0)] 
        
scaleHelper n =
    if n > 3 then
        1/((n-3)/2)
    else
        1

graphActTime times number length =  
    case times of 
        (x,y,z) -> group[
                        rect 15 (20*x)
                            |> filled lightBlue
                            |> addOutline (dotted 1) black
                            |> move (-16,-50 + 10*x)
                        ,rect 15 (20*y)
                            |> filled red
                            |> addOutline (dotted 1) black
                            |> move (0,-50 + 10*y)
                        ,rect 15 (20*z)
                            |> filled orange
                            |> addOutline (dotted 1) black
                            |> move (16,-50 + 10*z)
                        ,text ("Day " ++ (toString -(length - number)))
                                |> bold
                                |> size 12
                                |> filled black
                                |> move (-15,-65)
                    ] |> scaleX (scaleHelper length)

--graphDecoder jsonString 
--    = decodeString (field "array")

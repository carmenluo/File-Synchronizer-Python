module Dropdown exposing (..)

import GraphicSVG exposing (..)

--main = gameApp Tick { model = init "Activity", view = view, update = update }

--view model listText= collage 110 180 (dropdown model listText)


dropdown model  textList = List.concat [dropdownHelper model textList 0]


dropdownHelper model textList number = 
    case textList of 
        [] -> (dropdownHead model )
        [item] -> List.concat [dropdownHelper model [] (number + 1),(dropdownOption model item (number + 1))]
        (item::xs) -> List.concat [dropdownHelper model xs (number + 1),(dropdownOption model item (number + 1))]


dropdownHead model  =  
    [
        rect 100 10 
            |> filled white
            |> addOutline (solid 1) green
        ,text  model.buttonText
            |> fixedwidth
            |> bold
            |> size 8
            |> filled black
            |> move (-47,-3)
        ,rect 11 11 
            |> filled green
            |> move (45,0)
            |> notifyTap ButtonPress
        ,ngon 3 5 
            |>filled blue
            |> rotate (degrees model.degTri)
            |> move (45,0)
            |> notifyTap ButtonPress
    ]

dropdownOption model buttonText number = 
    case model.clicked of 
        True -> [group[
                    rect 100 10 
                        |> filled white
                        |> addOutline (solid 1) green
                        |> notifyTap (AOption buttonText)
                    ,text  buttonText
                        |> fixedwidth
                        |> bold
                        |> size 8
                        |> filled black
                        |> move (-47,-3)
                        |> notifyTap (AOption buttonText)
                ] |> move (0,-10 * number)]
        False -> [
                    rect 0 0 
                        |> filled white
                ]

-- THE TYPE OF MESSAGE, 
--EITHER MOVE THE SLIDER OF PRESS THE SUBMIT BUTTON
type Msg = Tick Float GetKeyState
        | ButtonPress
        | AOption String
        --| Button
        
update msg model = case msg of
                    --Button -> 
                    Tick t _     -> { model | time = t } 

                    ButtonPress ->  if model.clicked == True then
                                        { model | clicked = False,
                                                    degTri = -90}
                                    else 
                                        { model | clicked = True,
                                                     degTri = 90}
                    AOption text -> {model | buttonText = text}
type alias Model =
        {
        time: Float,
        clicked : Bool,
        degTri : Float,
        buttonName : String,
        buttonText : String
      }                 


init name = { 
    time = 0, 
    clicked = False,
    degTri = -90,
    buttonName = name,
    buttonText = name
    }

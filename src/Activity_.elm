module Activity exposing (..)

import Icons
import GraphicSVG exposing (..)



main = gameApp Tick {model = init, view = view, update = update}

-- Model 
type alias Model = 
    {time : Float
    ,social : ActivityLevel
    ,excercise : ActivityLevel
    ,relax : ActivityLevel
    }

type ActivityLevel 
    = Low 
    | Medium
    | High

type Activity
    = SocialActivity
    | ExcerciseActivity
    | RelaxActivity

type SActivity
    = HangOut
    | Club
    | Family

type PActivity
    = Walking
    | Running
    | Biking

type RActivity
    = Sleeping
    | Reading
    | BikingAct

init : Model
init = 
    { time = 0
    , social = Low
    , excercise = Low
    , relax = Low
    }

--View 



view model = collage 400 400 (myShapes model)

myShapes model = 
    [ graphPaper 10
        |> makeTransparent 0.6
    , group [
        group [
            Icons.excercise
            ,button model ExcerciseActivity Low |> move (100,0)
            ,button model ExcerciseActivity Medium |> move (160,0)
            ,button model ExcerciseActivity High |> move (220,0)
        ]
        ,group [
            Icons.relax
            ,button model RelaxActivity Low |> move (100,0)
            ,button model RelaxActivity Medium |> move (160,0)
            ,button model RelaxActivity High |> move (220,0)
        ] |> move (0,-100)
        ,group [
            Icons.social
            ,button model SocialActivity Low |> move (100,0)
            ,button model SocialActivity Medium |> move (160,0)
            ,button model SocialActivity High |> move (220,0)
        ] |> move (0,100)
        ] |> move (-125,0)
    , submitButton model |> move (150, -150)
    ]

submitButton model = group [
    rect 50 25
        |> filled green
        |> addOutline (solid 1) black
    ,text "Submit"
        |> fixedwidth
        |> bold
        |> size 12
        |> filled black
        |> addOutline (solid 1) black
        |> move (-20,-5)
    ] |> notifyTap Submit

button model activityType activityLevel  = group [
    rect 50 25
        |> filled (getColor model activityType activityLevel)
        |> addOutline (solid 1) black
        
    ,text (buttonTextHelper activityLevel)
        |> fixedwidth
        |> bold
        |> size 12
        |> filled black
        |> addOutline (solid 1) black
        |> move (-20,-5)
  ]|> notifyTap ((messageHelper activityType) activityLevel)

--UPDATE

getColor model activityType activityLevel = 
    case activityType of 
        SocialActivity -> 
            if model.social == activityLevel then yellow else green
        ExcerciseActivity ->
            if model.excercise == activityLevel then yellow else green
        RelaxActivity ->
            if model.relax == activityLevel then yellow else green

messageHelper activityType = case activityType of
    SocialActivity -> Social
    ExcerciseActivity -> Excercise
    RelaxActivity -> Relax

buttonTextHelper activityLevel = case activityLevel of
    Low -> "0-1 Hr"
    Medium -> "1-2 Hr"
    High -> "2+ Hr"


type Msg 
    = Tick Float GetKeyState
    | Social ActivityLevel
    | Excercise ActivityLevel
    | Relax ActivityLevel
    | Submit
    


update msg model = 
    case msg of  
    Tick t _ -> {model | time = t}
    Social level -> 
        {model | social = level}
    Excercise level -> 
        {model | excercise = level}
    Relax level -> 
        {model| relax = level}
    Submit -> {model | relax = Low, excercise = Low, social = Low}

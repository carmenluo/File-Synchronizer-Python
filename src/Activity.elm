module Activity exposing (..)

import Dropdown
import Icons
import ActivityGraph
import GraphicSVG exposing (..)



main = gameApp Tick {model = init, view = view, update = update}

-- Model 
type alias Model = 
    {time : Float
    ,page : Page
    ,social : ActivityLevel
    ,excercise : ActivityLevel
    ,relax : ActivityLevel
    ,socialDrop : Dropdown.Model
    ,activityDrop : Dropdown.Model
    ,relaxDrop : Dropdown.Model
    }

init : Model
init = 
    { time = 0
    , page = Add
    , social = Low
    , excercise = Low
    , relax = Low
    , socialDrop = Dropdown.init "Social"
    , activityDrop = Dropdown.init "Activity"
    , relaxDrop = Dropdown.init "Relaxation"
    }

type Page 
    = Add
    | Graph

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

--getVal 

-- Update

type Msg 
    = Tick Float GetKeyState
    | Social ActivityLevel
    | Excercise ActivityLevel
    | Relax ActivityLevel
    | Submit
    | SocDrop Dropdown.Msg
    | ActDrop Dropdown.Msg
    | RelDrop Dropdown.Msg
    | PageChange
    


update msg model = 
    case msg of  
        Tick t g -> {model | time = t}
        Social level -> 
            {model | social = level}
        Excercise level -> 
            {model | excercise = level}
        Relax level -> 
            {model| relax = level}
        Submit -> {model | relax = Low
                    ,excercise = Low 
                    ,social = Low
                    ,socialDrop = Dropdown.init "Social"
                    ,activityDrop = Dropdown.init "Activity"
                    ,relaxDrop = Dropdown.init "Relaxation"
                    }
        SocDrop m1 -> {model | socialDrop = Dropdown.update m1 model.socialDrop}
        ActDrop m1 -> {model | activityDrop = Dropdown.update m1 model.activityDrop}
        RelDrop m1 -> {model | relaxDrop = Dropdown.update m1 model.relaxDrop}
        PageChange -> 
            case model.page of
                Add -> {model | page = Graph}
                Graph -> {model | page = Add}


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

--View 
view model = 
    case model.page of 
        Add -> collage 400 400 (addActivity model)
        Graph -> collage 400 400 (viewGraph model)
myShapes model = 
    case model.page of 
        Add -> addActivity model
        Graph -> viewGraph model
-- SWAP PAGE BUTTON
swapButton model name offset= group [
    rect 100 25
        |> filled green
        |> addOutline (solid 1) black
    ,text name
        |> fixedwidth
        |> bold
        |> size 12
        |> filled black
        |> addOutline (solid 1) black
        |> move (-offset,-5)
    ] |> notifyTap PageChange 


-- VIEW FOR VIEWING GRAPHS
viewGraph model = 
    [
        swapButton model "Add Activity" 40 
            |> move (0,180)
        ,ActivityGraph.graphAct
    ]
-- VIEW FOR ADDING ACTIVITY
addActivity model = 
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
    , swapButton model "View Graph" 35 |> move (0,180)
    , group (List.map (map SocDrop) (Dropdown.dropdown model.socialDrop ["Chill with Friends","Family Time","Hello"]))
        |> scaleX 2
        |> scaleY 2
        |> move (50,130)
    , group (List.map (map ActDrop) (Dropdown.dropdown model.activityDrop ["Skiing","Hike"]))
        |> scaleX 2
        |> scaleY 2
        |> move (50,30)
    , group (List.map (map RelDrop) (Dropdown.dropdown model.relaxDrop ["Sleep","Video Games"]))
        |> scaleX 2
        |> scaleY 2
        |> move (50,-70)
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



module Tetris exposing (init,view,update,Msg(..),Direction,Model,windowResize)

{-
   Copyright 2017 Christopher Kumar Anand

   Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

   1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

   3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

import GraphicSVG exposing (..)
import String exposing (..)
import List
import Array

type alias Model =
        {
          time : Float
        , width : Float
        , height : Float
        , blocks : List Block
        , blockNum : Float
        , x : Float
        , y : Float
        , rotate : Float
        , block : Block
        , hasMove : Bool
        , board : List (Shape (GraphicSVG.Msg Msg))
        , board2 : List (Shape (GraphicSVG.Msg Msg))
        , deadSpace : List (Int,Int)
        , li : List AVal
        , screen : String
        , hasGameStart : Bool
        , flash : Float
        , hasRowFull : Bool
        , animation : { time : Float, xcoord : Float, ycoord : Float , aniTime : Float, mouthRotate : Float, size : Float, hasAnimate : Bool }
        , animationOn : Bool
        , temp : (Float,Float)
        }

type Msg
    = Tick Float GetKeyState
    | MovePoint Direction
    | Rotate
    | NextBlock
    | GoToMainMenu
    | GoToInstructions
    | StartGame
    | RestartGame

type Direction
    =  Left
     | Right
     | Downn

type alias Block =
         {
            shape : Shape (GraphicSVG.Msg Msg)
          , points :
                     {
                       r0 : List (Float,Float)
                     , r1 : List (Float,Float)
                     , r2 : List (Float,Float)
                     , r3 : List (Float,Float)
                     }
          , name : Int
          }

type alias AVal =
  { coord : List (Int,Int)
    , name : Int
    , x : Float
    , y : Float
    , r : Float

   }

init : Model
init =
        {
          time = 0
        , width = 380
        , height = 512
        , blocks = [block6]
        , blockNum = 0
        , x = 0
        , y = 0
        , rotate = 0
        , block = block1
        , hasMove = True
        , board =[rect 20 30 |> filled white |> move (200,-200)]
        , board2 =[rect 20 30 |> filled white |> move (200,-200)]
        , deadSpace=[]
        , li=[]
        , screen = "mainMenu"
        , hasGameStart = False
        , flash=1
        , hasRowFull =False
        , animation =  { time = 0, xcoord = 0, ycoord = 0 , aniTime = 0, mouthRotate = (degrees 90), size = 20, hasAnimate = False }
        , animationOn = False
        , temp =(0,0)
        }

windowResize : Float -> Float -> Model -> Model
windowResize w h model =
    { model | width = w
            , height = h
    }

update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick t (gks,(x1,y1),(x2,y2)) ->
                 if model.hasGameStart then
                    let
                       animation = model.animation
                       this = { animation | hasAnimate = True }
                    in
                       if model.animation == this then
                            {model | animation = model.animation |> setTime t
                                                                 |> setXcoord
                                                                 |> setYcoord
                                                                 |> setHasAnimate
                                                                 |> setMouthRotate
                                                                 |> setSize
                                                                 |> setAniTime  }
                        else
                            { model | time = t
                                      , hasMove = case model.rotate of
                                                      0 -> if (inDeadSpace (toTileCoordinates model model.rotate) (model.deadSpace) 1) == False &&
                                                                tileInNum "y" 1 (toTileCoordinates model model.rotate) /= True then
                                                              True
                                                          else
                                                              False

                                                      1 -> if (inDeadSpace (toTileCoordinates model model.rotate) (model.deadSpace) 1)==False &&
                                                                tileInNum "y" 1 (toTileCoordinates model model.rotate) /= True then
                                                              True
                                                           else
                                                              False

                                                      2 -> if (inDeadSpace (toTileCoordinates model model.rotate) (model.deadSpace) 1)==False &&
                                                                tileInNum "y" 1 (toTileCoordinates model model.rotate) /= True then
                                                              True
                                                           else
                                                              False

                                                      _ -> if (inDeadSpace (toTileCoordinates model model.rotate) (model.deadSpace) 1)==False &&
                                                              tileInNum "y" 1 (toTileCoordinates model model.rotate) /= True then
                                                              True
                                                           else
                                                              False
                                        , y = if model.hasMove == True  then
                                                  if (gks DownArrow == Down)&& model.y > -260 &&
                                                      (inDeadSpace (toTileCoordinates model model.rotate) (model.deadSpace) 1) == False &&
                                                      (inDeadSpace (toTileCoordinates model model.rotate) (model.deadSpace) 2) == False &&
                                                      (inDeadSpace (toTileCoordinates model model.rotate) (model.deadSpace) 3) == False &&
                                                      (inDeadSpace (toTileCoordinates model model.rotate) (model.deadSpace) 4) == False  then
                                                    model.y-10
                                                  else
                                                    model.y - 1
                                              else
                                                  model.y
                                      , x = if model.hasMove == True then
                                                if (gks LeftArrow==JustDown) then
                                                    if tileInNum "x" 1 (toTileCoordinates model model.rotate)== False &&
                                                        (inDeadSpaceXL (toTileCoordinates model model.rotate) (model.deadSpace)) == False then
                                                      model.x-20
                                                    else
                                                      model.x
                                                else if (gks RightArrow==JustDown) then
                                                    if tileInNum "x" 11 (toTileCoordinates model model.rotate)==False &&
                                                        (inDeadSpaceXR (toTileCoordinates model model.rotate) (model.deadSpace)) == False then
                                                      model.x+20
                                                    else
                                                      model.x
                                                else
                                                  model.x
                                            else
                                              model.x
                                      ,flash= if (gks LeftArrow ==JustDown ) ||(gks RightArrow==JustDown)||(gks DownArrow==Down) then
                                                0
                                              else
                                                model.flash
                                      ,rotate=(if (gks Space==JustDown) then
                                                if model.hasMove == True then
                                                  if model.x<0 then
                                                      if tileInNum "x" 0 (toTileCoordinates model (model.rotate+1) ) /= True then
                                                          case model.rotate of
                                                              0->1
                                                              1->2
                                                              2->3
                                                              _->0
                                                      else
                                                          model.rotate
                                                  else
                                                      if tileInNum "x" 12 (toTileCoordinates model (model.rotate+1) ) /= True then
                                                          case model.rotate of
                                                              0->1
                                                              1->2
                                                              2->3
                                                              _->0
                                                      else
                                                          model.rotate

                                                else
                                                    model.rotate
                                            else
                                                model.rotate)

                                    , li = if (isRowFull (model.deadSpace++ toTileCoordinates model model.rotate))==True then
                                             (first (model.li ++[ aVal (toTileCoordinates model model.rotate) model.block.name model.x model.y model.rotate]) (rn (model.deadSpace++ toTileCoordinates model model.rotate)))
                                           else
                                             model.li
                                    , hasRowFull = if (isRowFull (model.deadSpace++ toTileCoordinates model model.rotate))==True then
                                                      True
                                                    else
                                                      model.hasRowFull

                                    , animation = if (isRowFull (model.deadSpace++ toTileCoordinates model model.rotate))==True then
                                           let
                                               oldanimation = model.animation
                                               newanimation = {oldanimation | hasAnimate = True}
                                            in
                                               newanimation
                                         else
                                             model.animation
                                    , deadSpace =  if (isRowFull (model.deadSpace++ toTileCoordinates model model.rotate))==True then
                                                     updateDeadSpace (first (model.li ++[ aVal (toTileCoordinates model model.rotate) model.block.name model.x model.y model.rotate]) (rn (model.deadSpace++ toTileCoordinates model model.rotate)))
                                                  else
                                                      model.deadSpace
                                   , board = if (isRowFull (model.deadSpace++ toTileCoordinates model model.rotate))==True then
                                              ( createShape (first (model.li ++[ aVal (toTileCoordinates model model.rotate) model.block.name model.x model.y model.rotate]) (rn (model.deadSpace++ toTileCoordinates model model.rotate))) )
                                              else
                                                model.board



                            }
              else
                {model|hasGameStart=False}

        MovePoint direct ->
             if model.hasMove == True then
                  case direct of
                      Left -> if tileInNum "x" 1 (toTileCoordinates model model.rotate)== False &&
                              (inDeadSpaceXL (toTileCoordinates model model.rotate) (model.deadSpace)) == False then
                                {model| x = model.x-20,flash=0}
                              else
                                {model| x=model.x,flash=0}


                      Right -> if tileInNum "x" 11 (toTileCoordinates model model.rotate)==False &&
                              (inDeadSpaceXR (toTileCoordinates model model.rotate) (model.deadSpace)) == False then
                                {model| x = model.x+20,flash=0}
                              else
                                {model| x = model.x,flash=0}
                      Downn -> if model.y > -270 && (inDeadSpace (toTileCoordinates model model.rotate) (model.deadSpace) 1) == False then
                                {model| y = model.y-20,flash=0}
                              else
                                {model| y = model.y,flash=0}



              else
                  {model| x = model.x,flash=0}



        Rotate -> { model | rotate = if model.hasMove == True then
                                        if model.x<0 then
                                            if tileInNum "x" 0 (toTileCoordinates model (model.rotate+1) ) /= True then
                                                case model.rotate of
                                                    0->1
                                                    1->2
                                                    2->3
                                                    _->0
                                            else
                                                model.rotate
                                        else
                                            if tileInNum "x" 12 (toTileCoordinates model (model.rotate+1) ) /= True then
                                                case model.rotate of
                                                    0->1
                                                    1->2
                                                    2->3
                                                    _->0
                                            else
                                                model.rotate

                                      else
                                          model.rotate


                    }

        NextBlock -> { model |  -- deadSpace = model.deadSpace ++ toTileCoordinates model model.rotate
                              li = (first (model.li ++[ aVal (toTileCoordinates model model.rotate) model.block.name model.x model.y model.rotate]) (rn (model.deadSpace++ toTileCoordinates model model.rotate)))
                       , deadSpace = if model.hasRowFull == False then
                                            if (isRowFull (model.deadSpace++ toTileCoordinates model model.rotate))==True then
                                                      updateDeadSpace (first (model.li ++[ aVal (toTileCoordinates model model.rotate) model.block.name model.x model.y model.rotate]) (rn (model.deadSpace++ toTileCoordinates model model.rotate)))
                                              else
                                                          model.deadSpace ++ toTileCoordinates model model.rotate
                                          else
                                              model.deadSpace

                  --  , li = model.li ++ [ aVal (toTileCoordinates model model.rotate) model.block.name model.x model.y model.rotate]

                    -- , board = model.board ++ [model.block.shape |> move (model.x,model.y+115)|> rotate (degrees model.rotate*90)]
                     , y =0
                     , x= 0
                     , rotate =0
                     , block = Maybe.withDefault block2 (List.head model.blocks)
                     , blocks= List.drop 1 model.blocks ++ [model.block]
                     , screen = if findHighest model > -30 then
                                    "gameEnd"
                                else
                                    model.screen
                     --, li = tag model

                     , board = if model.hasRowFull == False then
                                 if (isRowFull (model.deadSpace++ toTileCoordinates model model.rotate))==True then
                                    ( createShape (first (model.li ++[ aVal (toTileCoordinates model model.rotate) model.block.name model.x model.y model.rotate]) (rn (model.deadSpace++ toTileCoordinates model model.rotate))) )
                                  else
                                     model.board ++ [model.block.shape |> move (model.x,model.y+115)|> rotate (degrees model.rotate*90)]
                               else
                                 model.board
                     , hasRowFull = False

                     }
        GoToMainMenu -> { model | screen = "mainMenu"}
        GoToInstructions -> { model | screen = "instructions"}
        StartGame -> { model | screen = "play", hasGameStart=True}
        RestartGame -> { model |  screen = "mainMenu"
                                , time = 0
                                , blocks = [block2, block3]
                                , blockNum = 0
                                , x = 0
                                , y = 0
                                , rotate = 0
                                , block = block1
                                , hasMove = True
                                , board =[rect 20 30 |> filled white |> move (200,-200)]
                                , deadSpace=[]
                                ,hasGameStart=False
                                ,flash=1
                       }

bruh l model =
  l ++ [ aVal (toTileCoordinates model model.rotate) model.block.name model.x model.y model.rotate]

view : Model -> List (Shape (GraphicSVG.Msg Msg))
view model =
                [ case model.screen of
                    "mainMenu" -> mainMenu model
                    "play" -> play model
                    "gameEnd" -> gameEnd model
                    _ -> instructions model
                ]

mainMenu m =
              group [
                      button yellow darkYellow 13 "PLAY"
                            |> notifyTap StartGame --tells the program to go to the main menu when clicked
                            |> move(0,-30)
                    , button lightBlue darkBlue 11 "INSTRUCTIONS"
                            |> notifyTap GoToInstructions --tells the program to go to the instructions screen when clicked
                            |> move(0,-90)
                    ,   text "TETRIS"
                            |> bold
                            |> size 120
                            |> centered
                            |> sansserif
                           -- |> fixedwidth
                            |> filled white
                            |> addOutline (solid 2) (rgb 215 82 118)
                            |> move(0,50)
                  {-}  ,   text "Created by Yaminah"
                            |> size 14
                            |> fixedwidth
                            |> centered
                            |> filled lightOrange
                            |> move(0,30) -}
                   ]

instructions m =
                  group [
                          text "Instructions"
                            |> fixedwidth
                            |> centered
                            |> size 20
                            |> filled black
                            |> move(0,200)
                         , text "You can use this screen to write from instructions for how to play."
                            |> fixedwidth
                            |> centered
                            |> size 12
                            |> filled black
                            |> move(0,150)
                         , text "You can include text, graphics and even animations!"
                            |> fixedwidth
                            |> centered
                            |> size 12
                            |> filled black
                            |> move(0,140)
                         , button lightOrange darkOrange 14 "Main Menu"
                              |> notifyTap GoToMainMenu --tells the program to go to the main menu when clicked
                              |> move(0,-150)
                        ]

gameEnd m =
            group [
                    button yellow darkYellow 12 "PLAY AGAIN!"
                        |> notifyTap RestartGame --tells the program to restart the game when clicked
                        |> rotate(degrees (15*sin(m.time)))
                  ]
play model =
             group [
                     --rect 330 300 |> filled (rgb 224 249 250) |> move (0,-40)
                     if model.animation.hasAnimate ==True then
                      block model |> makeTransparent 0

                     else
                      block model

                   , board1 model
                  -- , outlined (dashed 2) red (polygon [(-165,90),(165, 90)])
                   , buttons model
                   , let
                       animation = model.animation
                       this = { animation | hasAnimate = True }
                    in
                       if model.animation == this then
                           group  (pig model.animation) |> move (model.x,model.y)
                       else
                         square 1 |> filled white |> makeTransparent 0

                   ]



buttons m =
            group [
                    group [ --polygon [(165,190),(290,190),(290,0),(165,0),(165,190)] |> filled yellow
                           polygon [(-110,190),(-3,190),(-3,-124),(-110,-124),(-110,-124)] |> filled lightGrey |> makeTransparent (m.flash * ( 0.75*(sin(2*m.time) )))
                          , group[ rect 5 5 |> filled white |> move (0,-5.45), triangle 6 |> filled white |> move (0,0) |> rotate (degrees 90)]|> rotate (degrees 90)|>scale 3|>move(-60,0) |>makeTransparent (m.flash )
                          , polygon [(3,190),(110,190),(110,-124),(3,-124),(3,190)] |> filled lightGrey |> makeTransparent (m.flash * ( 0.75*(sin (2*(m.time) ) )))
                          , group[ rect 5 5 |> filled white |> move (0,-5.45), triangle 6 |> filled white |> move (0,0) |> rotate (degrees 90)]|> rotate (degrees -90)|>scale 3|>move(60,0) |> makeTransparent (m.flash )
                          , polygon [(-110,-130),(110,-130),(110,-190),(-110,-190),(-110,-190)]|> filled lightGrey |> makeTransparent (m.flash * ( 0.75*(sin(2*m.time) )))
                          , group[ rect 5 5 |> filled white |> move (0,-5.45), triangle 6 |> filled white |> move (0,0) |> rotate (degrees 90)]|> rotate (degrees 180)|>scale 3|>move(0,-165) |> makeTransparent (m.flash )
                          , polygon [(165,190),(290,190),(290,2),(165,2),(165,190)] |> filled lightBlue |> move (-55,0) |> makeTransparent (if m.hasMove==False then
                                                                                                                                              1
                                                                                                                                          else
                                                                                                                                             0.5)
                          , text "NEXT"|> bold |> sansserif |> size 14|> filled darkBlue  |> move( 150,95)|> makeTransparent (if m.hasMove==False then
                                                                                                                                              1
                                                                                                                                          else
                                                                                                                                             0.5)
                          , text "BLOCK"|> bold |> sansserif |> size 14|> filled darkBlue  |> move( 145,82)|> makeTransparent (if m.hasMove==False then
                                                                                                                                              1
                                                                                                                                          else
                                                                                                                                             0.5)
                          ]


                  , group [
                            polygon [(-110,190),(0,190),(0,-130),(-110,-130),(-110,-130)] |> filled blue |> move (0,0) |> makeTransparent 0 |> notifyTap (MovePoint Left)
                          , polygon [(0,190),(110,190),(110,-130),(0,-130),(0,190)] |> filled red |> move (0,0) |> makeTransparent 0 |> notifyTap (MovePoint Right)
                          , polygon [(-110,-130),(110,-130),(110,-190),(-110,-190),(-110,-190)]|> filled lightGrey |> makeTransparent 0 |> notifyTap (MovePoint Downn)
                          , polygon [(165,0),(290,0),(290,-190),(165,-190),(165,0)] |> filled yellow |> move (-55,4) |> makeTransparent 1 |> notifyTap (Rotate)
                          , text "ROTATE"|> bold |> sansserif |> size 14|> filled darkYellow  |> move( 140,-95) |>notifyTap (Rotate)
                          , if m.hasMove == False then
                             polygon [(165,190),(290,190),(290,2),(165,2),(165,190)] |> filled lightBlue |> move (-55,0) |> makeTransparent 0 |> notifyTap (NextBlock)
                            else
                              roundedRect 20 20 5 |> filled white |> makeTransparent 0 |> move (0,-200)

                          ]
              --  ,text (toString m.deadSpace) |> serif |> filled black
               -- , text (toString (toTileCoordinates m m.rotate) ) |> filled black |> move (0,20)
               --, text (toString (inDeadSpaceXL (toTileCoordinates m m.rotate) (m.deadSpace))) |> filled black |> move (0,35)
             --  , text (toString m.x) |> filled black |> move (80,-20)
             --  , text (toString m.y) |> filled black |> move (80,-40)
              -- , text (toString (rn m.deadSpace)) |> filled black |> move (80,-20)
              -- , text (toString (first m.li (highestFullRow m.deadSpace))) |> size 4 |> filled black |> move (-300,-20)
             --  , text (toString (createShape (first m.li (highestFullRow m.deadSpace))))|> size 4 |> filled black |> move (-300,-50)
            --   , text (toString m.li) |> size 4 |> filled black |> move (-300,-40)
             --  , group [ rect 60 30 |> filled red, rect 60 30 |> filled red |> move (30,-30)]
              -- , text (toString (createShape m)) |> filled black |> move (200, -60)
                  ]

aVal : List (Int,Int) -> Int -> Float -> Float -> Float -> AVal
aVal q w e t u =
  { coord = q
    , name = w
    , x = e
    , y = t
    , r = u

   }
--draws the current board onto the screen
board1 m = group m.board

block1 : Block
block1 =
         {
            shape = group [
                            rect 90 30  |> filled (rgb 215 82 118)
                          , rect 30 30  |> filled (rgb 215 82 118) |> move (0,30)
                          , rect 30 10 |> filled (rgb 215 82 118) |> move (0,15)
                          ]|> scale (2/3)
          , points =
                     {
                       r0 =[(-1,0),(0,0),(1,0),(0,1)]
                     , r1=[(0,-1),(0,0),(0,1),(-1,0)]
                     , r2=[(1,0),(0,0),(-1,0),(0,-1)]
                     , r3=[(0,1),(0,0),(0,-1),(1,0)]
                     }
          , name = 1
          }

block2 =
         {
            shape = group [
                            rect 90 30  |> filled (rgb 215 82 118)
                          , rect 30 30  |> filled (rgb 215 82 118) |> move (-30,30)
                          , rect 30 10 |> filled (rgb 215 82 118) |> move (-30,15)
                          ]|> scale (2/3)
          , points =
                     {
                       r0=[(-1,0),(0,0),(1,0),(-1,1)]
                     , r1=[(0,-1),(0,0),(0,1),(-1,-1)]
                     , r2=[(1,0),(0,0),(-1,0),(1,-1)]
                     , r3=[(0,1),(0,0),(0,-1),(1,1)]
                     }
          , name = 2
          }
block3 =
         {
            shape =
                  group [
                            group [
                                    rect 60 30  |> filled (rgb 215 82 118)
                                  , rect 60 30  |> filled (rgb 215 82 118) |> move (30,-30)
                                  ] |> move (-15, 30)
                          , group [
                                    rect 90 30  |> filled (rgb 215 82 118)
                                  , rect 30 32  |> filled (rgb 215 82 118) |> move (0,29)
                                  ]|> makeTransparent 0
                          , rect 30 10 |> filled (rgb 215 82 118) |> move (0,15)
                          ] |> scale (2/3)
          , points =
                     {
                       r0=[(0,0),(1,0),(-1,1),(0,1)]
                     , r1=[(0,0),(0,1),(-1,-1),(-1,0)]
                     , r2=[(0,0),(-1,0),(1,-1),(0,-1)]
                     , r3=[(0,0),(0,-1),(1,1),(1,0)]
                     }
          , name = 3
          }
block4 =
        {
            shape = group [
                            rect 90 30  |> filled (rgb 215 82 118)
                          , rect 30 30  |> filled (rgb 215 82 118) |> move (30,30)
                          , rect 30 10 |> filled (rgb 215 82 118) |> move (30,15)
                          ]|> scale (2/3)
          , points =
                     {
                       r0=[(-1,0),(0,0),(1,0),(1,1)]
                     , r1=[(0,-1),(0,0),(0,1),(-1,1)]
                     , r2=[(1,0),(0,0),(-1,0),(-1,-1)]
                     , r3=[(0,1),(0,0),(0,-1),(1,-1)]
                     }
          , name = 4
      }
block5 = {
            shape =
                  group [
                            group [
                                    rect 60 30  |> filled (rgb 215 82 118)
                                  , rect 60 30  |> filled (rgb 215 82 118) |> move (30,-30)
                                  ] |> move (-15, 30)
                          , group [
                                    rect 90 30  |> filled (rgb 215 82 118)
                                  , rect 30 32  |> filled (rgb 215 82 118) |> move (0,29)
                                  ]|> makeTransparent 0
                          , rect 30 10 |> filled (rgb 215 82 118) |> move (0,15)
                          ] |>mirrorX |> scale (2/3)
          , points =
                     {
                       r0=[(-1,0),(0,0),(0,1),(1,1)]
                     , r1=[(0,-1),(0,0),(-1,0),(-1,1)]
                     , r2=[(1,0),(0,0),(0,-1),(-1,-1)]
                     , r3=[(0,1),(0,0),(1,0),(1,-1)]
                     }
          , name = 5
     }
{-block7 = {
              shape= rect 120 30 |>filled orange |> move (15,0)
            , points =
                     {
                       r0=[(-1,0),(0,0),(1,0),(2,0)]
                     , r1=[(0,-1),(0,0),(0,1),(0,2)]
                     , r2=[(1,0),(0,0),(-1,0),(-2,0)]
                     , r3=[(0,1),(0,0),(0,-1),(0,-2)]
                     }
        }
-}
block6 = {
             shape= pig1 --rect 60 60 |>filled (rgb 215 82 118)|> scale (2/3) |>move (10,10)
            ,points ={
                       r0=[(0,0),(1,0),(0,1),(1,1)]
                     , r1=[(0,0),(1,0),(0,1),(1,1)]
                     , r2=[(0,0),(1,0),(0,1),(1,1)]
                     , r3=[(0,0),(1,0),(0,1),(1,1)]
                      }
            , name = 6
          }
block m =
 m.block.shape  |> move (0, 115)
                |> move (m.x, m.y)
                |> rotate (degrees m.rotate*90)


whichBlock  x =
  case x of
    1 -> block1.shape
    2 -> block2.shape
    3 -> block3.shape
    4 -> block4.shape
    5 -> block5.shape
    _ -> block6.shape


-- converts an (x,y) coordinate in terms of screen pixels into an (x,y) coordinate in terms of tile number
numSpace (x,y) =
  case (x,y) of
       (x,y) -> (round ((x+120)/20), num y) -- CHANGE 320 TO 350 IF FINDING OUT WHEEHR THE BLOCK IS IN DEADSPACE OR NOT

-- num converts the y part of an (x,y) coordinate into a tile number
num x =
  if -290 < x && x <= -270 then
    2
  else if -270 < x && x <= -250 then
    3
  else if -250 < x && x <= -230 then
    4
  else if -230 <x && x <= -210 then
    5
  else if -210 <x && x <= -190 then
    6
  else if -190 <x && x <= -170 then
    7
  else if -170 <x && x <= -150 then
    8
  else if -150 <x && x <= -130 then
    9
  else if -130 <x && x <= -110 then
    10
  else if -110 <x && x <= -90 then
    11
  else if -90 <x && x <= -70 then
    12
  else if -70 <x && x <= -50 then
    13
  else if -50 <x && x <= -30 then
    14
  else if -30 <x && x <= -10 then
    15
  else if x <= -290 then
    1
  else
    0

num2 x =
  if -290 < x && x <= -260 then
    2
  else if -260 < x && x <= -230 then
    3
  else if -230 < x && x <= -200 then
    4
  else if -200 <x && x <= -170 then
    5
  else if -170 <x && x <= -140 then
    6
  else if -140 <x && x <= -110 then
    7
  else if -110 <x && x <= -80 then
    8
  else if -80 <x && x <= -50 then
    9
  else if -50 <x && x <= -20 then
    10
  else if x <= -290 then
    1
  else
    0
-- retrieves the appropriate set of points for a shape based on which rotation its in and converts the shape's coordinates into tile coordinate,  m is model and r is the rotation
toTileCoordinates m r  =
  case r of
    1 -> List.map (func1 m)  (m.block.points.r1)
    2 -> List.map (func1 m)  (m.block.points.r2)
    3 -> List.map (func1 m)  (m.block.points.r3)
    _ -> List.map (func1 m)  (m.block.points.r0)

-- converts a shape's coordinate into tile coordinate for a specific block of the shape
func1 m (x,y) =
  case (x,y) of
       (x,y) -> numSpace ((20*x) + m.x, (20*y) + m.y)


-- determines whether the current tile coordinates of a shape and 1 tile above the tile coordinates of the shapes in deadspace coincide
inDeadSpace currentPos deadSpace n =

  List.member True (List.map (\x -> List.member x deadSpace) (List.map (\(x,y) -> (x, y-n)) currentPos))

inDeadSpaceXL currentPos deadSpace =
  List.member True ((List.map (\x-> List.member x deadSpace ) (List.map (\(x,y)-> (x-1,y-1)) currentPos))++ (List.map (\x-> List.member x deadSpace ) (List.map (\(x,y)-> (x-1,y)) currentPos)))

inDeadSpaceXR currentPos deadSpace =
  List.member True ((List.map (\x-> List.member x deadSpace ) (List.map (\(x,y)-> (x+1,y-1)) currentPos))++(List.map (\x-> List.member x deadSpace ) (List.map (\(x,y)-> (x+1,y)) currentPos)))

findHighest m =
  case m.rotate of
    0 -> (Maybe.withDefault 0 (List.maximum (List.map (\(x,y)->y) (m.block.points.r0))))*(20) + m.y
    1 -> (Maybe.withDefault 0 (List.maximum (List.map (\(x,y)->y) (m.block.points.r1))))*(20) + m.y
    2 -> (Maybe.withDefault 0 (List.maximum (List.map (\(x,y)->y) (m.block.points.r2))))*(20) + m.y
    _ -> (Maybe.withDefault 0 (List.maximum (List.map (\(x,y)->y) (m.block.points.r3))))*(20) + m.y

tileInNum coordinate n l =
  case coordinate of
    "x" -> List.member n (List.map (\(x,y)-> x) l)
    _  -> List.member n (List.map (\(x,y)-> y) l)

button colour c2 textSize txt =
                             group [
                                     rect 100 50
                                        |> filled colour
                                   , text txt
                                        |> bold
                                        |> sansserif
                                        |> size textSize
                                        |> centered
                                       -- |> fixedwidth
                                        |> filled c2
                                        |> move(0,-5)
                                   ]
--removeRow m =

findHighestY d =
  (Maybe.withDefault 0 (List.maximum (List.map (\(x,y)-> y)(d))))


f1 y d =
  List.map (\x->List.member (x,y) d)[1,2,3,4,5,6,7,8,9,10,11]

f2 d =
  List.map (\x-> List.member False (f1 x d)) (List.range 1 (findHighestY d))

rn d = List.map2 (\x s-> if x==False then
                          ( s)
                        else -1)(f2 d )[1,2,3,4,5,6,7,8,9,10,11]
--this
tag m lis =
  List.concat (List.map (\ s -> List.map (\x-> f3 x s) lis )(rn m))

f3 x s =
  if s>=1 then
    if List.member s (List.map (\(z,y) -> y ) x.coord) then
      {x | name = -1, y = x.y-20, coord = c2 "y" x s}
    else
      {x | name = x.name, y = x.y-20, coord = c2 "n" x s}
  else
    {x | name = x.name}

c2 check elem bad =
    case check of
      "y" -> List.map (\(x,y)-> if y == bad then
                            (-1,-1)
                        else
                             (x,y-1)) elem.coord
      _ -> List.map (\(x,y) -> (x,y-1)) elem.coord




--
createShape li =
  List.concat (List.map ( \z-> c1  z  ) li)

c1  z =
  case z.name of
    -1 -> List.map (\(x,y)-> if x>=1 then
                                  rect 20 20 |> filled (rgb 215 82 118) |> move (Basics.toFloat(x)*20 - 120, Basics.toFloat(y)*20 - 196)
                              else
                                    rect 5 5 |> filled white |> makeTransparent 0 ) (z.coord)
    _ -> [whichBlock  z.name |> move (z.x,z.y+115) |> rotate (degrees z.r*90)]


{-}
deadSpace =
  List.map (\elem-> elem.coord) li
-}
highestFullRow d =
  Maybe.withDefault 0 (List.maximum (List.filter (\x-> x>=1) (rn d)))

--send m.li and highestfullrow initilly
first li r =
  case r of
    [] -> li
    x::xs -> h1 x li xs

h1 x li xs =
  case x of
    -1-> first li xs
    _-> first (List.map (\t-> second t x) li) xs


second elem fr =
  if (List.member fr (List.map (\(x,y) -> y) elem.coord)) == True then
      {elem | name = -1, y = elem.y-20, coord = List.map (\(x,y)-> if y==(fr) then
                                                                    (0,0)
                                                                  else if y>fr then
                                                                     (x,y-1)
                                                                   else
                                                                      (x,y)) elem.coord}
  else
      {elem | name = elem.name, y = elem.y-20, coord = List.map (\(x,y)-> if y> fr then
                                                                            (x,y-1)
                                                                            else
                                                                              (x,y)) elem.coord}


isRowFull m =
  if ((List.length (rn m )) - (List.length (List.filter (\x-> x == -1) (rn m))) ) > 0 then
    True
  else
    False


updateDeadSpace li =
  List.filter filt (List.concat (List.map (\z-> z.coord) li))

filt coord =
  case coord of
    (x,y)->if y>0 then
              True
            else
              False






--ANIAMTION STUFF

pig animation =     [group [
                             square 20 |> filled blue |> makeTransparent (clamp 0 1 (1 - (0.8* animation.aniTime)))
                            , square 20 |> filled blue |> move (20,0) |> makeTransparent (clamp 0 1 (1 - (0.8* animation.aniTime)))
                            , square 20 |> filled blue |> move (20,20) |> makeTransparent (clamp 0 1 (1 - (0.8* animation.aniTime)))
                            , square 20 |> filled blue |> move (0,20) |> makeTransparent (clamp 0 1 (1 - (0.8* animation.aniTime)))
                            , rect 8 12 |> filled (hsl (degrees 320) 0.7 0.45) |> move (-3,-2)
                            , rect 8 12 |> filled (hsl (degrees 320) 0.7 0.45) |> move ((clamp 20 23 (animation.size + 5)),-2)
                            , rect (clamp 0 36 (animation.size + 14)) (clamp 0 30 (animation.size + 12)) |> filled pink |> move (10,10)
                             , oval 38 27 |> filled pink |> move (8,10) |> scale(1/clamp 1 10 (animation.size/3 - 3))
                             , square (clamp 0 17 (animation.size + 1)) |> filled (hsl (degrees 320) 1 0.75) |> move (19,20)
                             , circle 10 |> filled (hsl (degrees 320) 1 0.75) |> move (19,20) |> scale (1/clamp 1 10 (animation.size/3 - 3))
                             , wedge 2 0.5 |> filled darkCharcoal |> move (20,13) |> rotate (degrees 90) |> scaleX (clamp -1 1 (-1 * animation.aniTime + 1.5))
                                            |> scaleY (clamp 1 2 ((animation.aniTime/2) - 0.5))
                             , oval 10 5 |> filled pink |> move (20,17)
                             , circle 1 |> filled (hsl (degrees 320) 0.7 0.45) |> move (18,17)
                             , circle 1 |> filled (hsl (degrees 320) 0.7 0.45) |> move (23,17)
                            , group[ circle 2 |> filled white
                             ,  circle 1 |> filled black |> move (0.2,0)
                             ,  circle 0.5 |> filled white |> move (0.7,0.7)
                              ] |> move(25,22)
                            , group[ circle 2 |> filled white
                             ,  circle 1 |> filled black |> move (0.2,0)
                             ,  circle 0.5 |> filled white |> move (0.7,0.7)
                              ] |> move(14,22)
                            , triangle 4 |> filled (hsl (degrees 320) 1 0.75) |> move (11,26) |> rotate (degrees 15)
                            , triangle 4 |> filled (hsl (degrees 320) 1 0.75) |> move (27,26.3) |> rotate (degrees 30)
                            , curve (20,20) [Pull(20,0)(10,10),Pull (30,10) (10,0)] |> outlined (solid 1.5) pink |> move (7,20)
                              |> scaleY 0.45 |> scaleX -0.8

                            ] |> move (animation.xcoord,animation.ycoord) |> move (0,115)--|> notifyTap Button
                            ]
pig1  =     group[
                             square 20 |> filled blue |> makeTransparent 1
                            , square 20 |> filled blue |> move (20,0) |> makeTransparent 1
                            , square 20 |> filled blue |> move (20,20) |> makeTransparent 1
                            , square 20 |> filled blue |> move (0,20) |> makeTransparent 1
                            , rect 8 12 |> filled (hsl (degrees 320) 0.7 0.45) |> move (-3,-2)
                            , rect 8 12 |> filled (hsl (degrees 320) 0.7 0.45) |> move (23,-2)
                            , rect (34) (30) |> filled pink |> move (10,10)                    --
                             , oval 38 27 |> filled pink |> move (8,10) |> scale(1/(20/3 - 3))
                             , square (17) |> filled (hsl (degrees 320) 1 0.75) |> move (19,20) --
                             , circle 10 |> filled (hsl (degrees 320) 1 0.75) |> move (19,20) |> scale (1/(20/3 - 3))
                             , wedge 2 0.5 |> filled darkCharcoal |> move (20,13) |> rotate (degrees 90) |> scaleX (( 1))
                                            |> scaleY (( 1))
                             , oval 10 5 |> filled pink |> move (20,17)
                             , circle 1 |> filled (hsl (degrees 320) 0.7 0.45) |> move (18,17)
                             , circle 1 |> filled (hsl (degrees 320) 0.7 0.45) |> move (23,17)
                            , group[ circle 2 |> filled white
                             ,  circle 1 |> filled black |> move (0.2,0)
                             ,  circle 0.5 |> filled white |> move (0.7,0.7)
                              ] |> move(25,22)
                            , group[ circle 2 |> filled white
                             ,  circle 1 |> filled black |> move (0.2,0)
                             ,  circle 0.5 |> filled white |> move (0.7,0.7)
                              ] |> move(14,22)
                            , triangle 4 |> filled (hsl (degrees 320) 1 0.75) |> move (11,26) |> rotate (degrees 15)
                            , triangle 4 |> filled (hsl (degrees 320) 1 0.75) |> move (27,26.3) |> rotate (degrees 30)
                            , curve (20,20) [Pull(20,0)(10,10),Pull (30,10) (10,0)] |> outlined (solid 1.5) pink |> move (7,20)
                              |> scaleY 0.45 |> scaleX -0.8

                            ]



setTime new animation =
    { animation | time = new }

setAniTime animation =
    { animation | aniTime = case animation.hasAnimate of
                                        True -> animation.aniTime + 0.05
                                        _ -> animation.aniTime }


setXcoord animation =
    { animation | xcoord = case animation.hasAnimate of
                                    True -> if animation.aniTime >= 1.75
                                                 then animation.xcoord + 0.5
                                                 else animation.xcoord

                                    _ -> animation.xcoord }

setYcoord animation =
    { animation | ycoord = case animation.hasAnimate of
                                    True -> if animation.aniTime < 1
                                                 then animation.ycoord + animation.aniTime
                                                 else if animation.aniTime < 1.5
                                                      then animation.ycoord - animation.aniTime
                                                      else abs(cos(animation.aniTime * 3 + 0.17)*3)

                                    _ -> animation.ycoord }

setMouthRotate animation =
    { animation | mouthRotate = case animation.hasAnimate of
                                    True -> if animation.mouthRotate > (degrees -87)
                                                  then animation.mouthRotate - (degrees 3.75)
                                                  else animation.mouthRotate
                                    _ -> animation.mouthRotate }

setSize animation =
    { animation | size = case animation.hasAnimate of
                                 True -> if animation.size <= 0
                                              then 0
                                              else animation.size - animation.aniTime
                                 _ -> animation.size }

setHasAnimate animation =
    { animation | hasAnimate = case animation.hasAnimate of
                                       True -> if animation.aniTime >= 4.5
                                                    then False
                                                    else True
                                       False -> False }



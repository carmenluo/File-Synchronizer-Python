
import GraphicSVG exposing (..)

import Html
import Html.Attributes
import Html.Events

import Http as Http
import Svg
import Svg.Attributes exposing (..)
import String exposing (..)
import Svg exposing (Attribute)
import Array
import Mouse exposing (Position)
import Json.Decode as Json exposing (..)
import Time exposing (..)
import Window
import Task
import Keyboard exposing (..)
import Dict
import Char
import Tuple
import List

{-| Sorts points by x-coordinate.
-}
sortByX : List(Float, Float) -> List(Float,Float)
sortByX list = 
    List.sortBy Tuple.first list 

{-| Sorts points by x-coordinate for each line. Uses sortByX function.
-}
sortLines : List (List (Float,Float) ) -> List ( List(Float,Float) )
sortLines lists = map sortByX lists



maxX : List(List(Float,Float)) -> Maybe Float
maxX lists =
    let 
        (x,y) = unzip (concat lists)

    in
        maximum x

maxY : List(List(Float,Float)) ->Maybe Float 
maxY lists =
    let 
        (x,y) = unzip (concat lists)

    in
        maximum y

minY : List(List(Float,Float)) ->Maybe Float 
maxX lists =
    let 
        (x,y) = unzip (concat lists)

    in
        minimum x

--drawGraph : List (List (Float,Float) ) -> List (Shape userMsg)
drawGraph lists =
    let
        sortedLists = sortLines lists
             
    in
    group[
    line (0,0) (maxX sortedLists,0) |> outlined (solid 3) black
    ,line (0,maxY sortedLists) (0,minY sortedLists) |> outlined (solid 3) black
    ,drawLines sortedLists
    ]

drawLine xP yP line shapes = 
    case line of 
        [] -> shapes 
        [(xN,yN)] -> drawLine xN yN [] (shapes ++ line (xP,yP) (xN,yN) |> outlined (solid 3) black )
        ((xN,yN)::xs) -> drawLine xN yN xs (shapes ++ line (xP,yP) (xN,yN) |> outlined (solid 3) black )

drawLines lines shapes = 
    case lines of 
        [] -> shapes 
        [x] -> drawLines [] (shapes ++ drawLine 0 0 x shapes)
        (x::xs) -> drawLines xs (shapes ++ drawLine 0 0 x shapes) 
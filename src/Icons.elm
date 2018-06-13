module Icons exposing (running, sleeping, biking, excercise,relax , social )
import GraphicSVG exposing (..)


{- ALL OF THE CODE FOR THE FACE -}
background  = group [
  circle 50 
    |> filled white
  ,circle 45 
    |> filled lightBlue
  ,circle 45 
    |> outlined (solid 1.5) black
  ,circle 50 
    |> outlined (solid 1.5) black
          ]
swimming  = group [
  background 
  ,group [
    circle 8
      |> filled white
      |> addOutline (solid 1) black
      |> move (68,-44)
    ,polygon [(18,-75),(18,-65),(25,-63),(37,-67),(47,-63),(60,-67),(70,-63)
      ,(81,-67), (81,-75),(70,-71),(60,-75),(48,-71),(37,-75),(25,-71)]
      |> filled white
      |> addOutline (solid 1) black
    ,polygon [(29,-58),(37,-61),(48,-57),(59,-60),(65,-59)
      ,(50,-38),(64,-31),(60,-25),(40,-34),(40,-37),(46,-45)]
      |> filled white
      |> addOutline (solid 1) black
      ] |> move (-50,50)
  ]
sleepZ = group [
  polygon [(53,-28),(53,-25),(58,-19),(53,-19),(53,-15),(64,-15),(64,-19)
    ,(58,-25),(64,-25),(64,-28)]
      |> outlined (solid 1.5) black
  ,polygon [(53,-28),(53,-25),(58,-19),(53,-19),(53,-15),(64,-15),(64,-19)
    ,(58,-25),(64,-25),(64,-28)]
      |> filled white
  ]
sleeping = group [
  background 
  ,group [
    polygon [(17,-76),(17,-35),(25,-35),(25,-59),(83,-59)
      ,(83,-76),(76,-76),(76,-67),(25,-67),(25,-76)]
      |> filled white
    ,polygon [(42,-56),(42,-45),(43,-43),(46,-41)
      ,(74,-41),(80,-44),(83,-49),(83,-56)]
      |> filled white
    ,sleepZ 
    ,sleepZ  
      |> scale (0.7)
      |> move (5,-10)
    ,sleepZ  
      |> scale (0.5)
      |> move (6,-20)
    ,circle 6 
      |> filled white
      |> move (33,-50)
    ,polygon [(17,-76),(17,-35),(25,-35),(25,-59),(83,-59)
      ,(83,-76),(76,-76),(76,-67),(25,-67),(25,-76)]
      |> outlined (solid 1) black
    ,polygon [(42,-56),(42,-45),(43,-43),(46,-41)
      ,(74,-41),(80,-44),(83,-49),(83,-56)]
      |> outlined (solid 1) black
    ,sleepZ 
    ,sleepZ  
      |> scale (0.7)
      |> move (5,-10)
    ,sleepZ  
      |> scale (0.5)
      |> move (6,-20)
    ,circle 6 
      |> outlined (solid 1) black
      |> move (33,-50)
      ] |> move (-50,50)
  ]
running = group [
  background 
  ,group [
    polygon [(14,-71),(15,-63),(33,-64),(41,-53),(52,-35),(45,-33),(34,-43) 
      ,(28,-37), (44,-25) ,(61,-28) ,(69,-34) ,(73,-45),(86,-46)
      ,(86,-53),(67,-52),(62,-43),(53,-57),(67,-68),(57,-87),(49,-84)
      ,(56,-70),(45,-62),(36,-72)]
      |> filled white
    ,circle 7 
      |> filled white
      |> move (70,-23)
    ,polygon [(14,-71),(15,-63),(33,-64),(41,-53),(52,-35),(45,-33),(34,-43) 
      ,(28,-37), (44,-25) ,(61,-28) ,(69,-34) ,(73,-45),(86,-46)
      ,(86,-53),(67,-52),(62,-43),(53,-57),(67,-68),(57,-87),(49,-84)
      ,(56,-70),(45,-62),(36,-72)]
      |> outlined (solid 1) black
    ,circle 7 
      |> outlined (solid 1) black
      |> move (70,-23)
      ] |> move (-50,50)
  ]
biking  = group [
  background 
  ,group [
    polygon [(52,-69),(45,-68),(48,-53),(33,-44),(34,-36)
    ,(51,-24),(58,-24),(65,-37),(79,-37),(79,-42)
    ,(63,-42),(57,-34),(47,-42),(55,-49)]
      |> filled white
    ,circle 7 
      |> filled white
      |> move (68,-22)
    ,circle 14 
      |> filled white
      |> move (28,-64)
    ,circle 14 
      |> filled white
      |> move (71,-64)
    ,circle 8 
      |> filled lightBlue
      |> move (28,-64)
    ,circle 8 
      |> filled lightBlue
      |> move (71,-64)
   , polygon [(52,-69),(45,-68),(48,-53),(33,-44),(34,-36)
    ,(51,-24),(58,-24),(65,-37),(79,-37),(79,-42)
    ,(63,-42),(57,-34),(47,-42),(55,-49)]
      |> outlined (solid 1) black
    ,circle 7 
      |> outlined (solid 1) black
      |> move (68,-22)
    ,circle 14 
      |> outlined (solid 1) black
      |> move (28,-64)
    ,circle 14 
      |> outlined (solid 1) black
      |> move (71,-64)
    ,circle 8 
      |> outlined (solid 1) black
      |> move (28,-64)
    ,circle 8 
      |> outlined (solid 1) black
      |> move (71,-64)
      ] |> move (-50,50)
  ]

friends = group [
  background 
  ,group [
    polygon [(-38,39),(-10,39),(-6,36.3),(-4,32.4),(-4,-2.6),(-11.8,-2.6),
    (-11.8,23.3),(-13.4,23.3),(-13.4,-57),(-14.9,-60),(-18.5,-61),(-22,-60),
    (-23.8,-57),(-23.8,-20),(-25.2,-20),(-25.2,-57),(-27,-60),(-30.3,-61),
    (-33.7,-60),(-35.3,-57),(-35.3,23.3),(-36.7,23.3),(-36.7,-2.6),(-43.6,-2.6),
    (-43.6,32.4),(-41.8,36.3)]
      |> filled white
      |> addOutline (solid 1) black
    ,polygon [(-38,39),(-10,39),(-6,36.3),(-4,32.4),(-4,-2.6),(-11.8,-2.6),
    (-11.8,23.3),(-13.4,23.3),(-13.4,-57),(-14.9,-60),(-18.5,-61),(-22,-60),
    (-23.8,-57),(-23.8,-20),(-25.2,-20),(-25.2,-57),(-27,-60),(-30.3,-61),
    (-33.7,-60),(-35.3,-57),(-35.3,23.3),(-36.7,23.3),(-36.7,-2.6),(-43.6,-2.6),
    (-43.6,32.4),(-41.8,36.3)]
      |> filled white
      |> move (48,0)
      |> addOutline (solid 1) black
    ,circle 10
      |> filled white
      |> addOutline (solid 1) black
      |> move (24,51)
    ,circle 10
      |> filled white
      |> addOutline (solid 1) black
      |> move (-24,51)
      ] |> scale 0.55
  ]
excercise  = group[
    running 
    ,text "EXCERCISE"
        |> fixedwidth
        |> bold
        |> size 17
        |> filled pink
        |> addOutline (solid 1) black
        |> move (-40,-5)
    ]
relax = group[
    sleeping 
    ,text "RELAXING"
        |> fixedwidth
        |> bold
        |> size 17
        |> filled pink
        |> addOutline (solid 1) black
        |> move (-35,-5)
    ]
social  = group[
    friends 
    ,text "SOCIAL"
        |> fixedwidth
        |> bold
        |> size 17
        |> filled pink
        |> addOutline (solid 1) black
        |> move (-27,-5)
    ]

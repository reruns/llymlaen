module Example.IntermissionA where

import App.Element
import App.Static

import Prelude
import Data.Maybe (Maybe(Just,Nothing))

interA = { time: 0
         , ctx: Nothing
         , color: {r:175, g: 143, b:90}
         , statics: []
         , elements: [[]]
         , targetIndex: {layer: -1, idx: -1}
         }
          

{-            
line s p = boxStatic $ staticRect $ defaultRectMoment { enabled = true
                                                      , time = 0
                                                      , size = s
                                                      , pos = p
                                                      , opacity = 100
                                                      }
                     
lines = (map (line {w:1000,h:1}) [{x:0,y:200}, {x:0,y:400},{x:0,y:600}])
        <> (map (line {w:1,h:1000}) [{x:200, y:0}, {x:400,y:0},{x:600, y:0}])


testm1 =  defaultCircleMoment 
           { enabled = true
           , time = 0
           , radius = 200
           , pos = {x: 150, y: 150}
           , opacity = 100
           , color = {r:255,g:0,b:0}
           }
        
        
test1 = unfoldDrawable $ (\(Element e) -> Element $ e {layer = 1, keys = [testm1], current = testm1}) circleElem
test2 = unfoldDrawable $ (\(Element e) -> Element $ e {layer = 1, keys = [testm2], current = testm2}) circleElem
        
testm2 =  defaultCircleMoment 
           { enabled = true
           , time = 0
           , radius = 200
           , pos = {x: 450, y: 450}
           , opacity = 100
           , color = {r:0,g:0,b:255}
           }
         
          

{-
megabeam =  unfoldDrawable $ (\(Element e) -> Element $ e { layer = 2, keys = beams }) rectElem      
            
beam0 = defaultRectMoment
        { enabled = true
        , time = 900
        , angle = 0
        , size = {w: 400, h: 80}
        , pos = {x:0, y:360}
        , opacity = 30
        , color = {r: 100, g: 255, b: 255}
        }
        
beam1 = beam0 {angle= -45, pos= {x:400, y: 350}, size={w:600, h:80}, time= 1680}

beams = [beam0 {time=0}, beam0 {time=60, enabled=false}]

beams = [beam0, beam0 {time=1050}, beam0 {time=1080, opacity=100}, beam0 {time=1140, opacity = 80, enabled = false},
         beam1, beam1 {time=1830}, beam1 {time=1860, opacity=100}, beam1 {time=1920, opacity = 80, enabled=false}]

       
--EVERYTHING BELOW COMMENTED OUT DUDE

--objects for height indicators
h0 = { enabled: true,
       time: 0,
       bordered: false,
       angle: 0,
       size: {w:200, h: 200},
       pos: {x:0,y:0},
       opacity: 50,
       color: {r: 0, g:0, b: 0}
     }
     
height c p = let h1 = h0 {pos = p, color = c} in
             defaultEl {
              shape = Rectangle, 
              layer = 2,
              keys = [h1, h1 {time=1530}, h1 {time=1560, opacity=100}, h1 {time=1620, opacity=80, enabled=false}]
             }
             
heights = map (height {r:255,g:0,b:0}) [{x:0,y:200},{x:0,y:600},{x:200,y:0},{x:200,y:400},{x:400,y:200},{x:400,y:600},{x:600,y:0},{x:600,y:400}]
          

b0 = {
      enabled: true,
      time: 120,
      bordered: true,
      angle: 0,
      size: {w:40, h: 40},
      pos: {x:400, y:600},
      opacity: 100,
      color: {r: 255, g: 0, b: 0}
     }

brawler = defaultEl {keys = [b0]}

li0 = baseInst { 
                 time = 60, 
                 size= {w:40, h:5}, 
                 pos= {x:400,y:400},
                 color= {r:255, g:0, b:0},
                 opacity= 50
               }
li1 = li0 {size = {w: 500, h:5}}

landingIndicator = defaultEl {
                               shape= Donut, 
                               layer= 2, 
                               keys = [li0, li1 {time=120},li0 {time=121}, li1 {time=180}, li0 {time=181}, li1 {time=240}, li0 {time=241, enabled= false}]
                             }
                             
o0 = baseInst {
                time = 60,
                size = {w:40, h: 40},
                pos = {x:400, y: 400},
                color = {r:225, g:115, b:10},
                bordered = false,
                opacity = 50
              }
              
onslaughter = defaultEl {
                          keys = [o0, o0 {time=355}, o0 {time=360, size={w:80,h:80}, color = {r:255,g:0,b:0}, bordered=true, opacity=100}]
                        }
                        
-}
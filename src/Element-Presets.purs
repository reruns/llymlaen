module App.Element.Presets where

import App.Element

circBase :: Int -> Point -> Element
circBase t p = { layer: 0
               , keys: []
               , current: { time: t
                          , props: [ Enabled true
                                   , Bordered false
                                   , Color {r:128,g:128,b:128}
                                   , Position p
                                   , Opacity 100
                                   , Circle 50
                                   ]
                          }
               }

rectBase :: Int -> Point -> Element
rectBase t p = { layer: 0
               , keys: []
               , current: { time: t
                          , props: [ Enabled false
                                   , Bordered false
                                   , Color {r:128,g:128,b:128}
                                   , Position p
                                   , Opacity 100
                                   , Angle 0
                                   , Rect 0 0
                                   ]
                          }
               }
           
dnutBase :: Int -> Point -> Element
dnutBase t p = { layer: 0
               , keys: []
               , current: { time: t
                          , props: [ Enabled false
                                   , Color {r:128,g:128,b:128}
                                   , Position p
                                   , Opacity 100
                                   , Donut 0 1
                                   ]
                          }
               }        
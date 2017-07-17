module App.Element.Presets where

import Prelude

import App.Types

circBase :: Int -> Point -> Element
circBase t p = Element { layer: 0
               , keys: [Keyframe { time: t
                          , props: [ Enabled true
                                   , Color $ RGB {r:128,g:128,b:128}
                                   , Bordered false
                                   , Position p
                                   , Opacity 100
                                   , Circle 50
                                   ]
                        }]
               }

rectBase :: Int -> Point -> Element
rectBase t p = Element { layer: 0
               , keys: [Keyframe { time: t
                          , props: [ Enabled true
                                   , Color $ RGB{r:128,g:128,b:128}
                                   , Bordered false
                                   , Position p
                                   , Opacity 100
                                   , Angle 0
                                   , Rect 50 50
                                   ]
                        }]
               }
           
dnutBase :: Int -> Point -> Element
dnutBase t p = Element { layer: 0
               , keys: [Keyframe { time: t
                          , props: [ Enabled true
                                   , Color $ RGB {r:128,g:128,b:128}
                                   , Position p
                                   , Opacity 100
                                   , Donut 40 60
                                   ]
                        }]
               }        
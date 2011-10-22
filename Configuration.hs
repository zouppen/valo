module Configuration where

import Data.Word
import Network

udpPort :: PortNumber
udpPort = 9909

maxline :: Int
maxline = 1500

-- |Lights in format of (logical channel, DMX start channel). FIXME stupid types.
lights = [(0,1)
         ,(1,26)
         ,(2,11)
         ,(3,6)
         ,(4,42)
         ,(5,31)
         ,(6,16)
         ,(7,36)
         ,(8,64)
         ,(9,(64+8))
         ,(10,(64+16))
         ,(11,(64+32))
         ,(12,(64+36))
         ]

-- |Values are initially zeros. 
initialValues :: [(Int, Word8)]
initialValues = [(31,34) -- wash 1 positions
                ,(32,50)
                ,(33,77)
                ,(34,50)
                ,(49,255) -- wash 1 dimmer
                ,(51,34) -- wash 2 positions
                ,(52,50)
                ,(53,77)
                ,(54,50)
                ,(69,255) -- wash 2 dimmer
                ,(71,34) -- wash 3 positions
                ,(72,50)
                ,(73,77)
                ,(74,50)
                ,(89,255) -- wash 3 dimmer
                ,(91,34) -- wash 4 positions
                ,(92,50)
                ,(93,77)
                ,(94,50)
                ,(109,34) --wash 4 positions
                ]

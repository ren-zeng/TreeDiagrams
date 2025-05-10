module Visualization.Text where

import Diagrams
import Diagrams.Prelude
import Prettyprinter
import Visualization.BackEnd

drawText :: String -> Diagram BackEnd
drawText x =
    ( text x # fontSizeL 1 # fc black
        <> (rect (fromIntegral (length x) / 2) 1 :: Diagram BackEnd)
            # phantom
    )

drawPretty :: (Pretty a) => a -> Diagram BackEnd
drawPretty = pad 1.1 . drawText . show . pretty

coloredText :: Colour Double -> String -> Diagram BackEnd
coloredText c x =
    text x # fontSizeL 1 # fc c
        <> (rect (fromIntegral (length x) / 2) 1 :: Diagram BackEnd)
            # phantom
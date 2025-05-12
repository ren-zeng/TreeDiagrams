module Visualization.Text (drawText,withTitle) where

import Diagrams
import Diagrams.Prelude
import Prettyprinter hiding (vsep)
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

withTitle :: String -> Diagram BackEnd -> Diagram BackEnd
withTitle title = centerXY . (\x -> vsep 5 [text title # fontSize 20 , x])
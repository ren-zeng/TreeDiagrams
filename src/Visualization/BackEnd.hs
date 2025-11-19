module Visualization.BackEnd (
    BackEnd,
    renderSVG,
    mkSizeSpec2D,
    writeSVG,
    ) where

import Diagrams.Backend.SVG (B, renderSVG)
import Diagrams (mkSizeSpec2D)


type BackEnd = B

writeSVG path = renderSVG path (mkSizeSpec2D (Just 1000) (Just 1000) )
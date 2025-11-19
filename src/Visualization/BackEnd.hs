module Visualization.BackEnd (
    BackEnd,
    renderSVG,
    ) where

import Diagrams.Backend.SVG (B, renderSVG)


type BackEnd = B


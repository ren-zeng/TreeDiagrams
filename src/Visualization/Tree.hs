module Visualization.Tree where

import Data.Maybe
import Data.Tree
import Diagrams
import Diagrams.Prelude
import Diagrams.TwoD.Layout.Tree
import Visualization.BackEnd


treeDiagram :: (a -> Diagram BackEnd) -> Tree a -> Diagram BackEnd
treeDiagram drawNode t =
    renderTree
        id
        (closer 0.8 (~~) # lwL 0.25) -- # lw 0.25
        (symmLayout' opts (drawNode <$> t))
        # centerXY
        # frame 1
  where
    opts =
        with
            & slHSep
            .~ 4
            & slVSep
            .~ 4
            & slWidth
            .~ fromMaybe (0, 0)
            . extentX
            & slHeight
            .~ fromMaybe (0, 0)
            . extentY

closer ::
    (Functor f, Num a, Num (f a)) =>
    a ->
    (f a -> f a -> t) ->
    f a ->
    f a ->
    t
closer r f x y = f (x + (r *^ d)) (y - (r *^ d))
  where
    d = y - x
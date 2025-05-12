module Visualization.ProofTree (drawProofTree) where

import Core.ProofTree
import Data.Functor.Foldable
import Diagrams
import Diagrams.Prelude
import Visualization.BackEnd

drawProofTree :: (a -> Diagram BackEnd) -> (v -> Diagram BackEnd) -> ProofTree a v -> Diagram BackEnd
drawProofTree drawRule drawStatement =
    cata $
        \case
            AxiomF v -> drawStatement v # frame 1
            ProofTreeF v a rs -> ruleSep rulebox top bot
              where
                rulebox = drawBoxAround (drawRule a)
                top = drawStatement v
                bot = centerX diagramPremises
                diagramPremises = hsep 1 rs

ruleSep :: Diagram BackEnd -> Diagram BackEnd -> Diagram BackEnd -> Diagram BackEnd
ruleSep rulebox top bot =
    mid
        # \x ->
            beside (r2 (0, 1)) x top
                # \y -> beside (r2 (0, -1)) y bot
  where
    mid = rulebox <> hrule (max (width top) (width bot)) # lwL 0.1 # lc grey

drawBoxAround :: Diagram BackEnd -> Diagram BackEnd
drawBoxAround x =
    (x
        <> (rect (width x) (height x) # bg white)) # lwL 0.1 # lc grey # frame 1 # pad 1.1

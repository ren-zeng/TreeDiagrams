module Visualization.ProofTree where
import Visualization.BackEnd
import Core.ProofTree
import Diagrams
import Data.Functor.Foldable
import Visualization.Text
import Diagrams.Prelude

drawProofTreeDefault :: (_) => (v -> Diagram BackEnd) -> ProofTree a (w, v) -> Diagram BackEnd
drawProofTreeDefault drawV weightProofTree =
    drawProofTree
        drawPretty
        (\(w, item) -> hsep 0 [drawText (show w) <> circle 1 # lwL 0.1, drawText ":", drawV item] # centerX)
        weightProofTree
        # frame 1
        # bg white
        
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
    x
        <> (rect (width x) (height x) # bg white) # lwL 0.1 # lc grey # frame 1 # pad 1.1

module ProofTreeVisTest (plotTestProofTree) where

import Core.ProofTree
import Data.Functor.Foldable
import Diagrams
import Diagrams.Backend.SVG
import Diagrams.Prelude
import TestExample
import Visualization.ProofTree
import Visualization.Text (withTitle,drawText)

fibProofTree :: Nat -> ProofTree String String
fibProofTree = ana $ \case
    Z -> AxiomF "fib 0 ≔ 1"
    S Z -> AxiomF "fib 1 ≔ 1"
    x@(S (S n)) ->
        ProofTreeF
            ("fib " <> show x <> " = " <> show (fib $ toInt x))
            "∀n ∈ ℕ. fib (n+2) ≔ fib n + fib (n+1)"
            [S n, n]

testProofTree :: ProofTree String String
testProofTree = fibProofTree (mkNat 4)

plotTestProofTree :: FilePath -> IO ()
plotTestProofTree path = do
    let diagram = drawProofTree drawText drawText testProofTree 
            # withTitle "Fib Proof Tree"
            # frame 1
            # bg white

    renderSVG path (mkWidth 1000)  diagram

module TreeVisTest(plotTestTree) where
import TestExample
import Data.Tree
import Data.Functor.Base
import Data.Functor.Foldable
import Visualization.Tree
import Visualization.Text
import Diagrams
import Diagrams.Prelude
import Diagrams.Backend.SVG

fibTree :: Nat -> Tree Int
fibTree = ana $ \case 
    Z -> NodeF 1 [] 
    S Z -> NodeF 1 [] 
    x@(S (S n)) -> NodeF (fib $ toInt x) [S n, n]

plotTestTree :: FilePath -> IO ()
plotTestTree path = do
    let diagram = treeDiagram (drawText . show) (fibTree (mkNat 7)) 
            # withTitle "Fib Tree"
            # frame 1
            # bg white
    renderSVG path (mkWidth 1000) diagram
module Visualization.ParseTree (drawParseTree) where

import Core.ParseTree
import Core.SymbolTree
import Diagrams
import Visualization.BackEnd
import Visualization.ProofTree (drawProofTree)

drawParseTree ::
    (a -> Diagram BackEnd) ->
    (Symbol nt t -> Diagram BackEnd) ->
    ParseTree a nt t ->
    Diagram BackEnd
drawParseTree drawSymbol drawRule t = 
    drawProofTree drawSymbol drawRule (parseTreeToProofTree t)
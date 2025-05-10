module Visualization.SymbolTree (drawSymbolTree) where

import Core.SymbolTree
import Diagrams
import Visualization.BackEnd
import Visualization.Tree

drawSymbolTree ::
    (Symbol nt t -> Diagram BackEnd) ->
    SymbolTree nt t ->
    Diagram BackEnd
drawSymbolTree drawSymbol t = treeDiagram drawSymbol (symbolTreetoTree t)
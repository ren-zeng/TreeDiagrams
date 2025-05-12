module Core.ParseTree where 
import Data.Functor.Foldable.TH
import Core.ProofTree
import Core.SymbolTree
import Data.Functor.Foldable

data ParseTree r nt t = Leaf t | ParseTree nt r [ParseTree r nt t]
  deriving (Show)
makeBaseFunctor ''ParseTree

parseTreeToProofTree :: ParseTree r nt t -> ProofTree r (Symbol nt t)
parseTreeToProofTree  = cata $ \case 
    LeafF t -> Axiom (T t) 
    ParseTreeF nt r ts -> ProofTree (NT nt) r ts
  
inferParseTree ::
    (nt -> [Symbol nt t] -> Maybe r) ->
    SymbolTree nt t ->
    Maybe (ParseTree r nt t)
inferParseTree inferRule = \case
    NTNode nt ts -> do
        r <- inferRule nt (rootSymbol <$> ts)
        ts' <- mapM (inferParseTree inferRule) ts
        return $ ParseTree nt r ts'
    TLeaf t -> return $ Leaf t
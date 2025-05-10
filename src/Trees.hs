module Trees where
import Data.Functor.Foldable.TH
import Data.Tree
import Prettyprinter
import Data.Functor.Foldable
import Symbol


{- | @SymbolTree nt t@ encodes a traditional parse tree for a context free grammar
- @nt@ node label type (non-terminal)
- @t@ leaf label type (temrinal)
-}
data SymbolTree nt t = TLeaf t | NTNode nt [SymbolTree nt t]
    deriving Show
makeBaseFunctor ''SymbolTree

symbolTreetoTree :: SymbolTree nt t -> Tree (Symbol nt t)
symbolTreetoTree = cata $ \case
    TLeafF t -> Node (T t) [] 
    NTNodeF nt ts -> Node (NT nt) ts

rootNT :: SymbolTree a t -> Maybe a
rootNT (NTNode nt ts) = Just nt 
rootNT _ = Nothing

data ParseTree r nt t = Leaf t | ParseTree nt r [ParseTree r nt t]
  deriving (Show)
makeBaseFunctor ''ParseTree

parseTreeToProofTree :: ParseTree r nt t -> ProofTree r (Symbol nt t)
parseTreeToProofTree  = cata $ \case 
    LeafF t -> Axiom (T t) 
    ParseTreeF nt r ts -> ProofTree (NT nt) r ts

{- | @ProofTree a v@ represent of a proof
- @a@: inference rule label
- @v@: statement type
-}
data ProofTree a v = Axiom v | ProofTree v a [ProofTree a v]
  deriving (Show, Functor)

makeBaseFunctor ''ProofTree

instance (Pretty a, Pretty v) => Pretty (ProofTree a v) where
  pretty = cata $ \case
    AxiomF v -> "Axiom" <+> pretty v
    ProofTreeF v a ts ->
      vsep
        [ hsep ["ProofTree", pretty v, pretty a]
        , indent 4 $ vsep ts
        ]



proofTreeRoot :: ProofTree a v -> v
proofTreeRoot (Axiom x) = x 
proofTreeRoot (ProofTree x _ _) = x
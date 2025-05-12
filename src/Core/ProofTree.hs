module Core.ProofTree where
import Data.Functor.Foldable.TH
import Prettyprinter
import Data.Functor.Foldable
import Core.SymbolTree

{- | @ProofTree r a@ encodes the structure of a proof
- @r@: inference rule
- @a@: statement type
-}
data ProofTree r a = Axiom a | ProofTree a r [ProofTree r a]
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

proofGoal :: ProofTree r a -> a
proofGoal (Axiom x) = x 
proofGoal (ProofTree x _ _) = x


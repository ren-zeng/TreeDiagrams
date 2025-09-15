module Core.ProofTree where

import Core.SymbolTree
import Data.Aeson
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import GHC.Generics
import Prettyprinter

{- | @ProofTree r a@ encodes the structure of a proof
- @r@: inference rule
- @a@: statement type
-}
data ProofTree r a = Axiom a | ProofTree a r [ProofTree r a]
  deriving (Show, Generic)

makeBaseFunctor ''ProofTree

instance (ToJSON r, ToJSON a) => ToJSON (ProofTree r a)
instance (FromJSON r, FromJSON a) => FromJSON (ProofTree r a)

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

testJSON :: IO ()
testJSON =
  encodeFile @(ProofTree String String) "exampleProofTree.json" $
    ProofTree "Conclusion" "theorem" [ProofTree "premise 1" "lemma" [Axiom "premise 1.1"], Axiom "premise 2"]

-- >>>  testJSON

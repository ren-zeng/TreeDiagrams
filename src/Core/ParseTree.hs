module Core.ParseTree (
    -- * Types
    ParseTree,

    -- * Creation
    fromSymbolTree,
) where

import Core.ProofTree
import Core.SymbolTree
import Data.Functor.Foldable

type ParseTree r nt t = ProofTree r (Symbol nt t)

-- | Enrich a `SymbolTree` by production rule given its constraint on head and child symbols. The resulted tree is a `ProofTree`
fromSymbolTree ::
    -- | a function to infer production rules
    (nt -> [Symbol nt t] -> Maybe r) ->
    SymbolTree nt t ->
    Maybe (ParseTree r nt t)
fromSymbolTree inferRule = cata $ \case
    TLeafF t -> return $ Axiom (T t)
    NTNodeF nt ts -> do
        pts <- sequence ts
        r <- inferRule nt (proofGoal <$> pts)
        return $ ProofTree (NT nt) r pts

-- \case
-- NTNode nt ts -> do
--     r <- inferRule nt (rootSymbol <$> ts)
--     ts' <- mapM (fromSymbolTree inferRule) ts
--     return $ ProofTree (NT nt) r ts'
-- TLeaf t -> return $ Axiom (T t)
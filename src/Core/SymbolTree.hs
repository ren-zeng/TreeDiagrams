module Core.SymbolTree (
    -- * Types
    Symbol (..),
    SymbolTree (..),

    -- * Query
    rootNT,
    rootSymbol,

    -- * Conversion
    symbolTreetoTree,

    -- * (For fold/unfold) Base functor from recursion scheme
    SymbolTreeF (..),
) where

import Data.Aeson
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Tree
import GHC.Generics (Generic)
import Prettyprinter

{- | The sum type (disjoint union) of non-terminal and terminal.

  - @nt@ the non-terminal type
  - @t@ the terminal type
-}
data Symbol nt t = T t | NT nt
    deriving (Show, Eq, Ord, Generic)

instance (Pretty a, Pretty t) => Pretty (Symbol a t) where
    pretty = \case
        NT x -> pretty x
        T x -> pretty x

extractNT :: Symbol nt t -> Maybe nt
extractNT (NT x) = Just x
extractNT (T _) = Nothing

extractT :: Symbol nt t -> Maybe t
extractT (T x) = Just x
extractT (NT _) = Nothing

data StartSymbol = StartSymbol
    deriving (Show, Eq, Ord)

{- | @SymbolTree nt t@ encodes a traditional parse tree for a context free grammar.


- @nt@ node label type (non-terminal)
- @t@ leaf label type (temrinal)

It is a more restricted type than @Tree (Symbol nt t)@ in that the leaf node are always of type @t@ while branching node content always of type @nt@.
-}
data SymbolTree nt t = TLeaf t | NTNode nt [SymbolTree nt t]
    deriving (Show, Generic, Eq)

instance (ToJSON nt, ToJSON t) => ToJSON (SymbolTree nt t)
instance (FromJSON nt, FromJSON t) => FromJSON (SymbolTree nt t)

{- | Base functor for @SymbolTree nt t@

  @Fix (SymbolTreeF nt t) ≃ SymbolTree nt t@
-}
makeBaseFunctor ''SymbolTree

-- | root non-terminal of a `SymbolTree`. If the symbolTree is just a terminal leaf, it returns `Nothing`.
rootNT :: SymbolTree nt t -> Maybe nt
rootNT (NTNode nt _) = Just nt
rootNT _ = Nothing

-- | root symbol of a `SymbolTree`
rootSymbol :: SymbolTree nt t -> Symbol nt t
rootSymbol (TLeaf x) = T x
rootSymbol (NTNode x _) = NT x

symbolTreetoTree :: SymbolTree nt t -> Tree (Symbol nt t)
symbolTreetoTree = cata $ \case
    TLeafF t -> Node (T t) []
    NTNodeF nt ts -> Node (NT nt) ts

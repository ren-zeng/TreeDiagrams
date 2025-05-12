module Core.SymbolTree where

import Data.Aeson
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Tree
import GHC.Generics (Generic)
import Prettyprinter

{- | @Symbol nt t@ is the symbol type for a grammar.
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

{- | @SymbolTree nt t@ encodes a traditional parse tree for a context free grammar. @SymbolTree nt t@ is a more restricted type than @Tree (Symbol nt t)@
- @nt@ node label type (non-terminal)
- @t@ leaf label type (temrinal)
-}
data SymbolTree nt t = TLeaf t | NTNode nt [SymbolTree nt t]
    deriving (Show, Generic, Eq)

makeBaseFunctor ''SymbolTree

instance (ToJSON nt, ToJSON t) => ToJSON (SymbolTree nt t)
instance (FromJSON nt, FromJSON t) => FromJSON (SymbolTree nt t)

rootNT :: SymbolTree a t -> Maybe a
rootNT (NTNode nt ts) = Just nt
rootNT _ = Nothing

symbolTreetoTree :: SymbolTree nt t -> Tree (Symbol nt t)
symbolTreetoTree = cata $ \case
    TLeafF t -> Node (T t) []
    NTNodeF nt ts -> Node (NT nt) ts

rootSymbol :: SymbolTree nt t -> Symbol nt t
rootSymbol (TLeaf x) = T x
rootSymbol (NTNode x _) = NT x

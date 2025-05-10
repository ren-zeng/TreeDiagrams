module Core.SymbolTree where
import Prettyprinter
import Data.Functor.Foldable.TH
import Data.Tree
import Data.Functor.Foldable

{- | @Symbol nt t@ is the symbol type for a grammar. 
- @nt@ the non-terminal type 
- @t@ the terminal type
-}
data Symbol nt t = T t | NT nt
    deriving (Show, Eq, Ord)

instance (Pretty a, Pretty t) => Pretty (Symbol a t) where
    pretty = \case
        NT x -> pretty x
        T x -> pretty x

data StartSymbol = StartSymbol
    deriving (Show, Eq, Ord)
    
{- | @SymbolTree nt t@ encodes a traditional parse tree for a context free grammar. @SymbolTree nt t@ is a more restricted type than @Tree (Symbol nt t)@
- @nt@ node label type (non-terminal)
- @t@ leaf label type (temrinal)
-}
data SymbolTree nt t = TLeaf t | NTNode nt [SymbolTree nt t]
    deriving Show
makeBaseFunctor ''SymbolTree

rootNT :: SymbolTree a t -> Maybe a
rootNT (NTNode nt ts) = Just nt 
rootNT _ = Nothing

symbolTreetoTree :: SymbolTree nt t -> Tree (Symbol nt t)
symbolTreetoTree = cata $ \case
    TLeafF t -> Node (T t) [] 
    NTNodeF nt ts -> Node (NT nt) ts


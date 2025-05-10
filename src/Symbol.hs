module Symbol where
import Prettyprinter

data Symbol nt t = T t | NT nt
    deriving (Show, Eq, Ord)

instance (Pretty a, Pretty t) => Pretty (Symbol a t) where
    pretty = \case
        NT x -> pretty x
        T x -> pretty x

data StartSymbol = StartSymbol
    deriving (Show, Eq, Ord)
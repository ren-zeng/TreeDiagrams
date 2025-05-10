# Core type hierarchies

`SymbolTree nt t` is a restricted case of `Tree a` for type safety.
```hs
symbolTreetoTree :: SymbolTree nt t -> Tree (Symbol nt t)
```

`ParseTree r nt t` is a restricted case of `ProofTree r a` for type safety.
We have a total conversion.
```hs
parseTreeToProofTree :: ParseTree r nt t -> ProofTree r (Symbol nt t)
```

Therefore, we only need to provide visualization for `Tree a` and `ProofTree r a`. 
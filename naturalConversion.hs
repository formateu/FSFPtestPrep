{-Convert Int to Natural number with given definition-}
data Nat = Zero | Succ Nat deriving (Show)
toNat :: Int -> Maybe Nat
toNat i | i == 0 = Just Zero
        | i < 0 = Nothing
        | i > 0 = fmap Succ (toNat (i-1))

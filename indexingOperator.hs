{-Original excercise operator type declaration-}
(!!) :: [a] -> Int -> a

(!!) (x:xs) index | index == 0 = x
                  | otherwise = (!!) xs (index-1)

{-My improved version-}
(!!) :: [a] -> Int -> Maybe a
(!!) [] _ = Nothing
(!!) (x:xs) index | index == 0 = Just x
                  | index > 0 = (!!) xs (index-1)
                  | otherwise = Nothing -- case of negative index
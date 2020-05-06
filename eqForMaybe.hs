instance Eq a => Eq (Maybe a) where
    Nothing == Nothing = True
    (Just a) == (Just a') = (a == a')
    _ == _ = False 
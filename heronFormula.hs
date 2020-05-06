area :: Float -> Float -> Float -> Float
area a b c = sqrt (s *(s-a)*(s-b)*(s-c))
    where
        abc = a+b+c
        s = abc / 2
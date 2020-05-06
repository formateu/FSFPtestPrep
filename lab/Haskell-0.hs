{- SPOP. Lab 0. Nieoceniane -}

import Data.Char  -- funkcje 'ord' i 'chr' do zadania 5.

{- Zadanie 1. Napisz funkcję, która zwraca środkowy element podanej listy.
Wykorzystaj funkcje standardowe: 'div' i 'length' oraz operator (!!). Przykład:

ghci> middle "Haskell"
'k'
-}

middle :: [a] -> a
middle lst = lst !! div (length lst) 2

{- Zadanie 2. Napisz funkcję, która usuwa z listy występujące bezpośrednio
po sobie duplikaty danego elementu. Nie korzystaj z funkcji standardowych.
Przykład:

ghci> removeDuplicates [9, 3, 3, 3, 4, 5, 5, 3, 5]
[9,3,4,5,3,5]

Wskazówka: spójrz na definicję funkcji 'maximum' z wykładu. -}

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates [x] = [x]
removeDuplicates (x:y:xs) | x == y = removeDuplicates (y:xs)
                          | otherwise = x:(removeDuplicates (y:xs))
                

{- Zadanie 3. Napisz funkcję, która wstawia do danej listy nowy element
na podanej pozycji. Nie korzystaj z funkcji standardowych. Przykład:

ghci> insertAt "askell" 'H' 0
"Haskell"

Wskazówka: por. z definicją operatora (!!) z wykładu
-}

insertAt :: [a] -> a -> Int -> [a]
insertAt (x:xs) toInsert index | index == 0 = (toInsert:x:xs)
                               | otherwise = x:(insertAt xs toInsert (index - 1))

{- Zadanie 4. Napisz funkcję, która usuwa z listy wszystkie występujące
dalej duplikaty poszczególnych elementów. Przykład:

ghci> removeAllDuplicates [9, 3, 3, 3, 4, 5, 5, 3, 5]
[9,3,4,5]

Wskazówka: spójrz na definicję funkcji 'reverse' z wykładu. W akumulatorze
przechowuj elementy napotykane po raz pierwszy. Użyj funkcji 'elem' do
sprawdzenia, czy element jest już w akumulatorze. -}

removeAllDuplicates :: Eq a => [a] -> [a]
removeAllDuplicates lst = rem lst []
                          where rem [] acc = []
                                rem (x:xs) acc | elem x acc = rem xs acc
                                               | otherwise = x:rem xs (x:acc)

{- Zadanie 5. Zadanie dotyczy szyfrowania tekstów. Prosty kod Cezara polega
na tym, że w miejsce danej litery wstawiamy literę o kodzie większym np.
o 3 (liczbę tę nazywamy kluczem w kodzie Cezara). Końcowe litery alfabetu
zastępujemy literami z początku alfabetu. Np. w miejsce 'A' wstawiamy 'D',
w miejsce 'X' wstawiamy 'A'. Napisz dwie funkcje, które odpowiednio kodują
i dekodują napis szyfrem Cezara o podanym kluczu. Przykład:

ghci> codeCezar "Koty" 3
"Nrwb"
ghci> decodeCezar "Nrwb" 3
"Koty"

Wskazówka: kod ASCII danego znaku zwraca funkcja 'ord :: Char -> Int', natomiast
znak odpowiadający podanemu kodowi ASCII zwraca funkcja 'chr :: Int -> Char'.
Przykład:

ghci> ord 'A'
65
ghci> chr 65
'A' -}

codeCezar :: String -> Int -> String
codeCezar word shift = map (chr . (doShift shift) . ord) word
                       where doShift shift val | val >= 65 && val <= 90 = 65 + (mod (val + shift - 65) 26)
                                               | val >= 97 && val <= 122 = 97 + (mod (val + shift - 97) 26)

decodeCezar :: String -> Int -> String
decodeCezar word shift = undefined

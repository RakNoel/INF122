module Eksamen2012 where

import Data.List
import Data.Char

{-Programmer en Haskell funksjon posteval::String->[String]->Int (helst uten noen hjelpefunksjoner)
som evaluerer slike postfiks uttrykk, gitt som streng i første argumentet, ved ˚a
bruke stabel [String] som forklart over. Funksjonen kalles initielt med tom stabel og returnerer
tallet som ligger p˚a toppen av stabelen ved avsluttet evaluering, f.eks.,
(a) posteval "11 2 3 * + 4 -" [] skal gi 13, mens
(b) posteval "4 11 2 3 * + -" [] skal gi -13.-}

posteval :: String -> [String] -> Int
posteval (x:xs) stc = case x of
    ' ' -> posteval xs stc
    '*' -> read (head stc) * read (stc!!1)
    _ -> posteval rest (takers : stc)
    where
        (takers, rest) = span isDigit (x : xs)

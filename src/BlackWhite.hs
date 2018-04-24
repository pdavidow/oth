module BlackWhite
    ( BlackWhite(..)
    , Blacks(..)
    , Whites(..)
    , makeBlackWhite
    , blacksWhites )
    where


data BlackWhite a = BlackWhite (Blacks a) (Whites a)

newtype Blacks a = Blacks a

newtype Whites a = Whites a


makeBlackWhite :: a -> a -> BlackWhite a
makeBlackWhite b w =
    BlackWhite (Blacks b) (Whites w)


blacksWhites :: BlackWhite a -> ( a, a )
blacksWhites (BlackWhite (Blacks b) (Whites w)) =
    ( b, w )
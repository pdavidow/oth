module WeightingFactor
    ( WeightingFactor -- hiding constructor
    , makeWeightingFactor
    , highestWeight
    , mediumWeight
    , lowestWeight
    , isHighestWeight
    , isMediumWeight
    , isLowestWeight
    )
    where

newtype WeightingFactor = WeightingFactor Int deriving (Eq, Show)


highest :: Int
highest = 
    10


lowest :: Int
lowest = 
    1


makeWeightingFactor :: Int -> Either String WeightingFactor
makeWeightingFactor n
    | n < lowest || n > highest = Left $ "Range is from " ++ show lowest ++ " through " ++ show highest
    | otherwise = Right $ WeightingFactor n


highestWeight :: WeightingFactor
highestWeight =
    WeightingFactor highest


mediumWeight :: WeightingFactor
mediumWeight =
    WeightingFactor $ div (highest - lowest) 2


lowestWeight :: WeightingFactor
lowestWeight = 
    WeightingFactor lowest


isHighestWeight :: WeightingFactor -> Bool
isHighestWeight x =
    x == highestWeight


isMediumWeight :: WeightingFactor -> Bool
isMediumWeight x =
    x == mediumWeight
    

isLowestWeight :: WeightingFactor -> Bool
isLowestWeight x =
    x == lowestWeight
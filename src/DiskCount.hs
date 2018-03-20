module DiskCount
    ( TaggedDiskCount
    , makeTagged_BlackDiskCount
    , makeTagged_WhiteDiskCount
    , decreaseByOne
    )
    where

newtype DiskCount = DiskCount Int deriving (Eq, Show)

newtype BlackDiskCount = BlackDiskCount DiskCount deriving (Eq, Show)

newtype WhiteDiskCount = WhiteDiskCount DiskCount deriving (Eq, Show)

data TaggedDiskCount
    = Tagged_BlackDiskCount BlackDiskCount
    | Tagged_WhiteDiskCount WhiteDiskCount
        deriving (Eq, Show)

makeTagged_BlackDiskCount :: TaggedDiskCount
makeTagged_BlackDiskCount =
    Tagged_BlackDiskCount $ BlackDiskCount makeDiskCount


makeTagged_WhiteDiskCount :: TaggedDiskCount
makeTagged_WhiteDiskCount =
    Tagged_WhiteDiskCount $ WhiteDiskCount makeDiskCount


makeDiskCount :: DiskCount
makeDiskCount =
    DiskCount 32 -- assume boardSize 8


decreaseByOne :: TaggedDiskCount -> TaggedDiskCount
decreaseByOne tagged =
    case tagged of
        Tagged_BlackDiskCount (BlackDiskCount x) -> Tagged_BlackDiskCount $ BlackDiskCount $ decreaseByOne' x
        Tagged_WhiteDiskCount (WhiteDiskCount x) -> Tagged_WhiteDiskCount $ WhiteDiskCount $ decreaseByOne' x


decreaseByOne' :: DiskCount -> DiskCount
decreaseByOne' d@(DiskCount n) =
    if n == 0 then
        d
    else
        DiskCount $ n - 1
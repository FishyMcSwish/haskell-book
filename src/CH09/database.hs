module Database where

import Data.Time

data DatabaseItem = DBString String 
                  | DBNumber Integer
                  | DBDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = 
  [DBDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DBNumber 9001
  , DBString "Hello, world!"
  , DBDate (UTCTime
            (fromGregorian 1921 5 1)(secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate =
  foldr (\item acc -> case item of 
                        DBDate time -> time : acc
                        _ -> acc)
                        []

filterDbDate' :: [DatabaseItem] -> [UTCTime]
filterDbDate' =
  foldr getDates []
    where
      getDates (DBDate time) acc = time : acc
      getDates _ acc = acc

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber =
  foldr getNumbers []
    where
      getNumbers (DBNumber num) acc = num : acc
      getNumbers _ acc = acc

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent =
  foldr maxTime (UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0))
    where
      maxTime (DBDate time1) time2 = max time1 time2
      maxTime _ acc = acc

sumDB :: [DatabaseItem] -> Integer
sumDB =
  foldr sumNumbers 0
    where
      sumNumbers (DBNumber num) acc = num + acc
      sumNumbers _ acc = acc

avgDb :: [DatabaseItem] -> Double
avgDb db =
  (fromIntegral $ sumDB db) /( fromIntegral $ length $ filterDbNumber db)


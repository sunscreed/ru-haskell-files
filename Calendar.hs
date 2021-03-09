module Calendar where

import Prelude (Int, Char, String, Show(..), (++))

-- Date
data Date = Date Year Month Day

--Year
data Year = Year Int

--Month
data Month = Janyary | February | March| Aprol | May | June | July | August | September | Octover | November | December

-- Day 
data Day = Day Int

--Weekday
data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

--Time
data Time = Time Hour Minute Second
data Hour = Hour Int
data Minute = Minute Int
data Second = Second Int

instance Show Weekday where
     show Monday = "Mon"
     show Tuesday = "Tue"
     show Wednesday = "Wed"
     show Thursday = "Thu"
     show Friday = "Fri"
     show Saturday = "Sat"
     show Sunday = "Sun"

instance Show Time where
     show (Time h m s) =  show h ++ ":" ++ show m ++ ":" ++ show s

instance Show Hour where
     show (Hour h) = addZero (show h)

instance Show Minute where
     show (Minute m) = addZero (show m)

instance Show Second where
     show (Second s) = addZero (show s)

addZero :: String -> String
addZero (a:[]) = '0' : a : []
addZero as = as
 

module Defuzzy where

import Fuzzy

-- Defuzzification

centroid :: Domain Double -> Fuzzy Double -> Double
centroid dom f = (sum (zipWith (*) dom fdom))/(sum fdom)
    where fdom = map f dom
	
maxima :: Ord a => Domain a -> Fuzzy a -> [a]
maxima dom f = maxima' dom []
    where
        maxima' [] ms             = ms
        maxima' (x:xs) []         = maxima' xs [x]
        maxima' (x:xs) (m:ms)
            | f x > f m           = maxima' xs [x]
            | f x == f m          = maxima' xs (x:m:ms)
            | otherwise           = maxima' xs (m:ms)
minmax, medmax, maxmax :: Ord a => Domain a -> Fuzzy a -> a

--helper function to compute median
qsort :: Ord a => [a] -> [a]
qsort []   = []
qsort (x:xs) = qsort [y | y <- xs, y <= x] ++ [x] ++
			 qsort [y | y <- xs, y > x]

median :: Ord a => [a] -> a
median  []    = undefined
median xs     = (qsort xs) !! middle
    where middle = (length xs) `div` 2

minmax dom f  = minimum (maxima dom f)
medmax dom f  = median (maxima dom f)
maxmax dom f  = maximum (maxima dom f)

type Day = (Degree, Humidity, Occasion, Mood)

degree :: Day -> Degree
degree (d,_,_,_) = d

humidity :: Day -> Humidity
humidity (_,h,_,_) = h

occasion :: Day -> Occasion
occasion (_,_,o,_) = o

mood :: Day -> Occasion
mood (_,_,_,m) = m

-- ffilter to take a fuzzy predicate and a list, return members of the list
-- that satisfy predicate, and degree to which they satisfy it.

ffilter :: Fuzzy a -> [a] -> [(a, Double)]
ffilter pred xs = filter ((/=) 0 . snd) (map (\x -> (x, pred x)) xs)

forecast :: [Day]
forecast = undefined

--predicates
-- sweltering, hot, warm, chilly, cold, freezing.
isWarm day       = warm (degree day)
isCold day       = cold (degree day)
isHot day        = hot (degree day)
isChilly day     = chilly (degree day)
isFreezing day   = freezing (degree day)
isSweltering day = sweltering (degree day)

-- arid, dry, pleasant, damp, muggy.
isArid day     = arid (humidity day)
isDry day      = dry (humidity day)
isPleasant day = pleasant (humidity day)
isDamp day     = damp (humidity day)
isMuggy day    = muggy (humidity day)

-- sickDay, relaxedOccasion, walMart, inPublic, special, fancy, work.
isSickDay day         = sickDay (occasion day)
isRelaxedOccasion day = relaxedOccasion (occasion day)
isWalMart day         = walMart (occasion day)
isInPublic day        = inPublic (occasion day)
isSpecial day         = special (occasion day)
isFancy day           = fancy (occasion day)
--isWork day            = work (occasion day)

-- conservative, lazy, happy, sad, serious, silly, darkMood.
isConservative day = conservative (mood day)
isLazy day         = lazy (mood day)
isHappy day        = happy (mood day)
isSad day          = sad (mood day)
isSerious day      = serious (mood day)
isSilly day        = silly (mood day)
isDarkMood day     = darkMood (mood day)

-- combine predicates in the following way: pred1 day = somewhat warm (degree day) && very fancy (occasion day)
-- >> pred1 (68.0, 86.0, 55.0, 78.0)
-- Then define a list of days, and run >> ffilter pred1 days
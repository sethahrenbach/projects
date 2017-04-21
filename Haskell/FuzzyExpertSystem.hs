{-# LANGUAGE MultiParamTypeClasses,TypeSynonymInstances,FlexibleInstances, FunctionalDependencies, FlexibleContexts #-}

module Fuzzy where
import Prelude hiding ((&&), (||), not, and, or, any, all) --so we can redefine.


-- I try to implement some fuzzy logic operators in this module, to 
-- automatically choose an appropriate wardrobe for you.
-- The current prototype can choose clothing along two dimensions: 1) light/heavy, 2) fanciness.
-- (1) is based on temperature and humidity predicates.
-- (2) is based on occasion predicates.
-- Much of the underlying framework comes from Gary Meehan and Mike Joy "Animated Fuzzy Logic" Journal of Functional Programming, 1993.
-- I introduce some extensions to the logic, like Yager operations, and alternative operator definitions.
-- The application to this specific decision problem is my original work. 
-- I denote all of my original code with notes to that effect.

--
--
--            Logic and Operators
--
--
--
class Logic a where
    true  :: a
    false :: a
    (&&)  :: a -> a -> a
    (||)  :: a -> a -> a
    not   :: a -> a
    (==>) :: Double -> a -> a	
	
instance Logic Bool where
    true             = True
    false            = False
    not True         = False
    not False        = True
    (&&) True True   = True
    (&&) _ _         = False
    (||) False False = False
    (||) _ _         = True	
    w ==> False      = False
    w ==> True       = w > 0.5	
	
instance Logic Double where
    true      = 1
    false     = 0
    (&&)      = min
    (||)      = max
    not x     = 1 - x
    x ==> y   = min x y -- x * y --correlation product. Zadeh implication is not very useful here. Z = min 1 ((1-x)+ y) --. Min correlation imp: min x y --
	
-- Fuzzy subset operators: 
instance (Logic b) => Logic (a -> b) where
    true     = \x -> true
    false    = \x -> false
    (&&) f g = \x -> f x && g x  -- intersection
    (||) f g = \x -> f x || g x  -- union
    not f    = \x -> not (f x)	 -- complement
    w ==> f  = \x -> w ==> f x
	
instance (Num a, Num b) => Num (a,b) where
    (a, b) + (c, d)    = (a + c, b + d)
    (a, b) * (c, d)    = (a * c, b * d)
    abs (a, b)         = (abs a, abs b)
    signum (a, b)      = (signum a, signum b)
    --fromInteger (a, b) = (fromInteger a, fromInteger b)
	
instance (Num b) => Num (a -> b) where
    f + g         = \x -> f x + g x   
    f * g         = \x -> f x * g x 
    abs f         = \x -> abs (f x)
    signum f      = \x -> signum (f x)
    negate f      = \x -> negate (f x)
    fromInteger i = \x -> fromInteger i	
	
instance (Logic a, Logic b) => Logic (a, b) where
    true             = (true, true)
    false            = (false, false)
    (a, b) && (c, d) = (a && c, b && d)
    (a, b) || (c, d) = (a || c, b || d)
    not (a, b)       = (not a, not b)	
    w ==> (a, b)     = ((w ==> a), (w ==> b))
--
--
--    ALTERNATIVE LOGIC OPERATORS
--
--	
-- product conjunction. My code.
(&&&) :: (Num a) => a -> a -> a
p &&& q = p * q

-- sum minus product disjunction. My code.
(|||) :: (Num a) => a -> a -> a
p ||| q = p + q - (p * q)

-- correlation min implication. My code.
(===>) :: (Ord a, Num a) => a -> a -> a
p ===> q = min p q
 
-- Yager Operations. My code. Bugs to fix.
yagerOr :: (Fractional b, Integral b, Num b, Num a, Ord a) => b -> a -> a -> a
yagerOr x p q = if x > 0 then (minimum [1, (((p^x) + (q^x)))^(1/x)]) else undefined

yagerAnd :: (Fractional b, Integral b, Num b, Num a, Ord a) => b -> a -> a -> a
yagerAnd x p q = if x > 0 then 1 - minimum [1, ((1-p)^x + (1-q)^x)^(1/x)] else undefined

yagerComp :: (Fractional b, Integral b, Num b, Num a, Ord a) => b -> a -> a        
yagerComp x p = if x > 0 then (1 - p^x)^(1/x) else undefined

-- logic operations over lists.
and, or :: Logic a => [a] -> a
and     = foldr (&&) true
or      = foldr (||) false

any, all :: Logic b => (a -> b) -> [a] -> b
any p     = or . map p
all p     = and . map p

--
--
--
--          PREDICATES
--
--
--Type synonyms for up, down, triangle, trapezoid, s-, z-, and pi functions. --
--Linguistic variables take these as values. My code, though very similar to Meehan and Joy. 
-- I wrote this before discovering their paper.--

type Down      = Double -> Double -> Fuzzy Double
type Up        = Double -> Double -> Fuzzy Double
type Triangle  = Double -> Double -> Double -> Fuzzy Double 
type Trapezoid = Double -> Double -> Double -> Double -> Fuzzy Double
type S_func    = Double -> Double -> Double -> Fuzzy Double
type Z_func    = Double -> Double -> Double -> Fuzzy Double
type Pi_func   = Double -> Double -> Double -> Double -> Double -> Double -> Fuzzy Double


-- All of the following shape definitions are my original code.

-- 's_function' is 0 until a, where it curves up through b (where it is .5) to c, at which point it becomes 1.
s_function :: S_func
s_function a b c x | (x <= a) 				= 0
					|(a < x) && (x <= b) 	= ((x - a)^2)/(2*(b -a)^2)
					| (b < x) && (x <= c)   = (-1)*((x - c)^2)/(2*(b - c)^2) + 1
					|(x > c) 				= 1

-- 'trap_func' is 0 until a, where it angles up to 1 at b, remains 1 through c, then angles down to d, after which it is 0 again.					
trap_func :: Trapezoid
trap_func a b c d x | (x <= a)            = 0
                    | (d <= x)            = 0
					| ((a<x) && (x < b)) = (x - a)/(b - a)
					| ((c<x)&&(x<d))      = (d-x)/(d-c)
					| ((b<=x)&&(x<=c))    = 1

-- 'tri_func' is 0 until a, then it angles up to b where it becomes 1, then angles down to c, after which it is 0 again.					
tri_func :: Triangle
tri_func a b c x | (x<=a)           = 0
                 | (x>c)            = 1
                 | ((a<x)&&(x<=b))  = (x-a)/(b-a)
				 | ((b<x)&&(x<=c))  = (c-x)/(c-b)

-- 'z_function' is 1 until a, at which point it curves down through b and curves level at c where it is 1.
z_function :: Z_func
z_function a b c x = 1 - (s_function x a b c)

-- 'pi_func' looks like a Gaussian, but is not necessarily symmetric.
pi_func :: Pi_func
pi_func a b c d e f x | (x<=c)         = s_function x a b c
                      | ((c<x)&&(x<d)) = 1
					  | (x >= d)       = z_function x d e f

-- 'up' is 0 until a, then in a straight line slopes up to b, then levels off at 1.					  
up :: Up
up a b x | (x<=a)          = 0
         | ((a<x)&&(x<=b)) = (x-a)/(b-a)
		 | (x>b)           = 1

-- 'down' is 1 until a, then in a straight line slopes down to b, then levels off at 0.		 
down :: Down
down a b x | (x<=a)           = 1
           | ((a<x)&&(x<=b))  = (b-x)/(b-a)
           | (x>b)            = 0		   
					

--
--
--
--            DOMAINS
--
--		

-- Domain of Fuzzy Subset. Haskell allows '..' definition of lists
-- as long as the things have an order. Example: ['a'..'z'] is the
-- list of lowercase alphabet.
type Domain a = [a]

--Declare a domain. All my code.

--
-- On a scale from 0 to 100, how 'heavy' should the clothes be?
-- For example, dress lightly, in layers, or bundle up?
--
amountLayered :: Domain Heaviness
amountLayered = [0,1..100]
--
-- On a scale from 0 to 100, how fancy should the clothes be?
--
dressedUp :: Domain Fanciness
dressedUp = [0,1..100]

--
--
--    FUZZY SUBSETS
--
--
-- This is a type declaration for the "Fuzzy set of u things",
-- which is a type synonym that takes an object of any type and returns a Double.
-- This captures fuzzy subsets by treating the membership function as definition.

type Fuzzy u = u -> Double			


-- support function
supp :: Domain a -> Fuzzy a -> [a]
supp dom f = filter (\x -> f x > 0) dom
--
--
--
--
--         LINGUISTIC VARIABLES
--       AND THEIR VALUES. My code.
--
--
--
type Temperature = Double

sweltering :: Fuzzy Temperature
sweltering = up 85 100

hot :: Fuzzy Temperature
hot = up 60 90

warm :: Fuzzy Temperature
warm = trap_func 57 75 80 90

chilly :: Fuzzy Temperature
chilly = trap_func 46 55 60 70

cold :: Fuzzy Temperature
cold = down 40 50

freezing :: Fuzzy Temperature
freezing = down 32 42  


type Humidity = Double

arid :: Fuzzy Humidity
arid = down 15 30  -- 100% arid through 15% humidity, then decreases through 30%, where it is no longer arid.

dry :: Fuzzy Humidity
dry = down 40 50

pleasant :: Fuzzy Humidity
pleasant = trap_func 30 50 60 70

damp :: Fuzzy Humidity
damp = tri_func 65 75 85

muggy :: Fuzzy Humidity
muggy = up 80 85

type Heaviness = Double

nothing :: Fuzzy Heaviness
nothing = down 0 7

light :: Fuzzy Heaviness
light = trap_func 3 8 12 15

regular :: Fuzzy Heaviness
regular = trap_func 13 21 28 35

layers :: Fuzzy Heaviness
layers = trap_func 30 40 50 65

bundled :: Fuzzy Heaviness
bundled = trap_func 60 70 80 85

parka:: Fuzzy Heaviness
parka = up 80 85

type Fanciness = Double

professor :: Fuzzy Fanciness
professor = down 10 20

jammies :: Fuzzy Fanciness
jammies = trap_func 15 25 35 45

relaxedClothes :: Fuzzy Fanciness
relaxedClothes = trap_func 35 50 55 70

businessCasual :: Fuzzy Fanciness
businessCasual = trap_func 55 70 80 90

businessPro :: Fuzzy Fanciness
businessPro = up 80 90


type Occasion = Double

sickDay :: Fuzzy Occasion
sickDay = trap_func 10 15 20 25

relaxedOccasion :: Fuzzy Occasion
relaxedOccasion = trap_func 20 30 40 50

walMart :: Fuzzy Occasion
walMart = down 10 25

inPublic :: Fuzzy Occasion
inPublic = trap_func 40 50 60 70

special :: Fuzzy Occasion 
special = trap_func 65 75 85 90

fancy :: Fuzzy Occasion
fancy = up 85 90


--
--
--     RULES
--
--
-- rulebase takes a subset operation and a list of subsets, and returns weighted result. 
rulebase :: (Logic a) => (a -> a -> a) -> [a] -> a
rulebase = foldr1
--
--
--
-- Rule firing on heaviness dimension, with min conjunction. My code.
--
--
how_many_layers :: Temperature -> Humidity -> Heaviness
how_many_layers deg hum = centroid amountLayered (
    rulebase (+) [
        (hot deg)  && (muggy hum)    ==> barely nothing,
        (hot deg)                    ==> light,
        (warm deg) && (pleasant hum) ==> regular,
        (warm deg) && (damp hum)     ==> barely regular,
        (warm deg) && (very damp hum)==> slightly layers,
        (slightly cold deg) && (damp hum)     ==> extremely layers,
        (cold deg)                   ==> bundled,
        (freezing deg) && (dry hum)  ==> parka,
        (freezing deg) && (arid hum) ==> extremely parka])
--
--
-- Rule firing on heaviness dimension, with product conjunction. My code.
--
--
how_many_layers' :: Temperature -> Humidity -> Heaviness		
how_many_layers' deg hum = centroid amountLayered(
    rulebase (+) [
        (hot deg)  &&& (muggy hum)    ==> barely nothing,
        (hot deg)                     ==> light,
        (warm deg) &&& (pleasant hum) ==> regular,
        (warm deg) &&& (damp hum)     ==> barely regular,
        (warm deg) &&& (very damp hum)==> slightly layers,
        (slightly cold deg) &&& (damp hum)     ==> extremely layers,
        (cold deg)                    ==> bundled,
        (freezing deg) &&& (dry hum)  ==> parka,
        (freezing deg) &&& (arid hum) ==> extremely parka])		
--
--
-- Rule firing on heaviness dimension, with correlation min implication. My code. Buggy.
--
--
{-how_many_layers_min_impl :: Temperature -> Humidity -> Heaviness
how_many_layers_min_impl deg hum = centroid amountLayered (
    rulebase (+) [
        (hot deg)  && (muggy hum)    ===> barely nothing,
        (hot deg)                    ===> light,
        (warm deg) && (pleasant hum) ===> regular,
        (warm deg) && (damp hum)     ===> regular,
        (warm deg) && (very damp hum)===> layers,
        (cold deg) && (damp hum)     ===> extremely layers,
        (cold deg)                   ===> bundled,
        (freezing deg) && (dry hum)  ===> parka,
        (freezing deg) && (arid hum) ===> extremely parka])
-}

--
--
--         Rule firing on fanciness dimension. My code.
--
--
how_dressy :: Occasion -> Heaviness
how_dressy occ = centroid dressedUp (
    rulebase (+) [
        sickDay occ         ==> jammies,
        relaxedOccasion occ ==> relaxedClothes,
        walMart occ         ==> professor,
        inPublic occ        ==> businessCasual,
        special occ	    ==> businessPro,
        fancy occ           ==> extremely businessPro])		
--
--
--
--              Hedges 
--
--
hedge :: Double -> Fuzzy a -> Fuzzy a
hedge pwr f x = if fx == 0 then 0 else fx ** pwr
    where fx = f x
	
very, extremely, somewhat, slightly, barely :: Fuzzy a -> Fuzzy a
very         = hedge 2
extremely    = hedge 3
somewhat     = hedge 0.5
slightly     = hedge (1/3)
barely       = hedge 0.2	

--
-- 
--        Defuzzification
--
--
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
-- Median function is my code.
median :: Ord a => [a] -> a
median  []    = undefined
median xs     = (qsort xs) !! middle
    where middle = (length xs) `div` 2

minmax dom f  = minimum (maxima dom f)
medmax dom f  = median (maxima dom f)
maxmax dom f  = maximum (maxima dom f)

--
--
--
--        FOR FUTURE APPLICATIONS
--
--
--
--       
--      Extend program to receive a day as input,
--      output clothing dimension values. My code.
--
{-type Day = (Temperature, Humidity, Occasion)

Temperature :: Day -> Temperature
Temperature (d,_,_) = d

humidity :: Day -> Humidity
humidity (_,h,_) = h

occasion :: Day -> Occasion
occasion (_,_,o) = o

--mood :: Day -> Occasion
--mood (_,_,_,m) = m
--
--
--    Input: Forecast = a list of Days.
--    Output: List of wardrobes.
--
--
forecast :: [Day]
forecast = undefined

--predicates
-- sweltering, hot, warm, chilly, cold, freezing.
isWarm day       = warm (Temperature day)
isCold day       = cold (Temperature day)
isHot day        = hot (Temperature day)
isChilly day     = chilly (Temperature day)
isFreezing day   = freezing (Temperature day)
isSweltering day = sweltering (Temperature day)

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
-}







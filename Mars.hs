module Mars where

import Data.Char
import Test.QuickCheck
import Data.Typeable


type Newtonseconds = Float
type Poundseconds = Int


calculate :: Float -> Newtonseconds
calculate x = 40.0 * x

badcalculate :: Int -> Poundseconds
badcalculate x = (40 * x)*(4)

isNewtonseconds :: (Typeable a) => a -> Bool
isNewtonseconds n = typeOf n == typeOf (calculate 2.22)

-- QuickCheck example set --

getList :: IO [Char]
getList = fmap take5 getContents

take5 :: [Char] -> [Char]
take5 = take 5 . filter (`elem` ['a'..'e'])

--deepCheck p = check (Args {maxSuccess = 1000, maxSize = 1000}) p
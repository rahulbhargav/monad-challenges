{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where
import MCPrelude

nRands :: Int -> [Integer]
nRands n = take n $ 
            iterate (\v -> fst (rand $ mkSeed v)) 1

fiveRands = nRands 5

randLetter :: Seed -> (Char, Seed)
randLetter s = 
    let (x, y) = rand s
    in (toLetter x, y)

randString :: Int -> String
randString l = (\x -> fst $ randLetter (mkSeed x)) `map` (nRands l) 

randString3 :: String
randString3 = randString 3

type Gen a = (a, Seed)

generalA :: (Integer -> b) -> Gen a -> Gen b
generalA genF g = 
    let (x, y) = rand (snd g)
    in (genF x, y)

randEven :: Gen Integer -> Gen Integer
randEven = generalA (2 *) 

randOdd :: Gen Integer -> Gen Integer
randOdd = generalA (\x -> (2 * x) + 1)

randTen :: Gen Integer -> Gen Integer
randTen = generalA (10 *)

randChar :: Gen Char -> Gen Char
randChar = generalA toLetter

randNum :: Gen Integer -> Gen Integer
randNum = generalA fromIntegral

-- generalPair :: Gen a -> Gen b -> Gen (a,b)
-- generalPair gA gB = 
--     let (c,s1) = randChar gA
--     in let (n, s2) = rand
--         in ((c,n), s2)
    -- (n,s2) = randNum (1, s1)
    -- ((c,n), s2)

-- randPair :: Gen (Char, Integer)
-- randPair = generalA (\x -> x) randChar ('', mkSeed 1)



    
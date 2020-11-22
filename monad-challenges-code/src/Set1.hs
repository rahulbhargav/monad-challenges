{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where
import MCPrelude

nRands :: Int -> [Integer]
nRands n = take n $ 
            iterate (\v -> fst (rand $ mkSeed v)) 1

fiveRands = nRands 5

-- randLetter :: Seed -> (Char, Seed)
-- randLetter s = 
--     let (x, y) = rand s
--     in (toLetter x, y)

randString :: Int -> String
randString l = (\x -> fst $ randLetter (mkSeed x)) `map` (nRands l) 

randString3 :: String
randString3 = randString 3

type Gen a = Seed -> (a, Seed)

myRand :: Gen Integer
myRand = rand

randLetter :: Gen Char
randLetter s = 
    let (x ,y) = myRand s
    in (toLetter x, y)

generalA :: (a -> b) -> Gen a -> Gen b
generalA genF g seed = 
    let (x, y) = g seed
    in (genF x, y)

randEven :: Gen Integer
randEven = generalA (2 *) myRand

randOdd :: Gen Integer
randOdd = generalA (\x -> (2 * x) + 1) myRand

randTen :: Gen Integer
randTen = generalA (10 *) myRand

generalPair :: Gen a -> Gen b -> Gen (a,b)
generalPair gA gB seed = 
    let (aVal,seed1) = gA seed
    in let (bVal, seed2) = gB seed1
    in ((aVal, bVal), seed2)
    
randPair :: Gen (Char, Integer)
randPair = generalPair randLetter myRand

-- Even more generalised version of generalPair
generalB :: Gen a -> Gen b -> (a -> b -> c) -> Gen c
generalB gA gB constructor seed = 
    let (aVal,seed1) = gA seed
    in let (bVal, seed2) = gB seed1
    in (constructor aVal bVal, seed2)

-- same as randPair. But, using generalB
randPair2 :: Gen (Char, Integer)
randPair2 = generalB randLetter myRand (\a b -> (a,b))

-- fiveRands_ :: Seed -> [(Integer, Seed)]
-- fiveRands_ s = 
--     take 5 $ 
--         iterate myRand (\s -> (1, s))

-- repRandom :: [Gen a] -> Gen [a]
-- repRandom [x] seed = x seed
-- repRandom [gHead: gTail] seed = 
--     let (value, nextSeed) = gHead seed
--     in (value : repRandom gTail nextSeed) 


-- [S -> (I,S), S -> (I, S), S -> (I,S)]
-- S -> ([I,I,I], S))
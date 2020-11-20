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

generalA :: (a -> a) -> Gen a
generalA genF = 
    let (x, y) = rand (mkSeed 1)

randEven :: Gen Integer
randEven = 
    let (x, y) = rand (mkSeed 1)
    in (x * 2, y)

randOdd :: Gen Integer
randOdd = 
    let (x, y) = randEven (mkSeed 1)
    in (x + 1, y)

-- randTen :: Gen Integer -- the output of rand * 10
randTen = 
    let (x, y) = rand (mkSeed 1)
    in (x * 10, y)



    
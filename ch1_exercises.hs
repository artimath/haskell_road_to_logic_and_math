module PrimeNumberTest where

-- Let n > 1 be a natural number.
-- We use LD(n) for least natural number that divides n.
-- A number d divides n if there is a natural number a where d * a = n.

-- Proposition 1.2
-- if n > 1 then LD(n) is a prime number.
-- if n > 1 and n is not a prime, then (LD(n))^2 <= n
--  thus LD(N) <= √n

divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

rem1 :: Integer -> Integer -> Integer
rem1 d n = rem d n

ld :: Integer -> Integer
ld n = ldf 2 n

ldf :: Integer -> Integer -> Integer
ldf k n
  | divides k n = k
  | k ^ 2 >= n = n
  | otherwise = ldf (k + 1) n

prime0 :: Integer -> Bool
prime0 n
  | n < 1 = error "not a positive integer"
  | n == 1 = False
  | otherwise = ld n == n

-- Minimum of list of integers
mnmInt :: Ord a => [a] -> a
mnmInt [] = error "empty list"
mnmInt [x] = x
mnmInt (x : xs) = min' x (mnmInt xs)

min' :: Ord a => a -> a -> a
min' x y
  | x <= y = x
  | otherwise = y

max' :: Ord a => a -> a -> a
max' x y
  | x >= y = x
  | otherwise = y

mxmInt [] = error "empty list"
mxmInt [x] = x
mxmInt (x : xs) = max' x (mxmInt xs)

removeFst :: Ord a => a -> [a] -> [a]
removeFst _ [] = []
removeFst m [x]
  | m == x = []
  | otherwise = [x]
removeFst m (x : xs)
  | m == x = xs
  | otherwise = x : removeFst m xs

-- Example 1.11
minSort :: [Int] -> [Int]
minSort [] = []

minsort [x] = [x]
minsort (x : (y : ys))
  | x <= y = x : minsort (y : ys)
  | otherwise = y : minsort (x : ys)
-- ^ ^ does not work lol.
-- λ > minsort [5,2,3,7,2,3]
-- [2,3,5,2,3,7]
-- I'm probably one little bit away from it working, but it's ugly. The book solution was my first introduction
-- to the "oh my god it's beautiful" experience of haskell.

-- here's the where version of the beautiful solution
minsortWhere :: Ord a => [a] -> [a]
minsortWhere [x] = [x]
minsortWhere xs = m : minsortWhere (removeFst m xs)
  where
    m = mnmInt xs

-- I am not used to lazy evaluation so I didn't even think about... essentially looping through it all to make it really easy. Oh, I'll find the smallest one. Remove that. Then Recursively call the rest of my list. Like, duh. But I guess my mind was prematurely optimizing lol.
-- let version
minsortLet :: Ord a => [a] -> [a]
minsortLet [x] = [x]
minsortLet xs =
  let m = mnmInt xs
   in m : minsortLet (removeFst m xs)

--  Yup, it works. vry nice.

-- Average of list of Integers
-- but let's implement our own sum and count... for science

-- count' :: [Int] -> Int
-- count' [] = 0
-- count' (x : xs) = 1 + count' xs

-- above works but it wants me to use foldr
-- let's try.
countFoldR :: [Int] -> Int
countFoldR xs = foldr (\x -> (+) 1) 0 xs

-- looks like fold r is like a reduce that starts on the right.
-- so to get a count... we just want to add one per list
-- in the above it's... creating a lambda, which allows us to take the inputted element, but return 1. then we add those 1's to 0.
countFoldR' xs = foldr (\x -> (+) 1) 0 xs

-- but it wants to remove the xs at the end... which i don't understand yet why. oh... because if I don't give it a parameter, then it returns a function. which then when I GIVE it a parameter, it's essentially returning FoldR, curried, which is waiting for a final argument which is the list?
-- thus...
countFoldR'' :: [Int] -> Int
countFoldR'' = foldr (\x -> (+) 1) 0

-- well that was fun. Alright, now a sum function.
-- which with our new foldR function should be easy, no? let's try regular recursive style first.

-- sum' :: [Int] -> Int
-- sum' [] = 0
-- sum' (x : xs) = x + sum' xs

-- and it suggests using foldr, haha
-- so let's write that out manually.

sumFoldR :: [Int] -> Int
sumFoldR = foldr (+) 0

-- it wants me to replace with sum. you're smart, compiler, but no.
-- success.

-- now let's implement the average function. again, i see how we can use foldr. but let's not.
avg :: [Int] -> Float
avg [] = 0
avg xs =
  sum' / count'
  where
    sum' = fromIntegral $ sumFoldR xs
    count' = fromIntegral $ countFoldR'' xs

-- instances of a character in a string.
countOccurence :: Char -> String -> Int
countOccurence _ [] = 0
countOccurence c (x : xs)
  | c == x = 1 + countOccurence c xs
  | otherwise = countOccurence c xs

-- blowup string. multiply by index
strBlowup :: String -> String
strBlowup xs =
  multiplyStr xs 0
  where
    multiplyStr :: String -> Int -> String
    multiplyStr [] _ = ""
    multiplyStr (x : xs) n = map (\_ -> x) [0 .. n] ++ multiplyStr xs (n + 1)

-- sort a string alphabetically

srtString :: [String] -> [String]
srtString = minsortWhere

prefix :: String -> String -> Bool
prefix "" _ = True
prefix _ "" = False
prefix (x : xs) (y : ys) = x == y && prefix xs ys

substring :: String -> String -> Bool
substring "" _ = True
substring _ "" = False
substring (x : xs) (y : ys) =
  (x == y && substring xs ys) || substring (x : xs) ys

primeFactor :: Integer -> [Integer]
primeFactor 1 = []
primeFactor n = f : primeFactor (div n f)
  where
    f = ld n

lengths :: [[a]] -> [Int]
lengths x = map countFoldR' x

sumLengths :: [[a]] -> Int
sumLengths x = sumFoldR $ lengths x

primes0 :: [Integer]
primes0 = filter prime0 [2 ..]

ldp :: Integer -> Integer
ldp = ldpf primes1

ldpf :: [Integer] -> Integer -> Integer
ldpf (p : ps) n | rem n p == 0 = p | p ^ 2 > n = n | otherwise = ldpf ps n

primes1 :: [Integer]
primes1 = 2 : filter prime [3 ..]

prime :: Integer -> Bool
prime n
  | n < 1 = error "not a positive integer"
  | n == 1 = False
  | otherwise = ldp n == n

main :: IO ()
main = do
  putStrLn "Hello"
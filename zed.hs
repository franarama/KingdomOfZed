{-

   Francesca Ramunno: T00053082
   Comp 3120

   --------------
   Kingdom of Zed
   --------------

   The additional functionality I have completed is:
      - 1. Allow maps of Zed or arbitrary size, i.e. any n×n map for n >= 2.
      - 2. Allow incomplete information. For this option, merchants may withold information from
           you. If they do, their clue is represented as a 0 (zero).

   The additional functionality is noted in the comments above the appropriate
   function, with a **.

   Note this solution uses no intelligent solving which means it can be
   slow for larger map sizes.

-}


import Data.List
import Data.Maybe


-- Returns a list off all combinations of size n given a list of elements.
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs

-- Returns a list of all possible maps of size n x n.
-- ** This allows for getting maps of arbitrary size
getMaps :: Int -> [[[Int]]]
getMaps n = concatMap permutations (combinations n (permutations [1..n]))

-- Helper function that simply checks if a list has all unique elements.
hasUniqueElems :: Eq a => [a] -> Bool
hasUniqueElems [] = True
hasUniqueElems (x:xs)
  | x `elem` xs = False
  | otherwise = hasUniqueElems xs

-- Checks the rows and columns for uniqueness for a given map.
hasUniqueRowsAndCols :: Eq a => [[a]] -> Bool
hasUniqueRowsAndCols kzMap = and (map hasUniqueElems kzMap) &&
                             and (map hasUniqueElems (transpose kzMap))

-- Returns how many visits a merchant would make given a list of trading posts.
getVisits :: (Ord t1, Num t2, Num t1) => [t1] -> t2
getVisits xs = numVisits xs 0 0
  where
    numVisits [] count _ = count
    numVisits (x:xs) count n
      | x > n = numVisits xs (count + 1) x
      | otherwise = numVisits xs count n

-- Takes a map and a clue and checks if the clue is consistent
-- with the given map.
-- ** This is where it checks for merchant clues of 0, which are ignored.
checkMerchantClues :: (Ord t, Num a, Num t, Eq a) => [[t]] -> [a] -> Bool
checkMerchantClues kzMap clues = checkMerchants kzMap clues 0
  where
    checkMerchants [] _ _ = True
    checkMerchants (x:xs) visits count
      | (getVisits x == (visits !! count)) || (visits !! count == 0) =
          (checkMerchants xs visits (count+1))
      | otherwise = False

-- Helper function that reverses each list in a list of lists
reverseEach :: [[a]] -> [[a]]
reverseEach xs = map (\x -> reverse x) xs

-- Checks a map with provided merchant clues to see if its valid.
hasValidMerchants :: (Ord t, Num a, Num t, Eq a) => [[t]] -> [[a]] -> Bool
hasValidMerchants kzMap clues =
  -- check West-travelling merchants
  checkMerchantClues (reverseEach kzMap) (clues !! 1) &&
  -- check East-travelling merchants
  checkMerchantClues kzMap (reverse (clues !! 3)) &&
  -- check South-travelling merchants
  checkMerchantClues (transpose kzMap) (clues !! 0) &&
  -- check North-travelling merchants
  checkMerchantClues (reverseEach (transpose kzMap)) (reverse (clues !! 2))

-- Checks if the map is valid for the conditions above.
-- (ie: all unique rows/cols, and consistent with merchant clues)
isValidMap :: (Ord a1, Num a2, Num a1, Eq a2) => [[a1]] -> [[a2]] -> Bool
isValidMap kzMap clues = hasValidMerchants kzMap clues &&
                         hasUniqueRowsAndCols kzMap

-- Make a Clue type so input can be of the form ([north],[east],[south],[west])
type Clue = ([Integer],[Integer],[Integer],[Integer])

-- Helper function that returns side length given a Clue type input.
getSizeFromClue :: Foldable t => (t a, b, c, d) -> Int
getSizeFromClue (n,_,_,_) = length n

-- Returns a possible map if one exists otherwise nothing.
getValidMaps :: (Ord a1, Num a2, Num a1, Eq a2) => ([a2], [a2], [a2], [a2]) -> [[[a1]]] -> Maybe [[a1]]
getValidMaps clue kzMaps = getZedMaps clue kzMaps
  where
    getZedMaps _ [] = Nothing
    getZedMaps (n,e,s,w) (x:xs)
      | isValidMap x [n,e,s,w] == True = Just x
      | otherwise = (getZedMaps (n,e,s,w) xs)

-- Main zed function. Takes a clue and returns a possible map.
zed (clue) = if (hasMap /= Nothing) then mapM_ print (fromJust hasMap) else putStrLn "No maps"
  where hasMap = (getValidMaps clue (getMaps(getSizeFromClue clue)))

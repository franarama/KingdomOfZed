import Data.List
import Data.Maybe

-- Returns a list of all possible maps with side length n.
-- Essentially returns n combinations of n permutations.
getMaps :: Int -> [[[Int]]]
getMaps n = combinations (permutations [1..n]) []
  where
    combinations [] acc = if n == length acc then [acc] else []
    combinations (x:xs) acc
      | n == length acc = [acc]
      | otherwise = combinations xs (x:acc) ++ combinations xs acc

-- Helper function that simply checks if a list has all unique elements.
allUnique :: Eq a => [a] -> Bool
allUnique [] = True
allUnique (x:xs)
  | x `elem` xs = False
  | otherwise = allUnique xs

-- Checks the rows and columns for uniqueness for a given map.
hasUniqueRowsAndCols :: Eq a => [[a]] -> Bool
hasUniqueRowsAndCols kzMap = and (map allUnique kzMap) &&
                             and (map allUnique (transpose kzMap))

-- numVisits returns how many visits a merchant would make
-- given a list of trading posts
getVisits :: (Ord t1, Num t2, Num t1) => [t1] -> t2
getVisits xs = numVisits xs 0 0
  where
    numVisits [] count _ = count
    numVisits (x:xs) count n
      | x > n = numVisits xs (count + 1) x
      | otherwise = numVisits xs count n

-- Takes a zed map and a clue and checks if the clue is consistent
-- with the given map.
checkMerchantClues :: (Ord t, Num a, Num t, Eq a) => [[t]] -> [a] -> Bool
checkMerchantClues kzMap clues = checkMerchants kzMap clues 0
  where
    checkMerchants [] _ _ = True
    checkMerchants (x:xs) visits count
      | count == length visits = True
      | getVisits x == (visits !! count) = (checkMerchants xs visits (count+1))
      | otherwise = False

-- helper function that reverses each list in a list of lists (maintains
-- their order though!)
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

-- Checks if the map is valid for the conditions above
-- (uniqueness, merchant visits)
isValidMap :: (Ord a1, Num a2, Num a1, Eq a2) => [[a1]] -> [[a2]] -> Bool
isValidMap kzMap clues = hasValidMerchants kzMap clues &&
                         hasUniqueRowsAndCols kzMap

zed clue (north,east,south,west = [north, east, south, west]



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
getVisits xs = numVisits xs 0 0
numVisits [] count _ = count
numVisits (x:xs) count n
  | x > n = numVisits xs (count + 1) x
  | otherwise = numVisits xs count n

-- helper function that reverses each list in a list of lists (maintains
-- their order though!)
reverseEach xs = map (\x -> reverse x) xs

-- Takes a zed map and a clue and checks if the clue is consistent
-- with the given map.
checkMerchants [] _ _ = True
checkMerchants (x:xs) visits count
    | count == length visits = True
    | getVisits x == (visits !! count) = (checkMerchants xs visits (count+1))
    | otherwise = False


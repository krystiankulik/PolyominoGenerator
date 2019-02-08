type Point = (Int, Int)
type Polyomino = [Point]

minPoint :: Polyomino -> Point
minPoint (first:rest) = foldr (\(x, y) (mx, my) -> (min x mx, min y my)) first rest
 
normalizePosition :: Polyomino -> Polyomino
normalizePosition pol = let (minx, miny) =  minPoint pol in map (\(x, y) -> (x - minx, y - miny)) pol

rotate90 :: Point -> Point
rotate90 (x, y) = (y, -x)
  
rotate180 :: Point -> Point
rotate180 (x, y) = (-x, -y)
 
rotate270 :: Point -> Point
rotate270 (x, y) = (-y, x)
 
rotate :: Polyomino -> [Polyomino]
rotate p = [p,  map rotate90 p, map rotate180 p, map rotate270 p]

quicksort :: (Ord a) => [a] -> [a] 
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]
     
normalizeRotation :: Polyomino -> Polyomino
--normalizeRotation p = minimum ( map (\x -> quicksort(normalizePosition x)) ( rotate p))
normalizeRotation = minimum . map (quicksort . normalizePosition) . rotate

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldl (\x y -> if y `elem` x then x else y:x) []

addNeighbours :: Point -> [Point]
addNeighbours (x, y) =
     [(x - 1, y),
      (x + 1, y),
      (x, y - 1),
     (x, y + 1)]

newPoints :: Polyomino -> [Point]
newPoints pol = removeDuplicates ( filter (`notElem` pol) (concatMap addNeighbours pol))

newPolyominos :: Polyomino -> [Polyomino]
--newPolyominos pol = removeDuplicates ( map (\x -> normalizeRotation (x:pol)) (newPoints pol)) 
newPolyominos pol = (removeDuplicates . map (normalizeRotation . (:pol)) ) (newPoints pol)

generate :: Int -> [Polyomino]

generate 0 = []
generate 1 = [[(0, 0)]]
generate n = (removeDuplicates . concatMap newPolyominos)  (generate (n - 1))

countPolyominos :: Int -> Int
countPolyominos n = foldl (const.(1 +)) 0 (generate n)

--printing Polyominos as graphics
getPrintRowRec :: Int -> [String]
getPrintRowRec n = foldl (const.(" ":)) [] [1..n]

getMatrix :: Int -> [[String]]
getMatrix n = foldl (const.( getPrintRowRec n : )) [] [1..n]

updateMatrix :: a ->[[a]] -> (Int, Int) -> [[a]]
updateMatrix  x m (r,c) =
  take r m ++
  [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++
  drop (r + 1) m

applyPolyomino :: [[String]] -> Polyomino -> [[String]] 
applyPolyomino = foldl  (updateMatrix "x")

getPrintMatrix :: [[String]] -> String
getPrintMatrix = unlines . map unwords  

printPolyominos :: Int -> String
printPolyominos n = unlines (map ( getPrintMatrix.applyPolyomino (getMatrix n)) (generate n))

main = do 
    putStrLn "Podaj wielkosc polyomino:"
    number <-getLine
    putStr (printPolyominos (read number))
    putStr "Wygenerowano nastepujaca liczbe polyomino:"
    putStrLn (show (countPolyominos  (read number)))
    end <- getLine
    return ()


    
    



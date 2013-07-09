module Main where
import Text.CSV
import Control.Parallel.Strategies

main :: IO ()
main = do
  myCSV <- makeModel $ parseCSVFromFile "digitssample.csv"
  testData <- makeModel $ parseCSVFromFile "digitscheck.csv"
  let results = (map (pick myCSV) testData `using` (parListChunk 12 rseq))
  mapM_ (putStrLn . show) results
  putStrLn $ accuracy results

accuracy :: [Bool] -> String
accuracy results = show ((fromIntegral (length $ filter id results)) / (fromIntegral (length results))) ++ "%"


isCorrect :: String -> (String, [Int])-> String
isCorrect guess (label, _) = show $ label == guess

convertCSV :: [String] -> (String, [Int])
convertCSV (label:xs) = (label, map read xs)

pick :: Model -> (String, [Int]) -> Bool
pick model (label, rowData) = label == fst ( foldr ( findLowest rowData ) ("", 1/0) model)

findLowest rowToClassify (label, xs) (winnerLabel, winnerDist) =
    if tempDist < winnerDist then (label, tempDist) else (winnerLabel, winnerDist)
                             where tempDist = euclidian xs rowToClassify

euclidian :: [Int] -> [Int] -> Double
euclidian xs ys = sqrt . fromIntegral . sum $ zipWith dist xs ys

dist :: Int -> Int -> Int
dist x y = (x - y) ^ 2

makeModel file = do
  x <- file
  return $ map convertCSV $ either (error . show) (init . drop 1) x

type Model = [(String, [Int])]

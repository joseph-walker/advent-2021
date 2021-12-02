pairs :: [a] -> [(a, a)]
pairs xs =
    zip xs (tail xs)

triplets :: [a] -> [(a, a, a)]
triplets xs =
    zip3 xs (tail xs) (tail . tail $ xs)

solve :: [Int] -> Int
solve =
    length . filter (== True) . fmap (uncurry (<)) . pairs

readInput :: IO [Int]
readInput = do
    input <- readFile "input.txt"
    return $ read <$> lines input

main :: IO ()
main = do
    sonar <- readInput
    print . solve $ sonar
    print . solve . fmap (\(a, b, c) -> a + b + c) . triplets $ sonar

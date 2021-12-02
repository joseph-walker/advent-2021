import Text.Parsec
import Control.Monad.Identity ( Identity )

type Position = (Int, Int)

type Bearing = (Int, Int, Int)

data Instruction
    = Forward Int
    | Down Int
    | Up Int
    deriving (Show, Eq)

pDirection :: ParsecT String u Identity (Int -> Instruction)
pDirection =
    choice
        [ Forward <$ string "forward"
        , Down <$ string "down"
        , Up <$ string "up"
        ]

pInstruction :: ParsecT String u Identity Instruction
pInstruction = do
    direction <- pDirection <* space
    amt <- read <$> many1 digit
    return $ direction amt

pInstructions :: ParsecT String u Identity [Instruction]
pInstructions =
    pInstruction `sepBy` string "\n"

runInstruction :: Position -> Instruction -> Position
runInstruction (x, y) (Forward n) =
    (x + n, y)
runInstruction (x, y) (Down n) =
    (x, y + n)
runInstruction (x, y) (Up n) =
    (x, y - n)

runComplicatedInstruction :: Bearing -> Instruction -> Bearing
runComplicatedInstruction (x, y, z) (Forward n) =
    (x + n, y + (n * z), z)
runComplicatedInstruction (x, y, z) (Down n) =
    (x, y, z + n)
runComplicatedInstruction (x, y, z) (Up n) =
    (x, y, z - n)

main :: IO ()
main = do
    input <- readFile "input.txt"
    case parse pInstructions "" input of
        Left _ ->
            error "Invalid Input"
        Right instructions -> do
            print . uncurry (*) $ foldl runInstruction (0, 0) instructions
            print . (\(x, y, _) -> x * y) $ foldl runComplicatedInstruction (0, 0, 0) instructions

import Data.Maybe

score_map :: [(String, Integer)]
score_map = [("A", 0), ("B", 1), ("C", 2)]

score_match :: (Integer, Integer) -> Integer
score_match (a, b) =
    1 + b + case mod (a - b) 3 of
        0 -> 3
        1 -> 0
        2 -> 6

convert_match :: String -> (Integer, Integer)
convert_match match_string =
    let [opponent, outcome] = words match_string 
    in do 
        let opponent' = fromJust (lookup opponent score_map)
        (opponent', choose_shape opponent' outcome)

choose_shape :: Integer -> String -> Integer
choose_shape opponent outcome
    | outcome == "X" = (opponent - 1) `mod` 3
    | outcome == "Y" = opponent
    | outcome == "Z" = (opponent + 1) `mod` 3

main = do
    file <- readFile "input"
    let matches = lines file
    let total_score = (sum . map (score_match . convert_match)) matches
    print total_score

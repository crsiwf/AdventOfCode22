import Data.Maybe

score_map :: [(String, Integer)]
score_map = [("A", 1), ("B", 2), ("C", 3), ("X", 1), ("Y", 2), ("Z", 3)]

score_match :: (Integer, Integer) -> Integer
score_match (opponent, me) =
    me + case mod (opponent - me) 3 of
        0 -> 3
        1 -> 0
        2 -> 6

convert_match :: String -> (Integer, Integer)
convert_match match_string =
    let [opponent, me] = words match_string
    in (fromJust (lookup opponent score_map), fromJust (lookup me score_map))

main :: IO ()
main = do
    file <- readFile "input"
    let matches = lines file
    let total_score = (sum . map (score_match . convert_match)) matches
    print total_score

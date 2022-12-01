import Text.Read

count_calories :: [[Integer]] -> Integer
count_calories = max3 . (map sum) 

group_snacks :: [String] -> [Integer] -> [[Integer]] -> [[Integer]]
group_snacks [] _ groups = groups
group_snacks (l:ls) current groups = case readMaybe l :: Maybe Integer of
    Just a -> group_snacks ls (a:current) groups
    Nothing -> group_snacks ls [] (current:groups)

max3 :: [Integer] -> Integer
max3 ls = max3' ls 0 0 0
max3' (l:ls) a b c
    | l > a = max3' ls l a b
    | l > b = max3' ls a l b
    | l > c = max3' ls a b l
    | otherwise = max3' ls a b c
max3' [] a b c = a + b + c

main = do
    file <- readFile "input"
    let snacks = lines file
    let grouped_snacks = group_snacks snacks [] []
    let most_calories = count_calories grouped_snacks
    putStrLn (show most_calories)
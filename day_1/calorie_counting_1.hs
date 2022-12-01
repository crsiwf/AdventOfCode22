import Text.Read

count_calories :: [[Integer]] -> Integer
count_calories = maximum . (map sum) 

group_snacks :: [String] -> [Integer] -> [[Integer]] -> [[Integer]]
group_snacks [] _ groups = groups
group_snacks (l:ls) current groups = case readMaybe l :: Maybe Integer of
    Just a -> group_snacks ls (a:current) groups
    Nothing -> group_snacks ls [] (current:groups)


main = do
    file <- readFile "input"
    let snacks = lines file
    let grouped_snacks = group_snacks snacks [] []
    let most_calories = count_calories grouped_snacks
    putStrLn (show most_calories)


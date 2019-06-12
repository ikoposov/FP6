toIndex n = n - 1
getWeight w n = w !! (toIndex n)
getWeights w xs = map (getWeight w) xs
sumWeights w xs = sum (getWeights w xs)

data Weight = Light | Equal | Heavy

scales :: [Int] -> [Int] -> [Int] -> Weight
scales w xs ys =
  case compare (sumWeights w xs) (sumWeights w ys) of
    LT -> Light
    EQ -> Equal
    GT -> Heavy 

solution w = do
  putStrLn ""
  putStrLn "1 взвешивание кучи из 1 2 3 4 и 5 6 7 8 монет"
  case scales w [1,2,3,4]  [5,6,7,8] of
    Light -> do
      putStrLn "Первая куча легче"
      putStrLn ""
      putStrLn "2 взвешивание кучи из 1 2 5 и 3 6 9 монет"
      case scales w [1,2,5] [3,6,9] of
        Light -> do
          putStrLn "Первая куча легче"
          putStrLn ""
          putStrLn "3 взвешивание 1 и 2 монеты"
          case scales w [1] [2] of
            Light -> do
              putStrLn "Первая монета легче"
              putStrLn ""
              putStrLn "Монета под номером 1 отличается от других по весу"
            Equal -> do
              putStrLn "Вес монет равен"
              putStrLn ""
              putStrLn "Монета под номером 6 отличается от других по весу"
            Heavy -> do
              putStrLn "Первая монета тяжелее"
              putStrLn ""
              putStrLn "Монета под номером 2 отличается от других по весу"
        Equal -> do
          putStrLn "Вес куч равен"
          putStrLn ""
          putStrLn "3 взвешивание 7 и 8 монеты"
          case scales w [7] [8] of
            Light -> do
              putStrLn "Первая монета легче"
              putStrLn ""
              putStrLn "Монета под номером 8 отличается от других по весу"
            Equal -> do
              putStrLn "Вес монет равен"
              putStrLn ""
              putStrLn "Монета под номером 4 отличается от других по весу"
            Heavy -> do
              putStrLn "Первая монета тяжелее"
              putStrLn ""
              putStrLn "Монета под номером 7 отличается от других по весу"
        Heavy -> do
          putStrLn "Первая куча тяжелее"
          putStrLn ""
          putStrLn "3 взвешивание 3 и 9 монеты"
          case scales w [3] [9] of
            Light -> do
              putStrLn "Первая монета легче"
              putStrLn ""
              putStrLn "Монета под номером 3 отличается от других по весу"
            Equal -> do
              putStrLn "Вес монет равен"
              putStrLn ""
              putStrLn "Монета под номером 5 отличается от других по весу"
            Heavy -> do
              putStrLn "Первая монета тяжелее"
              putStrLn ""
              putStrLn "Монета под номером 3 отличается от других по весу"
    Equal -> do
      putStrLn "Вес куч равен"
      putStrLn ""
      putStrLn "2 взвешивание кучи из 1 2 3 и 9 10 11 монет"
      case scales w [1,2,3] [9,10,11] of
        Light -> do
          putStrLn "Первая куча легче"
          putStrLn ""
          putStrLn "3 взвешивание 9 и 10"
          case scales w [9] [10] of
            Light -> do
              putStrLn "Первая монета легче"
              putStrLn ""
              putStrLn "Монета под номером 10 отличается от других по весу"
            Equal -> do
              putStrLn "Вес монет равен"
              putStrLn ""
              putStrLn "Монета под номером 11 отличается от других по весу"
            Heavy -> do
              putStrLn "Первая монета тяжелее"
              putStrLn ""
              putStrLn "Монета под номером 9 отличается от других по весу"
        Equal -> do
          putStrLn "Вес куч равен"
          putStrLn ""
          putStrLn "3 взвешивание 1 и 12 монеты"
          case scales w [1] [12] of
            Light -> do
              putStrLn "Первая монета легче"
              putStrLn ""
              putStrLn "Монета под номером 12 отличается от других по весу"
            Equal -> do
              putStrLn "Вес монет равен"
              putStrLn ""
              putStrLn "Монета под номером 13 отличается от других по весу"
            Heavy -> do
              putStrLn "Первая монета тяжелее"
              putStrLn ""
              putStrLn "Монета под номером 12 отличается от других по весу"
        Heavy -> do
          putStrLn "Первая куча тяжелее"
          putStrLn ""
          putStrLn "3 взвешивание 9 и 10 монеты"
          case scales w [9] [10] of
            Light -> do
              putStrLn "Первая монета легче"
              putStrLn ""
              putStrLn "Монета под номером 9 отличается от других по весу"
            Equal -> do
              putStrLn "Вес монет равен"
              putStrLn ""
              putStrLn "Монета под номером 11 отличается от других по весу"
            Heavy -> do
              putStrLn "Первая монета тяжелее"
              putStrLn ""
              putStrLn "Монета под номером 10 отличается от других по весу"
    Heavy -> do
      putStrLn "Первая куча тяжелее"
      putStrLn ""
      putStrLn "2 взвешивание кучи из 1 2 5 и 3 6 9 монет"
      case scales w [1,2,5] [3,6,9] of
        Light -> do
          putStrLn "Первая куча легче"
          putStrLn ""
          putStrLn "3 взвешивание 3 и 9 монеты"
          case scales w [3] [9] of
            Light -> do
              putStrLn "Первая монета легче"
              putStrLn ""
              putStrLn "Монета под номером 3 отличается от других по весу"
            Equal -> do
              putStrLn "Вес монет равен"
              putStrLn ""
              putStrLn "Монета под номером 5 отличается от других по весу"
            Heavy -> do
              putStrLn "Первая монета тяжелее"
              putStrLn ""
              putStrLn "Монета под номером 3 отличается от других по весу"
        Equal -> do
          putStrLn "Вес куч равен"
          putStrLn ""
          putStrLn "3 взвешивание 7 и 8 монеты"
          case scales w [7] [8] of
            Light -> do
              putStrLn "Первая монета легче"
              putStrLn ""
              putStrLn "Монета под номером 7 отличается от других по весу"
            Equal -> do
              putStrLn "Вес монет равен"
              putStrLn ""
              putStrLn "Монета под номером 4 отличается от других по весу"
            Heavy -> do
              putStrLn "Первая монета тяжелее"
              putStrLn ""
              putStrLn "Монета под номером 8 отличается от других по весу"
        Heavy -> do
          putStrLn "Первая куча тяжелее"
          putStrLn ""
          putStrLn "3 взвешивание 1 и 2 монеты"
          case scales w [1] [2] of
            Light -> do
              putStrLn "Первая монета легче"
              putStrLn ""
              putStrLn "Монета под номером 2 отличается от других по весу"
            Equal -> do
              putStrLn "Вес монет равен"
              putStrLn ""
              putStrLn "Монета под номером 6 отличается от других по весу"
            Heavy -> do
              putStrLn "Первая монета тяжелее"
              putStrLn ""
              putStrLn "Монета под номером 1 отличается от других по весу"

cast :: String -> [Int]
cast str = map read (words str)

main = do
  putStrLn "Ввведите массу каждой из 13 монет"
  input <- getLine
  let weights = cast input in
    if length weights == 13
       then solution weights
       else putStrLn "Неверное значение"
module Week01.CardValidator where

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0    = []
  | otherwise = (x `rem` 10) : toDigitsRev (x `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . go . reverse
  where
    go (x:y:xs) = x : y * 2 : go xs
    go xs       = xs

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool
validate x = digitSum `rem` 10 == 0
  where
    digitSum = sumDigits . doubleEveryOther . toDigits $ x

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

divideByTen :: (Fractional a) => a -> a
divideByTen = (/10)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f ( f x)
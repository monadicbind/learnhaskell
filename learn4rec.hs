maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Cannot call maximum' on empty list"
maximum' (x:[])  = x
maximum' (x:xs')
		| x > maxTail = x
		| otherwise = maxTail
		where
			maxTail = maximum' xs'

maximumRec :: (Ord a) => [a] -> a
maximumRec [] = error "Cannot call maximumRec on empty list"
maximumRec [x] = x
maximumRec (x:xs') = x `max` maximumRec xs'	


--Note: Num is not a subclass of Ord. 
--That means that what constitutes for a number doesn't really have to adhere to an ordering. 
--So that's why we have to specify both the Num and Ord class constraints when doing addition or subtraction and also comparison.

replicate' :: (Num a , Ord a) => a -> i -> [i]
replicate' rep i 
		| rep <= 0 = []
		| otherwise = i:replicate' (rep-1) i	


take' :: (Num a , Ord a) => a -> [i] -> [i]
take' rep xs
	| rep <= 0 = []
	| otherwise = case xs of
							[] -> []
							(x:xs') ->  x:take' (rep-1) xs'

take'' :: (Num a , Ord a) => a -> [i] -> [i]
take'' rep _
	| rep <= 0 = []
take'' _ [] = []
take'' n (x:xs') = x:take'' (n-1) xs'

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x


zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a)=> a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) 
	| a == x = True
	| otherwise = elem' a xs	

quicksort :: (Ord a) => [a] -> [a]	
quicksort [] = []
quicksort (x:xs) =
	let
		smallersorted = quicksort [a | a <- xs , a <= x]
		biggersorted = quicksort [a | a <- xs , a > x]
	in
		smallersorted ++[x]++biggersorted


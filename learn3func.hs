lucky :: (Integral a) => a -> String
lucky 7 = "Lucky Number Seven"
lucky x = " Out of luck"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (x1,y1) (x2,y2) = (x1+x2 , y1+y2)

head' :: [a] -> a
head' [] = error "Cant call head on empty list."
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "List is empty."
tell (x :[]) = "One element list " ++ show x
tell (x : y : []) = "Two element list" ++ show x ++ " and " ++ show y
tell (x : y : _) =" Too many elements in the list"

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x : xs) = x + sum' xs

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
		| bmi <= 18.5 = "Underweight"
		| bmi <= 25.0 ="Normal"
		| bmi <= 30.0 = "Fat"
		| otherwise = "Whale"

max' :: (Ord a) => a -> a -> a
max' x y
	| x >= y = x
	| otherwise = y

mycompare :: (Ord a) => a -> a -> Ordering
mycompare x y
	| x > y = GT
	| x == y = EQ
	| otherwise = LT	


initials :: String -> String -> String
initials firstName lastName = [f] ++ "."
	where 
		(f:_) = firstName
		(l:_) = lastName

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
	let 
		sideArea = 2 * pi * r * h
		topArea = pi * r^2
	in sideArea + 2 * topArea

describeList :: [a]	-> String
describeList xs = "The List is " ++ case xs of 
											[] -> "Empty"
											[x] -> "Single Element List"
											xs -> "a longer list"


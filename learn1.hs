doubleme x = x + x
doubleus x y = doubleme x + doubleme y
doubleSmallNumber' x = (if x > 100
						then x
						else doubleme x ) + 1
length' xs = sum [1 | _ <- xs]

removeNonUpperCase str = [ c | c <- str , elem c ['A'..'Z']]
data Thing  = Shoe
			| Ship
			| SealingWax
			| Cabbage
			| King
	deriving Show


isSmall :: Thing -> Bool
isSmall Ship 	= False
isSmall King	= False
isSmall _		= True


data FailableDouble = Failure
					| OK Double
	deriving Show

data Name = Name String 
	deriving Show

data Age = Age Int

-- name, age, favorite thing
data Person = Person Name Int Thing
	deriving Show


brent :: Person
brent = Person (Name "Brent") 31 SealingWax

stan :: Person
stan = Person (Name "Stan") 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _) = a

getName :: Person -> String
getName (Person (Name x) _ _ ) = x


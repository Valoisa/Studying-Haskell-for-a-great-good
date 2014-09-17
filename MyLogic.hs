module MyLogic
( Logic3 (..)
, not3
, and3
, or3
) where 	
	data Logic3 = T|F|U	deriving (Eq, Show)
	not3 :: Logic3 -> Logic3
	not3 b
		| b == T	= U
		| b == U	= F
		| b == F	= T		
	
	and3 :: Logic3 -> Logic3 -> Logic3
	and3 x y
		| (x == T) && (y== T)	= T
		| (x == T) && (y== U)	= U
		| (x == T) && (y== F)	= F
	
		| (x == U) && (y == T)	= U
		| (x == U) && (y == U)	= U
		| (x == U) && (y == F)	= F
	
		| (x == F) && (y == T)	= F
		| (x == F) && (y == T)	= F
		| (x == F) && (y == T)	= F
	
	or3 :: Logic3 -> Logic3 -> Logic3
		| (x == T) || (y== T)	= T
		| (x == T) || (y== U)	= T
		| (x == T) || (y== F)	= T
	
		| (x == U) || (y == T)	= T
		| (x == U) || (y == U)	= U
		| (x == U) || (y == F)	= U
	
		| (x == F) || (y == T)	= T
		| (x == F) || (y == T)	= U
		| (x == F) || (y == T)	= F
module MyLogic
( Logic3 (..)
, not3
, and3
, or3
) where 	
	data Logic3 = T|F|U	deriving (Eq, Show)
	not3 :: Logic3 -> Logic3
	not3 T = U
	not3 U = F
	not3 F = T
		
	
	and3 :: Logic3 -> Logic3 -> Logic3
		T `and3` T = T
		T `and3` U = U
		T `and3` F = F
	
		U 'and3' T = U
		U 'and3' U = U
		U 'and3' F = F
	
		F 'and3' T = F
		F 'and3' U = F
		F 'and3' U = F
	
	or3 :: Logic3 -> Logic3 -> Logic3
		T 'or3' T = T
		T 'or3' U = T
		T 'or3' F = T
		
		U 'or3' T = T
		U 'or3' U = U
		U 'or3' F = U
		
		F 'or3' T = T
		F 'or3' U = U
		F 'or3' F = F 
	
	

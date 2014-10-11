module MyLogic
(	Logic3 (..)
,	not3
,	and3
,	or3
,	no4th
)
where

data Logic3 = F | U | T

instance Eq Logic3 where
	F == F = True
	U == U = True
	T == T = True
	_ == _ = False

instance Show Logic3 where
	show F = "False"
	show U = "Unknown"
	show T = "True" 

instance Ord Logic3 where
	F < U = True
	U < T = True

not3 :: Logic3 -> Logic3
not3 T = U
not3 U = F
not3 F = T

and3 :: Logic3 -> Logic3 -> Logic3
a `and3` b = min a b

or3 :: Logic3 -> Logic3 -> Logic3
a `or3` b = max a b 

no4th :: Logic3 -> Logic3
no4th a = a `and3` (not3 a) `and3` (not3 (not3 a))

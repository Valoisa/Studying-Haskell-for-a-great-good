module MyLogic
(	Logic3 (..)
,	not3
,	and3
,	or3
,	no4th
)
where

data Logic3 = F | U | T deriving (Eq, Show, Ord)

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

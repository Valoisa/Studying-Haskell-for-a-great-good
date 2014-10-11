module Drunk 
( Suit(..)
, Value(..)
, Card(..)
{-, sameSuit
, beats
, move
, num_of_moves
, read_cards_from_file
, deal_cards
-}
)
where

data Suit = C | D | H | S --deriving (Read)
instance Eq Suit where
	C == C = True
	D == D = True
	H == H = True
	S == S = True
	_ == _ = False

instance Show Suit where
	 show C = "Clubs"
	 show D = "Diamonds"
	 show H = "Hearts"
	 show S = "Spades"

{-instance Read Suit where -- Как?
 	read "Clubs" = C
	read "Diamonds" = D
	read "Hearts" = H
	read "Spades" = S
	read "C" = C
	read "D" = D
	read "H" = H
	read "S" = S -}


data Value = C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | C10 | J | Q | K | A deriving (Ord, Eq, Read)

instance Show Value where
 	show C2 = "2"
 	show C3 = "3"
 	show C4 = "4"
 	show C5 = "5"
 	show C6 = "6"
 	show C7 = "7"
 	show C8 = "8"
 	show C9 = "9"
 	show C10 = "10"
 	show J = "Jack"
 	show Q = "Queen"
 	show K = "King"
 	show A = "Ace"



data Card = Card (Value, Suit) | Jocker -- deriving (Ord)

instance Ord Card where
	Card (_, _) < Jocker		= True
	Card (C2, _) < Card (C3, _)	= True
	Card (C3, _) < Card (C4, _)	= True
	Card (C4, _) < Card (C5, _)	= True
	Card (C5, _) < Card (C6, _)	= True
	Card (C6, _) < Card (C7, _)	= True
	Card (C7, _) < Card (C8, _)	= True
	Card (C8, _) < Card (C9, _)	= True
	Card (C9, _) < Card (C10, _)	= True
	Card (C10, _) < Card (J, _)		= True
	Card (J, _) < Card (Q, _)		= True
	Card (Q, _) < Card (K, _)		= True
	Card (K, _) < Card (A, _)		= True
-}
type Deck = [Card]

{-sameSuit :: Card -> Card -> Bool
sameSuit Card (_, _) Jocker = False
sameSuit Card (a, b) Card (x, y) = (b == y)

beats :: Card -> Card -> Bool
beats Jocker Card (_, _) 		= False
beats Card (_, _) Jocker		= False
beats Card (a, b) Card (x, y)	= (a > x)

move :: (Deck, Deck) -> (Deck, Deck)
move (x:xs, y:ys) = move_plus (x:xs, y:ys) [] 
    where 
    move_plus (x:xs, y:ys) zs
		|beats x y == True    = (xs ++ [y] ++ [x] ++ zs, ys)  
		|beats y x == True    = (xs, ys ++ [x] ++ [y] ++ zs)
		|otherwise            = move_plus (xs, ys) (zs ++ [x] ++ [y])
    
num_of_moves :: (Deck, Deck) -> Int
--num_of_moves =
num_of_moves p@(xs, ys)
    | (move p == ([], ys)) || (move p == (xs, []))    = 1
    | otherwise                                   = 1 + num_of_moves (move p)
        --where 
        --pair = move (xs, ys)-}

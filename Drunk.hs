module Drunk 
( Suit(..)
, Value(..)
, Card(..)
, sameSuit
, beats
, move
, num_of_moves
{-, read_cards_from_file
, deal_cards -}
)
where

data Suit = Clubs|Diamonds|Hearts|Spades deriving (Eq, Show, Read)
data Value = Six|Seven|Eight|Nine|Ten|Jack|Queen|King|Ace deriving (Ord, Eq, Show, Read) 
-- Как сделать джокера?
-- Можно ли заменить слова цифрами?
type Card = (Value, Suit)
type Deck = [Card]

sameSuit :: Card -> Card -> Bool
sameSuit (a, b) (x, y) = (b == y)

beats :: Card -> Card -> Bool
beats (a, b) (x, y) = (a > x)

move :: (Deck, Deck) -> (Deck, Deck)
move (x:xs, y:ys) = move_plus (x:xs, y:ys) [] -- Почему не работает?
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
        --pair = move (xs, ys)


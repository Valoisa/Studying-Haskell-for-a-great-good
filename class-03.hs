import Data.List
-- 1. Простейшие задачи на применение функций map и filter.
-- 1) Дан список целых чисел. Увеличить все его элементы в два раза.
double_el :: (Num a) => [a] -> [a]
double_el [] = []
double_el (x:xs) = map x2 (x:xs)
	where
		x2 a = a*2

-- 2) Дан список целых чисел. Увеличить все его элементы с четными значениями
-- в два раза.
double_even :: (Integral a) => [a] -> [a]
double_even [] = []
double_even (x:xs) = map x2even (x:xs)
	where
		x2even x
			| x `mod` 2 == 0	= x*2
			| otherwise			= x

-- 3) Дан список целых чисел. Обнулить все его элементы с нечетными значениями.
set_odd_to_zero :: (Integral a) => [a] -> [a]
set_odd_to_zero [] = []
set_odd_to_zero (x:xs) = map set2zero (x:xs)
	where
		set2zero x
			| x `mod` 2 /= 0	= 0
			| otherwise = x

-- 4) Дан список целых чисел. Удалить из него элементы, большие заданного числа k.
del_gt_k :: (Num a, Ord a) => a -> [a] -> [a]
del_gt_k k xs = filter gt_k xs
	where 
		gt_k x
			| x <= k 	= True
			| otherwise	= False

-- 5) Дан список целых чисел, отфильтровать его,
-- оставив в списке только отрицательные числа.
leave_neg :: (Num a, Ord a) => [a] -> [a]
leave_neg xs = filter neg xs
	where
		neg x
			| x < 0		= True
			| otherwise	= False

-- 6) Дан список целых чисел. Удалить из него все положительные чётные числа.
del_pos_even :: (Integral a, Ord a) => [a] -> [a]	
del_pos_even xs = filter notpos_odd xs
	where
		notpos_odd x
			| (x <= 0) && (x `mod` 2 /= 0)	= True
			| otherwise						= False					

-- 7) Дан список координат точек на плоскости (пар вещественных чисел). 
-- Отфильтровать список так, чтобы в нём остались точки из заданной
-- координатной четверти.			
belongs_quadrant :: (Num a, Ord a) => Int -> [(a, a)] -> [(a, a)]
belongs_quadrant 1 xs = filter belongs_1 xs
	where
		belongs_1 (x, y) = if (x > 0) && (y > 0) then True else False
belongs_quadrant 2 xs = filter belongs_2 xs
	where
		belongs_2 (x, y) = if (x < 0) && (y > 0) then True else False
belongs_quadrant 3 xs = filter belongs_3 xs
	where
		belongs_3 (x, y) = if (x < 0) && (y < 0) then True else False
belongs_quadrant 4 xs = filter belongs_4 xs	
	where
		belongs_4 (x, y) = if (x > 0) && (y < 0) then True else False

-- 8) Дан список декартовых координат точек на плоскости (пар вещественных чисел).
-- Преобразовать декартовы координаты в полярные. 		
cartesian2polar :: (Floating a, Ord a) => [(a, a)] -> [(a, a)]
cartesian2polar xs = map convert xs
	where
		convert (x, y)
			| (x > 0) && (y >= 0)	= (r, atan (x / y))
			| (x > 0) && (y < 0)	= (r, atan (x / y) + 2 * pi)
			| (x == 0) && (y > 0)	= (r, pi / 2)
			| (x == 0) && (y < 0)	= (r, 3 * pi / 2)
			| otherwise				= (r, 0)
				where
					r = sqrt ((x * x) + (y * y))

-- 9) Дан список слов. Преобразовать все слова к верхнему регистру.
upper_case :: [[Char]] -> [[Char]]
upper_case xs = undefined

-- 10) Дан список слов. Извлечь из него подсписок слов заданной длины.
required_length :: Int -> [[Char]] -> [[Char]]
required_length k xs = filter lenght_k xs
	where
		lenght_k xs
			| (length xs) == k 	= True
			| otherwise			= False
			
-- 2. Формирование числовых последовательностей (iterate).
-- 1) Список натуральных чисел, начиная с 0.
natural :: (Integral a) => [a]
natural = iterate inc 0 	
	where
		inc x = x + 1				

-- 2) Список чётных чисел.
even_numbers :: (Integral a) => [a]
even_numbers = iterate add_2 0
	where
		add_2 x = x + 2 		

-- 3) Список элементов последовательности: a0=1, an=(1+an-1)/2.
my_sequence :: (Fractional a) => [a]
my_sequence = iterate regul 1
	where
		regul x = (1 + x)/2

-- 4) Список символов английского алфавита.
alphabet :: [Char]
alphabet = ['a' .. 'z']	

-- 5) Список строк, представляющих n-значные двоичные числа.
binary_numbers :: Int -> [[Char]]
binary_numbers n = iterate inc_bin_num (zeros n)
	where
		zeros n
			| n > 1		= 0 : zeros (n-1)
			| n == 1	= 0
		inc_bin_num xs
			| (last xs) == 0	= (init xs) : 1
			| (last xs) == 1	= (inc_bin_num (init xs)) : 0

-- 3. Группировка списков.
-- 1) Дан список символов. Сгруппировать подряд идущие символы по принципу:
-- цифры — не цифры — цифры — не цифры — …
group_numbers :: [Char] -> [[Char]]
group_numbers xs = groupBy num_or_not xs -- Беда!
	where
		num_or_not x y
			| ('0' <= x && x <= '9') && ('0' <= y && y <= '9') 	= True 
			| ('0' > x && x > '9') && ('0' > y && y > '9')		= True
			| otherwise 										= False

-- 2) Дан список пар вещественных чисел (координат точек на плоскости).
-- Сгруппировать подряд идущие координаты точек, лежащие в одной
-- координатной четверти.
same_quadrant = undefined

-- 3) Дан список и ненулевое натуральное число n.
-- Разбить список на подсписки длиной n каждый.
-- Последний подсписок может содержать менее n элементов.
group_n_elements = undefined

-- 4) Дан список и ненулевые натуральные числа n и m. Разбить список на перекрывающиеся подсписки
-- длиной n элементов со сдвигом относительно предыдущего подсписка на m элементов.
-- Например, если n=4, m=2, то список [1,2,3,4,5,6,7,8,9,10] должен быть преобразован
-- в [[1,2,3,4],[3,4,5,6],[5,6,7,8],[7,8,9,10],[9,10]].

-- 5) Дан список. Определить длину самого длинного подсписка,
-- содержащего подряд идущие одинаковые элементы.


import MyLogic

import Control.Exception.Base (assert)

test1 = [
	T `and3` T == T,
	T `and3` U == U,
	T `and3` F == F

	U `and3` T == U,
	U `and3` U == U,
	U `and3` F == U]

test7 = F `and3` T
test8 = F `and3` U
test9 = F `and3` F

test10 = T `or3` T
test11 = T `or3` U
test12 = T `or3` F

test13 = U `or3` T
test14 = U `or3` U
test15 = U `or3` F

test16 = F `or3` T
test17 = F `or3` U
test18 = F `or3` F

test19 = no4th T
test20 = no4th U
test21 = no4th F

doTest = map (flip assert ()) test1

main = print doTest

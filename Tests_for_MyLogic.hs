import MyLogic

import Test.HUnit

test1 = [
	(T, T `and3` T),
	(U, T `and3` U),
	(F, T `and3` F)
{-
	U `and3` T == U,
	U `and3` U == U,
	U `and3` F == U,

	F `and3` T == F,
	F `and3` U == F,
	F `and3` F == F
-}
	]

test2 = [
	T `or3` T == T,
	T `or3` U == T,
	T `or3` F == T,

	U `or3` T == T,
	U `or3` U == U,
	T `or3` F == T,

	F `or3` T == T,
	F `or3` U == U,
	F `or3` F == F]


test19 = [
	no4th T == F,
	no4th U == F,
	no4th F == F]

{-
doTest = map (flip assert ()) test1
main = print doTest

doTest = map (flip assert ()) test2
main = print doTest

doTest = map (flip assert ()) test3
main = print doTest
-}

testAssertEqual s a b = TestCase $ assertEqual s a b 

main = runTestTT $ TestList $ map (uncurry $ testAssertEqual "") test1

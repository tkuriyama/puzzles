
Enumerate all ways of inserting + and x as operations into the list [1..9] so as to make 100.

**Initial solution: solver1**

	> solver1 100 [1..9] |> display;;
	100 = 1 x 2 x 3 + 4 + 5 + 6 + 7 + 8 x 9
	100 = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 x 9
	100 = 1 x 2 x 3 x 4 + 5 + 6 + 7 x 8 + 9
	100 = 12 + 3 x 4 + 5 + 6 + 7 x 8 + 9
	100 = 1 + 2 x 3 + 4 + 5 + 67 + 8 + 9
	100 = 1 x 2 + 34 + 5 + 6 x 7 + 8 + 9
	100 = 12 + 34 + 5 x 6 + 7 + 8 + 9
	Real: 00:00:00.013, CPU: 00:00:00.013, GC gen0: 1, gen1: 0

	> expressions [1..9] |> List.length;;
	val it : int = 6561

**Optimized solution: solver2**

	> let pi = [3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5; 8; 9; 7];;

	> solver1 100 pi |> display;;
	100 = 3 + 1 x 4 + 1 x 5 + 9 + 2 + 6 + 5 x 3 + 5 x 8 + 9 + 7
	100 = 3 x 1 + 4 + 1 x 5 + 9 + 2 + 6 + 5 x 3 + 5 x 8 + 9 + 7
	100 = 3 + 1 x 4 x 1 + 5 + 9 + 2 + 6 + 5 x 3 + 5 x 8 + 9 + 7
	100 = 3 x 1 + 4 x 1 + 5 + 9 + 2 + 6 + 5 x 3 + 5 x 8 + 9 + 7
	100 = 3 + 1 x 4 + 1 x 5 + 9 x 2 + 6 + 5 + 3 + 5 x 8 + 9 + 7
	100 = 3 x 1 + 4 + 1 x 5 + 9 x 2 + 6 + 5 + 3 + 5 x 8 + 9 + 7
	100 = 3 + 1 x 4 x 1 + 5 + 9 x 2 + 6 + 5 + 3 + 5 x 8 + 9 + 7
	100 = 3 x 1 + 4 x 1 + 5 + 9 x 2 + 6 + 5 + 3 + 5 x 8 + 9 + 7
	100 = 3 + 1 x 4 + 1 x 5 + 9 x 2 + 6 + 5 + 35 + 8 + 9 + 7
	100 = 3 x 1 + 4 + 1 x 5 + 9 x 2 + 6 + 5 + 35 + 8 + 9 + 7
	100 = 3 + 1 x 4 x 1 + 5 + 9 x 2 + 6 + 5 + 35 + 8 + 9 + 7
	100 = 3 x 1 + 4 x 1 + 5 + 9 x 2 + 6 + 5 + 35 + 8 + 9 + 7
	100 = 3 + 1 + 4 + 1 x 5 + 9 x 2 + 6 x 5 + 3 x 5 + 8 + 9 + 7
	100 = 3 + 1 + 4 x 1 + 5 + 9 x 2 + 6 x 5 + 3 x 5 + 8 + 9 + 7
	100 = 3 + 1 x 4 + 1 + 5 + 9 x 2 + 6 x 5 + 3 x 5 + 8 + 9 + 7
	100 = 3 x 1 + 4 + 1 + 5 + 9 x 2 + 6 x 5 + 3 x 5 + 8 + 9 + 7
	100 = 3 + 14 + 15 + 9 x 2 + 6 + 5 + 3 x 5 + 8 + 9 + 7
	100 = 3 + 14 + 15 + 9 x 2 + 6 + 5 x 3 + 5 + 8 + 9 + 7
	100 = 3 x 1 x 4 + 15 + 9 + 2 + 6 x 5 + 3 + 5 + 8 + 9 + 7
	Real: 00:00:03.422, CPU: 00:00:03.435, GC gen0: 158, gen1: 1

	> solver2 100 pi |> display;;
	100 = 3 + 1 x 4 + 1 x 5 + 9 + 2 + 6 + 5 x 3 + 5 x 8 + 9 + 7
	100 = 3 x 1 + 4 + 1 x 5 + 9 + 2 + 6 + 5 x 3 + 5 x 8 + 9 + 7
	100 = 3 + 1 x 4 x 1 + 5 + 9 + 2 + 6 + 5 x 3 + 5 x 8 + 9 + 7
	100 = 3 x 1 + 4 x 1 + 5 + 9 + 2 + 6 + 5 x 3 + 5 x 8 + 9 + 7
	100 = 3 + 1 x 4 + 1 x 5 + 9 x 2 + 6 + 5 + 3 + 5 x 8 + 9 + 7
	100 = 3 x 1 + 4 + 1 x 5 + 9 x 2 + 6 + 5 + 3 + 5 x 8 + 9 + 7
	100 = 3 + 1 x 4 x 1 + 5 + 9 x 2 + 6 + 5 + 3 + 5 x 8 + 9 + 7
	100 = 3 x 1 + 4 x 1 + 5 + 9 x 2 + 6 + 5 + 3 + 5 x 8 + 9 + 7
	100 = 3 + 1 x 4 + 1 x 5 + 9 x 2 + 6 + 5 + 35 + 8 + 9 + 7
	100 = 3 x 1 + 4 + 1 x 5 + 9 x 2 + 6 + 5 + 35 + 8 + 9 + 7
	100 = 3 + 1 x 4 x 1 + 5 + 9 x 2 + 6 + 5 + 35 + 8 + 9 + 7
	100 = 3 x 1 + 4 x 1 + 5 + 9 x 2 + 6 + 5 + 35 + 8 + 9 + 7
	100 = 3 + 1 + 4 + 1 x 5 + 9 x 2 + 6 x 5 + 3 x 5 + 8 + 9 + 7
	100 = 3 + 1 + 4 x 1 + 5 + 9 x 2 + 6 x 5 + 3 x 5 + 8 + 9 + 7
	100 = 3 + 1 x 4 + 1 + 5 + 9 x 2 + 6 x 5 + 3 x 5 + 8 + 9 + 7
	100 = 3 x 1 + 4 + 1 + 5 + 9 x 2 + 6 x 5 + 3 x 5 + 8 + 9 + 7
	100 = 3 + 14 + 15 + 9 x 2 + 6 + 5 + 3 x 5 + 8 + 9 + 7
	100 = 3 + 14 + 15 + 9 x 2 + 6 + 5 x 3 + 5 + 8 + 9 + 7
	100 = 3 x 1 x 4 + 15 + 9 + 2 + 6 x 5 + 3 + 5 + 8 + 9 + 7
	Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0

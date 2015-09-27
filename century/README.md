
Enumerate all ways of inserting + and x as operations into the list [1..9] so as to make 100.

**Initial solution: solver1**

	> solver1 [1..9] |> display;;
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

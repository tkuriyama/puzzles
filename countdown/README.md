
From the French TV show *Des chiffres et des lettres* ([wiki](https://en.wikipedia.org/wiki/Des_chiffres_et_des_lettres), segment *Le compte est bon*): use some numbers from a list of numbers to construct an arithmetic expression that evaluates to a value as close to the target as possible.

**Initial solution: countdown1**

    > display(countdown1 831 [1;3;7;10;25;50]);;

    (7+((1+10)*(25+50))) = 832
    Real: 00:00:26.163, CPU: 00:00:28.788, GC gen0: 314, gen1: 3

**Optimized solution: countdown2**

	> display(countdown2 831 [1;3;7;10;25;50]);;

	(7+((1+10)*(25+50))) = 832
	Real: 00:00:00.445>, CPU: 00:00:00.448, GC gen0: 10, gen1: 0
	val it : unit = ()


The Burrows-Wheeler transform (and untransform).

**bwt.fsx**

Naive implementation.
 
    > input;;
    val it : char list = ['y'; 'o'; 'k'; 'o'; 'h'; 'a'; 'm'; 'a']

    > transform input;;
    val it : int * char list = (7, ['h'; 'm'; 'o'; 'o'; 'a'; 'k'; 'y'; 'a'])

    > transform input |> untransform;;
    val it : char list = ['y'; 'o'; 'k'; 'o'; 'h'; 'a'; 'm'; 'a']

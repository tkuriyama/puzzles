### Longest Subpalindrome (SP) in Linear Time ###

Implements Manacher's algorithm to find the longest palindromic substring,
or subpalindrome (SP). For a full explanation of the algorithm, see:

[http://tarokuriyama.com/projects/palindrome2.php](http://tarokuriyama.com/projects/palindrome2.php)
   
<hr>
    
##### Usage Notes #####

* The algorithm is case-insensitive as all text is converted to lower case first
* If multiple SPs exist of the same length, the first one is returned 
* Single characters are considered SPs
* No special treatment is accorded to whitespace

The script can be called from the command line, where the sole argument is the string in which to search.

    $ python palindrome.py xyz
    x

    $ python palindrome.py abcracecarxyz
    racecar


##### Tolstoy Test #####

The test script calls the algorithm against the Gutenberg text of Leo Tolstoy's *War and Peace*, in which the longest SP comes from the line "Pierre saw that Pla*ton did not* want to understand what the Frenchman was saying, and he looked on without interfering."

    $ python test.py
    ton did not
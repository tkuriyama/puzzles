#! /bin/zsh

function run_test {
    cat findwords.c | ./findwords_comp $2;
    if [ $? -eq 0 ] && [ $1 -eq 0 ]; then
	echo "\nExpected pass, got pass: $2"
    elif [ $? -eq 0 ] && [ $1 -eq 1 ]; then
	echo "\n>>> Expected pass, got fail: $2"
    elif [ $? -eq 1 ] && [ $1 -eq 1 ]; then
	echo "\nExpected fail, got fail: $2"	
    else
	echo "\n>>> Expecteded fail, but got pass: $2" >&2
    fi
}

setopt shwordsplit

run_test 1 "whilst"
run_test 0 "while"
run_test 0 "while if"
run_test 1 "while if nope"

echo "\n"

unset shwordspit

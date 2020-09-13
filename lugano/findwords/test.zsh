#! /bin/zsh

function run_test {
    cat findwords.c | ./findwords_comp $2;
    if [ $? -eq 0 ] && [ $1 -eq 0 ]; then
	echo "\nOK: Expected pass, got pass: $2"
    elif [ $? -eq 0 ] && [ $1 -eq 1 ]; then
	echo "\n>>> Expected fail, got pass: $2"
    elif [ $? -eq 1 ] && [ $1 -eq 1 ]; then
	echo "\nOK: Expected fail, got fail: $2"	
    else
	echo "\n>>> Expecteded pass, but got fail: $2" >&2
    fi
}

setopt shwordsplit

run_test 1 "whilst"
run_test 0 "while"
run_test 0 "while while while while while"
run_test 0 "while if"
run_test 1 "while if nope"

echo "\n"

unset shwordspit

#!/bin/bash

binary=./imageCompressor

function test_exit_value() {
    out=$($binary $1)
    exit=$?

    if [[ $exit != $2 ]]; then
        echo >&2 -e "\x1b[31;01m[X]\x1b[0m $binary $1"
        echo -e "got return: $exit"
        echo -e "exp return: $2"
    else
        echo >&2 -e "\x1b[32;01m[✔]\x1b[0m $binary $1"
    fi
}

echo >&2 -e "\033[1mTest args (Error expected):\033[0m"

test_exit_value "" 84
test_exit_value "zertyu" 84
test_exit_value "-h -h" 84

test_exit_value "-l 1 -f pixels/subjectExemple.in" 84
test_exit_value "-n -l 1 -f pixels/subjectExemple.in" 84
test_exit_value "-n 2 -f pixels/subjectExemple.in" 84
test_exit_value "-n 2 -l -f pixels/subjectExemple.in" 84
test_exit_value "-n 2 -l 1" 84
test_exit_value "-n 2a -l 0.8 -f pixels/subjectExemple.in" 84
test_exit_value "-n 2 -l 1 -f" 84
test_exit_value "-n 2 -l 0.8a -f pixels/subjectExemple.in" 84
test_exit_value "-n 0 -l 0.8 -f pixels/subjectExemple.in" 84

echo >&2 -e "\n\033[1mTest args (Success expected):\033[0m"

test_exit_value "-n 2 -l 1 -f pixels/subjectExemple.in" 0
test_exit_value "-n 2 -l 0.8 -f pixels/subjectExemple.in" 0

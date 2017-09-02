#!/bin/bash

# Why not rebar?  For now I'm keeping things dirt simple.

case $1 in
    clean)
        rm *.beam;
        ;;
    *)
        erlc fibonacci.erl &&
            erlc fibonacci_tests.erl &&
            erl -noshell -s eunit test fibonacci_tests -s init stop;
        ;;
esac

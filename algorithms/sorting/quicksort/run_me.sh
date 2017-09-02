#!/bin/bash

# Why not rebar?  For now I'm keeping things dirt simple.

case $1 in
    clean)
        rm *.beam;
        ;;
    *)
        erlc quicksort.erl &&
            erlc quicksort_tests.erl &&
            erl -noshell -s eunit test quicksort_tests -s init stop;
        ;;
esac

#!/bin/bash

# Why not rebar?  For now I'm keeping things dirt simple.

case $1 in
    clean)
        rm *.beam;
        ;;
    *)
        erlc bubblesort.erl &&
            erlc bubblesort_tests.erl &&
            erl -noshell -s eunit test bubblesort_tests -s init stop;
        ;;
esac

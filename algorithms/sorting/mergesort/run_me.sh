#!/bin/bash

# Why not rebar?  For now I'm keeping things dirt simple.

case $1 in
    clean)
        rm *.beam;
        ;;
    *)
        erlc mergesort.erl &&
            erlc mergesort_tests.erl &&
            erl -noshell -s eunit test mergesort_tests -s init stop;
        ;;
esac

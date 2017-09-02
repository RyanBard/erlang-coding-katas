#!/bin/bash

# Why not rebar?  For now I'm keeping things dirt simple.

case $1 in
    clean)
        rm *.beam;
        ;;
    *)
        erlc factorial.erl &&
            erlc factorial_tests.erl &&
            erl -noshell -s eunit test factorial_tests -s init stop;
        ;;
esac

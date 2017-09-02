#!/bin/bash

# Why not rebar?  For now I'm keeping things dirt simple.

case $1 in
    clean)
        rm *.beam;
        ;;
    *)
        erlc max_sub_array_sum.erl &&
            erlc max_sub_array_sum_tests.erl &&
            erl -noshell -s eunit test max_sub_array_sum_tests -s init stop;
        ;;
esac

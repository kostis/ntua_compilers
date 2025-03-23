#!/usr/bin/env bash

# Generates N Mini-Basic programs in the 'progs' dir.
#   (N is a command-line argument.)
#
# To use it, you need to first declare the dirs
# where Erlang/OTP and PropEr are in your system.

readonly ERL=/home/kostis/HiPE/otp/bin/erl
export ERL_LIBS=/home/kostis/HiPE/proper

${ERL}c -pa ${ERL_LIBS}/ebin +debug_info *.erl

mkdir -p progs
for i in $(seq 1 $1)
do
  $ERL -noshell -noinput -s minibasic_pp p -s erlang halt > progs/p$i.mba
done

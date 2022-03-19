#! /bin/bash

# Generates N Llama programs in the 'progs' dir.
#   (N is a command-line argument.)
#
# To use it, you need to first declare the dirs
# where Erlang/OTP and PropEr are in your system.

ERL=/home/kostis/HiPE/otp/bin/erl
export ERL_LIBS=/home/kostis/HiPE/proper

mkdir -p progs
for i in $(seq 1 $1)
do
  $ERL -noshell -noinput -s llama_pp p -s erlang halt > progs/p$i.lla
done

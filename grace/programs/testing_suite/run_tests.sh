#!/usr/bin/env bash

# Replace the following with the directory you wish to test
for FILE in testing_suite/parser/*;
do 
    f="$(basename -- $FILE)"
    echo "Running $f"
    if !([[ $f == err* ]]); then
        # Testcase should not produce an error
        # Make sure you specify the correct relative/absolute path to the grace executable as well as using the correct way to 
        # pass the grace source code (eg redirection from stdin or filename as argument)
        # Same on the elif line below
        if (! ./gracec $FILE) &> /dev/null
            then echo "$f failed: Error not expected but happened"
        fi
    # Testcase should produce an error
    elif (./gracec $FILE) &> /dev/null
        then echo "$f failed: Error expected but did not happen"
    fi
    # else echo "Success"
done

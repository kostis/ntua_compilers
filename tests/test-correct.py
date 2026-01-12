#!/usr/bin/env python3

import argparse
import os
import subprocess

class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'

def print_ok(format):
    print(bcolors.OKGREEN + format + bcolors.ENDC)

def print_fail(format):
    print(bcolors.FAIL + format + bcolors.ENDC)

def extension(lang):
    if lang == "alan":
        return '.alan'
    elif lang == "dana":
        return '.dana'
    elif lang == "grace":
        return '.grc'
    elif lang == "llama":
        return '.lla'
    
def run_test(language, compiler_path, test_dir):
    total = 0
    ok = 0
    failed = 0
    ext = extension(language)
    # Iterate over each file in the test directory
    for file in os.listdir(test_dir):
        if file.endswith(ext):
            total += 1
            basename = file[:-(len(ext))]
            src_file = os.path.join(test_dir, file)
            result_file = os.path.join(test_dir, basename + '.result')

            # Compile the .grc file
            print(f"Compiling {basename}", end=" ... ")
            compile_process = subprocess.run([compiler_path, src_file], text=True, capture_output=True)

            # Check if the compile process had an error
            if compile_process.returncode == 0:
                print_ok("OK")
            else:
                failed += 1
                print_fail("FAILED")
                print(compile_process.stderr)
                continue

            print(f"  Running {basename}", end=" ... ")
            # Check if basename .input file exists
            input_file = os.path.join(test_dir, basename + '.input')
            if os.path.exists(input_file):
                # Run the compiled program with the input file as stdin
                run_process = subprocess.run(['./a.out'], text=True, capture_output=True, input=open(input_file, 'r').read())
            else:
                # Run the compiled program
                run_process = subprocess.run(['./a.out'], text=True, capture_output=True)

            # Check if the run process had an error
            if run_process.returncode == 0:
                print_ok("OK")
            else:
                failed += 1
                print_fail("FAILED")
                print(run_process.stderr)
                continue

            # Compare output to .result file
            with open(result_file, 'r') as f:
                expected_output = f.read()

            if run_process.stdout != expected_output:
                failed += 1
                print_fail(f"Test failed for {src_file}")
                print("Expected:")
                print(expected_output)
                print("Got:")
                print(run_process.stdout)
            else:
                ok += 1
                print_ok(f"Test passed for {src_file}")

    print(bcolors.OKBLUE + f"\nTotal tested: {total} ({ok} correct and {failed} failed)" + bcolors.ENDC)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Tests a Compiler for a Language by running all the tests residing in a Test Directory.'
                                     ' Compiler should be an executable or script that takes as input a program of the language and creates an a.out executable in the directory where this test driver resides.')
    parser.add_argument('language', help='Currently one of: \'alan\', \'grace\' or \'llama\'.')
    parser.add_argument('compiler_path', help='The path to the compiler executable.')
    parser.add_argument('test_dir', help='The directory containing the test programs, .result outputs expected for each program and .input files to be used as stdin for each program if needed.')
    args = parser.parse_args()

    run_test(args.language, args.compiler_path, args.test_dir)

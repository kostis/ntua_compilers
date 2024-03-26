#!/usr/bin/env python3

import argparse
import subprocess
import os

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

def run_test(language, compiler_path, test_dir):
    total = 0
    ok = 0
    failed = 0
    # Iterate over each .grc file in the test directory
    for file in os.listdir(test_dir):
        if file.endswith('.grc'):
            total += 1
            basename = file[:-4]
            grc_file = os.path.join(test_dir, file)
            result_file = os.path.join(test_dir, basename + '.result')

            # Compile the .grc file
            print(f"Compiling {basename}", end=" ... ")
            compile_process = subprocess.run([compiler_path, grc_file], text=True, capture_output=True)

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
                print_fail(f"Test failed for {grc_file}")
                print("Expected:")
                print(expected_output)
                print("Got:")
                print(run_process.stdout)
            else:
                ok += 1
                print_ok(f"Test passed for {grc_file}")

    print(bcolors.OKBLUE + f"\nTotal tested: {total} ({ok} correct and {failed} failed)" + bcolors.ENDC)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Test a compiler. Compiler should be an executable that takes as an input the Grace program and outputs an a.out executable')
    parser.add_argument('compiler_path', help='The path to the compiler executable.')
    parser.add_argument('test_dir', help='The directory containing the test files. The directory contains .grc programs, .result outputs expected for each program and .input files to be used as stdin for each program if needed.')
    args = parser.parse_args()

    run_test('grace', args.compiler_path, args.test_dir)

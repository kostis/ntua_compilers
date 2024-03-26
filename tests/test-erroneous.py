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

            print(f"Compiling {basename}", end=" ... ")
            compile_process = subprocess.run([compiler_path, src_file], text=True, capture_output=True)

            # Check if the compilation returned an error
            if compile_process.returncode != 0:
                ok += 1
                print_ok("Error (OK)")
                # print(compile_process.stderr)
            else:
                failed += 1
                print_fail("FAILED to detect the error")

    print(bcolors.OKBLUE + f"\nTotal tested: {total} ({ok} correct and {failed} failed)" + bcolors.ENDC)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Tests a Compiler for a Language by compiling all test programs in a Directory.'
                                     ' Compiler should be an executable that takes as input a program for the language.')
    parser.add_argument('language', help='Currently one of: \'alan\', \'grace\' or \'llama\'.')
    parser.add_argument('compiler_path', help='The path to the compiler executable.')
    parser.add_argument('test_dir', help='The directory containing the erroneous programs for the language.')
    args = parser.parse_args()

    run_test(args.language, args.compiler_path, args.test_dir)

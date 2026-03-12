#!/usr/bin/env python3

import argparse
import os
import sys
import subprocess

class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'

def colored(text: str, color: str) -> str:
    """Return colored text if terminal supports it"""
    if sys.stdout.isatty():
        return f"{color}{text}{bcolors.ENDC}"
    return text

def print_ok(format):
    print(colored(format, bcolors.OKGREEN))

def print_fail(format):
    print(colored(format, bcolors.FAIL))

def extension(lang):
    if lang == "alan":
        return '.alan'
    elif lang == "dana":
        return '.dana'
    elif lang == "grace":
        return '.grc'
    elif lang == "llama":
        return '.lla'
    elif lang == "minibasic":
        return '.mba'
    elif lang == "pcl":
        return '.pcl'

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
                print_ok("Error (✓ OK)")
                # print(compile_process.stderr)
            else:
                failed += 1
                print_fail("✗ FAILED to detect the error")

    print(colored(f"\nTotal tested: {total} ({ok} correct and {failed} failed)", bcolors.OKBLUE))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Tests a Compiler for a Language by compiling all test programs in a Directory.'
                                     ' Compiler should be an executable that takes as input a program for the language.')
    parser.add_argument('language', help='One of: \'alan\', \'dana\', \'grace\', \'llama\', \'minibasic\' or \'pcl\'.')
    parser.add_argument('compiler', help='The path to the compiler executable or script to invoke it.')
    parser.add_argument('test_dir', help='The directory containing the erroneous programs for the language.')
    args = parser.parse_args()

    run_test(args.language, args.compiler, args.test_dir)

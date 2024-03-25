import argparse
import subprocess
import os

def run_test(compiler_path, test_dir):
    # Iterate over each .grc file in the test directory
    for file in os.listdir(test_dir):
        if file.endswith('.grc'):
            base = file[:-4]
            grc_file = os.path.join(test_dir, file)
            result_file = os.path.join(test_dir, base + '.result')

            # Compile the .grc file
            compile_process = subprocess.run([compiler_path, grc_file], text=True, capture_output=True)

            # Check if the compile process had an error
            if compile_process.returncode != 0:
                print(f"Failed to compile {grc_file}")
                print(compile_process.stderr)
                continue


            # Check if base .input file exists
            input_file = os.path.join(test_dir, base + '.input')
            if os.path.exists(input_file):
                # Run the compiled program with the input file as stdin
                run_process = subprocess.run(['./a.out'], text=True, capture_output=True, input=open(input_file, 'r').read())

            else:
            # Run the compiled program
                run_process = subprocess.run(['./a.out'], text=True, capture_output=True)

            # Check if the run process had an error
            if run_process.returncode != 0:
                print(f"Failed to run {grc_file}")
                print(run_process.stderr)
                continue

            # Compare output to .result file
            with open(result_file, 'r') as f:
                expected_output = f.read()

            if run_process.stdout != expected_output:
                print(f"Test failed for {grc_file}")
                print("Expected:")
                print(expected_output)
                print("Got:")
                print(run_process.stdout)
            else:
                print(f"Test passed for {grc_file}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Test a compiler. Compiler should be an executable that takes as an input the Grace program and outputs an a.out executable')
    parser.add_argument('compiler_path', help='The path to the compiler executable.')
    parser.add_argument('test_dir', help='The directory containing the test files. The directory contains .grc programs, .result outputs expected for each program and .input files to be used as stdin for each program if needed.')
    args = parser.parse_args()

    run_test(args.compiler_path, args.test_dir)

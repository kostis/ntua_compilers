This directory contains an extensive testing suite for the `grace` programming language. It is provided here not only to be used when testing compilers for the `grace` programming language, but also with hope that it will act as inspiration/guideline for future students taking the course.

Tests are organized in the following way:

- `lexical`: testing lexer
- `optimizations`: testing optimization passes
- `parser`: testing parser
- `scope`: testing scopes
- `type_checking`: testing types
- `runtime`: testing code generation
- `runtime/leetcode`: leetcode problems solved in `grace`
- `runtime/unit`: unit tests

The convention used in the naming of tests is that if a filename begins with `err` (e.g. `parser/err_arr_def_expr.grc`) the compilation should not be successful.

A utility script is provided in `run_tests.sh` to automatically run the test suite (make sure you change the directory path at the beginning of a file)
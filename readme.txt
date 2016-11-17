In order to compile and run tests for parser and type checker:

1. Run ./run.sh in the shell in the root folder
2. To run the parser tests, run ./testParser in the shell, optionally write the output into a file
3. To run the type checker tests, run ./testTypeChecker in the shell, optionally write the output into a file

Tips on how to evaluate the tests and what they are testing for -

For parser:

1. The parser will spit out an AST that is not yet enriched with empty locals as long as the program is
correct in terms of grammar, it doesn't matter if the type checking will fail the program along with the file name,
compare the AST against the file name to see the program and check if the program is valid
2. For invalid parsing, the parser tests will print "Failed with %s" reason along with the file name,
open the file with the file name in order to see the invalid program and a comment on why the program is invalid
and thus should be parsed as invalid

For type checker:

1. The type checker will spit out an AST that is enriched with filled locals as long as it doesn't fail,
along with the file name, compare the AST that is spit out against the file name to see the enriching and the
validity of the program
2. For invalid programs, the type checker tests will print "Failed with %s" reason along with the file name,
open the file with the file name in order to see the invalid program and a comment on why the program is invalid
and thus should be type checked as invalid

Some comments:

1. We did not check for Go block returns as we think it does not make sense for there to be returns in concurrency blocks
2. We chose to allow for redeclaration of variables but it might lead to unspecified behaviors that may not be caught
3. The AST pretty printer is written by us and we think it suits our context better

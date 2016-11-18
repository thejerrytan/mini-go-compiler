# Team members:

1. Jerry Tan - A0097689Y
2. Andhieka Putra - A0113672L
3. Yap Jun Hao - A0113694A

# How to Build (Note the following instructions are for mac/linux users)

#### 1. Install ppx deriving

```bash
opam install ppx_deriving
```

#### 2. Build

```bash
./run.sh
```

You may encounter an error for the first time because of residual file
if you previously do a manual build. Just follow the instruction to
resolve this: `./_build/sanitize.sh`.

#### 3. Running and testing

1. For compiling a specific go program saved on disk:

```bash
./calc.native "filename relative to project root"
```

This can be used for testing specific source files.

Prints AST before and after normalization, Intermediate code and finally list of VM instructions

There is no output for type checking, if intermediate code is printed it is assumed the program type checks.

If compilation fails at any of the stages, the print output for later stages will not be shown.

2. To run the parser tests, run 

```bash
./testParser.native
```
in the shell

3. To run the type checker tests, run 

```bash
./testTypeChecker.native
```
 in the shell

# Notes on the tests

For parser:

1. The parser will spit out an AST that is not yet enriched with empty locals as long as the program is correct in terms of grammar, it doesn't matter if the type checking will fail the program along with the file name, compare the AST against the file name to see the program and check if the program is valid

2. For invalid parsing, the parser tests will print "Failed with reason: %s" reason along with the file name, open the file with the file name in order to see the invalid program and a comment on why the program is invalid and thus should be parsed as invalid

3. Note that we have less tests here as the type checker tests following also uses the parser as a basis and hence has it "tested" as well

For type checker:

1. The type checker will spit out an AST that is enriched with filled locals as long as it doesn't fail, along with the file name and an abbreviated comment on the program in the file, compare the AST that is spit out against the content in the file name to see the enriching and the
validity of the program

2. For invalid programs, the type checker tests will print "Failed with reason: %s" reason along with the file name, open the file with the file name in order to see the invalid program and a comment on why the program is invalid and thus should be type checked as invalid

# Design decisions and project scope:

## Lexer
1. We do not allow uppercase characters.
2. We do not allow _ and punctuation in function names and variables
3. Function names and variable names must start with a lowercase character
4. Recognition of NAME and VAR is being done at this stage

## Parser
1. We did not check for Go block returns as we think it does not make sense for there to be returns in concurrency blocks
2. We would like to forbid redeclaration of variables within same scope, but we did not implement it at the parser stage. It will ultimately fail at code generation stage.
3. The AST pretty printer is written by us and we think it suits our context better
4. Procedure with declared return type must end with a return statement

## Typechecker
1. We collected all locals while threading through the environment

## Normalize
1. We removed all side effects from the AST and replaced them with Skip statements subsequently in later stages
2. We renamed all variables, locals and procedure arguments into distinct names using freshName to ensure proper lookup of variables in later stages

## Intermediate
1. We tried our best to implement code for procedure, function calls and function expressions.

## Codegen
1. We rename distinct variable names to distinct integers corresponding to mem locations which can be used directly in VM code generation.

## VM
1. Arithmetic expressions are working
2. Boolean short circuit evaluations are supported
3. Control-flow statements are supported
4. Function calls, expressions compiles into VM instructions, but have not tested to be working

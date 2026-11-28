


# Assignment 8, CS3250



This repository contains a (template) implementation of CatML, an ML variant used at UVM for CS3250.

## Building this repository with `dune`

This repository can be built with [`dune`](https://github.com/ocaml/dune), a build system for OCaml projects. It helps manage linking when your project is made up of multiple `.ml` files.

### Install `dune`

Check if dune is already installed via the following terminal command:

```
$ dune --version
```

If the command is not found, you can install it via `opam` [(detailed instructions)](https://dune.readthedocs.io/en/latest/howto/install-dune.html).

```
$ opam install dune
$ eval $(opam config env)
$ dune --version
```

### Build the project

With `dune` installed, you can build the project by running `dune build` from the command line.

```
$ dune build
```

You can also access the project modules by running `dune utop`, which will build the project and then open the `utop` toplevel. Different files (such as `src/lang.ml`) correspond to OCaml modules, and data structures (such as the `Var` constructor in the CatML AST) can be accessed via `CatML.Lang.Bool`, where `CatML` is the overall name of the project.

### Structure of the repository

```
- <root>
  - dune-project
  - src
    - dune
    - lang.ml
    - lexer.mll
    - parser.mly
    - main.ml
  - examples
    - example.catml
    - factorial.catml
```

The `dune-project` plaintext file at the top level tells dune how to build your project. The file `src/dune` tells dune the name of the library (`CatML`) and the fact that we have a ocamllex file `lexer.mll` and a menhir parser specification file `parser.mly` in the `src` subdirectory.

```
(library (name CatML))
(menhir (modules parser))
(ocamllex lexer)
```

The files in the `src` directory include:

- `lang.ml`: The main definition of the CatML language, including syntax, evaluation, and type checking
- `lexer.mll`: The specification of the lexer used by ocamllex
- `parser.mly`: The specification of the parser used by menhir
- `main.ml`: Demonstration of how to access the lexer and parser generated from the above files

In the `examples` directory I have included two sample CatML programs. You should write your own additional CatML programs to test your work.

## Other Resources

1. Parsing and lexing generators in OCaml, [OCaml Programming, Correct + Efficient + Beautiful](https://cs3110.github.io/textbook/chapters/interp/parsing.html)
2. Parsing with OCamllex and Menhir, [Real World OCaml](https://dev.realworldocaml.org/parsing-with-ocamllex-and-menhir.html)
3. [Dune build system reference](https://github.com/ocaml/dune)

## How to interact with a CatML program

To run the file, 
call dune install followed by 
catml examples/<yourprojectfile> 



# Reflection Questions

1. If you worked as a group, list all the group members.

Ej Orr, Faris Rawlings

2. Did you use generative AI to assist you in implementing this project? If so, briefly explain how you used it and for which components.

I used generative AI to ensure the environment was set up, some steps of the dune install were giving me issues on my device. I also utilized it to learn how to automatically lint and format Ocaml code. Created a .git/hooks to lint and format on commit. In part 3 of the project I utilized a coding agent to fetch documentation for dune build system to call the program with command line arguments and handle file IO and error rendering, then used it for error diagnosis on file structure issues with dune calling catml as a standalone bin. (bin/dune, bin/catml.ml) 

3. Did you make any design decisions that we should know about?
I decided to treat the fix operator as well as functions as values in the code, I decided to overwrite the existing type_of_expr to utilize the extra credit we are attempting with type inference. Also along with type inference, TYPE counter variables are added in intermediate that get resolved out by the type checker. Type checking is done through the "public" (maybe OOP languages are on to something) facing API type_of_program. 

4. Did you implement any of the extra credit features? Briefly summarize your work.
Yes, I implemented type inference as my extra language feature, for this I had to implement the functions:
fresh, to create a new typing context, 
apply_subst, to apply a substitution for type inference
occurs as a helper for unify which unifies a type variable to its intended type
I then turned type_of_expr to be infer, since now type inference is in the language and wow was it a lot of match statements 
each of the infer then unify steps on the binary and up operators. I regret implementing this and I dislike TypeScript and and python significantly more now. 


5. Any feedback on the structure of this assignment?
I feel like implementing this language helped me understand other programming languages better. Having to think in abstract syntax trees while writing and debugging the type checker/ inference broke up some of the ideas I had about the process of learning a programming language. I also think that this project also cemented a lot of the concepts from class as actual physical code and seeing the exact rules that we have been working on the whiteboard as code was fulfilling I think also the distribution of the amount of effort required between the three parts of the assignment was a bit unbalanced, but that could just have been our implementation decisions that led to that. Lastly, sum types, matching and optional are quite useful language features



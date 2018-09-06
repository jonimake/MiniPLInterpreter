cmake_minimum_required(VERSION 3.9)
project(MiniPLInterpreter)

set(CMAKE_CXX_STANDARD 11)

add_executable(MiniPLInterpreter
        src/lexer/lexeme.rs
        src/lexer/lexeme_iterator.rs
        src/lexer/mod.rs
        src/parser/ast.rs
        src/parser/interpreter.rs
        src/parser/mod.rs
        src/parser/token.rs
        src/parser/token_iterator.rs
        src/parser/token_type.rs
        src/main.rs
        Cargo.lock
        Cargo.toml
        LICENSE
        README.md
        rustfmt.toml
        sample1.txt
        sample2.txt
        sample3.txt
        sample4.txt
        sample5.txt)

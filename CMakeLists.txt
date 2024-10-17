cmake_minimum_required(VERSION 3.29)
project(relational_engine_ocaml)

set(CMAKE_CXX_STANDARD 20)

include_directories($ENV{LIBRESSL_INCLUDE_PATH})
include_directories($ENV{MERKLECPP_INCLUDE_PATH})
include_directories($ENV{CAML_INCLUDE_PATH})

add_library(relational_engine_ocaml STATIC
        lib/merklecpp_stubs.cpp)

target_link_libraries(relational_engine_ocaml
        $ENV{LIBRESSL_LIB_PATH}/libssl.$ENV{OS_LIB_EXTENSION}
        $ENV{LIBRESSL_LIB_PATH}/libcrypto.$ENV{OS_LIB_EXTENSION}
)
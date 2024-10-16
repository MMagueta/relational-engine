#include <iostream>
#include <merklecpp.h>

void cpp_normal_function() {
  std::cout << "HELLO OCAML! :)" << std::endl;
}

extern "C" {
#include <caml/mlvalues.h>
#include <caml/memory.h>
  extern value hello_ocaml(value unit){
    cpp_normal_function();
    return 0;
  }
}

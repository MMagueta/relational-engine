#include <iostream>
#include <merklecpp.h>
#include <openssl/sha.h>
#include <iomanip>

void cpp_normal_function(char* text) {
  std::cout << text << std::endl;
}

const char* internal_compute_hash(void *content) {
  unsigned char hash[SHA256_DIGEST_LENGTH];
  SHA256_CTX sha256;
  SHA256_Init(&sha256);
  SHA256_Update(&sha256, content, sizeof(content));
  SHA256_Final(hash, &sha256);
  std::cout << (char *) content << std::endl;
  std::stringstream ss;
  for (int i = 0; i < SHA256_DIGEST_LENGTH; i++)
    ss << std::hex << std::setw(2) << std::setfill('0') << (int) hash[i];
  std::string computed_hash = ss.str();
  std::cout << "⚙️ COMPUTED HASH: " << ss.str() << std::endl;
  return computed_hash.c_str();
}

extern "C" {
#include <caml/mlvalues.h>
#include <caml/memory.h>
  extern value hello_ocaml(value unit){
    cpp_normal_function((char*)"HELLO OCAML! :)");
    return 0;
  }
  extern value_ptr compute_hash(value_ptr content) {
    const char* hash = internal_compute_hash((void*)content);
    printf("TRASH %s\n", hash);
    return value_ptr(hash);
  }
}

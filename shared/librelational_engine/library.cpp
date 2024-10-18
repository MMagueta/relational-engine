#include <iostream>
#include <merklecpp.h>
#include <openssl/sha.h>
#include <iomanip>
extern "C" {
#include "library.h"
    extern void hello() {
        std::cout << "Hello, World!" << std::endl;
    }
    extern void compute_hash(char* computed_hash, char *content) {

        unsigned char hash[SHA256_DIGEST_LENGTH];
        SHA256_CTX sha256;
        SHA256_Init(&sha256);
        SHA256_Update(&sha256, content, sizeof(content));
        SHA256_Final(hash, &sha256);
        std::stringstream ss;
        for (int i = 0; i < SHA256_DIGEST_LENGTH; i++)
            ss << std::hex << std::setw(2) << std::setfill('0') << static_cast<int>(hash[i]);

        std::memcpy(computed_hash, ss.str().c_str(), HASH_SIZE);
        std::cout << "⚙️ COMPUTED HASH: " << computed_hash << std::endl;
    }

    extern int trash() { return 100; }
}
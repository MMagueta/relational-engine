open Ctypes

module Sha256 = struct
  let compute_hash content =
    let size_with_null_terminator = C.Types.hash_size + 1 in
    let buf = Ctypes.allocate_n char ~count:size_with_null_terminator in
    let () = C.Functions.compute_hash buf (Bytes.to_string content) in
    Ctypes.coerce (ptr char) Ctypes.string buf
end

module Merkle = struct
  let merkle_generate_root hashes =
    let size_with_null_terminator = C.Types.hash_size + 1 in
    let buf = Ctypes.allocate_n char ~count:size_with_null_terminator in
    let len = List.length hashes in
    let hashes = CArray.of_list string hashes in
    C.Functions.merkle_generate_root (CArray.start hashes) len buf;
    Ctypes.coerce (ptr char) Ctypes.string buf
end

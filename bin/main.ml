open Relational_engine

let () =
  begin
    print_endline @@ Disk.Interop.Merkle.merkle_generate_root [Disk.Filesystem._EMPTY_SHA_HASH_; Disk.Filesystem._EMPTY_SHA_HASH_]
  end

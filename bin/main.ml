open Relational_engine

let () =
  begin
    print_endline @@ Interop.Merkle.merkle_generate_root [Disk.Executor._EMPTY_SHA_HASH_; Disk.Executor._EMPTY_SHA_HASH_]
  end

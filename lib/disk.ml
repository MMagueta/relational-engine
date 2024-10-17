module Command = struct
  module StringMap = Map.Make(String)
  module Location = struct
    type t =
      { offset: int64;
        size: int64;
        hash: string }
  end
  module Hashes = struct
    type t = string list
  end
  type command_kind =
    | OPEN
    | WRITE
    | READ
  type t =
    { kind: command_kind;
      timestamp: float;
      key: string;
      filename: string;
      content: string }
  let parse_command (_data: Bytes.t) (_files) = 0

  let writer (files: Hashes.t StringMap.t) (_locations: Location.t StringMap.t) filename (_hash_to_replace) (_content: Bytes.t) =
    match StringMap.find_opt filename files with
    | Some _file_hashes ->
       let _computed_hash = Sha256.compute_hash(_content) in
       print_endline _computed_hash;
       0
    | None ->
       let _computed_hash = Sha256.compute_hash(_content) in
       print_endline _computed_hash;
       0
end

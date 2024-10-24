module Interop = struct
  open Ctypes
  module Sha256 = struct
    let compute_hash content =
      let size_with_null_terminator = C.Types.hash_size + 1 in
      let buf = Ctypes.allocate_n char ~count:size_with_null_terminator in
      let () = C.Functions.compute_hash buf (Bytes.to_string content) in
      Ctypes.coerce (ptr char) Ctypes.string buf
  end
end

module Filesystem = struct
  module StringMap = Map.Make(String)
  module Location = struct
    type t =
      { offset: int64;
        size: int;
        hash: string }
  end
  module Hashes = struct
    type t = string list
  end
  type files = Hashes.t StringMap.t
  type locations = Location.t StringMap.t
  let _EMPTY_SHA_HASH_ = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
  let _STORAGE_PATH_ = ""
  (* Initializes the virtual file or opens it. *)
  let assure_storage () =
    if Sys.is_directory "/tmp/relational-engine"
    then ()
    else Sys.mkdir "/tmp/relational-engine" 0o700
  let init (files: Hashes.t StringMap.t) filename: Hashes.t StringMap.t =
    assure_storage();
    match StringMap.find_opt filename files with
    | None -> StringMap.add filename [_EMPTY_SHA_HASH_] files
    | Some _ -> files
  let write (files: files) (locations: locations) filename ?hash_to_replace (content: Bytes.t) =
    let write_to_disk content =
      let channel = Out_channel.open_bin "/tmp/relational-engine/storage" in
      let offset_written = Out_channel.length channel in
      Out_channel.seek channel offset_written;
      Out_channel.output_bytes channel content;
      Out_channel.flush channel;
      Out_channel.close channel;
      (Bytes.length content, offset_written)
    in
    let computed_hash: string = Interop.Sha256.compute_hash content in
    match StringMap.find_opt computed_hash locations with
    | None ->
        begin match hash_to_replace with
        | None ->
          begin
            (* This instead should store a list of list to keep the history *)
            let files =
              StringMap.update
                filename
                (function Some file_hashes -> Some (computed_hash::file_hashes) | None -> Some [computed_hash])
                files
            in
            let (size, offset) = write_to_disk content in
            let locations = StringMap.add computed_hash ({size; offset; hash = computed_hash}: Location.t) locations in
            (files, locations)
          end
        | Some hash_to_replace ->
          let files = StringMap.update filename (function Some file_hashes -> Some (List.map (fun hash -> if hash = hash_to_replace then computed_hash else hash) file_hashes) | None -> Some [computed_hash]) files in
          let (size, offset) = write_to_disk content in
          let locations = StringMap.add computed_hash ({size; offset; hash = computed_hash}: Location.t) locations in
          (files, locations)
        end
    | Some _ ->
        begin match hash_to_replace with
        | Some hash_to_replace ->
          let files = StringMap.update filename (function Some file_hashes -> Some (List.map (fun hash -> if hash = hash_to_replace then computed_hash else hash) file_hashes) | None -> Some [computed_hash]) files in
          (files, locations)
        | None ->
          (files, locations)
        end
      end
module Command = struct
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

  let commit (files: Filesystem.files) (locations: Filesystem.locations) ({kind;_} as command) =
    match kind with
    | WRITE -> 
      Filesystem.init
    | _ -> failwith ""
  
end

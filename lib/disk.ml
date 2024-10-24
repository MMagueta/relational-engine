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

module Extensions = struct
  module Option = struct
    let ( let+ ) = Option.bind
  end
  module Result = struct
    let ( let+ ) = Result.bind
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
    if Sys.file_exists "/tmp/relational-engine"
    then ()
    else Sys.mkdir "/tmp/relational-engine" 0o700
  let init (files: Hashes.t StringMap.t) filename: Hashes.t StringMap.t =
    assure_storage();
    match StringMap.find_opt filename files with
    | None -> StringMap.add filename [_EMPTY_SHA_HASH_] files
    | Some _ -> files
  let write (files: files) (locations: locations) filename ?hash_to_replace (content: Bytes.t) =
    let files = init files filename in
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
    module Transaction = struct
      let log_command command =
        assure_storage();
        try let register channel = 
              let offset_written = Out_channel.length channel in
              Out_channel.seek channel offset_written;
              Out_channel.output_bytes channel command;
              Out_channel.flush channel;
              Ok ()
            in
            Out_channel.with_open_bin "/tmp/relational-engine/transactions" register
        with _ -> Error "Failed to log command to transaction graph."
    end
end

module Command = struct
  open Data_encoding
  type command_kind =
    | OPEN
    | WRITE
    | READ
  let command_kind_encoding =
    union [
      case ~title:"open" (Tag 0)
        Data_encoding.empty
        (function OPEN -> Some () | _ -> None)
        (function () -> OPEN);
      case ~title:"write" (Tag 1)
        Data_encoding.empty
        (function WRITE -> Some () | _ -> None)
        (function () -> WRITE);
      case ~title:"read" (Tag 2)
        Data_encoding.empty
        (function READ -> Some () | _ -> None)
        (function () -> READ);
    ]
  type t =
    { kind: command_kind;
      timestamp: float;
      hash: string;
      filename: string;
      (* branch must be added here later *)
      content: string }
  let command_encoding =
    conv 
      (fun {kind; timestamp; hash; filename; content} -> (kind, timestamp, hash, filename, content))
      (fun (kind, timestamp, hash, filename, content) -> {kind; timestamp; hash; filename; content})
      Data_encoding.(tup5 command_kind_encoding float string string string)
  
  let parse_command ~data =
    let contract_encoding = Data_encoding.(tup4 command_kind_encoding string string string) in
    match Binary.of_bytes_opt contract_encoding data with
    | Some (kind, hash, filename, content) -> Ok {kind; timestamp = Unix.time(); hash; filename; content}
    | None -> Error "Failed to parse command"

  (** TODO: This does not write atomically. If the system crashes while it attempts to write, it will corrupt.
     Solve this with a temporary file and move later. *)
  let commit 
    (* (_files: Filesystem.files) (_locations: Filesystem.locations)  *)
   command =
      let open Extensions.Result in
      let+ serialized_command = 
        Result.map_error (fun _ -> "Failed to serialize command to binary format.")
        @@ Binary.to_bytes command_encoding command in
      Filesystem.Transaction.log_command serialized_command
  
end

module type Configuration = sig
  val base_dir: string
  val storage: string
  val transactions: string
  val assure: unit -> unit
end

module DevelopmentConfiguration: Configuration = struct
  let base_dir = "/tmp/relational-engine/"
  let storage = base_dir ^ "storage/"
  let transactions = base_dir ^ "transactions/"
  let assure () =
    if Sys.file_exists base_dir
    then Sys.remove base_dir;
    Sys.mkdir base_dir 0o700;
    Sys.mkdir storage 0o700;
    Sys.mkdir transactions 0o700;
end

module type FileSystem = sig
  type target =
    | Storage
    | Transaction
  val append: target -> bytes -> (int*int64, string) result
end

module FileSystem (C: Configuration): FileSystem = struct
  type target =
    | Storage
    | Transaction
  let target_to_string = function
    | Storage -> "storage"
    | Transaction -> "transaction"

  (** [append target content] writes content to file in append only format, returning [content_length * offset_written] *)
  let append target content =
    let register channel =
      let offset_written = Out_channel.length channel in
      Out_channel.seek channel offset_written;
      Out_channel.output_bytes channel content;
      Out_channel.flush channel;
      Out_channel.close channel;
      Ok (Bytes.length content, offset_written)
    in
    try Out_channel.with_open_bin (target_to_string target) register
    with _ -> Error (Printf.sprintf "Error on appending to target '%s'" (target_to_string target))
end

module Executor = struct
  module StringMap = Map.Make(String)
  module FS = FileSystem(DevelopmentConfiguration)
  module Location = struct
    type t =
      { offset: int64;
        size: int;
        hash: string }
  end
  module Hashes = struct
    type t = { values: string list;
               hash: string }
    type history = t list
  end
  type commit = { state: string; files: Hashes.history StringMap.t }
  type history = commit list
  type locations = Location.t StringMap.t
  let _EMPTY_SHA_HASH_ = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

  (** Initializes the virtual file or opens it. *)
  let init_file (files: Hashes.history StringMap.t) filename: Hashes.history StringMap.t =
    match StringMap.find_opt filename files with
    | None ->
       let default: Hashes.t = { values = []; hash = _EMPTY_SHA_HASH_} in
       let new_files = StringMap.add filename [default] files
       in new_files
    | Some _ ->
       files

  let compose_new_state files =
    let history = StringMap.to_list files in
    let all_hashes = List.concat
                     @@ List.map (fun (_, (history:Hashes.history)) ->
                            List.map (fun ({hash; _}: Hashes.t) -> hash) history) history
    in Interop.Merkle.merkle_generate_root all_hashes
  
  let write ({files;_} as commit: commit) (locations: locations) filename ?hash_to_replace (content: Bytes.t) =
    let files = init_file files filename in
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
                (function
                 | Some (file_hashes: Hashes.history) ->
                    let {values; _}: Hashes.t = List.hd file_hashes in
                    let new_state = Interop.Merkle.merkle_generate_root (computed_hash::values) in
                    Some ({values = computed_hash::values; hash = new_state}::file_hashes)
                 | None -> Some [{values = [computed_hash]; hash = computed_hash}])
                files
            in
            let open Extensions.Result in
            let+ (size, offset) = FS.append Storage content in
            let locations = StringMap.add computed_hash ({size; offset; hash = computed_hash}: Location.t) locations in
            let commit = {state = compose_new_state files; files}
            in Ok (commit, locations)
          end
        | Some hash_to_replace ->
           let update_fun = function
             | Some (file_hashes: Hashes.history) ->
                let {values; _}: Hashes.t = List.hd file_hashes in
                let updated_entry = List.map (fun hash -> if hash = hash_to_replace then computed_hash else hash) values in
                let new_entry: Hashes.t = {values = updated_entry; hash = Interop.Merkle.merkle_generate_root updated_entry} in
                Some (new_entry::file_hashes)
             | None -> Some [{values = [computed_hash]; hash = computed_hash}]
           in
           let files = StringMap.update filename update_fun files in
           let open Extensions.Result in
           let+ (size, offset) = FS.append Storage content in
           let locations = StringMap.add computed_hash ({size; offset; hash = computed_hash}: Location.t) locations in
           let commit = {state = compose_new_state files; files}
           in Ok (commit, locations)
        end
    | Some _ ->
        begin match hash_to_replace with
        | Some hash_to_replace ->
           let update_fun = function
             | Some (file_hashes: Hashes.history) ->
                let {values; _}: Hashes.t = List.hd file_hashes in
                let updated_entry = List.map (fun hash -> if hash = hash_to_replace then computed_hash else hash) values in
                let new_entry: Hashes.t = {values = updated_entry; hash = Interop.Merkle.merkle_generate_root updated_entry} in
                Some (new_entry::file_hashes)
             | None -> Some [{values = [computed_hash]; hash = computed_hash}]
           in
           let files = StringMap.update filename update_fun files in
           let commit = {state = compose_new_state files; files}
           in Ok (commit, locations)
        | None ->
           Ok (commit, locations)
        end
end

module Startup = struct
  (* TODO: Weird behavior with a system error on the dir /tmp/relational-engine opening. Disabling for now *)
  (* let () = DevelopmentConfiguration.assure() *)
end

module Command = struct
  module FS = FileSystem(DevelopmentConfiguration)
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
      FS.append FS.Transaction serialized_command
end

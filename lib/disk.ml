module type Configuration = sig
  val base_dir : string
  val storage : string
  val transactions : string
  val assure : unit -> unit
end

module DevelopmentConfiguration : Configuration = struct
  let base_dir = "/tmp/relational-engine/"
  let storage = base_dir ^ "storage/"
  let transactions = base_dir ^ "transactions/"

  let assure () =
    if Sys.file_exists base_dir then Sys.remove base_dir;
    Sys.mkdir base_dir 0o700;
    Sys.mkdir storage 0o700;
    Sys.mkdir transactions 0o700
end

module type FileSystem = sig
  type target = Storage | Transaction

  val append : target -> bytes -> (int * int64, string) result
  val read : target -> int64 -> int -> (bytes, string) result
end

module FileSystem (C : Configuration) : FileSystem = struct
  type target = Storage | Transaction

  let target_to_string = function
    | Storage -> C.storage ^ "storage.re"
    | Transaction -> C.transactions ^ "transaction.re"

  (** [append target content] writes content to file in append only format, returning [content_length * offset_written] *)
  let append target content =
    let register channel =
      let offset_written =
        try
          let stats = Unix.stat (target_to_string target) in
          Int64.of_int stats.Unix.st_size
        with _ -> 0L
      in
      (* Out_channel.seek channel offset_written; *)
      (* let offset_written = Out_channel.length channel in *)
      Out_channel.output_bytes channel content;
      Out_channel.flush channel;
      Out_channel.close channel;
      Ok (Bytes.length content, offset_written)
    in
    try
      Out_channel.with_open_gen
        [ Open_append; Open_binary; Open_creat ]
        0o666 (target_to_string target) register
    with e ->
      Error
        (Printf.sprintf "Error on appending to target '%s': %s"
           (target_to_string target) (Printexc.to_string e))

  let read target (offset : int64) size =
    print_endline ("READ_OFFSET: " ^ Int64.to_string offset);
    print_endline ("READ_SIZE: " ^ string_of_int (Int64.to_int offset + size));
    let register (channel : in_channel) =
      let buffer = Bytes.create size in
      In_channel.seek channel offset;
      let _ = In_channel.really_input channel buffer 0 size in
      print_bytes buffer;
      print_newline ();
      In_channel.close channel;
      Ok buffer
    in
    try
      In_channel.with_open_gen
        [ Open_binary; Open_rdonly ]
        0o666 (target_to_string target) register
    with e ->
      Error
        (Printf.sprintf "Error on reading from target '%s': %s"
           (target_to_string target) (Printexc.to_string e))
end

module Executor = struct
  module StringMap = Map.Make (String)
  module FS = FileSystem (DevelopmentConfiguration)

  module Location = struct
    type t = { offset : int64; size : int; hash : string } [@@deriving show]
  end

  module Hashes = struct
    type t = { values : string list; hash : string } [@@deriving show]
    type history = t list [@@deriving show]
  end

  type commit = { state : string; files : Hashes.history StringMap.t }
  type history = commit list
  type locations = Location.t StringMap.t

  let _EMPTY_SHA_HASH_ =
    "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

  (** Initializes the virtual file or opens it. *)
  let init_file (files : Hashes.history StringMap.t) filename :
      Hashes.history StringMap.t =
    match StringMap.find_opt filename files with
    | None ->
        let default : Hashes.t = { values = []; hash = _EMPTY_SHA_HASH_ } in
        let new_files = StringMap.add filename [ default ] files in
        new_files
    | Some _ -> files

  let compose_new_state files =
    let history = StringMap.to_list files in
    let all_hashes =
      List.concat
      @@ List.map
           (fun (_, (history : Hashes.history)) ->
             List.map (fun ({ hash; _ } : Hashes.t) -> hash) history)
           history
    in
    Interop.Merkle.merkle_generate_root all_hashes

  let write ({ files; _ } as commit : commit) (locations : locations) ~filename
      ?hash_to_replace (content : Bytes.t) =
    let files = init_file files filename in
    let computed_hash : string = Interop.Sha256.compute_hash content in
    match StringMap.find_opt computed_hash locations with
    | None -> (
        match hash_to_replace with
        | None ->
            (* This instead should store a list of list to keep the history *)
            let files =
              StringMap.update filename
                (function
                  | Some (file_hashes : Hashes.history) ->
                      let { values; _ } : Hashes.t = List.hd file_hashes in
                      let new_state =
                        Interop.Merkle.merkle_generate_root
                          (computed_hash :: values)
                      in
                      Some
                        ({ values = computed_hash :: values; hash = new_state }
                        :: file_hashes)
                  | None ->
                      Some
                        [ { values = [ computed_hash ]; hash = computed_hash } ])
                files
            in
            let open Extensions.Result in
            let+ size, offset = FS.append Storage content in
            let () = print_endline @@ "OFFSET: " ^ Int64.to_string offset in
            let locations =
              StringMap.add computed_hash
                ({ size; offset; hash = computed_hash } : Location.t)
                locations
            in
            let commit = { state = compose_new_state files; files } in
            Ok ((commit, locations), Some computed_hash)
        | Some hash_to_replace ->
            let update_fun = function
              | Some (file_hashes : Hashes.history) ->
                  let { values; _ } : Hashes.t = List.hd file_hashes in
                  let updated_entry =
                    List.map
                      (fun hash ->
                        if hash = hash_to_replace then computed_hash else hash)
                      values
                  in
                  let new_entry : Hashes.t =
                    {
                      values = updated_entry;
                      hash = Interop.Merkle.merkle_generate_root updated_entry;
                    }
                  in
                  Some (new_entry :: file_hashes)
              | None ->
                  Some [ { values = [ computed_hash ]; hash = computed_hash } ]
            in
            let files = StringMap.update filename update_fun files in
            let open Extensions.Result in
            let+ size, offset = FS.append Storage content in
            let () = print_endline @@ "OFFSET: " ^ Int64.to_string offset in
            let locations =
              StringMap.add computed_hash
                ({ size; offset; hash = computed_hash } : Location.t)
                locations
            in
            let commit = { state = compose_new_state files; files } in
            Ok ((commit, locations), Some computed_hash))
    | Some _ -> (
        match hash_to_replace with
        | Some hash_to_replace ->
            let update_fun = function
              | Some (file_hashes : Hashes.history) ->
                  let { values; _ } : Hashes.t = List.hd file_hashes in
                  let updated_entry =
                    List.map
                      (fun hash ->
                        if hash = hash_to_replace then computed_hash else hash)
                      values
                  in
                  let new_entry : Hashes.t =
                    {
                      values = updated_entry;
                      hash = Interop.Merkle.merkle_generate_root updated_entry;
                    }
                  in
                  Some (new_entry :: file_hashes)
              | None ->
                  Some [ { values = [ computed_hash ]; hash = computed_hash } ]
            in
            let files = StringMap.update filename update_fun files in
            let commit = { state = compose_new_state files; files } in
            Ok ((commit, locations), Some computed_hash)
        | None -> Ok ((commit, locations), None))

  let read ({ files; _ } as _commit : commit) (locations : locations) ~filename
      =
    let history : Hashes.history = StringMap.find filename files in
    let { values = location_hashes; _ } : Hashes.t = List.hd history in
    let physical_locations =
      List.map (fun hash -> StringMap.find hash locations) location_hashes
    in
    (* Terrible! Here I read everything opening a new IO every time. We should be able to read with only one call, but changing the offset every time *)
    let content =
      List.map
        (fun ({ offset; size; _ } : Location.t) ->
          match FS.read FS.Storage offset size with
          | Ok x -> x
          | Error err -> failwith err)
        physical_locations
    in
    List.rev content
end

module Startup = struct
  (* TODO: Weird behavior with a system error on the dir /tmp/relational-engine opening. Disabling for now *)
  (* let () = DevelopmentConfiguration.assure() *)
end

module Command = struct
  module FS = FileSystem (DevelopmentConfiguration)
  open Data_encoding

  type command_kind = OPEN | WRITE | READ

  let command_kind_encoding =
    union
      [
        case ~title:"open" (Tag 0) Data_encoding.empty
          (function OPEN -> Some () | _ -> None)
          (function () -> OPEN);
        case ~title:"write" (Tag 1) Data_encoding.empty
          (function WRITE -> Some () | _ -> None)
          (function () -> WRITE);
        case ~title:"read" (Tag 2) Data_encoding.empty
          (function READ -> Some () | _ -> None)
          (function () -> READ);
      ]

  type t = {
    kind : command_kind;
    timestamp : float;
    hash : string;
    filename : string;
    references : string list;
    (* branch must be added here later *)
    content : string;
  }

  let command_encoding =
    conv
      (fun { kind; timestamp; hash; filename; references; content } ->
        (kind, timestamp, hash, filename, references, content))
      (fun (kind, timestamp, hash, filename, references, content) ->
        { kind; timestamp; hash; filename; references; content })
      Data_encoding.(
        tup6 command_kind_encoding float string string (list string) string)

  let parse_command ~data =
    let contract_encoding =
      Data_encoding.(
        tup5 command_kind_encoding string string (list string) string)
    in
    match Binary.of_bytes_opt contract_encoding data with
    | Some (kind, hash, filename, references, content) ->
        Ok
          {
            kind;
            timestamp = Unix.time ();
            hash;
            filename;
            references;
            content;
          }
    | None -> Error "Failed to parse command"

  type return =
    | ComputedHash of string
    | Read of Protocol.relation list
    | Nothing

  (** TODO: This does not write atomically. If the system crashes while it attempts to write, it will corrupt.
     Solve this with a temporary file and move later. *)
  let commit_and_perform (stream : Executor.history)
      (locations : Executor.locations) command =
    let open Extensions.Result in
    let+ serialized_command =
      Result.map_error (fun _ ->
          "Failed to serialize command to binary format.")
      @@ Binary.to_bytes command_encoding command
    in
    let+ _ = FS.append FS.Transaction serialized_command in
    match command.kind with
    | WRITE -> (
        let open Extensions.Result in
        let+ (commit, locations), computed_hash_handle =
          Executor.write (List.hd stream) locations ~filename:command.filename
          @@ Bytes.of_string command.content
        in
        let updated_stream = commit :: stream in
        match computed_hash_handle with
        | Some computed_hash_handle ->
            Ok ((updated_stream, locations), ComputedHash computed_hash_handle)
        | None -> Ok ((updated_stream, locations), Nothing))
    | READ ->
        let ({ files; _ } : Executor.commit) = List.hd stream in
        let attributes =
          Executor.StringMap.filter_map
            (fun attr_name _ ->
              print_endline attr_name;
              if String.starts_with ~prefix:(command.filename ^ "/") attr_name
              then Some attr_name
              else None)
            files
          |> Executor.StringMap.to_list |> List.map fst
        in
        let relation =
          List.map
            (fun attr ->
              let content =
                Executor.read (List.hd stream) locations ~filename:attr
              in
              ({
                 attribute_name = attr;
                 attribute_type = "string";
                 tuples = List.map Bytes.to_string content;
               }
                : Protocol.relation))
            attributes
        in
        (* print_string "CONTENT: "; *)
        (* print_endline @@ Bytes.to_string content; *)
        Ok ((stream, locations), Read relation)
    | _ -> Error "Unimplemented method"
end

open Sexplib0
open Sexplib0.Sexp_conv

module Protocol = struct
  type tuple = string list [@@deriving sexp]
  type relation =
    { attribute_name: string;
      attribute_type: string;
      tuples: tuple }
      [@@deriving sexp]
  type t = relation list [@@deriving sexp]
end

let write_and_retrieve() =
  let open Disk in
  let stream: Executor.history = [{state = Executor._EMPTY_SHA_HASH_; files = Executor.StringMap.empty}] in
  let locations: Executor.locations = Executor.StringMap.empty in
  let open Extensions.Result in
  let command_write1: Command.t =
    {kind = Command.WRITE; timestamp = 10.0; hash = ""; content = "Daisy"; filename = "user/first-name"; references = []} in
  let+ ((stream, locations), Command.ComputedHash handle) = Command.commit_and_perform stream locations command_write1 in
  let command_write2: Command.t =
    {kind = Command.WRITE; timestamp = 10.0; hash = ""; content = "Blossom"; filename = "user/last-name"; references = [handle]} in
  let command_write3: Command.t =
    {kind = Command.WRITE; timestamp = 10.0; hash = ""; content = "Minnie"; filename = "user/first-name"; references = []} in
  let+ ((stream, locations), Command.ComputedHash _handle) = Command.commit_and_perform stream locations command_write2 in
  let+ ((stream, locations), Command.ComputedHash handle) = Command.commit_and_perform stream locations command_write3 in
  let command_write4: Command.t =
    {kind = Command.WRITE; timestamp = 10.0; hash = ""; content = "Mouse"; filename = "user/last-name"; references = [handle]} in
  let+ ((stream, locations), Command.ComputedHash _handle) = Command.commit_and_perform stream locations command_write4 in
  let command_write5: Command.t =
    {kind = Command.WRITE; timestamp = 10.0; hash = ""; content = "Billie"; filename = "user/first-name"; references = []} in
  let+ ((stream, locations), Command.ComputedHash handle) = Command.commit_and_perform stream locations command_write5 in
  let command_write6: Command.t =
    {kind = Command.WRITE; timestamp = 10.0; hash = ""; content = "Beans"; filename = "user/last-name"; references = [handle]} in
  let+ ((stream, locations), Command.ComputedHash _handle) = Command.commit_and_perform stream locations command_write6 in
  let command_read1: Command.t = {kind = Command.READ; timestamp = 0.0; hash = ""; content = ""; filename = "user/first-name"; references = []} in
  let+ (_, Command.Read first_name) = Command.commit_and_perform (List.tl stream) locations command_read1 in
  let command_read2: Command.t = {kind = Command.READ; timestamp = 0.0; hash = ""; content = ""; filename = "user/last-name"; references = []} in
  let+ (_, Command.Read last_name) = Command.commit_and_perform (List.tl stream) locations command_read2 in
  (* List.iter (fun a -> print_endline a) (List.map Bytes.to_string first_name); *)
  (* List.iter (fun a -> print_endline a) (List.map Bytes.to_string last_name); *)
  Ok (Sexp.to_string (Protocol.sexp_of_t [{attribute_name = "user/first-name"; attribute_type = "string"; tuples = List.map Bytes.to_string first_name};
                                          {attribute_name = "user/last-name"; attribute_type = "string"; tuples = List.map Bytes.to_string last_name}]))
    [@@warning "-8"] (* Suppress pattern match incomplete warnings *)

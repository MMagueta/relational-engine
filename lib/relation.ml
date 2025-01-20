module Protocol = struct
  open Protocol_conv_xml
  open Sexplib0.Sexp_conv
  type tuple =
    string list [@@deriving sexp, protocol ~driver:(module Xml_light)]
  type relation =
    { attribute_name: string;
      attribute_type: string;
      tuples: tuple }
      [@@deriving sexp, protocol ~driver:(module Xml_light)]
  type facts = relation list [@@deriving sexp, protocol ~driver:(module Xml_light)]
end

(*
let test_write_and_retrieve() =
  let open Disk in
  let stream: Executor.history = [{state = Executor._EMPTY_SHA_HASH_; files = Executor.StringMap.empty}] in
  let locations: Executor.locations = Executor.StringMap.empty in
  let open Extensions.Result in
  let command_write1: Command.t =
    {kind = Command.WRITE; timestamp = 10.0; hash = ""; content = "Daisy"; filename = "user/first-name"} in
  let+ ((stream, locations), Command.ComputedHash handle) = Command.commit_and_perform stream locations command_write1 in
  let command_write2: Command.t =
    {kind = Command.WRITE; timestamp = 10.0; hash = ""; content = "Blossom"; filename = "user/last-name"} in
  let command_write3: Command.t =
    {kind = Command.WRITE; timestamp = 10.0; hash = ""; content = "Minnie"; filename = "user/first-name"} in
  let+ ((stream, locations), Command.ComputedHash _handle) = Command.commit_and_perform stream locations command_write2 in
  let+ ((stream, locations), Command.ComputedHash handle) = Command.commit_and_perform stream locations command_write3 in
  let command_write4: Command.t =
    {kind = Command.WRITE; timestamp = 10.0; hash = ""; content = "Mouse"; filename = "user/last-name"} in
  let+ ((stream, locations), Command.ComputedHash _handle) = Command.commit_and_perform stream locations command_write4 in
  let command_write5: Command.t =
    {kind = Command.WRITE; timestamp = 10.0; hash = ""; content = "Billie"; filename = "user/first-name"} in
  let+ ((stream, locations), Command.ComputedHash handle) = Command.commit_and_perform stream locations command_write5 in
  let command_write6: Command.t =
    {kind = Command.WRITE; timestamp = 10.0; hash = ""; content = "Beans"; filename = "user/last-name"} in
  let+ ((stream, locations), Command.ComputedHash _handle) = Command.commit_and_perform stream locations command_write6 in
  let command_read1: Command.t = {kind = Command.READ; timestamp = 0.0; hash = ""; content = ""; filename = "user/first-name"} in
  let+ (_, Command.Read first_name) = Command.commit_and_perform (List.tl stream) locations command_read1 in
  let command_read2: Command.t = {kind = Command.READ; timestamp = 0.0; hash = ""; content = ""; filename = "user/last-name"} in
  let+ (_, Command.Read last_name) = Command.commit_and_perform (List.tl stream) locations command_read2 in
  (* List.iter (fun a -> print_endline a) (List.map Bytes.to_string first_name); *)
  (* List.iter (fun a -> print_endline a) (List.map Bytes.to_string last_name); *)
  let relation_result: Protocol.relation list =
    [{attribute_name = "user/first-name"; attribute_type = "string"; tuples = List.map Bytes.to_string first_name};
     {attribute_name = "user/last-name"; attribute_type = "string"; tuples = List.map Bytes.to_string last_name}] in
  Ok (Xml.to_string (Protocol.facts_to_xml_light relation_result))
    (* (Sexp.to_string (Protocol.sexp_of_t )) *)
    [@@warning "-8-27"] (* Suppress pattern match incomplete warnings *)
*)

let write_and_retrieve() =
  let open Disk in
  let stream: Executor.history = [{state = Executor._EMPTY_SHA_HASH_; files = Executor.StringMap.empty}] in
  let locations: Executor.locations = Executor.StringMap.empty in
  let references = Executor.StringMap.empty in
  let relation_name = "user" in
  (* let entity_id, _ = Executor.StringMap.find relation_name references |> Executor.IntMap.max_binding in *)
  let entity_id = 0L in
  (* let references = 
    Executor.StringMap.empty 
    |> Executor.StringMap.add "user" (Executor.StringMap.empty
                                      |> Executor.StringMap.add "entity_0" [handle11; handle12]
                                      |> Executor.StringMap.add "entity_1" [handle21; handle22]
                                      |> Executor.StringMap.add "entity_2" [handle31; handle32]) in *)
  let open Extensions.Result in
  let command_write1: Command.t =
    {kind = Command.WRITE; timestamp = 10.0; hash = ""; content = "Daisy"; filename = "user/first-name"} in
  let+ ((stream, locations, references), Command.ComputedHash handle11) = Command.commit_and_perform stream locations references ~entity_id command_write1 in
  let command_write2: Command.t =
    {kind = Command.WRITE; timestamp = 10.0; hash = ""; content = "Blossom"; filename = "user/last-name"} in
  let+ ((stream, locations, references), Command.ComputedHash handle12) = Command.commit_and_perform stream locations references ~entity_id command_write2 in
  let entity_id = 1L in
  let command_write3: Command.t =
    {kind = Command.WRITE; timestamp = 10.0; hash = ""; content = "Minnie"; filename = "user/first-name"} in
  let+ ((stream, locations, references), Command.ComputedHash handle21) = Command.commit_and_perform stream locations references ~entity_id command_write3 in
  let command_write4: Command.t =
    {kind = Command.WRITE; timestamp = 10.0; hash = ""; content = "Mouse"; filename = "user/last-name"} in
  let+ ((stream, locations, references), Command.ComputedHash handle22) = Command.commit_and_perform stream locations references ~entity_id command_write4 in
  let entity_id = 2L in
  let command_write5: Command.t =
    {kind = Command.WRITE; timestamp = 10.0; hash = ""; content = "Billie"; filename = "user/first-name"} in
  let+ ((stream, locations, references), Command.ComputedHash handle31) = Command.commit_and_perform stream locations references ~entity_id command_write5 in
  let command_write6: Command.t =
    {kind = Command.WRITE; timestamp = 10.0; hash = ""; content = "Beans"; filename = "user/last-name"} in
  let+ ((stream, locations, references), Command.ComputedHash handle32) = Command.commit_and_perform stream locations references ~entity_id command_write6 in
  let command_read: Command.t = {kind = Command.READ; timestamp = 0.0; hash = ""; content = ""; filename = "user"} in
  let+ (_, (Command.Read content as response)) = Command.commit_and_perform (List.tl stream) locations references ~entity_id command_read in
  print_endline (Command.show_return response);
  Ok (stream, locations)
    [@@warning "-8-27-26"] (* Suppress pattern match incomplete warnings *)

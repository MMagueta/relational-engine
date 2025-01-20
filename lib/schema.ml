module Protocol = struct
  open Data_encoding

  type attribute = { name : string; attr_type : string }
  type relation = { name : string; attributes : attribute list }

  let attribute_encoding =
    conv
      (fun { name; attr_type } -> (name, attr_type))
      (fun (name, attr_type) -> { name; attr_type })
      Data_encoding.(tup2 string string)

  let parse_attribute ~data =
    let contract_encoding = Data_encoding.(tup2 string string) in
    match Binary.of_bytes_opt contract_encoding data with
    | Some (name, attr_type) -> Ok { name; attr_type }
    | None -> Error "Failed to parse attribute"

  let relation_encoding =
    conv
      (fun { name; attributes } -> (name, attributes))
      (fun (name, attributes) -> { name; attributes })
      Data_encoding.(tup2 string (list attribute_encoding))

  let parse_relation ~data =
    let contract_encoding =
      Data_encoding.(tup2 string (list attribute_encoding))
    in
    match Binary.of_bytes_opt contract_encoding data with
    | Some (name, attributes) -> Ok { name; attributes }
    | None -> Error "Failed to parse relation"
end

(*
let test_schema_write_and_retrieve() =
  let open Disk in
  let open Data_encoding in
  let stream: Executor.history = [{state = Executor._EMPTY_SHA_HASH_; files = Executor.StringMap.empty}] in
  let locations: Executor.locations = Executor.StringMap.empty in
  let open Extensions.Result in
  let+ user_relation =
    let relation: Protocol.relation = { name = "schema~user"; attributes = [{name = "first-name"; attr_type = "string"}; {name = "last-name"; attr_type = "string"}] } in
    Result.map_error (fun _ -> "Failed to serialize relation to binary format.")
        @@ Binary.to_bytes Protocol.relation_encoding relation
  in
  let command_write: Command.t =
    {kind = Command.WRITE; timestamp = 10.0; hash = ""; content = Bytes.to_string user_relation; filename = "user"} in
  let+ ((stream, locations), Command.ComputedHash handle) = Command.commit_and_perform stream locations Executor.StringMap.empty command_write in
  let command_read: Command.t =
    {kind = Command.READ; timestamp = 10.0; hash = handle; content = ""; filename = "schema~user"} in
  let+ ((stream, locations), Command.Read content) = Command.commit_and_perform stream locations Executor.StringMap.empty command_read in
  let+ user_relation =
    Result.map_error (fun _ -> "Failed to deserialize relation from binary format.")
        @@ Binary.of_bytes Protocol.relation_encoding (Bytes.concat Bytes.empty content) in
  Ok (stream, locations, user_relation)
    [@@warning "-8"] (* Suppress pattern match incomplete warnings *)
*)

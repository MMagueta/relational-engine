let%test _ =
  let open Relational_engine.Disk.Executor in
  let stream : history =
    [ { state = _EMPTY_SHA_HASH_; files = StringMap.empty } ]
  in
  let locations : locations = StringMap.empty in
  let (new_commit, _), _ =
    Result.get_ok
    @@ write (List.hd stream) locations ~filename:"trash"
    @@ Bytes.of_string "abc"
  in
  let expected_state_hash =
    "bca39cc55b19274133ce57d0bea1ff794589f025d4bcf670c44d5d2811837830"
  in
  new_commit.state = expected_state_hash

let%test _ =
  let open Relational_engine.Disk.Executor in
  let stream : history =
    [ { state = _EMPTY_SHA_HASH_; files = StringMap.empty } ]
  in
  let locations : locations = StringMap.empty in
  let (new_commit, new_locations), _ =
    Result.get_ok
    @@ write (List.hd stream) locations ~filename:"trash"
    @@ Bytes.of_string "abc"
  in
  let result_test =
    let open Relational_engine.Extensions.Option in
    let+ new_file_content = StringMap.find_opt "trash" new_commit.files in
    let { values; _ } : Hashes.t = List.hd new_file_content in
    StringMap.find_opt (List.hd values) new_locations
  in
  match result_test with
  | Some location ->
      print_endline @@ Location.show location;
      location.size = 3 && location.offset = 0L
  | None -> false

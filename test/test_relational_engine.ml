
let%test _ =
  let open Relational_engine.Disk.Executor in
  let stream: history = [{state = _EMPTY_SHA_HASH_; files = StringMap.empty}] in
  let locations: locations = StringMap.empty in
  let (new_commit, _) = Result.get_ok @@ write (List.hd stream) locations ~filename:"trash" @@ Bytes.of_string "abc" in
  let expected_state_hash = "bca39cc55b19274133ce57d0bea1ff794589f025d4bcf670c44d5d2811837830" in
  new_commit.state = expected_state_hash

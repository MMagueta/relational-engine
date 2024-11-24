open Relational_engine.Disk

let x() =
  let command_write1: Command.t =
    {kind = Command.WRITE; timestamp = 10.0; hash = "bca39cc55b19274133ce57d0bea1ff794589f025d4bcf670c44d5d2811837830"; content = "asd"; filename = "trash"} in
  let command_write2: Command.t =
    {kind = Command.WRITE; timestamp = 10.0; hash = "bca39cc55b19274133ce57d0bea1ff794589f025d4bcf670c44d5d2811837831"; content = "qwe"; filename = "trash2"} in
  let command_write3: Command.t =
    {kind = Command.WRITE; timestamp = 10.0; hash = "bca39cc55b19274133ce57d0bea1ff794589f025d4bcf670c44d5d2811837832"; content = "fgh"; filename = "trash"} in
  let command_write4: Command.t =
    {kind = Command.WRITE; timestamp = 10.0; hash = "bca39cc55b19274133ce57d0bea1ff794589f025d4bcf670c44d5d2811837833"; content = "rty"; filename = "trash2"} in
  let command_write5: Command.t =
    {kind = Command.WRITE; timestamp = 10.0; hash = "bca39cc55b19274133ce57d0bea1ff794589f025d4bcf670c44d5d2811837834"; content = "jkl"; filename = "trash"} in
  let command_read: Command.t = {kind = Command.READ; timestamp = 0.0; hash = ""; content = ""; filename = "trash"} in
  let stream: Executor.history = [{state = Executor._EMPTY_SHA_HASH_; files = Executor.StringMap.empty}] in
  let locations: Executor.locations = Executor.StringMap.empty in
  let open Relational_engine.Extensions.Result in
  let+ (stream, locations) = Command.commit_and_perform stream locations command_write1 in
  let+ (stream, locations) = Command.commit_and_perform stream locations command_write2 in
  let+ (stream, locations) = Command.commit_and_perform stream locations command_write3 in
  let+ (stream, locations) = Command.commit_and_perform stream locations command_write4 in
  let+ (stream, locations) = Command.commit_and_perform stream locations command_write5 in
  Command.commit_and_perform (List.tl stream) locations command_read

let () =
  begin
    let _ = x() in ()
  end

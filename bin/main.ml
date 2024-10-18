open Relational_engine

module Implementations = struct
  open Ctypes
  let compute_hash content =
    let size_with_null_terminator = C.Types.hash_size + 1 in
    let buf = Ctypes.allocate_n char ~count:size_with_null_terminator in
    let () = C.Functions.compute_hash buf content in
    Ctypes.coerce (ptr char) Ctypes.string buf
  let gethostname () =
    let size_with_null_terminator = 20 in
    let buf = Ctypes.allocate_n char ~count:size_with_null_terminator in
    let _ = C.Functions.get_host_name_c buf size_with_null_terminator in
    Ctypes.coerce (ptr char) Ctypes.string buf
end

let () =
  begin
    print_endline (Implementations.compute_hash "test");
    print_endline (Implementations.gethostname ())
  end

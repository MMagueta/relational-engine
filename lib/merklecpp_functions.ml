(* open Ctypes *)

module Types = Types_generated

module Functions (F : Ctypes.FOREIGN) = struct
  (* open F *)
  open Ctypes_static
  let libmerklecpp = Dl.dlopen ~flags:[RTLD_GLOBAL; RTLD_NOW] ~filename:"/Users/mmagueta/CLionProjects/merklecpp_stub/cmake-build-debug/libmerklecpp_stub.dylib"
  let compute_hash = Foreign.foreign ~from:libmerklecpp "compute_hash" (ptr char @-> Ctypes.string @-> returning void)
end

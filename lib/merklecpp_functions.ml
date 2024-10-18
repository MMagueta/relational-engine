(* open Ctypes *)

module Types = Types_generated

module Functions (F : Ctypes.FOREIGN) = struct
  (* open F *)
  open Ctypes_static
  let libmerklecpp = Dl.dlopen ~flags:[RTLD_GLOBAL; RTLD_NOW] ~filename:((Sys.getenv "LIBRELATIONAL_ENGINE_LIB_PATH") ^ "/librelational_engine." ^ (Sys.getenv "OS_LIB_EXTENSION"))
  let compute_hash = Foreign.foreign ~from:libmerklecpp "compute_hash" (ptr char @-> Ctypes.string @-> returning void)

  let get_host_name_c = Foreign.foreign "gethostname" (ptr char @-> int @-> returning int)
end

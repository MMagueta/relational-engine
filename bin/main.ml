open Relational_engine.Disk

let () = print_int (Command.writer Command.StringMap.empty Command.StringMap.empty "" "" (Bytes.of_string "trash"))

let () =
  begin
    (* let response = *)
      (* Relational_engine.Relation.write_and_retrieve() *)
    (* in match response with *)
       (* | Ok response -> print_endline response *)
       (* | Error err -> print_endline err; *)
    Relational_engine.Server.start()
  end

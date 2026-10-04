open Butler

let version = { Types.major = 0; minor = 1; patch = 0 }
let version_string = Printf.sprintf "%d.%d.%d" version.major version.minor version.patch

let run file =
  Logger.info (Printf.sprintf "butler v%s" version_string);
  Logger.info (Printf.sprintf "config file: %s" file);
  match App.parse_service_schema file with
  | Error message -> Logger.error (Printf.sprintf "config error: %s" message)
  | Ok services ->
    Logger.info (Printf.sprintf "loaded %d service(s)" (List.length services));
    (match App.validate_service_schema services with
     | Error message -> Logger.error (Printf.sprintf "config error: %s" message)
     | Ok () ->
       (match Graph.generate_graph services with
        | Error message -> Logger.error (Printf.sprintf "graph error: %s" message)
        | Ok graph ->
          (match Runner.run graph with
           | Ok () -> ()
           | Error message -> Logger.error (Printf.sprintf "runner error: %s" message))))
;;

let () =
  let file = ref "butler.yaml" in
  let spec =
    [ "--file", Arg.Set_string file, "PATH path to the service config file"
    ; "-f", Arg.Set_string file, "PATH path to the service config file"
    ]
  in
  Arg.parse spec (fun arg -> raise (Arg.Bad ("unexpected argument: " ^ arg))) "butler [--file PATH]";
  run !file
;;

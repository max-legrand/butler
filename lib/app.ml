open Types

let try_read_file filepath =
  match In_channel.with_open_bin filepath In_channel.input_all with
  | contents -> Ok contents
  | exception Sys_error message -> Error message
;;

let parse_service_schema_internal entries =
  List.fold_right
    (fun entry acc ->
       let* acc = acc in
       let* service = Yaml_lite.service_of_yaml entry in
       Ok (service :: acc))
    entries
    (Ok [])
;;

let parse_service_schema filepath =
  let* contents = try_read_file filepath in
  let* yaml = Yaml_lite.of_string contents in
  match yaml with
  | Yaml_lite.Seq entries -> parse_service_schema_internal entries
  | _ -> Error "Expected a list of services"
;;

let validate_service_schema services =
  let seen = Hashtbl.create 16 in
  let* () =
    List.fold_left
      (fun acc service ->
         let* () = acc in
         if Hashtbl.mem seen service.name
         then Error (Printf.sprintf "Duplicate service name: %s" service.name)
         else (
           Hashtbl.add seen service.name ();
           match service.command with
           | Command_string _ -> Ok ()
           | Command_args _ when service.run_as_shell ->
             Error
               (Printf.sprintf
                  "Service %s sets run-as-shell but command is not a string"
                  service.name)
           | Command_args _ -> Ok ()))
      (Ok ())
      services
  in
  List.fold_left
    (fun acc service ->
       let* () = acc in
       List.fold_left
         (fun acc dependency ->
            let* () = acc in
            if Hashtbl.mem seen dependency
            then Ok ()
            else
              Error
                (Printf.sprintf
                   "Unknown dependency for service %s: %s"
                   service.name
                   dependency))
         (Ok ())
         (Option.value service.dependencies ~default:[]))
    (Ok ())
    services
;;

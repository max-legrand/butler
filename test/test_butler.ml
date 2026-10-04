open Butler
open Types

let service ?(dependencies = []) ?watchlist name =
  { name
  ; command = Command_args [ "dummy" ]
  ; cwd = None
  ; run_as_shell = false
  ; color = None
  ; dependencies = Some dependencies
  ; watchlist
  }
;;

let get = function
  | Ok x -> x
  | Error e -> failwith e
;;

let parse source =
  Yaml_lite.service_of_yaml (get (Yaml_lite.of_string source))
;;

let test_command_string_and_sequence () =
  let s = get (parse "name: scalar\ncommand: \"echo hello\"\n") in
  assert (s.command = Command_string "echo hello");
  assert (not s.run_as_shell);
  assert (s.cwd = None);
  let s =
    get
      (parse
         "name: argv\ncommand: [echo, hello]\ncwd: ./project\nrun-as-shell: true\n")
  in
  assert (s.command = Command_args [ "echo"; "hello" ]);
  assert s.run_as_shell;
  assert (s.cwd = Some "./project");
  assert (Result.is_error (App.validate_service_schema [ s ]))
;;

let test_sample_config () =
  let source =
    "- name: example\n  command: \"cargo build\"\n  color: '#74ACDF'\n\
     - name: dep example\n  command: [cargo, build]\n  dependencies: [echo]\n\
    \  watchlist: [\".*\\\\.txt\"]  # comment\n\
     - name: echo\n  run-as-shell: true\n  command: \"cat test/test.txt\"\n\
     - name: block\n  command:\n    - a\n    - b\n  dependencies:\n  - echo\n"
  in
  match get (Yaml_lite.of_string source) with
  | Yaml_lite.Seq entries ->
    let services = List.map (fun e -> get (Yaml_lite.service_of_yaml e)) entries in
    assert (List.length services = 4);
    let s1 = List.nth services 0 in
    assert (s1.color = Some "#74ACDF");
    let s2 = List.nth services 1 in
    assert (s2.watchlist = Some [ ".*\\.txt" ]);
    assert (s2.dependencies = Some [ "echo" ]);
    let s4 = List.nth services 3 in
    assert (s4.command = Command_args [ "a"; "b" ]);
    assert (s4.dependencies = Some [ "echo" ]);
    assert (Result.is_ok (App.validate_service_schema services))
  | _ -> assert false
;;

let test_validation () =
  assert (Result.is_error (App.validate_service_schema [ service "a"; service "a" ]));
  assert (
    Result.is_error (App.validate_service_schema [ service ~dependencies:[ "x" ] "a" ]))
;;

let test_shell_words () =
  assert (Shell_words.split "echo 'a b' \"c d\" e\\ f" = Ok [ "echo"; "a b"; "c d"; "e f" ]);
  assert (Result.is_error (Shell_words.split "echo 'oops"))
;;

let graph_of services = get (Graph.generate_graph services)

let test_closure () =
  let graph =
    graph_of
      [ service "base"
      ; service ~dependencies:[ "base" ] "middle"
      ; service ~dependencies:[ "middle" ] "leaf-a"
      ; service ~dependencies:[ "base" ] "leaf-b"
      ]
  in
  let order = get (Graph.topological_sort graph) in
  assert (order = [ 0; 1; 3; 2 ] || order = [ 0; 1; 2; 3 ] || order = [ 0; 3; 1; 2 ]);
  let closure =
    Runner.prerequisite_closure graph order (Runner.Int_set.singleton 2)
  in
  assert (closure = [ 0; 1 ])
;;

let test_cycle () =
  let graph =
    graph_of [ service ~dependencies:[ "b" ] "a"; service ~dependencies:[ "a" ] "b" ]
  in
  assert (Graph.topological_sort graph = Error "dependency cycle: a, b")
;;

let test_watch_patterns_anchored () =
  let graph = graph_of [ service ~watchlist:[ "foo|bar" ] "watch" ] in
  let targets = get (Runner.compile_watch_targets graph [ 0 ]) in
  let hit path =
    not
      (Runner.Int_set.is_empty
         (Runner.matching_targets [ "/root/" ^ path ] targets "/root"))
  in
  assert (hit "foo");
  assert (hit "bar");
  assert (not (hit "nested/foo/file.txt"))
;;

let test_content_filter () =
  assert (Runner.is_content_change "Create(File)");
  assert (Runner.is_content_change "Modify(Data(Content))");
  assert (Runner.is_content_change "Modify(Name(To))");
  assert (not (Runner.is_content_change "Modify(Metadata(Any))"));
  assert (not (Runner.is_content_change "Access(Open(Any))"))
;;

let test_event_paths () =
  let line =
    {|{"tags":[{"kind":"source","source":"filesystem"},{"kind":"fs","simple":"modify","full":"Modify(Data(Content))"},{"kind":"path","absolute":"/r/a.txt","filetype":"file"}]}|}
  in
  assert (Runner.event_paths line = [ "/r/a.txt" ])
;;

let test_rgb () =
  assert (Logger.parse_rgb "#74ACDF" = Some (116, 172, 223));
  assert (Logger.parse_rgb "74ACDF" = None);
  assert (Logger.parse_rgb "#bad" = None)
;;

let () =
  test_command_string_and_sequence ();
  test_sample_config ();
  test_validation ();
  test_shell_words ();
  test_closure ();
  test_cycle ();
  test_watch_patterns_anchored ();
  test_content_filter ();
  test_event_paths ();
  test_rgb ();
  print_endline "ok"
;;

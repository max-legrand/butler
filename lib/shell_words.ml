(* Split a command string into arguments the way a POSIX shell would,
   without expanding anything. *)
let split source =
  let buf = Buffer.create 32 in
  let words = ref [] in
  let in_word = ref false in
  let flush () =
    if !in_word
    then (
      words := Buffer.contents buf :: !words;
      Buffer.clear buf;
      in_word := false)
  in
  let len = String.length source in
  let rec go i =
    if i >= len
    then Ok ()
    else (
      match source.[i] with
      | ' ' | '\t' | '\n' ->
        flush ();
        go (i + 1)
      | '\\' ->
        if i + 1 >= len
        then Error "trailing backslash"
        else (
          Buffer.add_char buf source.[i + 1];
          in_word := true;
          go (i + 2))
      | '\'' ->
        in_word := true;
        (match String.index_from_opt source (i + 1) '\'' with
         | None -> Error "missing closing quote"
         | Some j ->
           Buffer.add_string buf (String.sub source (i + 1) (j - i - 1));
           go (j + 1))
      | '"' ->
        in_word := true;
        double (i + 1)
      | c ->
        Buffer.add_char buf c;
        in_word := true;
        go (i + 1))
  and double i =
    if i >= len
    then Error "missing closing quote"
    else (
      match source.[i] with
      | '"' -> go (i + 1)
      | '\\' when i + 1 < len ->
        (match source.[i + 1] with
         | ('"' | '\\' | '$' | '`') as c ->
           Buffer.add_char buf c;
           double (i + 2)
         | c ->
           Buffer.add_char buf '\\';
           Buffer.add_char buf c;
           double (i + 2))
      | c ->
        Buffer.add_char buf c;
        double (i + 1))
  in
  match go 0 with
  | Error _ as e -> e
  | Ok () ->
    flush ();
    Ok (List.rev !words)
;;

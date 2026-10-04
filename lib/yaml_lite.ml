(* A small YAML subset parser, enough for butler service files:
   block sequences, block mappings, flow sequences ([a, b]), plain, 'single'
   and "double" quoted scalars, and # comments. No anchors, tags, multi-line
   scalars, or flow mappings. *)

open Types

type t =
  | Scalar of string
  | Seq of t list
  | Map of (string * t) list

exception Parse_error of string

let fail fmt = Printf.ksprintf (fun s -> raise (Parse_error s)) fmt

(* Remove a trailing comment, ignoring # inside quotes. A comment starts at a
   # that begins the line or follows whitespace. *)
let strip_comment line =
  let len = String.length line in
  let rec go i quote =
    if i >= len
    then line
    else (
      let c = line.[i] in
      match quote with
      | Some q ->
        if q = '"' && c = '\\'
        then go (i + 2) quote
        else if c = q
        then go (i + 1) None
        else go (i + 1) quote
      | None ->
        if (c = '"' || c = '\'') && (i = 0 || line.[i - 1] = ' ' || line.[i - 1] = '[' || line.[i - 1] = ',' || line.[i - 1] = ':' )
        then go (i + 1) (Some c)
        else if c = '#' && (i = 0 || line.[i - 1] = ' ' || line.[i - 1] = '\t')
        then String.sub line 0 i
        else go (i + 1) None)
  in
  go 0 None
;;

let indent_of line =
  let len = String.length line in
  let rec go i =
    if i < len && line.[i] = ' '
    then go (i + 1)
    else if i < len && line.[i] = '\t'
    then fail "tabs are not allowed for indentation"
    else i
  in
  go 0
;;

let rtrim s =
  let n = ref (String.length s) in
  while !n > 0 && (s.[!n - 1] = ' ' || s.[!n - 1] = '\t' || s.[!n - 1] = '\r') do
    decr n
  done;
  String.sub s 0 !n
;;

(* Parse a quoted scalar starting at s.[i] (a quote). Returns the value and the
   index after the closing quote. *)
let parse_quoted s i =
  let q = s.[i] in
  let buf = Buffer.create 16 in
  let len = String.length s in
  let rec go j =
    if j >= len
    then fail "unterminated quoted string: %s" s
    else (
      let c = s.[j] in
      if q = '\'' && c = '\''
      then
        if j + 1 < len && s.[j + 1] = '\''
        then (
          Buffer.add_char buf '\'';
          go (j + 2))
        else Buffer.contents buf, j + 1
      else if q = '"' && c = '"'
      then Buffer.contents buf, j + 1
      else if q = '"' && c = '\\' && j + 1 < len
      then (
        (match s.[j + 1] with
         | 'n' -> Buffer.add_char buf '\n'
         | 't' -> Buffer.add_char buf '\t'
         | 'r' -> Buffer.add_char buf '\r'
         | '0' -> Buffer.add_char buf '\000'
         | e -> Buffer.add_char buf e);
        go (j + 2))
      else (
        Buffer.add_char buf c;
        go (j + 1)))
  in
  go (i + 1)
;;

let parse_flow_seq s =
  let len = String.length s in
  let rec skip i = if i < len && s.[i] = ' ' then skip (i + 1) else i in
  let rec items i acc =
    let i = skip i in
    if i >= len
    then fail "unterminated flow sequence: %s" s
    else if s.[i] = ']'
    then List.rev acc, i + 1
    else (
      let item, j =
        if s.[i] = '"' || s.[i] = '\''
        then (
          let v, j = parse_quoted s i in
          v, j)
        else (
          let j = ref i in
          while !j < len && s.[!j] <> ',' && s.[!j] <> ']' do
            incr j
          done;
          rtrim (String.sub s i (!j - i)), !j)
      in
      let j = skip j in
      if j < len && s.[j] = ','
      then items (j + 1) (Scalar item :: acc)
      else if j < len && s.[j] = ']'
      then List.rev (Scalar item :: acc), j + 1
      else fail "bad flow sequence: %s" s)
  in
  let items, stop = items 1 [] in
  if skip stop <> len then fail "unexpected text after flow sequence: %s" s;
  Seq items
;;

let parse_inline s =
  let s = String.trim s in
  if s = ""
  then Scalar ""
  else if s.[0] = '['
  then parse_flow_seq s
  else if s.[0] = '"' || s.[0] = '\''
  then (
    let v, stop = parse_quoted s 0 in
    if String.trim (String.sub s stop (String.length s - stop)) <> ""
    then fail "unexpected text after quoted string: %s" s;
    Scalar v)
  else Scalar s
;;

(* Split "key: rest" at the first ": " (or a trailing ":"). *)
let split_key text =
  let len = String.length text in
  let rec go i quote =
    if i >= len
    then None
    else (
      match quote, text.[i] with
      | Some q, c when c = q -> go (i + 1) None
      | Some _, _ -> go (i + 1) quote
      | None, (('"' | '\'') as q) when i = 0 -> go (i + 1) (Some q)
      | None, ':' when i + 1 = len || text.[i + 1] = ' ' ->
        Some (String.sub text 0 i, String.sub text (i + 1) (len - i - 1))
      | None, _ -> go (i + 1) None)
  in
  match go 0 None with
  | None -> None
  | Some (k, rest) ->
    let k = String.trim k in
    let k =
      if String.length k >= 2 && (k.[0] = '"' || k.[0] = '\'')
      then fst (parse_quoted k 0)
      else k
    in
    Some (k, rest)
;;

let is_dash text = text = "-" || (String.length text >= 2 && String.sub text 0 2 = "- ")

let parse source =
  let lines =
    String.split_on_char '\n' source
    |> List.map (fun l -> rtrim (strip_comment (rtrim l)))
    |> List.filter (fun l -> String.trim l <> "" && String.trim l <> "---")
    |> List.map (fun l ->
      let n = indent_of l in
      ref (n, String.sub l n (String.length l - n)))
    |> Array.of_list
  in
  let pos = ref 0 in
  let peek () = if !pos < Array.length lines then Some !(lines.(!pos)) else None in
  let rec parse_block indent =
    match peek () with
    | Some (_, text) when is_dash text -> parse_seq indent
    | Some _ -> parse_map indent
    | None -> Scalar ""
  and parse_seq indent =
    let rec go acc =
      match peek () with
      | Some (n, text) when n = indent && is_dash text ->
        let rest = if text = "-" then "" else String.sub text 2 (String.length text - 2) in
        let lead = String.length text - String.length (String.trim rest) in
        if String.trim rest = ""
        then (
          incr pos;
          go (parse_nested indent :: acc))
        else if String.trim rest <> "" && (split_key rest <> None) && rest.[0] <> '"' && rest.[0] <> '\'' && rest.[0] <> '['
        then (
          (* "- key: value": reparse the line as a mapping entry deeper in. *)
          lines.(!pos) := indent + lead, String.trim rest;
          go (parse_map (indent + lead) :: acc))
        else (
          incr pos;
          go (parse_inline rest :: acc))
      | Some (n, _) when n > indent -> fail "bad indentation in sequence"
      | _ -> Seq (List.rev acc)
    in
    go []
  and parse_nested indent =
    match peek () with
    | Some (n, _) when n > indent -> parse_block n
    | _ -> Scalar ""
  and parse_map indent =
    let rec go acc =
      match peek () with
      | Some (n, text) when n = indent && not (is_dash text) ->
        (match split_key text with
         | None -> fail "expected \"key: value\", got: %s" text
         | Some (key, rest) ->
           incr pos;
           let value =
             if String.trim rest <> ""
             then parse_inline rest
             else (
               match peek () with
               | Some (n2, t2) when n2 = indent && is_dash t2 -> parse_seq indent
               | _ -> parse_nested indent)
           in
           go ((key, value) :: acc))
      | Some (n, _) when n > indent -> fail "bad indentation in mapping"
      | _ -> Map (List.rev acc)
    in
    go []
  in
  match peek () with
  | None -> Scalar ""
  | Some (n, _) ->
    let v = parse_block n in
    if !pos < Array.length lines then fail "unexpected content: %s" (snd !(lines.(!pos)));
    v
;;

let of_string source =
  match parse source with
  | v -> Ok v
  | exception Parse_error message -> Error message
;;

(* ---- decoding into the service schema ---- *)

let scalar_string field = function
  | Scalar s -> Ok s
  | _ -> Error (Printf.sprintf "field `%s` must be a string" field)
;;

let string_list field = function
  | Seq items ->
    List.fold_right
      (fun item acc ->
         let* acc = acc in
         let* s = scalar_string field item in
         Ok (s :: acc))
      items
      (Ok [])
  | _ -> Error (Printf.sprintf "field `%s` must be a list of strings" field)
;;

let optional field fields decode =
  match List.assoc_opt field fields with
  | None | Some (Scalar "") -> Ok None
  | Some v ->
    let* x = decode v in
    Ok (Some x)
;;

let service_of_yaml = function
  | Map fields ->
    let* name =
      match List.assoc_opt "name" fields with
      | Some v -> scalar_string "name" v
      | None -> Error "missing field `name`"
    in
    let* command =
      match List.assoc_opt "command" fields with
      | Some (Scalar s) -> Ok (Command_string s)
      | Some (Seq _ as v) ->
        let* args = string_list "command" v in
        Ok (Command_args args)
      | Some _ -> Error "field `command` must be a string or a list of strings"
      | None -> Error (Printf.sprintf "missing field `command` in service %s" name)
    in
    let* cwd = optional "cwd" fields (scalar_string "cwd") in
    let* run_as_shell =
      match List.assoc_opt "run-as-shell" fields with
      | None -> Ok false
      | Some (Scalar "true") -> Ok true
      | Some (Scalar "false") -> Ok false
      | Some _ -> Error "field `run-as-shell` must be true or false"
    in
    let* color = optional "color" fields (scalar_string "color") in
    let* dependencies = optional "dependencies" fields (string_list "dependencies") in
    let* watchlist = optional "watchlist" fields (string_list "watchlist") in
    Ok { name; command; cwd; run_as_shell; color; dependencies; watchlist }
  | _ -> Error "expected a service mapping"
;;

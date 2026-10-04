let red = "\027[0;31m"
let yellow = "\027[0;33m"
let cyan = "\027[0;36m"
let reset = "\027[0m"

let timestamp () =
  let t = Unix.localtime (Unix.time ()) in
  Printf.sprintf
    "%04d-%02d-%02d %02d:%02d:%02d"
    (t.tm_year + 1900)
    (t.tm_mon + 1)
    t.tm_mday
    t.tm_hour
    t.tm_min
    t.tm_sec
;;

let print color label message =
  Printf.printf "%s%s [%s]:%s %s\n%!" color (timestamp ()) label reset message
;;

let error message = print red "ERROR" message
let warn message = print yellow "WARN" message
let info message = print cyan "INFO" message

let parse_rgb color =
  let len = String.length color in
  if len <> 7 || color.[0] <> '#'
  then None
  else (
    let channel i = int_of_string_opt ("0x" ^ String.sub color i 2) in
    match channel 1, channel 3, channel 5 with
    | Some r, Some g, Some b -> Some (r, g, b)
    | _ -> None)
;;

let service name color message =
  let ansi =
    match Option.bind color parse_rgb with
    | Some (r, g, b) -> Printf.sprintf "\027[38;2;%d;%d;%dm" r g b
    | None -> cyan
  in
  print ansi name message
;;

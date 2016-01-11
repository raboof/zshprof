type costline = { lineno: int; micros: int };;
type func = { name: string; costlines: costline list };;
type file = { name: string; functions: func list };;

type lff = { file: string; line: int; fragment: string; }
type association = {
  caller: lff;
  callee: lff;
  micros_inclusive: int;
};;
type profile = {
  files: file list;
  associations: association list;
  stack: association list;
 };;

let print_costline line =
  print_int line.lineno; print_char ' '; print_int line.micros; print_char '\n';;

let print_function (funct: func) =
  print_string "fn=";
  print_string funct.name;
  print_char '\n';
  List.iter print_costline funct.costlines;;

let print_file file =
  print_string "fl=";
  print_string file.name;
  print_char '\n';
  List.iter print_function file.functions;;

let print_association association =
  print_string "fl="; print_string association.caller.file; print_char '\n';
  print_string "fn="; print_string association.caller.fragment; print_char '\n';
  print_string "cfl="; print_string association.callee.file; print_char '\n';
  print_string "cfn="; print_string association.callee.fragment; print_char '\n';
  print_string "calls=1 "; print_int association.callee.line; print_char '\n';
  print_int association.caller.line; print_char ' '; print_int association.micros_inclusive; print_char '\n';;

let print_profile profile =
  print_string "events: micros\n";
  List.iter print_file profile.files;
  List.iter print_association profile.associations;;

type line = { timestamp: int; file: string; line: int; fragment: string; depth: int; full: string };;

exception InvalidLine of string;;

let rec next_line_starting_with char =
  let line = input_line stdin in
  if line = "" then next_line_starting_with char
  else if String.get line 0 == char then line
  else next_line_starting_with char;;

let parse_line _ =
  let line = next_line_starting_with '\\' in
  let split = (Str.split (Str.regexp "\t") line) in
  let field idx = List.nth split idx in
  try
  {
    timestamp = int_of_string (field 1);
    file = field 2;
    line = int_of_string (field 3);
    fragment = field 4;
    depth = int_of_string (field 5);
    full = line
  }
  with Failure s -> raise (InvalidLine line);;

let costline_for line nextline = {
  lineno = line.line;
  micros = nextline.timestamp - line.timestamp
};;

let function_for line nextline =
{
  name = line.fragment;
  costlines = [ costline_for line nextline ]
};;

let file_for line nextline =
  {
    name = line.file;
    functions = [ function_for line nextline ]
  };;

let rec next_line lastline =
  let result = parse_line () in
  if result.timestamp < lastline.timestamp then next_line lastline
  else result;;

let rec add_stackframes from next stack =
  if (next.depth > List.length stack) then add_stackframes from next ({
    caller = {
      file = from.file;
      line = from.line;
      fragment = from.fragment
    };
    callee = {
      file = next.file;
      line = next.line;
      fragment = next.fragment
    };
    micros_inclusive = 0;
  } :: stack)
  else stack;;

let rec pop_calls n stack calls last_micros =
  if n = 0 then (stack, calls)
  else
    match stack with
    | popped :: next_head :: tail ->
      let call = { caller = popped.caller; callee = popped.callee; micros_inclusive = popped.micros_inclusive + last_micros } in
      let updated_head = {next_head with micros_inclusive = next_head.micros_inclusive + popped.micros_inclusive + last_micros  } in
      pop_calls (n-1) (updated_head :: tail) (call :: calls) 0;;

let add_micros_to_top stack micros =
  let current_frame = List.hd stack in
  let updated_frame = { current_frame with micros_inclusive = current_frame.micros_inclusive +  micros } in
  updated_frame :: (List.tl stack)

let update_stack from next stack =
  let newstack = add_stackframes from next stack in
  let last_micros = next.timestamp - from.timestamp in
  let newstack_with_micros = add_micros_to_top newstack last_micros in
  if (from.depth = next.depth && from.depth = List.length stack) then
    (newstack_with_micros, [])
  else
  let finished_calls = List.length newstack - next.depth in
  if finished_calls > 0 then pop_calls finished_calls newstack [] last_micros
  else (newstack_with_micros, []);;

let rec process_lines lastline profile_so_far =
  try
    let nextline = next_line lastline in
    let (updated_stack, calls) = update_stack lastline nextline profile_so_far.stack in
    let updated_files = file_for lastline nextline :: profile_so_far.files in
    let updated_profile = {
      files = updated_files;
      stack = updated_stack;
      associations = List.append profile_so_far.associations calls
    } in
    process_lines nextline updated_profile
  with End_of_file -> profile_so_far;;

let initial_profile = {
  files = [];
  stack = [];
  associations = [] } in
let profile = process_lines (parse_line ()) initial_profile in
print_profile profile

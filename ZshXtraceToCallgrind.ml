open Callgrind;;
open ZshXtrace;;

type stackframe = {
  file: string;
  lineno: int;
  fragment: string;
  micros_inclusive: int;
};;

type stack = stackframe list;;

let rec add_stackframes (from: ZshXtrace.line) (next: ZshXtrace.line) stack =
  if (next.depth > List.length stack) then add_stackframes from next ({
    file = next.file;
    lineno = next.line;
    fragment = next.fragment;
    micros_inclusive = 0;
  } :: stack)
  else stack;;

let frame_to_lff stackframe =
  Callgrind.lff_for stackframe.file stackframe.lineno stackframe.fragment;;

let rec pop_calls n stack calls last_micros =
  if n = 0 then (stack, calls)
  else
    match stack with
    | popped :: next_head :: tail ->
      let call = Callgrind.association_for (frame_to_lff next_head) (frame_to_lff popped) (popped.micros_inclusive + last_micros) in
      let updated_head = { next_head with micros_inclusive = next_head.micros_inclusive + popped.micros_inclusive + last_micros } in
      pop_calls (n-1) (updated_head :: tail) (call :: calls) 0
    | _ ->
      prerr_string "Warning: stack error. Output might be meaningless. Parallelism?\n";
      (stack, calls);;

let add_micros_to_top stack micros =
  let current_frame = List.hd stack in
  let updated_frame = { current_frame with micros_inclusive = current_frame.micros_inclusive + micros } in
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

let updated_profile profile_so_far calls (lastline: ZshXtrace.line) (nextline: ZshXtrace.line) =
  let profile_with_calls = Callgrind.add_calls profile_so_far calls in
  Callgrind.add_line profile_with_calls lastline.file lastline.fragment lastline.line (nextline.timestamp - lastline.timestamp);;

let rec process_lines lastline stack_so_far profile_so_far =
  try
    let nextline = ZshXtrace.next_line lastline in
    let (updated_stack, calls) = update_stack lastline nextline stack_so_far in
    process_lines nextline updated_stack (updated_profile profile_so_far calls lastline nextline)
  with End_of_file -> profile_so_far;;

let initial_stack = [] in
let profile = process_lines (ZshXtrace.parse_line ()) initial_stack Callgrind.empty in
Callgrind.print_profile profile

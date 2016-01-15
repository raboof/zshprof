module ZshXtrace = struct
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

    let rec next_line lastline =
      let result = parse_line () in
      if result.timestamp < lastline.timestamp then next_line lastline
      else result;;

end

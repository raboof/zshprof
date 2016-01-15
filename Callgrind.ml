module Callgrind = struct
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
    associations: association list
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

  let association_for caller callee micros_inclusive =
    { caller; callee; micros_inclusive };;

  let lff_for file line fragment = { file; line; fragment };;

  let function_for fragment lineno micros =
    {
      name = fragment;
      costlines = [
        {
          lineno = lineno;
          micros = micros;
        }
      ]
    };;

  let file_for file fragment lineno micros =
    {
      name = file;
      functions = [ function_for fragment lineno micros ]
    };;

  let add_calls profile calls = {
      files = profile.files;
      associations = List.append profile.associations calls;
    }

  let add_line profile file fragment lineno micros =
    let new_file = file_for file fragment lineno micros in
    {
      files = new_file :: profile.files;
      associations = profile.associations;
    }

  let empty = {
    files = [];
    associations = []
  }
end

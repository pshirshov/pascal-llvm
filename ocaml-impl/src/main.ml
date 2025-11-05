open Printf

let usage_msg = "Usage: pascalc <input.pas> [-o output]"

let input_file = ref ""
let output_file = ref ""

let speclist = [
  ("-o", Arg.Set_string output_file, "Output file name");
]

let anon_fun filename =
  input_file := filename

let () =
  Arg.parse speclist anon_fun usage_msg;

  if !input_file = "" then begin
    eprintf "Error: No input file specified\n";
    Arg.usage speclist usage_msg;
    exit 1
  end;

  if !output_file = "" then
    output_file := Filename.remove_extension !input_file ^ ".ll";

  try
    (* Read input file *)
    let ic = open_in !input_file in
    let lexbuf = Lexing.from_channel ic in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = !input_file };

    (* Parse *)
    printf "Parsing %s...\n" !input_file;
    let ast = Parser.program Lexer.token lexbuf in
    close_in ic;

    printf "Program: %s\n" ast.Ast.program_name;

    (* Type check *)
    printf "Type checking...\n";
    Types.check_program ast;
    printf "Type checking passed.\n";

    (* Code generation *)
    printf "Generating LLVM IR...\n";
    let llmodule = Codegen.codegen_program ast in

    (* Verify module *)
    (match Llvm_analysis.verify_module llmodule with
     | None -> ()
     | Some msg ->
         eprintf "Error: LLVM module verification failed:\n%s\n" msg;
         exit 1);

    (* Write output *)
    printf "Writing output to %s...\n" !output_file;
    let oc = open_out !output_file in
    fprintf oc "%s" (Llvm.string_of_llmodule llmodule);
    close_out oc;

    printf "Compilation successful!\n";
    printf "\nTo compile to native code:\n";
    printf "  llc %s -o %s.s\n" !output_file (Filename.remove_extension !output_file);
    printf "  gcc %s.s -o %s\n" (Filename.remove_extension !output_file)
      (Filename.remove_extension !output_file);

  with
  | Lexer.Lexical_error msg ->
      eprintf "Lexical error: %s\n" msg;
      exit 1
  | Parser.Error ->
      eprintf "Parse error\n";
      exit 1
  | Types.Type_error msg ->
      eprintf "Type error: %s\n" msg;
      exit 1
  | Codegen.Codegen_error msg ->
      eprintf "Code generation error: %s\n" msg;
      exit 1
  | Sys_error msg ->
      eprintf "System error: %s\n" msg;
      exit 1
  | e ->
      eprintf "Unexpected error: %s\n" (Printexc.to_string e);
      exit 1

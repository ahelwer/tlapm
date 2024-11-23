open Parse_sany_xml_output
open Tlapm_lib

let () =
  match parse_sany_xml_output "AddTwo.xml" with
  | Ok modules -> print_endline (show_modules modules)
  | Error (e, trace) ->
      print_endline e;
      print_endline trace

let () =
  let ic = "AddTwo.tla" |> open_in in
  try
    let text = ic |> In_channel.input_all in
    close_in ic;
    match modctx_of_string ~content:text ~filename:"AddTwo.tla" ~loader_paths:[] ~prefer_stdlib:true with
    | Ok _ -> print_endline "Success!"
    | Error _ -> print_endline "Failure."
  with e ->
    close_in_noerr ic;
    raise e
  
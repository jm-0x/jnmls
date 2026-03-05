let hash_string s = 
  Digest.to_hex(Digest.string s)

let read_file path = 
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let rec ensure_dir path =
    if not (Sys.file_exists path) then begin
        ensure_dir (Filename.dirname path);
        Sys.mkdir path 0o755
    end

let render ~source ~output_path ~quality =
    let q_flag = match quality with
        | "l" -> "-ql"
        | "h" -> "-qh"
        | "k" -> "-qk"
        | _ -> "-ql"
    in
    let (py_file, tmp) = match source with
        | Ast.Inline code ->
            let path = Filename.temp_file "jnmls_manim_" ".py" in
            write_file path code;
            (path, true)
        | Ast.File path -> (path, false)
    in
    let media_dir = Filename.get_temp_dir_name () ^ "/jnmls_media" in
    let log_file = Filename.temp_file "jnmls_manim_log_" ".txt" in
    let cmd = Printf.sprintf
        "manim render %s --media_dir %s %s > %s 2>&1"
        q_flag media_dir py_file log_file
    in
    Printf.printf "  manim: rendering...\n%!";
    let exit_code = Sys.command cmd in
    if tmp then Sys.remove py_file;
    if exit_code <> 0 then begin
        Printf.eprintf "  manim: render failed\n%!";
        let log = read_file log_file in
        Printf.eprintf "%s\n" log;
        Sys.remove log_file;
        false
    end else begin
        Sys.remove log_file;
        let find_cmd = Printf.sprintf
            "find %s -name '*.mp4' -not -path '*/partial_movie_files/*' | head -1"
            media_dir
        in
        let ic = Unix.open_process_in find_cmd in
        let found = try input_line ic with End_of_file -> "" in
        ignore (Unix.close_process_in ic);
        if found <> "" then begin
            ignore (Sys.command (Printf.sprintf "cp '%s' '%s'" found output_path));
            ignore (Sys.command (Printf.sprintf "rm -rf %s" media_dir));
            Printf.printf "  manim: done\n%!";
            true
        end else begin
            Printf.eprintf "  manim: could not find rendered video\n%!";
            ignore (Sys.command (Printf.sprintf "rm -rf %s" media_dir));
            false
        end
    end
let resolve_block ~output_dir ~quality (block : Ast.block) : Ast.block =
    match block with
    | Ast.AtBlock { kind = Ast.Manim; source; options } ->
        let content = match source with
            | Ast.Inline code -> code
            | Ast.File path ->
                if Sys.file_exists path then read_file path
                else begin
                    Printf.eprintf "  manim: file not found: %s\n%!" path;
                    ""
                end
        in
        if content = "" then block
        else begin
            let hash = hash_string content in
            let video_name = hash ^ ".mp4" in
            let video_path = Filename.concat output_dir video_name in
            if Sys.file_exists video_path then begin
                Printf.printf "  manim: cached %s\n%!" video_name;
                Ast.AtBlock { kind = Ast.Manim;
                              source = Ast.File video_name;
                              options }
            end else begin
                ensure_dir output_dir;
                let success = render ~source ~output_path:video_path ~quality in
                if success then
                    Ast.AtBlock { kind = Ast.Manim;
                                  source = Ast.File video_name;
                                  options }
                else
                    block
            end
        end
    | _ -> block

let resolve_all ~output_dir ~quality (blocks : Ast.block list) : Ast.block list =
    List.map (resolve_block ~output_dir ~quality) blocks
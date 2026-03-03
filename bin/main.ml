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

let rec replace_placeholder placeholder replacement template =
    let plen = String.length placeholder in
    let rec find i =
        if i + plen > String.length template then template
        else if String.sub template i plen = placeholder then
            let result = String.sub template 0 i ^ replacement ^
                String.sub template (i + plen) (String.length template - i - plen) in
            replace_placeholder placeholder replacement result
        else find (i + 1)
    in
    find 0

let process_post content_dir output_dir filename =
    let slug = Filename.chop_suffix filename ".jnml" in
    let source = read_file (Filename.concat content_dir filename) in
    let tokens = Jnmls.Lexer.tokenize source in
    let doc = Jnmls.Parser.parse tokens in
    let body_html = Jnmls.Html.render_document doc in
    let template = read_file "static/post.html" in
    let html = template
        |> replace_placeholder "{{title}}" doc.meta.Jnmls.Ast.title
        |> replace_placeholder "{{date}}" doc.meta.Jnmls.Ast.date
        |> replace_placeholder "{{body}}" body_html
    in
    let post_dir = Filename.concat output_dir slug in
    if not (Sys.file_exists post_dir) then Sys.mkdir post_dir 0o755;
    write_file (Filename.concat post_dir "index.html") html;
    Printf.printf "  %s → /%s/\n" filename slug;
    (doc.meta, slug)

let render_index posts =
    let template = read_file "static/index.html" in
    let post_items = List.map (fun (meta, slug) ->
        Printf.sprintf "    <li><time>%s</time> » <a href=\"%s/\">%s</a></li>"
            meta.Jnmls.Ast.date slug meta.Jnmls.Ast.title
    ) posts in
    let posts_html = String.concat "\n" post_items in
    replace_placeholder "{{posts}}" posts_html template

let () =
    let content_dir = "content/posts" in
    let output_dir = "_site" in
    if not (Sys.file_exists output_dir) then
        Sys.mkdir output_dir 0o755;
    if not (Sys.file_exists content_dir) then begin
        Printf.printf "No content directory found, generating empty site.\n";
        let index_html = render_index [] in
        write_file (Filename.concat output_dir "index.html") index_html;
    end else begin
        Printf.printf "Building site...\n";
        let files = Sys.readdir content_dir in
        let posts = Array.to_list files
            |> List.filter (fun f -> Filename.check_suffix f ".jnml")
            |> List.map (process_post content_dir output_dir)
            |> List.sort (fun (m1, _) (m2, _) ->
                compare m2.Jnmls.Ast.date m1.Jnmls.Ast.date)
        in
        let index_html = render_index posts in
        write_file (Filename.concat output_dir "index.html") index_html;
        if Sys.file_exists "static" then begin
            let static_files = Sys.readdir "static" in
            Array.iter (fun f ->
                let src = Filename.concat "static" f in
                let dst = Filename.concat output_dir f in
                if f <> "index.html" && f <> "post.html" then begin
                    let content = read_file src in
                    write_file dst content
                end
            ) static_files
        end;
        Printf.printf "Done! %d posts generated.\n" (List.length posts)
    end
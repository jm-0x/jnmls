let rec render_inline (inline : Ast.inline) : string =
  match inline with
  | Ast.Text s -> s
  | Ast.Bold inlines -> "<strong>" ^ render_inlines inlines ^ "</strong>"    | Ast.Italic inlines -> "<em>" ^ render_inlines inlines ^ "</em>"
  | Ast.Code s -> "<code>" ^ s ^ "</code>"
  | Ast.Link { label; url } -> "<a href=\"" ^ url ^ "\">" ^ render_inlines label ^ "</a>"
  | Ast.InlineMath s -> "\\(" ^ s ^ "\\)"
and render_inlines (inlines : Ast.inline list) : string =
    String.concat "" (List.map render_inline inlines)

let rec render_block (block : Ast.block) : string =
    match block with
    | Ast.Heading { level; content } ->
        let tag = "h" ^ string_of_int level in
        "<" ^ tag ^ ">" ^ render_inlines content ^ "</" ^ tag ^ ">"
    | Ast.Paragraph inlines ->
        "<p>" ^ render_inlines inlines ^ "</p>"
    | Ast.AtBlock { kind; source; options } ->
        (match kind, source with
        | Ast.Code, Ast.Inline code ->
            let lang_attr = match options with
                | l :: _ -> " class=\"language-" ^ l ^ "\""
                | [] -> ""
            in
            "<pre><code" ^ lang_attr ^ ">" ^ code ^ "</code></pre>"
        | Ast.Code, Ast.File path ->
            "<pre><code><!-- @code ref: " ^ path ^ " --></code></pre>"
        | Ast.Math, Ast.Inline math ->
            "<div class=\"math\">\\[" ^ math ^ "\\]</div>"
        | Ast.Math, Ast.File path ->
            "<div class=\"math\"><!-- @math ref: " ^ path ^ " --></div>"
        | Ast.Manim, Ast.File path ->
            let loop_attr = if List.mem "loop" options then " loop" else "" in
            "<div class=\"manim\"><video src=\"/assets/manim/" ^ path ^ "\" autoplay muted playsinline" ^ loop_attr ^ "></video></div>"
        | Ast.Manim, Ast.Inline _ ->
            "<div class=\"manim\"><!-- manim: unresolved inline --></div>")
    | Ast.Blockquote blocks ->
        "<blockquote>" ^ render_blocks blocks ^ "</blockquote>"
    | Ast.Image { alt; url } ->
        "<img src=\"" ^ url ^ "\" alt=\"" ^ alt ^ "\">"
    | Ast.HorizontalRule ->
        "<hr>"

and render_blocks (blocks : Ast.block list) : string =
    String.concat "\n" (List.map render_block blocks)

let render_document (doc : Ast.document) : string =
    render_blocks doc.body
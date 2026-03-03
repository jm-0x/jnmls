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
    | Ast.CodeBlock { language; code } ->
        let lang_attr = match language with
            | Some l -> " class=\"language-" ^ l ^ "\""
            | None -> ""
        in
        "<pre><code" ^ lang_attr ^ ">" ^ code ^ "</code></pre>"
    | Ast.BlockMath s ->
        "<div class=\"math\">\\[" ^ s ^ "\\]</div>"
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
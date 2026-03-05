type inline =
    | Text of string
    | Bold of inline list
    | Italic of inline list
    | Code of string
    | Link of { label : inline list; url : string }
    | InlineMath of string

type atblock_kind = Code | Math | Manim
type atblock_source = File of string | Inline of string

type block =
    | Heading of { level : int; content : inline list }
    | Paragraph of inline list
    | AtBlock of { kind : atblock_kind; source : atblock_source; options : string list }
    | Blockquote of block list
    | Image of { alt : string; url : string }
    | HorizontalRule

type meta = {
    title : string;
    date: string;
}

type document = {
    meta : meta;
    body: block list;
}

let default_meta = { title = "Untitled"; date = "" }


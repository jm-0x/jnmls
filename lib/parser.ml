open Lexer

let parse_meta (tokens: token list) : Ast.meta * token list =
  match tokens with
  | Triple_dash :: Newline :: rest ->
    let rec read_lines toks lines =
      match toks with
      | Triple_dash :: rest -> (lines, rest)
      | Newline :: rest -> read_lines rest lines
      | _ ->
        let rec collect_line t acc =
          match t with
          | Newline :: rest -> (acc, rest)
          | Text s :: rest -> collect_line rest (acc ^ s)
          | Triple_dash :: _ -> (acc, t)
          | _ :: rest -> collect_line rest (acc ^ " ")
          | [] -> (acc, [])
        in
        let (line, rest) = collect_line toks "" in
        if line = "" then read_lines rest lines
        else read_lines rest (line :: lines)
    in
    let (lines, rest) = read_lines rest [] in
    let parse_line line = 
      let colon = String.index line ':' in
      let key = String.sub line 0 colon in
      let value = String.trim(String.sub line (colon + 1) (String.length line - colon - 1)) in
      (key, value)
    in
    let meta = List.fold_left (fun acc line ->
      let (key, value) = parse_line line in 
      match key with
      | "title" -> {acc with Ast.title = value}
      | "date" -> { acc with Ast.date = value}
      | _ -> acc
      ) Ast.default_meta lines in 
      (meta, rest)
      | _ -> (Ast.default_meta, tokens)

let rec collect_until stop toks acc =
    match stop toks with
    | Some rest -> (acc, rest)
    | None ->
        (match toks with
         | Text s :: rest -> collect_until stop rest (acc ^ s)
         | _ :: rest -> collect_until stop rest acc
         | [] -> (acc, []))

let rec parse_inlines (tokens : token list) : Ast.inline list * token list =
    match tokens with
    | Newline :: rest -> ([], rest)
    | Blank_line :: rest -> ([], rest)
    | Eof :: _ -> ([], tokens)
    | [] -> ([], [])
    | Text s :: rest ->
        let (more, rest2) = parse_inlines rest in
        (Ast.Text s :: more, rest2)
    | Backtick :: rest ->
        let (code, rest2) = collect_until
            (fun toks -> match toks with Backtick :: r -> Some r | _ -> None)
            rest "" in
        let (more, rest3) = parse_inlines rest2 in
        (Ast.Code code :: more, rest3)
    | Dollar :: rest ->
        let (math, rest2) = collect_until
            (fun toks -> match toks with Dollar :: r -> Some r | _ -> None)
            rest "" in
        let (more, rest3) = parse_inlines rest2 in
        (Ast.InlineMath math :: more, rest3)
    | Star :: Star :: rest ->
        let (text, rest2) = collect_until
            (fun toks -> match toks with Star :: Star :: r -> Some r | _ -> None)
            rest "" in
        let (more, rest3) = parse_inlines rest2 in
        (Ast.Bold [Ast.Text text] :: more, rest3)
    | Star :: rest ->
        let (text, rest2) = collect_until
            (fun toks -> match toks with Star :: r -> Some r | _ -> None)
            rest "" in
        let (more, rest3) = parse_inlines rest2 in
        (Ast.Italic [Ast.Text text] :: more, rest3)
    | Open_bracket :: rest ->
        let (text, rest2) = collect_until
            (fun toks -> match toks with Close_bracket :: r -> Some r | _ -> None)
            rest "" in
        (match rest2 with
         | Open_paren :: rest3 ->
             let (url, rest4) = collect_until
                 (fun toks -> match toks with Close_paren :: r -> Some r | _ -> None)
                 rest3 "" in
             let (more, rest5) = parse_inlines rest4 in
             (Ast.Link { label = [Ast.Text text]; url } :: more, rest5)
         | _ ->
             let (more, rest3) = parse_inlines rest2 in
             (Ast.Text ("[" ^ text ^ "]") :: more, rest3))
    | _ :: rest ->
        let (more, rest2) = parse_inlines rest in
        (more, rest2)

let rec parse_blocks (tokens : token list) : Ast.block list * token list =
  match tokens with
  | Eof :: _ -> ([], tokens)
  | [] -> ([],[])
  | Blank_line :: rest -> parse_blocks rest
  | Newline :: rest -> parse_blocks rest
  | Heading_marker n :: rest ->
    let (content, rest2) = parse_inlines rest in
    let (more, rest3) = parse_blocks rest2 in
    (Ast.Heading { level = n; content } :: more, rest3)
  | Code_start lang :: rest ->
    let rec collect toks acc =
      match toks with
      | Code_end :: rest -> (acc, rest)
      | Text s :: rest -> collect rest (acc ^ s)
      | Newline :: rest -> collect rest (acc ^ "\n")
      | _ :: rest -> collect rest acc
      | [] -> (acc, [])
    in
    let (code, rest2) = collect rest "" in
    let (more, rest3) = parse_blocks rest2 in
    (Ast.CodeBlock { language = lang; code } :: more, rest3)
  | Triple_dash :: rest ->
    let (more , rest2) = parse_blocks rest in
    (Ast.HorizontalRule :: more, rest2) 
  | Dollar :: Dollar :: rest ->
    let rec collect toks acc = 
        match toks with
        | Dollar :: Dollar :: rest -> (acc, rest)
        | Text s :: rest -> collect rest (acc ^ s)
        | Newline :: rest -> collect rest (acc ^ "\n")
        | [] -> (acc, [])
        | _ :: rest -> collect rest acc 
    in 
    let (math, rest2) = collect rest "" in
    let (more, rest3) = parse_blocks rest2 in
    (Ast.BlockMath math :: more, rest3)
  | Bang :: Open_bracket :: rest ->
    let (alt, rest2) = collect_until
      (fun toks -> match toks with Close_bracket :: r -> Some r | _ -> None)
      rest "" in
    (match rest2 with
     | Open_paren :: rest3 ->
       let (url, rest4) = collect_until
         (fun toks -> match toks with Close_paren :: r -> Some r | _ -> None)
         rest3 "" in
       let (more, rest5) = parse_blocks rest4 in
       (Ast.Image { alt; url } :: more, rest5)
     | _ ->
       let (more, rest3) = parse_blocks rest2 in
       (more, rest3))
  | Gt :: rest ->
    let (content, rest2) = parse_inlines rest in
    let (more, rest3) = parse_blocks rest2 in
    (Ast.Blockquote [Ast.Paragraph content] :: more, rest3)
  | _ -> 
    let (content, rest) = parse_inlines tokens in
    let (more, rest2) = parse_blocks rest in
    (Ast.Paragraph content :: more, rest2)   

let parse (tokens : token list) : Ast.document =
    let (meta, rest) = parse_meta tokens in
    let (body, _) = parse_blocks rest in
    { meta; body }
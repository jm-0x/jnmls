type token =
    | Text of string
    | Star
    | Backtick
    | Dollar
    | Open_bracket
    | Close_bracket
    | Open_paren
    | Close_paren
    | Bang
    | Gt
    | Heading_marker of int
    | Triple_dash
    | Code_start of string option
    | Code_end
    | Newline
    | Blank_line
    | Eof

let tokenize (source: string) : token list = 
  let len = String.length source in
  let rec aux pos = 
    if pos >= len then [Eof]
    else 
      match source.[pos] with
      | '*' -> Star :: aux (pos + 1)
      | '$' -> Dollar :: aux (pos + 1)
      | '[' -> Open_bracket :: aux (pos + 1)
      | ']' -> Close_bracket :: aux (pos + 1)
      | '(' -> Open_paren :: aux (pos + 1)
      | ')' -> Close_paren :: aux (pos + 1)
      | '!' -> Bang :: aux (pos + 1) 
      | '>' -> Gt :: aux (pos + 1)
      | '`' -> Backtick :: aux (pos + 1)
      | '\n' ->
        if pos + 1 < len && source.[pos + 1] = '\n' then
          Blank_line :: aux (pos + 2)
        else
          Newline :: aux (pos + 1)
      | '#' ->
        let rec count_hashes p = 
          if p < len && source.[p] = '#' then count_hashes(p+1)
          else p
        in 
        let end_pos = count_hashes pos in
        let level = end_pos - pos in
        Heading_marker level :: aux end_pos
      | '-' ->
        if pos + 2 < len && source.[pos + 1] = '-' && source.[pos + 2] = '-' then
          Triple_dash :: aux (pos + 3)
        else
          Text "-" :: aux (pos + 1)
      | '@' ->
        let rec read_word p = 
          if p < len && source.[p] <> ' ' && source.[p] <> '\n' then read_word(p+1)
          else p
        in 
        let word_end = read_word(pos+1) in
        let word = String.sub source (pos + 1 ) (word_end - pos - 1) in
        (match word with
        | "end" -> Code_end :: aux word_end
        | "code" ->
          let lang =
            if word_end < len && source.[word_end] = ' ' then
              let lang_start = word_end + 1 in
              let rec read_lang p =
                if p < len && source.[p] <> '\n' then read_lang (p + 1)
                else p
              in
              let lang_end = read_lang lang_start in
              Some (String.sub source lang_start (lang_end - lang_start), lang_end)
            else
              None
          in
          let start_pos = match lang with
            | Some (_, p) -> p
            | None -> word_end
          in
          let start_pos = if start_pos < len && source.[start_pos] = '\n' then start_pos + 1 else start_pos in
          let rec find_end p =
            if p + 3 < len && source.[p] = '@' && source.[p+1] = 'e' && source.[p+2] = 'n' && source.[p+3] = 'd' then p
            else if p >= len then p
            else find_end (p + 1)
          in
          let end_pos = find_end start_pos in
          let code = String.sub source start_pos (end_pos - start_pos) in
          let lang_opt = match lang with Some (l, _) -> Some l | None -> None in
          let after_end = if end_pos + 4 <= len then end_pos + 4 else len in
          Code_start lang_opt :: Text code :: Code_end :: aux after_end        
          | _ -> Text ("@" ^ word) :: aux word_end)
      | _ ->
        let rec read_text p =
          if p < len &&
           source.[p] <> '*' && source.[p] <> '$' &&
           source.[p] <> '[' && source.[p] <> ']' &&
           source.[p] <> '(' && source.[p] <> ')' &&
           source.[p] <> '!' && source.[p] <> '>' &&
           source.[p] <> '#' && source.[p] <> '-' &&
           source.[p] <> '@' && source.[p] <> '`' &&
           source.[p] <> '\n'
        then read_text (p + 1)
        else p
    in
    let end_pos = read_text pos in
    let s = String.sub source pos (end_pos - pos) in
    Text s :: aux end_pos
  in 
  aux 0
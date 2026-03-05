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
    | At_block of string * string option * string option
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
        let known_blocks = ["code"; "math"; "manim"] in
        let rec read_word p = 
          if p < len && source.[p] <> ' ' && source.[p] <> '\n' && source.[p] <> '(' then read_word(p+1)
          else p
        in 
        let word_end = read_word(pos+1) in
        let word = String.sub source (pos + 1) (word_end - pos - 1) in
        if List.mem word known_blocks then
          (* Read optional (args) *)
          let (args, after_args) =
            if word_end < len && source.[word_end] = '(' then
              let rec find_close p =
                if p >= len then (None, word_end)
                else if source.[p] = ')' then
                  let arg = String.trim (String.sub source (word_end + 1) (p - word_end - 1)) in
                  (Some arg, p + 1)
                else find_close (p + 1)
              in
              find_close (word_end + 1)
            else
              (None, word_end)
          in
          (* Skip whitespace and one newline to get to body start *)
          let body_start =
            let p = ref after_args in
            while !p < len && source.[!p] = ' ' do incr p done;
            if !p < len && source.[!p] = '\n' then incr p;
            !p
          in
          (* Scan for @end at start of line, stop if we hit another known block keyword *)
          let at_line_start p =
            p = 0 || (p > 0 && source.[p - 1] = '\n')
          in
          let rec find_end p =
            if p >= len then None
            else if source.[p] = '@' && at_line_start p then
              let kw_end = read_word (p + 1) in
              let kw = String.sub source (p + 1) (kw_end - p - 1) in
              if kw = "end" then Some p
              else if List.mem kw known_blocks then None
              else find_end (p + 1)
            else find_end (p + 1)
          in
          (match find_end body_start with
          | Some end_pos ->
            let body = String.sub source body_start (end_pos - body_start) in
            let after_end = end_pos + 4 in
            let after_end = if after_end < len && source.[after_end] = '\n' then after_end + 1 else after_end in
            At_block(word, args, Some body) :: aux after_end
          | None ->
            (match args with
            | Some _ -> At_block(word, args, None) :: aux after_args
            | None -> Text ("@" ^ word) :: aux word_end))
        else if word = "end" then
          (* Stray @end — skip *)
          aux word_end
        else
          Text ("@" ^ word) :: aux word_end
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
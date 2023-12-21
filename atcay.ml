let is_vowel ch =
    match ch with
    | 'a' | 'e' | 'i' | 'o' | 'u'
    | 'A' | 'E' | 'I' | 'O' | 'U'
    -> true
    | _ -> false


let is_consonant ch =
    match ch with
    | 'b' | 'c' | 'd' | 'f' | 'g' | 'h' | 'j' | 'k' | 'l' | 'm' | 'n' | 'p' | 'q' | 'r' | 's' | 't' | 'v' | 'w' 
    | 'B' | 'C' | 'D' | 'F' | 'G' | 'H' | 'J' | 'K' | 'L' | 'M' | 'N' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'V' | 'W' 
    -> true
    | _ -> false


let starting_cons word =
    let s_len = String.length word in
    let rec aux w idx =
        if s_len <= idx then idx else
        match is_vowel w.[idx] with
        | true -> idx
        | false -> aux w (idx + 1)
    in aux word 0


let pig_latin_vow_rule word =
    word ^ "way"


let pig_latin_con_rule word =
    let len = starting_cons word in
    let start = String.sub word 0 len in
    let rest = String.sub word len (String.length word - len) in
    rest ^ start ^ "ay"


let pig_latin word =
    if String.length word = 0 then " " else
    match (is_vowel (String.get word 0), is_consonant (String.get word 0)) with
    | (true, _) -> pig_latin_vow_rule word
    | (_, true) -> pig_latin_con_rule word
    | _ -> word


let () = 
    let channels = match Array.length Sys.argv with
    | 1 -> [| In_channel.stdin |]
    | _ -> 
        let rest = Array.sub Sys.argv 1 ((Array.length Sys.argv) - 1) in
        Array.map open_in rest
    in

    let rec read_channel ch =
        let _ = Out_channel.flush in
        match try Some (input_line ch) with End_of_file -> None with
        | None -> ()
        | Some line -> 
            let pig_list = (String.split_on_char ' ' line)
                |> List.map pig_latin in
            let _ = List.iter (Printf.printf "%s ") pig_list in
            let _ = Printf.printf "\b\n" in
            read_channel ch in
    let _ = Array.iter read_channel channels in

    Array.iter close_in channels

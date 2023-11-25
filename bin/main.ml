open Stdio 

let rec last l = 
    match l with
    | [] -> None 
    | last :: [] -> Some last 
    | _ :: tail -> last tail
    
(* let print_option = function  *)
(*     | None -> print_endline "None" *)
(*     | Some x -> String.concat "" ["Some " ; x] |> print_endline *)

let rec last_two l =
    match l with 
    | [] -> None 
    | first :: second :: [] -> Some (first, second)
    | _ :: tail -> last_two tail

let rec at n l = 
    match l with 
    | [] -> None 
    | head :: tail -> 
            match n with 
            | 0 -> None
            | 1 -> Some head
            | _ -> at (n-1) tail

let rec length l = 
    match l with 
    | [] -> 0 
    | _ :: tail -> 1 + length tail 

let rec rev l = 
    match l with
    | [] -> [] 
    | head :: tail -> (rev tail) @ [head]

let is_palindrome l = List.rev l = l 

type 'a node = 
    | One of 'a
    | Many of 'a node list 

let rec flatten l = 
    match l with 
    | [] -> [] 
    | One head :: tail -> head :: flatten tail
    | Many list :: tail -> flatten (list @ tail)

let rec compress = function 
    | [] -> [] 
    | [x] -> [x]
    | x :: y :: tail -> if x = y then 
        compress (x :: tail) 
    else x :: compress (y :: tail)

let pack list = 
    let rec aux packed_list repeats = function
        | [] -> repeats :: packed_list 
        | x :: xs -> (
            match repeats with 
            | [] -> aux packed_list (x :: repeats) xs 
            | y :: _ -> if x = y then 
                aux packed_list (x :: repeats) xs 
            else aux (repeats :: packed_list) [x] xs )
    in
    List.rev (aux [] [] list)

let encode list = 
    let packed_list = pack list in 
    let rec aux = function 
        | [] -> [] 
        | [] :: _ -> [] 
        | (value :: _ as hd) :: tail -> (List.length hd, value) :: aux tail 
    in 
    aux packed_list 

type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let mod_encode list = 
    let packed_list = pack list in 
    let rec aux = function 
        | [] -> [] 
        | [] :: _ -> [] 
        | (value :: _ as hd) :: tail -> 
                let length = List.length hd in 
                match length with 
                | 1 -> One value :: aux tail
                | _ -> Many (length, value) :: aux tail
    in 
    aux packed_list 

let rec decode = 
    let rec decode_single = function
        | One x -> [x]
        | Many (a, x) -> 
                match a with 
                | 1 -> [x] 
                | _ -> x :: (Many (a-1, x) |> decode_single)
    in function
    | [] -> [] 
    | rle :: tail -> decode_single rle @ decode tail 

(* let encode list =  *)
(*     let rec aux count x xs =  *)
(*         let result () =  *)
(*             (match count with  *)
(*             | 0 -> One x *)
(*             | _ -> Many (count, x)) *)
(*         in  *)
(*         match xs with  *)
(*         | [] -> [result ()] *)
(*         | y :: ys -> if x = y then  *)
(*             aux (count + 1) x ys  *)
(*         else  *)
(*             result () :: aux 1 y ys  *)
(*     in  *)
(*     match list with  *)
(*     | [] -> []  *)
(*     | y :: ys -> aux 1 y ys  *)

let rec duplicate = function 
    | [] -> [] 
    | x :: xs -> x :: x :: duplicate xs 

let rec replicate list n = 
    let rec aux x n = 
        match n with 
        | 0 -> [] 
        | n -> x :: aux x (n-1) 
    in 
    match list with 
    | [] -> []
    | x :: xs -> aux x n @ replicate xs n 

let drop list n = 
    let rec drop' list counter = 
        match list with 
        | [] -> [] 
        | x :: xs -> if counter = n then 
            drop' xs 1 
        else 
            x :: drop' xs (counter + 1)
    in 
    drop' list 1

let split list n = 
    let rec aux first second n = 
        match n with 
        | 0 -> (first, second)
        | _ -> (
            match second with 
            | [] -> (first, [])
            | x :: xs -> aux (first @ [x]) xs (n-1)
        )
    in 
    aux [] list n 

let slice list n m = 
    let rec slice' list counter = 
        match list with 
        | [] -> [] 
        | x :: xs -> if counter < n then 
            slice' xs (counter + 1)
        else if counter <= m then x :: slice' xs (counter + 1)
        else [] 
    in
    slice' list 0

let rotate list n = 
    let modulo n m = 
        (m + (n mod m) mod m)
    in 
    let rec rotate' list' counter = 
        match list' with 
        | [] -> [] 
        | x :: xs -> if counter < modulo n (length list)
        then rotate' (xs @ [x]) (counter + 1)
        else list' 
    in 
    rotate' list 0 

let rec remove_at n list = 
    match list with 
    | [] -> [] 
    | x :: xs -> if n > 0 then 
        x :: remove_at (n-1) xs 
    else xs 

let rec insert_at insert n list = 
    if n > 0 then 
        match list with 
        | [] -> []
        | x :: xs -> x :: insert_at insert (n-1) xs 
    else insert :: list 

let () = 
    let _ = last ["a" ; "b" ; "c" ; "d"] in
    let _ = last_two ["a" ; "b" ; "c" ; "d"] in
    let _ = at 3 ["a"; "b"; "c"; "d"; "e"] in 
    let _ = length ["a"; "b"; "c"] in
    let _ = rev ["a"; "b"; "c"] in 
    let _ = is_palindrome ["x"; "a"; "m"; "a"; "x"] in 
    let _ = flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]] in 
    let _ = compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] in 
    let _ = pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"] in
    let _ = encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] in 
    let _ = mod_encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] in 
    let _ = decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")] in 
    let _ = encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] in 
    let _ = duplicate ["a"; "b"; "c"; "c"; "d"] in 
    let _ = replicate ["a"; "b"; "c"] 3 in 
    let _ = drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3 in 
    let (_, _) = split ["a"; "b"; "c"; "d"] 5 in 
    let _ = slice ["b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 7 in 
    let _ = rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2) in
    let _ = remove_at 1 ["a"; "b"; "c"; "d"] in 
    let list = insert_at "alfa" 4 ["a"; "b"; "c"; "d"] in 
    list |> List.iter (printf "%s ");
    ()

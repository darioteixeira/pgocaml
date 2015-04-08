module String =
struct

  include String

  let starts_with str prefix =
    let len = length prefix in
    if length str < len
    then
      false
    else
      let rec aux i =
        if i >= len
        then true
        else if unsafe_get str i <> unsafe_get prefix i
        then false
        else aux (i + 1) in
      aux 0

  let join = concat

  let implode xs =
    let buf = Buffer.create (List.length xs) in
    List.iter (Buffer.add_char buf) xs;
    Buffer.contents buf

  let fold_left f init str =
    let len = length str in
    let rec loop i accum =
      if i = len
      then accum
      else loop (i + 1) (f accum str.[i]) in
    loop 0 init

  (* Only available in the standard library since OCaml 4.02 *)
  let init n f =
    let s = Bytes.create n in
    for i = 0 to n - 1 do
      Bytes.unsafe_set s i (f i)
    done;
    Bytes.to_string s
end

module Option =
struct
  let default v = function
    | Some v -> v
    | None -> v

  let get = function
    | Some v -> v
    | None -> invalid_arg "PGOCaml_aux.Option.get"

  let map f = function
    | Some v -> Some (f v)
    | None -> None
end

module List =
struct
  include List

  let iteri f xs =
    let rec loop i = function
      | [] -> ()
      | hd :: tl -> f i hd; loop (i+1) tl in
    loop 0 xs

  let mapi f xs =
    let rec loop i = function
      | [] -> []
      | hd :: tl -> let hd' = f i hd in hd' :: loop (i+1) tl in
    loop 0 xs
end

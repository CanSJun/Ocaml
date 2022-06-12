module F = Format
type t = (Env.addr * Value.t) list

let pp fmt(s: t) : unit =
F.fprintf fmt "[%a]"
(F.pp_print_list ~pp_sep:(fun fmt() -> F.fprintf fmt "; ")
(fun fmt (a, v) -> F.fprintf fmt "a%d %a" a Value.pp v )) s

let empty = []

        let mem(x:Env.addr) (s:t) : bool =
               let rec cycle orin =
                       match orin with
                       |[] -> false
                       |(i,_) :: t -> if i = x then 
                               true 
                       else
                               cycle t
               in
               cycle s

 let add (x : Env.addr) (v : Value.t) (s : t) : t =
            let rec dupli who : t =
                    match who with
                    | [] -> []
                    | (i,j) :: z ->
                               if i = x then
                                     dupli(z)
                               else
                                   (i,j) :: dupli(z)
            in
             (x, v) ::   dupli s

        let find (x : Env.addr) (s : t) : Value.t =
                let rec search origin =
                    match origin with
                    | [] -> failwith ("Uninitialized address.")
                    | (i,j) :: [] -> if i = x then j else failwith ("Uninitialized address.")
                    | (i,j) :: z ->if i = x then j else search z
               in
             search s

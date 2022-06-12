module F = Format
type addr = int
type t = (string * addr) list



let empty = []

        let mem(x:string) (s:t) : bool =
               let rec cycle orin =
                       match orin with
                       |[] -> false
                       |(i,_) :: t -> if i = x then 
                               true 
                       else
                               cycle t
               in
               cycle s
 let add (x : string) (v : addr) (s : t) : t =
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
        let find (x : string) (s : t) : addr =
                let rec search origin =
                    match origin with
                    | [] -> failwith ("Free identifier: " ^ x)
                    | (i,j) :: [] -> if i = x then j else failwith ("Free identifier: " ^ x)
                    | (i,j) :: z ->
                                    if i = x then
                                         j
                                    else
                                    search z
               in
             search s



let pp fmt(s: t) : unit =
F.fprintf fmt "[%a]"
(F.pp_print_list ~pp_sep:(fun fmt() -> F.fprintf fmt "; ")
(fun fmt (x, a ) -> F.fprintf fmt "%s a%d" x a )) s



let index = ref 0
let new_addr () = 
let nindex = !index in
let _ = index := nindex + 1 in nindex





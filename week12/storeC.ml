module F = Format
type t = (string * value) list
         and value = NumV of int | ClosureV of string * AstC.expr * t
        let empty = []

let rec pp_value fmt (v:value) : unit =
        match v with
        |NumV n -> F.fprintf fmt "%d" n
        |ClosureV(x, e, s ) -> F.fprintf fmt " <λ%s. %a, %a>" x AstC.pp e pp_store s

and pp_store fmt(s: t) :unit = 
        F.fprintf fmt "[%a]"
        (F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt "; ")
        (fun fmt(x, v) -> F.fprintf fmt "%s -> %a" x pp_value v)) s


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
        let add (x : string) (v : value) (s : t) : t =
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
        
        let find (x : string) (s : t) : value =
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



        type t = (string * Value.t) list

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
        let add (x : string) (v : Value.t) (s : t) : t =
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
        
        let find (x : string) (s : t) : Value.t =
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


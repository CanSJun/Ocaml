type t = (string *(string list* Ast.expr)) list
let empty : t = []
let mem (x : string) ( fs : t ) : bool =
        let rec search n =
                match n with 
                |[] -> false
                |(m,_) ::z -> if m = x then 
                        true
                        else
                        search z
        in

    search fs

let add (x: string) (pl : string list) ( e : Ast.expr) (fs : t) : t =
        let rec search n  =
                match n with
                |[] ->[]
                |(i,j) :: z ->
                                if i = x then 
                                   search (z)
                                else
                                (i,j) :: search(z)
        in
        (x,(pl,e)) :: search fs
  
let find (x : string) (fs : t) : (string list * Ast.expr) =
        let rec search n =
               match n with
               |[] -> failwith ("Free identifier: " ^ x )
               |(i,(o,j)) :: [] -> if i = x then (o, j) else
                       failwith("Free identifier: " ^ x)
               |(i,(o,j)) :: z ->
                               if i = x then
                                      (o,j)
                               else
                                   
                                       search z
        in
        search fs

    


module ListSet = struct

        
        type t = int list
        let empty :t = []
        let add(elem : int) (set : t) : t =
            let rec dupli n = 
                    match n with 
                    | [] -> []
                    | x :: y ->
                               if x = elem then
                                    dupli(y)
                               else
                                   x :: dupli(y)           
            in   
              elem::  dupli set
             
         let union (set1 : t) (set2 : t) : t =
                 let rec result (x : int) (y : t) : t =
                match y with
                |[] -> []
                |n :: [] -> if n = x then [] else [n]
                |n::t -> 
                                if n = x then t
                                else n ::  result x t
                in
         let rec search origin other  =
                 match origin with
                 | [] -> []
                 | x1 :: [] -> result x1 other
                 | x1 :: t -> 
                                 search t (result x1 other)

               in
   set1 @ (search set1 set2)

         let intersection (set1 : t) (set2 : t ) : t =
                         let rec search origin other = 
                                 match origin with
                                 | [] -> []
                                 | n :: [] -> if List.mem n other then [n] else []
                                 | n :: t ->
                                             if List.mem n other then
                                                 n :: search t other
                                                 else
                                                   search t other
                         in
                         search set1 set2

     let relative_complement (set1 : t) (set2 : t ) : t =
                         let rec search origin other =
                                 match origin with
                                 | [] -> []
                                 | n :: [] -> if List.mem n other then [] else [n]
                                 | n :: t ->
                                             if List.mem n other then
                                                 search t other
                                                 else
                                                 n ::  search t other
                         in
                         (search set1 set2 ) @ (search set2 set1) 
                         
end


module ListMap = struct
        type t = (string * int) list
        let empty : t = []
        let add (key : string) ( value : int) (map : t) : t =
            let rec dupli who : t =
                    match who with
                   | [] -> []
                    | (x,y) :: z ->
                               if x = key then
                                     dupli(z)
                               else
                                   (x,y) :: dupli(z)
            in
             (key, value) ::   dupli map

        let get (key : string) ( map : t) : int =
                let rec search origin =
                    match origin with
                    | [] -> failwith "No such key here."
                    | (x,y) :: [] -> if x = key then y else failwith "No such key here."
                    | (x,y) :: z ->
                                    if x = key then
                                         y
                                    else
                                            search z
                in
             search map
        let remove (key : string) ( map : t) : t =
                let rec search origin =
                    match origin with
                    | [] -> []
                    | (x,y) :: z ->
                                    if x = key then
                                         search z
                                    else
                                          (x,y):: search z
                in
                if List.mem_assoc key map then
             search map
                else
                 failwith "No such key here."

                 let values (map : t) : ListSet.t =

                    let rec search n form = 
                    match form with
                    | [] -> []
                    | (x,y) :: z -> 
                                    if y = n then 
                                            search n z
                                    else
                                         (x,y) :: search n z
                   in
                 let rec value ori =
                       match ori with
                       | [] -> []
                       | (_,y) :: z -> y :: (value (search y z) )
                                             
                 in

                 value map
end


type value = Int of int | String of string
type node = N of value * node list

let  preorder (n : node) : value list =
        let rec cycle ori =
                match ori with
                | [] -> []
                | N(y,z) :: t->y :: cycle z @ cycle t

       in
       match n with
    
       |N(x,y)  -> x :: cycle y
      

   let  postorder (n : node) : value list =
        let rec cycle ori =
                match ori with
                | [] -> []
                | N(y,z) :: t-> cycle z @ (y :: cycle t)

       in
       match n with

       |N(x,y)  ->  cycle y @ [x]

  let  levelorder (n : node) : value list = 
               let rec cycle ori curr =

                match ori with
                | [] -> []
             | N(x,y) :: t ->
                             if curr > 0 then
                                x:: cycle y 1 @ cycle t 1
                             else       
                                     cycle y 1 @ cycle t 0
        in
       let rec test ori =
          match ori with
       |[] -> []
       | N(x,_) :: t -> x ::  test t 
       in
      match n with
 | N ( x, y ) ->x :: test y @ cycle y 0
                        

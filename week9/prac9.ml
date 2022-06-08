
let interp_fundef (fd : Ast.fundef) (fs :Fstore.t) : Fstore.t =

            
match fd with
|FunDef(x,pl,e) -> 
Fstore.add x pl e fs  



let rec interp_expr (e: Ast.expr) (fs:Fstore.t)(s:Store.t) : Value.t =
            match e with
          | Num n -> NumV n
          | Add(e1,e2) ->
                        begin
                                let n1 = interp_expr e1 fs s in
                                let n2 = interp_expr e2 fs s in
                                match n1, n2 with
                                |NumV x, NumV y ->NumV(x+y)    
                        end
          | Sub(e1,e2) ->
                          begin
                                  let n1 = interp_expr e1 fs s in
                                  let n2 = interp_expr e2 fs s in
                                  match n1, n2 with
                                  |NumV x, NumV y ->NumV(x-y)         
                          end
          |Id x -> Store.find x s
          |Call(x,e1) ->
                        begin
                         let rec oc1 i =
                                match i with
                               |[] -> []
                               | z::t -> [interp_expr z fs s] @  oc1 t 
                         in
                         let n1 = oc1 e1 in
                         let lam = Fstore.find x fs in
                         let rec oc2 i j check = 
                                 match i, j with
                                 |[] , [] -> []
                                 |[] , _::_ -> failwith ("Unmatched numbers of arguments: " ^ string_of_int(List.length(n1)) ^ " <> " ^ string_of_int(List.length(check)) )
                                 |_::_ , [] -> failwith ("Unmatched numbers of arguments: " ^ string_of_int(List.length(n1)) ^ " <> " ^ string_of_int(List.length(check)) )
                                 | x :: t ,y :: tt -> 
                                                 (x,y) :: oc2 t tt check


                                            
                        in
                         match lam with
                         |(x1,e1) -> let memory =  oc2 x1 n1 x1 in
                         interp_expr e1 fs memory      
                                        
                    
                    
                         end
          |LetIn(x,e1,e2) -> begin
                       let n1 = interp_expr e1 fs s in
                       let p = Store.add x n1 s in
                       let n2 =interp_expr e2 fs p in n2
                        end
        
let interp (p : Ast.prog) : Value.t =
             let rec def n m = 
                     match n with
                     |[]->[]
                     | x :: [] -> interp_fundef x m 
                     | x :: t -> 
                               let m2 = interp_fundef x m in
                             
                               def t m2  
             in
        match p with
        |Ast.Prog(x,y)->
                  
              let n1 = def x [] in
                     
              let n2= interp_expr y n1 [] in n2
                                 
        

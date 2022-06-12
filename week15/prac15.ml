
let rec interp_expr (p : Ast.expr)( env : Env.t)(mem : Memory.t) : Value.t =
        match p with
        |Name n ->  Memory.find((Env.find n env)) mem 
        |Num n -> NumV n
        |Bool n -> BoolV n
        |Ref n -> AddrV(Env.find n env)
        | Add(e1,e2) ->
                        begin
                                let n1 = interp_expr e1 env mem in
                                let n2 = interp_expr e2 env mem in
                                match n1, n2 with
                                |NumV x, NumV y ->NumV(x+y)
                                | _ , _ -> failwith "Not a number."
                         end
         | Sub(e1,e2) ->
                          begin
                                  let n1 = interp_expr e1 env mem in
                                  let n2 = interp_expr e2 env mem in
                                  match n1, n2 with
                                  |NumV x, NumV y ->NumV(x-y)
                                  | _ , _ -> failwith "Not a number."
                          end
	|Lt(e1,e2) -> 
                         begin
                                  let n1 = interp_expr e1 env mem in
                                  let n2 = interp_expr e2 env mem in
                                  match n1, n2 with
                                  |NumV x, NumV y ->
                                                  begin
                                                       if x < y then
                                                              BoolV(true)
                                                        else
                                                               BoolV(false)

                                                  end
                                  | _ , _ -> failwith "Not a number."
                          end
	|Gt(e1,e2) ->                          begin
                                  let n1 = interp_expr e1 env mem in
                                  let n2 = interp_expr e2  env mem in
                                  match n1, n2 with
                                  |NumV x, NumV y ->
                                                  begin
                                                        if x > y then
                                                               BoolV(true)
                                                        else
                                                                BoolV(false)

                                                  end
                                  | _ , _ -> failwith "Not a number."
                          end
                      
	|Eq(e1,e2) -> 
                                                  begin
                                  let n1 = interp_expr e1 env mem in
                                  let n2 = interp_expr e2  env mem in
                                  match n1, n2 with
                                  |NumV x, NumV y ->
                                                  begin
                                                        if x = y then
                                                               BoolV(true)
                                                        else
                                                                BoolV(false)

                                                  end
                                  |BoolV x, BoolV y ->
                                                  begin
                                                        if x = y then
                                                               BoolV(true)
                                                        else
                                                                BoolV(false)

                                                  end

                                  | _ , _ -> BoolV(false)
                          end
	|And(e1,e2) ->
                        begin
                                  let n1 = interp_expr e1 env mem in
                                  let n2 = interp_expr e2  env mem in
                                  match n1, n2 with
                                  |BoolV x, BoolV y ->
                                                  BoolV(x&&y)
                                                  
                                  | _ , _ -> failwith "Not a bool."
                                                  
                        end
        
        |Or(e1,e2) ->
                        begin
                                  let n1 = interp_expr e1 env mem in
                                  let n2 = interp_expr e2  env mem in
                                  match n1, n2 with
                                  |BoolV x, BoolV y ->BoolV(x||y)



                                  | _ , _ -> failwith "Not a bool."

                        end


let rec interp_stmt(e : Ast.stmt) (env: Env.t)(mem : Memory.t) : Env.t  * Memory.t= 
                                 
   let rec memory current (store, memo)= 
           match current with 
          | x :: t ->  memory t (interp_stmt x store memo)  
          | [] -> (store, memo)
    in
                  match e with 
                  |VarDeclStmt x ->
                       
                                 let new_address = Env.new_addr() in
                                 ((Env.add x new_address env), mem)
                  |LoadStmt(x, e) ->
                                begin

                                        
                                 let sign = Env.find x env in
                                        
                                
                                 
                                 let a = interp_expr e env mem in
                                 match a with
                                 |AddrV l -> 
                                        
                                                 let check = Memory.find l mem in
                                               
                                               (env, (Memory.add sign check mem))
                                                  

                                 |_ -> failwith "Not an address."

    
                                end
                                        
                  |StoreStmt(e1, e2) -> 
		         
                                 begin
                               
                                 let a = interp_expr e1 env mem in
                                 let v = interp_expr e2 env mem in
                                        match a with
                                        |AddrV l ->  (env, (Memory.add l v mem))
                                        | _ -> failwith "Not an address."
		end
  	|IfStmt (e, s1,s2) ->
                                   begin
                                   let bl = interp_expr e env mem in
                                   match bl with
                                   |BoolV(true) ->  let ( _ ,  m1 ) = memory s1 (env, mem) in (env, m1)
                                   |BoolV(false) ->  let (_,  m1) =  memory s2 (env, mem) in (env, m1)
                                   | _ ->failwith "Not a bool."



                                end
 	 |WhileStmt(e, s) -> 
                              begin
                                  let c = interp_expr e env mem in
                                match c with
                                 |BoolV(false) -> (env, mem)
                                 |BoolV(true)->
                                                 begin
                                                            let (_,y) =  memory s (env, mem) in
                                                            interp_stmt (WhileStmt(e,s)) env y                
                                                
                                                 end
                                 | _ ->failwith "Not a bool."
		end

let interp_prog (e: Ast.prog) : Env.t * Memory.t = 
match e with
|Program n -> 
                                                    begin
                                                        let rec memory current (env, mem) =
                                                                match current with
                                                                | x :: t ->  memory t (interp_stmt x env mem)
                                                                | [] -> (env,mem)
                                                        in
                                                         memory n (Env.empty, Memory.empty)
                                                   end


let rec interp_expr (p : Ast.expr)( s : Store.t) : Value.t =
        match p with
        |Name n -> Store.find n s 
        |Num n -> NumV n
	|Bool n -> BoolV n
        | Add(e1,e2) ->
                        begin
                                let n1 = interp_expr e1 s in
                                let n2 = interp_expr e2 s in
                                match n1, n2 with
                                |NumV x, NumV y ->NumV(x+y)
                                | _ , _ -> failwith "Not a number."
                         end
         | Sub(e1,e2) ->
                          begin
                                  let n1 = interp_expr e1 s in
                                  let n2 = interp_expr e2  s in
                                  match n1, n2 with
                                  |NumV x, NumV y ->NumV(x-y)
                                  | _ , _ -> failwith "Not a number."
                          end
	|Lt(e1,e2) -> 
                         begin
                                  let n1 = interp_expr e1 s in
                                  let n2 = interp_expr e2  s in
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
                                  let n1 = interp_expr e1 s in
                                  let n2 = interp_expr e2  s in
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
                                  let n1 = interp_expr e1 s in
                                  let n2 = interp_expr e2  s in
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
                                  let n1 = interp_expr e1 s in
                                  let n2 = interp_expr e2  s in
                                  match n1, n2 with
                                  |BoolV x, BoolV y ->
                                                  BoolV(x&&y)
                                                  
                                  | _ , _ -> failwith "Not a bool."
                                                  
                        end
        
        |Or(e1,e2) ->
                        begin
                                  let n1 = interp_expr e1 s in
                                  let n2 = interp_expr e2  s in
                                  match n1, n2 with
                                  |BoolV x, BoolV y ->BoolV(x||y)



                                  | _ , _ -> failwith "Not a bool."

                        end


let rec interp_stmt(e : Ast.stmt) (s: Store.t) : Store.t = 
                   match e with 
                   |AssignStmt (x,e ) -> 
                                   let v = interp_expr e s in
                                   Store.add x v s
                   |IfStmt (e, s1,s2) ->
                                   begin
                                   let bl = interp_expr e s in
                                   match bl with
                                   |BoolV(true) -> 
                                                   begin 
                                                        let rec memory current memo= 
                                                                match current with 
                                                                | x :: t ->  memory t (interp_stmt x memo)  
                                                                | [] -> memo
                                                        in
                                                         memory s1 s
                                                   end


                                   |BoolV(false) ->   
                                                   begin
                                                        let rec memory current memo=
                                                                match current with
                                                                | x :: t ->  memory t (interp_stmt x memo)
                                                                | [] -> memo
                                                        in
                                                         memory s2 s
                                                   end 

                                     | _ ->failwith "Not a bool."



                                end
let interp_prog (e: Ast.prog) : Store.t = 
match e with
|Program n -> 
                                                    begin
                                                        let rec memory current memo=
                                                                match current with
                                                                | x :: t ->  memory t (interp_stmt x memo)
                                                                | [] -> memo
                                                        in
                                                         memory n []
                                                   end


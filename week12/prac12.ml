
let rec interp (p : Ast.expr)( s : Store.t) : Store.value =
        match p with
        |Num n -> NumV n
          | Add(e1,e2) ->
                        begin
                                let n1 = interp e1 s in
                                let n2 = interp e2 s in
                                match n1, n2 with
                                |NumV x, NumV y ->NumV(x+y)
                                | _ , _ -> failwith "Not a number."
                         end
          | Sub(e1,e2) ->
                          begin
                                  let n1 = interp e1 s in
                                  let n2 = interp e2  s in
                                  match n1, n2 with
                                  |NumV x, NumV y ->NumV(x-y)
                                  | _ , _ -> failwith "Not a number."
                          end
          |Id x ->Store.find x s
          |LetIn(x,e1,e2) -> begin
                       let n1 = interp e1 s in
                       let p = Store.add x n1 s in
                       let n2 =interp e2 p in n2
                       end

           |App(e1,e2) ->
                        begin
                        let v1 = interp e2 s in 
                        let _e1 = interp e1 s in
                        match _e1 with
                        |NumV _ -> failwith "Not a function."
                        |BoolV _ -> failwith "Not a function."
                        |ClosureV(x,y,z) ->
                                        begin
                                            let memory = Store.add x v1 z in
                                                interp y memory
                                        end       
                        end
           |Lambda (x, e1) -> ClosureV(x,e1,s) 
           |Bool x -> BoolV(x)
           |Cond(e1,e2,e3)-> 
                           begin
                             let check = interp e1 s in
                               match check with 
                               | BoolV n ->
                                               if n = true then
                                                    interp e2 s 
                                               else
                                                     interp e3 s

                               | _  -> failwith "Not a bool."
                           end
           |LessThan(e1,e2) ->
                           begin
                           let n1 = interp e1 s in
                           let n2 = interp e2 s in
                           match n1,n2 with
                           |NumV x, NumV y -> if x < y then BoolV(true) else BoolV(false)
                           | _ , _ -> failwith "Not a number."

                           end

let rec interp_c (p : AstC.expr)( s : StoreC.t) : StoreC.value =
        match p with
        |Num n -> NumV n
          | Add(e1,e2) ->
                        begin
                                let n1 = interp_c e1 s in
                                let n2 = interp_c e2 s in
                                match n1, n2 with
                                |NumV x, NumV y ->NumV(x+y)
                                | _ , _ -> failwith "Not a number."
                         end
          | Sub(e1,e2) ->
                          begin
                                  let n1 = interp_c e1 s in
                                  let n2 = interp_c e2  s in
                                  match n1, n2 with
                                  |NumV x, NumV y ->NumV(x-y)
                                  | _ , _ -> failwith "Not a number."
                          end
          |Id x ->StoreC.find x s
          |LetIn(x,e1,e2) -> begin
                       let n1 = interp_c e1 s in
                       let p = StoreC.add x n1 s in
                       let n2 =interp_c e2 p in n2
                       end

           |App(e1,e2) ->
                        begin
                        let v1 = interp_c e2 s in 
                        let _e1 = interp_c e1 s in
                        match _e1 with
                        |NumV _ -> failwith "Not a function."
                        |ClosureV(x,y,z) ->
                                        begin
                                            let memory = StoreC.add x v1 z in
                                                interp_c y memory
                                        end       
                        end
           |Lambda (x, e1) -> ClosureV(x,e1,s) 
           |Cond(e1,e2,e3)-> 
                          begin
                                    let n = interp_c e1 s in 
                                      if n <> NumV( 0 )then
                                            interp_c e2 s 
                           else
                                            interp_c e3 s 
                          end
                           
           |LessThan(e1,e2) ->
                           begin
                           let n1 = interp_c  e1 s in
                           let n2 = interp_c e2 s in
                             match n1, n2 with
                           |NumV x, NumV y ->
                                                        if x < y then 
                                                             NumV(1)
                                                        else
                                                             NumV(0)
                           | _ , _ -> failwith "Not a number."
           

                           end
let rec interp_fp (p : AstFP.expr)( s : StoreFP.t) : StoreFP.value =
        match p with
        |Num n -> NumV n
          | Add(e1,e2) ->
                        begin
                                let n1 = interp_fp e1 s in
                                let n2 = interp_fp e2 s in
                                match n1, n2 with
                                |NumV x, NumV y ->NumV(x+y)
                                | _ , _ -> failwith "Not a number."
                         end
          | Sub(e1,e2) ->
                          begin
                                  let n1 = interp_fp e1 s in
                                  let n2 = interp_fp e2  s in
                                  match n1, n2 with
                                  |NumV x, NumV y ->NumV(x-y)
                                  | _ , _ -> failwith "Not a number."
                          end
          |Id x ->StoreFP.find x s
          |LetIn(x,e1,e2) -> begin
                       let n1 = interp_fp e1 s in
                       let p = StoreFP.add x n1 s in
                       let n2 =interp_FP e2 p in n2
                       end

           |App(e1,e2) ->
                        begin
                        let v1 = interp_fp e2 s in 
                        let _e1 = interp_fp e1 s in
                        match _e1 with
                        |NumV _ -> failwith "Not a function."
                        |ClosureV(x,y,z) ->
                                        begin
                                            let memory = StoreFP.add x v1 z in
                                                interp_fp y memory
                                        end       
                        end
           |Lambda (x, e1) -> ClosureV(x,e1,s) 
           |LessThan(e1,e2) ->
                           begin
                           let n1 = interp_fp  e1 s in
                           let n2 = interp_fp e2 s in
                             match n1, n2 with
                           |NumV x, NumV y ->
                                                        if x < y then 
                                                          ClosureV("x",Lambda("y", Id"x"),StoreFP.empty)
                                                        else
                                                          ClosureV("x",Lambda("y", Id"y"), StoreFP.empty)
                           | _ , _ -> failwith "Not a number."
           

                           end

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
                        |ClosureV(x,y,z) ->
                                        begin
                                            let memory = Store.add x v1 z in
                                                interp y memory
                                        end       
                        end
                |Lambda (x, e1) -> ClosureV(x,e1,s) 



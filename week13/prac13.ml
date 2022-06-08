
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

          |Id x ->
                       begin
                       let finding = Store.find x s in
                       match finding with
                       | FreezedV(e1,e2) -> interp e1 e2 
                       |_ -> finding 
                          end

          |LetIn(x,e1,e2) -> begin
                       let n1 = interp e1 s in
                       let p = Store.add x n1 s in
                       let n2 =interp e2 p in n2
                       end

           |App(e1,e2) ->
                        begin
                    
                        let _e1 = interp e1 s in
                        match _e1 with
                        
                        |ClosureV(x,y,z) ->
                                        begin

                                            let freezing = Store.FreezedV(e2,s) in
                                            let memory = Store.add x freezing z in
                                                interp y memory
                                       end
                                                    
                        |_ -> failwith "Not a function."

                        end
           |Lambda (x, e1) -> ClosureV(x,e1,s) 
           |LessThan(e1,e2) ->
                           begin
                           let n1 = interp  e1 s in
                           let n2 = interp e2 s in
                             match n1, n2 with
                           |NumV x, NumV y ->
                                                        if x < y then 
                                                          ClosureV("x",Lambda("y", Id"x"),Store.empty)
                                                        else
                                                          ClosureV("x",Lambda("y", Id"y"), Store.empty)
                           | _ , _ -> failwith "Not a number."
		end
            |RLetIn (x, e1,e2) ->
                            begin
                            let e_1 = interp e1 s in 
                            match e_1 with
                           |ClosureV(i,j,z) -> 
                                          let rec memory = (x, Store.ClosureV(i,j,memory)) :: z in
                                        interp e2 memory 
                           |_ -> failwith "Not a function." 

                            end


let rec interp (p : Ast.expr)( s : Store.t) : Store.value =
          | Num n -> NumV n
          | Add(e1,e2) ->
                        begin
                                let n1 = interp e1 s in
                                let n2 = interp e2 s in
                                match n1, n2 with
                                |NumV x, NumV y ->NumV(x+y)    

          | Sub(e1,e2) ->
                          begin
                                  let n1 = interp e1 s in
                                  let n2 = interp e2  s in
                                  match n1, n2 with
                                  |NumV x, NumV y ->NumV(x-y)
                          end
          |Id x -> Store.find x 
          |LetIn(x,e1,e2) -> begin
                       let n1 = interp e1 s in
                       let p = Store.add x n1 s in
                       let n2 =interp e2 p in n2
                             end

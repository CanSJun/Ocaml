module F = Format


let rec interp (e:Ast.expr) : Value.t = 
       
       (*
        let cal (x:Value.t) (y : Value.t) : int = 
                let n =
                       match x with
                       |NumV z ->z 
               
                in
                let m =
                        match y with
                        |NumV z -> z
                in
                n + m
               
                 
               
      in*)
        match e with
          | Num x -> NumV x
          | Add(x,y)->
                          let n =
                                  let x = interp(x) in 
                                  match x with
                                  |NumV n -> n
                                
                          in
                          let m = 
                                  let y = interp(y) in
                                  match y with
                                  |NumV m -> m
              
                          in
                          NumV(n+m)
         | Sub(x,y)->
                          let n =
                                  let x = interp(x) in
                                  match x with
                                  |NumV n -> n

                          in
                          let m =
                                  let y = interp(y) in
                                  match y with
                                  |NumV m -> m

                          in
                          NumV(n-m)
                       
                       

let lang1 lst =
    (* Check if the list consists of only '0' and '1' *)
    List.for_all (fun c -> c = '0' || c = '1') lst
  
  let lang2 lst = 
    (* Example: match pattern 0?1* (optional 0, followed by any number of 1's) *)
    match lst with
    | [] -> true
    | '0' :: rest | rest -> List.for_all (fun c -> c = '1') rest
  
  let lang3 lst = 
    (* Example: match pattern 0[01]*0 (starts and ends with 0) *)
    match lst with
    | '0' :: _ -> List.for_all (fun c -> c = '0' || c = '1') lst && List.hd (List.rev lst) = '0'
    | _ -> false
  
  let lang4 lst = 
    (* Example: match pattern 0*10*10* *)
    let rec check lst count =
      match lst with
      | [] -> count = 2
      | '1' :: rest -> check rest (count + 1)
      | '0' :: rest -> check rest count
      | _ -> false
    in
    check lst 0
  
  let lang5 lst =
    (* Example: match pattern (00|11)+ *)
    let rec check lst =
      match lst with
      | '0' :: '0' :: rest -> check rest
      | '1' :: '1' :: rest -> check rest
      | [] -> true
      | _ -> false
    in
    check lst
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  

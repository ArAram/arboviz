type pos = {x : float; y : int; offset : float}

type t = Node of string * t list * pos


module FloatMap = Util.FloatMap




(* val prefix_fold : t list -> ('a -> string * t list * pos -> 'a) -> 'a -> 'a *)
let prefix_fold initial_stack f state =
  let rec bf_rec stack state =
    match stack with
    | [] -> state
    | h :: t ->
      let Node (name, sons, pos) = h in
      let new_state = f state (name, sons, pos) in
      bf_rec (sons @ t) new_state
  in
  bf_rec initial_stack state






(* val queue_stack_of_tree : t -> (t * int) tt list *)
(*
	on donne l'arbre et il nous envoi une liste de type :
			(Node (name, [], {x = 0.; y = depth; offset = 0.}), nbFils) list
*)
let queue_stack_of_tree tree =
  let rec traversal to_visit current_level next_level stack =
    if MyQueue.is_empty to_visit then
      begin
        if MyQueue.is_empty next_level then
          (current_level :: stack)
        else
          traversal
            next_level
            MyQueue.empty
            MyQueue.empty
            (current_level :: stack)
      end
    else
      let (Node (name, sons, pos), depth), to_visit' = MyQueue.pop to_visit in
      let node = Node (name, [], {x = 0.; y = depth; offset = 0. }) in                      (* ************ *)
      match sons with
      | [] ->
        begin
          traversal to_visit'
            (MyQueue.push current_level (node,0))
            next_level stack
        end
      | _ ->
        begin
          let next_level' = List.fold_left (fun q x -> MyQueue.push q (x,(depth+1)))
            next_level sons in
          traversal to_visit' (MyQueue.push current_level (node, List.length sons))
            next_level' stack
        end
  in
  traversal (MyQueue.push MyQueue.empty (tree, 0)) MyQueue.empty MyQueue.empty []


let offset_genere name =	
	if ((float_of_int (String.length name)) *. 0.08) < 1.
		then 1.
		else (float_of_int (String.length name)) *. 0.08
	



(* val pos_tree_of_queue_stack : (t * int) tt list * float FloatMap.t * float FloatMap.t -> t *)
let pos_tree_of_queue_stack state =
  let rec aux  prev_level (stack, nexts, offsets) =
    match stack with
    | [] -> let (res,_) = MyQueue.pop prev_level in res
    | q :: t ->
      begin
	 let prev_level', nexts', offsets' =

          MyQueue.fold
            (fun (z, nexts, offsets) (Node (name, _, pos), arity) ->
              (
                if arity = 0 then (
		  		
                  let depth = pos.y in
                  let y = depth in
                  let x = Util.get_in_map nexts depth +. offset_genere name in
                  let self_offset = (max (Util.get_in_map offsets depth)
                                       ((Util.get_in_map nexts depth) -. x)) +. offset_genere name in
		  print_string ("THEN self_offset \t  " ^ name ^ "\t" ^ string_of_float(self_offset) ^ " \n");
                  let offsets' = FloatMap.add depth self_offset offsets in
                  let nexts' = FloatMap.add depth (x  +. offset_genere name) nexts in
                  let node = Node (name, [], {x = x; y = y; offset = self_offset }) in
                  (* print_string ("PAS FILS " ^ name ^ " \n"); *)
		  ((MyQueue.push z node), nexts', offsets')

                ) else (

                  let depth = pos.y in
                  let sons, q' = MyQueue.npop z arity in
                  let Node (_, _, pos_first) = List.hd sons in
                  let Node (_, _, pos_last) = List.hd (List.rev sons) in
                  let place = (pos_first.x +. pos_last.x ) /. 2. in
                  let self_offset =  (max (Util.get_in_map offsets depth)
                                        ((Util.get_in_map nexts depth) -. place )) +. offset_genere name in
		  print_string ("ELSE self_offset \t  " ^ name ^ "\t" ^ string_of_float(self_offset) ^ " \n");
                  let offsets' = FloatMap.add depth self_offset offsets in
                  let x = place +. self_offset in
                  let nexts' = FloatMap.add depth (x  +. offset_genere name) nexts in
                  let node = Node (name, sons, {x = x; y = depth; offset = self_offset}) in
			(* print_string ("A DES FILS " ^ name ^ " \n"); *)
                  ((MyQueue.push q' node ), nexts', offsets')

	        )
             ) 
           ) 
           (prev_level, nexts, offsets)
           q
	    
        in
        aux prev_level' (t, nexts', offsets')

      end
  in
  aux MyQueue.empty state







(* val first_pass : t -> t *)
let first_pass tree =
  let stack = queue_stack_of_tree tree in
  pos_tree_of_queue_stack (stack, FloatMap.empty, FloatMap.empty)




(* val offsum_stack_of_tree : t -> (t * int) list * int * float *)
let offsum_stack_of_tree tree =
  let rec traversal to_visit visited height width =
    match to_visit with
    | [] -> visited, height, width
    | (Node (name, sons, pos), offsum) :: t ->
      begin
        let x = pos.x +. offsum  in
        let offsum' = offsum +. pos.offset  in                                  (* *********************************** *)
        let height' = max pos.y height in
        let width' = max x width in
        let node = Node (name, [], {x = x; y = pos.y; offset = pos.offset}) in
        let sons_with_offsum = List.map (fun x -> (x,offsum')) sons in
        traversal (sons_with_offsum @ t) ((node, List.length sons) :: visited) height' width'
      end
  in
  traversal [(tree, 0.)] [] 0 0.






(* val pos_tree_of_offsum_stack : (t * int) list * 'a * 'b -> 'b * 'a * t *)
let pos_tree_of_offsum_stack state =
  let rec aux s stack =
    match stack with
    | [] -> List.hd s
    | h :: t ->
      begin
        let (node, nb_sons) = h in
        let Node (name, _, pos) = node in
        if nb_sons = 0 then
          aux (node :: s) t
        else
          let sons, s' = Util.npop nb_sons s in
          let node = Node (name, sons, pos) in
          aux (node :: s') t
      end
  in
  let (stack, height, width) = state in
  (width, height, aux [] stack)




(* val second_pass : t -> float * int * t *)
let second_pass tree =
  pos_tree_of_offsum_stack (offsum_stack_of_tree tree)




(* val pos_tree_of_tree : t -> float * int * t *)
let pos_tree_of_tree tree =
  second_pass (first_pass tree)

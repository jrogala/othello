(* SIMULATION JEU *)


#open "graphics";;

let aide = ref true;;
let push p a = p := !p@[a];;
let fst (a,b,c) = a;;
let snd (a,b,c) = b;;
let thd (a,b,c) = c;;
let primo (a,b) = a;;
let deuxio (a,b) = b;;

let copie t =
	let c,l = vect_length t, vect_length t.(0) in
	let s = make_matrix c l t.(0).(0) in
	for i = 0 to c-1 do
		for j = 0 to l-1 do
			s.(i).(j) <- t.(i).(j)
		done;
	done;
	s;;

let init () =
	let board = make_matrix 12 12 0 in
	for i = 1 to 10 do
		for j = 1 to 10 do
			board.(i).(j) <- 0
		done;
	done;
	for i = 0 to 11 do
		board.(0).(i) <- 50;
		board.(i).(0) <- 50;
		board.(11).(i) <- 50;
		board.(i).(11) <- 50
	done;
	
	board.(5).(5) <- -1;
	board.(6).(5) <- 1;
	board.(6).(6) <- -1;
	board.(5).(6) <- 1;
	board
	;;

let init_board () =
	open_graph "683x600+683";
   clear_graph ();
	let plateau = init () in
   set_color blue;
   fill_rect 60 60 500 500;
   let x = vect_length plateau-1 in
   let y = vect_length plateau.(0)-1 in
   set_color green;
   for i = 1 to x-1 do
      for j = 1 to y-1 do
      	if (j + i) mod 2 = 0 then
         fill_rect (10+50 *i) (10+50 * j) (50) (50)
      done;
   done;
   set_color magenta;
   fill_rect (10+50*12) (10+50*3) (50) (50);
   set_color yellow;
   fill_rect (10+50*12) (10+50*5) (50) (50);
   plateau;;


let refresh plateau =
   for i = 1 to 10 do
      for j = 1 to 10 do
         if plateau.(i).(j) = -1 then
            (set_color white;
               fill_circle (35 + 50 * i) (35 + 50 * j) 10)
         else
         if plateau.(i).(j) = 1 then
            (set_color black;
               fill_circle (35 + 50 * i) (35 + 50 * j) 10)
         else
         if (i+j) mod 2 = 0 then
         	   set_color green
  			 else
            	set_color blue;
         fill_circle (35 + 50 * i) (35 + 50 * j) 10
      done;
   done;
;;

let souris = function() ->
	  let e = wait_next_event [Button_down] in 
	  let x = (e.mouse_x)/50
	  and y = (e.mouse_y)/50 in
	  (x,y)
	  ;;

let click_on_pion x y board = 
		if x > 10 or y > 10 or x < 1 or y < 1 then (false,x,y)
		else
		if board.(x).(y) = -1 then (false,x,y)
		else
		if board.(x).(y) = 1 then (false,x,y)
		else
		if board.(x).(y) = 50 then (false,x,y)
		else
		(true,x,y);;

let test_case player x y ok board = 
		if ok = true then
			(if board.(x+1).(y) = 0 &&
				board.(x+1).(y+1) = 0 &&
				board.(x+1).(y-1) = 0 &&
				board.(x).(y) = 0 &&
				board.(x).(y+1) = 0 &&
				board.(x).(y-1) = 0 &&
				board.(x-1).(y) = 0 &&
				board.(x-1).(y+1) = 0 &&
				board.(x-1).(y-1) = 0
			then
				(false,x,y)
			else
				(true,x,y))
		else
			(false,x,y);;

let middlex board a b player y =
	
	let ma = max a b in
	let mi = min a b in
		for i = mi to ma do
				board.(i).(y) <- player
		done;
	board;;

let middley board a b player x =
	let ma = max a b in
	let mi = min a b in
		for i = mi to ma do
				board.(x).(i) <- player
		done; board
		;;

let middled board xo yo xf yf player =
			if xo < xf && yo < yf
	then	for i = 0 to xf - xo do
				board.(xo+i).(yo+i) <- player
			done
	else if xo > xf && yo > yf
	then	for i = 0 to xo - xf do
				board.(xo-i).(yo-i) <- player
			done
	else if xo > xf && yo < yf
	then	for i = 0 to xo - xf do
				board.(xo-i).(yo+i) <- player
			done
	else if xo < xf && yo > yf
	then	for i = 0 to xf - xo do
				board.(xo+i).(yo-i) <- player
			done; board
	;;

let middle board xo yo xf yf player =
	if yo = yf then middlex board xo xf player yo
	else if xo = xf then middley board yo yf player xo
	else middled board xo yo xf yf player;;

let verifx1 x y board player=
	let i = ref 1 in
	while board.(x + !i).(y) = (-player) do
		i := !i +1;
	done;
	if board.(x + !i).(y) = player && !i <> 1 then (true,x+ !i,y) else (false,x,y); 
	;;

let verifx2 x y board player=
	let i = ref 1 in
	while board.(x - !i).(y) = (-player) do
		i := !i +1
	done;
	if board.(x - !i).(y) = player && !i <> 1 then (true,x- !i,y) else (false,x,y); 
	;;


let verify1 x y board player=
	let i = ref 1 in
	while board.(x).(y + !i) = (-player) do
		i := !i +1
	done;
	if board.(x).(y + !i) = player && !i <> 1 then (true,x,y+ !i) else (false,x,y); 
	;;

let verify2 x y board player=
	let i = ref 1 in
	while board.(x).(y - !i) = (-player) do
		i := !i +1
	done;
	if board.(x).(y - !i) = player && !i <> 1 then (true,x,y- !i) else (false,x,y); 
	;;

let verifd1 x y board player=
	let i = ref 1 in
	while board.(x + !i).(y + !i) = (-player) do
		i := !i +1
	done;
	if board.(x+ !i).(y + !i) = player && !i <> 1 then (true,x+ !i,y+ !i) else (false,x,y); 
	;;

let verifd2 x y board player=
	let i = ref 1 in
	while board.(x - !i).(y - !i) = (-player) do
		i := !i +1
	done;
	if board.(x - !i).(y - !i) = player && !i <> 1 then (true,x- !i,y- !i) else (false,x,y); 
	;;

let verifd3 x y board player=
	let i = ref 1 in
	while board.(x - !i).(y + !i) = (-player) do
		i := !i +1
	done;
	if board.(x - !i).(y + !i) = player && !i <> 1 then (true,x- !i,y+ !i) else (false,x,y); 
	;;

let verifd4 x y board player=
	let i = ref 1 in
	while board.(x + !i).(y - !i) = (-player) do
		i := !i +1
	done;
	if board.(x + !i).(y - !i) = player && !i <> 1 then (true,x+ !i,y- !i) else (false,x,y); 
	;;

let verif x y board player = 
	let p = ref [] in
	push p (verifx1 x y board player);
	push p (verifx2 x y board player);
	push p (verify1 x y board player);
	push p (verify2 x y board player);
	push p (verifd1 x y board player);
	push p (verifd2 x y board player);
	push p (verifd3 x y board player);
	push p (verifd4 x y board player);
	
	!p;;

let attaque x y player board = 
	let ok = fst (test_case player x y (fst (click_on_pion x y board)) board) in
	let board_f = ref (copie board) in
	let flag = ref false in
	if ok = true then 
	 
		begin
			let p = verif x y board player in
			let rec aux p = match p with
				|[] -> ()
				|t::q -> (
					if (fst t) = true then
						begin 
						board_f := middle !board_f x y (snd t) (thd t) player;
						flag := true
						end;
					aux q) in
			aux p
		end;
	!board_f,!flag
	;;



let coup_possible player board = 
	let p = ref [] in
	let n = vect_length board in
	for i = 1 to n-2 do
		for j = 1 to n-2 do
			if board.(i).(j) = 0 then
				if deuxio (attaque i j player board) = true then
					p := (i,j):: !p
		done;
	done;!p;;

coup_possible 1 [|[||]|] ;;

let nombre_pion board = 
	let li = vect_length (board) in
	let co = vect_length (board.(0)) in
	let blanc = ref 0 in
	let noir = ref 0 in
	let autre = ref 0 in
	for i = 1 to li-2 do
		for j = 1 to co-2 do
			if board.(i).(j) = 1 then
				incr blanc
			else if board.(i).(j) = (-1) then
				incr noir
			else 
				incr autre
		done;
	done;!blanc, !noir,!autre;;

let play j1 j2 =
	let plateau = init_board () in
	let rec jeux player board =
		refresh board;
		let b,n,a = nombre_pion board in
		if a = 0 or b=0 or n=0then 
			b,n
		else
			if player = 1 then
				let (x,y) = j1 player board in
				if x <> -1 then(
					jeux (-player) (primo (attaque x y player board))
					)
				else
					jeux (-player) (board)
			else
				begin
				let (x,y) = j2 player board in
				if x <> -1 then(
					jeux (-player) (primo (attaque x y player board))
					)
				else
					jeux (-player) (board)
				end
	in jeux 1 plateau;;







(* IA MAINTENANT *)



let ia_alea player board = 
	let coup = coup_possible player board in
	let rec recup n l = match l with
		|[] -> failwith "error"
		|t::q -> if n = 0 then t else recup (n-1) q
	in 
	if list_length coup = 0 then (-1,-1)
	else
		let n = random__int (list_length coup) in
		recup n coup
	;;




let ia_simple player board = 
	let coup = coup_possible player board in
	let rec aux p (xf,yf) m = match p with
		|[] -> 
			if xf <> -1 then
				(xf,yf)
			else
				(-1,-1)
		|(x,y)::q -> 
				let coup_adv = list_length (coup_possible (-player) (primo (attaque x y player board))) in
				if coup_adv < m then
					aux q (x,y) coup_adv
				else
					aux q (xf,yf) m
		in aux coup (-1,-1) 99;;

let ia_simple player board = 
	let coup = coup_possible player board in
	let rec aux coup x y score = match coup with
		|[] -> x,y
		|(x1,y1)::q -> 
				let b,n,a = nombre_pion (primo (attaque x y player board)) in
				if (n-b)*player > score then aux q x1 y1 ((n-b)*player)
				else aux q x y score
	in aux coup (-1) (-1) (-max_int);;


let joueur_interactif player board = 
	let (x,y) = souris () in
	let coup = coup_possible player board in
	let rec aux p = match p with
		|[] -> false
		|(x1,y1)::q -> if x = x1 & y = y1 then
				true
			else
				aux q
	in 
		let ok = aux coup in
			if ok = true then 
				(x,y)
			else
				(-1,-1);;


let ia_better player board =
	let h = 4 in
	let rec note x y plbuff board h =
		let b,n,a = nombre_pion board in
		if h <= 0 or a = 0 or b=0 or n= 0then
			if a = 0 or b=0 or n=0 then (n-b)*player*999
			else (n-b)*player
		else
			let plateau = primo (attaque x y player board) in
			let x1,y1 = ia_simple player plateau in
			note x1 y1 (-plbuff) plateau (h-1)
	in 
	let p = coup_possible player board in
	let rec aux p n x y = match p with
		|[] -> x,y
		|(x1,y1)::q -> let nprime = note x1 y1 player board h in
			if (nprime) > n then aux q nprime x1 y1
			else aux q n x y
	in aux p (-max_int) (-1) (-1);;


let new_tas () = make_vect 400 [];;

let minimum tas =
	let n = vect_length tas in
	let i = ref 0 in
		while !i < n-1 & tas.(!i) = [] do
			incr i
		done;
		!i;;

let pop tas =
	let i = minimum tas in
	if i = vect_length tas -1 then [|[||]|]
	else
	(
	match tas.(i) with
		|[] -> failwith "tas vide"
		|t::q -> tas.(i) <- q;
					t
	)
		;;

let push tas prio t = 
	let n = vect_length tas in
	if prio >= n then 
		tas.(n-1) <- t::(tas.(n-1))
	else
	if prio < 0 then
		tas.(0) <- t::(tas.(0))
	else
		tas.(prio) <- t::(tas.(prio))
	;;


let heur_1 board player = 
	let b,n,a = nombre_pion board in
	200-(n-b)*player
	;;

let heur_2 board player =
	list_length (coup_possible player board)
	;;

let heur_3 board player = 
	let b,n,a = nombre_pion board in
	if b * player >= n * player then
		heur_1 board player
	else
		heur_2 board player
	;;

let add t = tim := t::!tim;;


let ia_dijkstra note player board =
	let f = sys__time() in
	let h = 20 in
	let pile = new_tas () in
	let rec parcours p joueur h = 
		if h= 0 then minimum pile
		else(
			let rec aux p = match p with
				|[] -> parcours (coup_possible (-joueur) (pop pile)) (-joueur) (h-1)
				|(x1,y1)::q -> push pile (note board player) (primo (attaque x1 y1 player board)) ; aux q
			in
			aux p)
	in
	let p = coup_possible player board in
	let rec final p x y m = match p with
		|[] -> 
			add (sys__time() -. f);
			x,y
		|(x1,y1)::q -> 
			let mprim = parcours (coup_possible (-player) (primo (attaque x1 y1 player board))) player h in
		if m < mprim
		then final q x1 y1 mprim
		else final q x y m
	in
	final p (0) (0) (-999);;

let tim = ref [];;

let reset() = tim := [];;
let moyenne() =
	let n = float_of_int (list_length !tim) in
	let rec aux t = match t with
		|[] -> 0.
		|t::q -> t +. (aux q)
	in (aux !tim) /. n;;

reset();;

let m = ref 0;;
for i = 0 to 50 do
	m := !m + primo(play (ia_simple) (ia_simple))
done;;

play joueur_interactif joueur_interactif;;



open TTFData

type glyf_path = {
	gp_type : int;
	gp_x : float;
	gp_y : float;
	gp_cx : float;
	gp_cy : float;
}

let mk_path t x y cx cy = {
	gp_type = t;
	gp_x = x;
	gp_y = y;
	gp_cx = cx;
	gp_cy = cy;
}

type simple_point = {
	x : float;
	y : float;
}

let build_paths ctx relative g =
	let len = Array.length g.gs_x_coordinates in
	let current_end = ref 0 in
	let end_pts = Array.init len (fun i ->
		if g.gs_end_pts_of_contours.(!current_end) = i then begin
			incr current_end;
			true
		end else
			false
	) in
	let is_on i = g.gs_flags.(i) land 0x01 <> 0 in
	let is_end i = end_pts.(i) in
	let arr = DynArray.create () in
	let last_added = ref {
		x = 0.0;
		y = 0.0;
	} in
	let add_rel t x y cx cy =
		let p = match t with
			| 0 ->
				mk_path t x y cx cy
			| 1 ->
				mk_path t (x -. !last_added.x) (y -. !last_added.y) cx cy
			| 2 ->
				mk_path t (x -. cx) (y -. cy) (cx -. !last_added.x) (cy -. !last_added.y)
			| _ ->
				assert false
		in
		last_added := { x = x; y = y; };
		DynArray.add arr p
	in
	let add_abs t x y cx cy = DynArray.add arr (mk_path t x y cx cy) in
	let add = if relative then add_rel else add_abs in
	let flush pl =
		let rec flush pl = match pl with
		| c :: a :: [] ->
			add 2 a.x a.y c.x c.y;
		| a :: [] ->
			add 1 a.x a.y 0.0 0.0;
		| c1 :: c2 :: pl ->
			add 2 (c1.x +. (c2.x -. c1.x) /. 2.0) (c1.y +. (c2.y -. c1.y) /. 2.0) c1.x c1.y;
			flush (c2 :: pl)
		| _ ->
			Printf.printf "Fail, len: %i\n" (List.length pl);
		in
		flush (List.rev pl)
	in
	let last = ref { x = 0.0; y = 0.0; } in
	let rec loop new_contour pl index p =
		let p = {
			x = p.x +. float_of_int g.gs_x_coordinates.(index);
			y = p.y +. float_of_int g.gs_y_coordinates.(index);
		} in
		let is_on = is_on index in
		let is_end = is_end index in
		let loop pl =
			if is_end then begin
				flush (!last :: pl);
			end;
			if index + 1 < len then
				loop is_end pl (index + 1) p;
		in
		if new_contour then begin
			last := p;
			add 0 p.x p.y 0.0 0.0;
			if is_on then begin
				loop []
			end else begin
				Printf.printf "Found off-curve starting point, not sure what to do\n";
				loop [p]
			end
		end else begin
			if not is_on then
				loop (p :: pl)
			else begin
				flush (p :: pl);
				loop []
			end
		end
	in
	loop true [] 0 !last;
	DynArray.to_list arr
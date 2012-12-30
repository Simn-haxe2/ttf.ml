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

let build_paths relative g =
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

let map_char_code cc c4 =
	let index = ref 0 in
	let seg_count = c4.c4_seg_count_x2 / 2 in
	if cc >= 0xFFFF then 0 else begin
		for i = 0 to seg_count - 1 do
			if c4.c4_end_code.(i) >= cc && c4.c4_start_code.(i) <= cc then begin
				if c4.c4_id_range_offset.(i) > 0 then
					let v = c4.c4_id_range_offset.(i)/2 + cc - c4.c4_start_code.(i) - seg_count + i in
					index := c4.c4_glyph_index_array.(v)
				else
					index := (c4.c4_id_delta.(i) + cc) mod 65536
			end
		done;
		!index
	end

let parse_range_str str =
	let len = String.length str in
	let last = ref str.[0] in
	let offset = ref 1 in
	let lut = Hashtbl.create 0 in
	while !offset < len do
		let cur = str.[!offset] in
		begin match cur with
		| '-' when !last = '\\' ->
			Hashtbl.replace lut (Char.code '-') true;
			incr offset;
		| c when !offset = len - 1 ->
			Hashtbl.replace lut (Char.code !last) true;
			Hashtbl.replace lut (Char.code cur) true;
			incr offset
		| '-' ->
			let first, last = match Char.code !last, Char.code str.[!offset + 1] with
				| first,last when first > last -> last,first
				| first,last -> first,last
			in
			for i = first to last do
				Hashtbl.add lut i true
			done;
			offset := !offset + 2;
		| c ->
			Hashtbl.replace lut (Char.code !last) true;
			incr offset;
		end;
		last := cur;
	done;
	lut

let build_lut ttf range_str =
	let lut = Hashtbl.create 0 in
	Hashtbl.add lut 0 0;
	Hashtbl.add lut 1 1;
	Hashtbl.add lut 2 2;
	let add_character = if range_str = "" then
			fun k v -> Hashtbl.replace lut k v
		else begin
			let range = parse_range_str range_str in
			fun k v -> if Hashtbl.mem range k then Hashtbl.replace lut k v
		end
	in
	let make_cmap4_map c4 =
		let seg_count = c4.c4_seg_count_x2 / 2 in
		for i = 0 to seg_count - 1 do
			for j = c4.c4_start_code.(i) to c4.c4_end_code.(i) do
				let index = map_char_code j c4 in
				add_character j index;
			done;
		done
	in
(*  	let make_cmap12_map c12 =
		List.iter (fun group ->
			let rec loop cc gi =
				add_character cc gi;
				if cc < (Int32.to_int group.c12g_end_char_code) then loop (cc + 1) (gi + 1)
			in
			loop (Int32.to_int group.c12g_start_char_code) (Int32.to_int group.c12g_start_glyph_code)
		) c12.c12_groups
	in *)
	List.iter (fun st -> match st.cs_def with
		| Cmap0 c0 ->
			Array.iteri (fun i c -> add_character i (int_of_char c)) c0.c0_glyph_index_array;
		| Cmap4 c4 ->
			make_cmap4_map c4;
		| Cmap12 c12 ->
			(*
				TODO: this causes an exception with some fonts:
				Fatal error: exception IO.Overflow("write_ui16")
			*)
			(* make_cmap12_map ctx lut c12; *)
			()
		| _ ->
			(* TODO *)
			()
	) ttf.ttf_cmap.cmap_subtables;
	lut
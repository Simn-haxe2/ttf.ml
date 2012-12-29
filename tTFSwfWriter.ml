open TTFData
open Swf

let _nbits x =
	if x = 0 then
		0
	else
		let x = ref x in
		if !x < 0 then x := -1 * !x;
		let nbits = ref 0 in
		while !x > 0 do
			x := !x lsr 1;
			incr nbits;
		done;
		!nbits + 1

let round x = int_of_float (floor (x +. 0.5))

let to_twips v = round (v *. 20.)

type ctx = {
	ttf : ttf;
	add_character : int -> int -> unit;
}

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

let build_paths ctx g =
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
	let add t x y cx cy =
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

let begin_fill =
	SRStyleChange {
		scsr_move = None;
		scsr_fs0 = Some(1);
		scsr_fs1 = None;
		scsr_ls = None;
		scsr_new_styles = None;
	}

let end_fill =
	SRStyleChange {
		scsr_move = None;
		scsr_fs0 = None;
		scsr_fs1 = None;
		scsr_ls = None;
		scsr_new_styles = None;
	}

let align_bits x nbits = x land ((1 lsl nbits ) - 1)

let move_to ctx x y =
	let x = to_twips x in
	let y = to_twips y in
	let nbits = max (_nbits x) (_nbits y) in
	SRStyleChange {
		scsr_move = Some (nbits, align_bits x nbits, align_bits y nbits);
		scsr_fs0 = Some(1);
		scsr_fs1 = None;
		scsr_ls = None;
		scsr_new_styles = None;
	}

let line_to ctx x y =
	let x = to_twips x in
	let y = to_twips y in
	let nbits = max (_nbits x) (_nbits y) in
	SRStraightEdge {
		sser_nbits = nbits;
		sser_line = (if x = 0 then None else Some(align_bits x nbits)), (if y = 0 then None else Some(align_bits y nbits));
	}

let curve_to ctx cx cy ax ay =
	let cx = to_twips cx in
	let cy = to_twips cy in
	let ax = to_twips ax in
	let ay = to_twips ay in
	let nbits = max (max (_nbits cx) (_nbits cy)) (max (_nbits ax) (_nbits ay)) in
	SRCurvedEdge {
		scer_nbits = nbits;
		scer_cx = align_bits cx nbits;
		scer_cy = align_bits cy nbits;
		scer_ax = align_bits ax nbits;
		scer_ay = align_bits ay nbits;
	}

let write_paths ctx paths =
	let scale = 1024. /. (float_of_int ctx.ttf.ttf_head.hd_units_per_em) in
	let srl = DynArray.create () in
	List.iter (fun path ->
		try
			DynArray.add srl (match path.gp_type with
			| 0 -> move_to ctx (path.gp_x *. scale) ((-1.) *. path.gp_y *. scale);
			| 1 -> line_to ctx (path.gp_x *. scale) ((-1.) *. path.gp_y *. scale);
			| 2 -> curve_to ctx (path.gp_cx *. scale) ((-1.) *. path.gp_cy *. scale) (path.gp_x *. scale) ((-1.) *. path.gp_y *. scale);
			| _ -> assert false)
		with Exit ->
			()
	) paths;
	DynArray.add srl (end_fill);
	{
		srs_nfbits = 1;
		srs_nlbits = 0;
		srs_records = DynArray.to_list srl;
	}

let write_glyph ctx key glyf =
	match glyf with
	| TglyfSimple (h,g) ->
		let path = build_paths ctx g in
		{
			font_char_code = key;
			font_shape = write_paths ctx path;
		}
	| TglyfComposite (h,g) ->
		{
			font_char_code = key;
			font_shape = write_paths ctx [];
		}
	| TGlyfNull ->
		{
			font_char_code = key;
			font_shape = write_paths ctx [];
		}

let write_font_layout ctx lut =
	let scale = 1024. /. (float_of_int ctx.ttf.ttf_head.hd_units_per_em) in
	(* check for shorter ocaml *)
	let hmtx = Array.of_list ctx.ttf.ttf_hmtx in
	let hmtx = Hashtbl.fold (fun k v acc -> (k,hmtx.(v)) :: acc) lut [] in
	let hmtx = List.stable_sort (fun a b -> compare (fst a) (fst b)) hmtx in
	let hmtx = List.map (fun (k,g) -> g) hmtx in
	{
			font_ascent = round((float_of_int ctx.ttf.ttf_os2.os2_us_win_ascent) *. scale *. 20.);
			font_descent = round((float_of_int ctx.ttf.ttf_os2.os2_us_win_descent) *. scale *. 20.);
			font_leading = round(((float_of_int(ctx.ttf.ttf_os2.os2_us_win_ascent + ctx.ttf.ttf_os2.os2_us_win_descent - ctx.ttf.ttf_head.hd_units_per_em)) *. scale) *. 20.);
			font_glyphs_layout = Array.of_list( ExtList.List.mapi (fun i h ->
			{
				font_advance = round((float_of_int h.advance_width) *. scale *. 20.);
				font_bounds = {rect_nbits=0; left=0; right=0; top=0; bottom=0};
			}) hmtx );
			font_kerning = [];
	}

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

let make_cmap4_map ctx c4 =
	let seg_count = c4.c4_seg_count_x2 / 2 in
	for i = 0 to seg_count - 1 do
		for j = c4.c4_start_code.(i) to c4.c4_end_code.(i) do
			let index = map_char_code j c4 in
			ctx.add_character j index;
		done;
	done

let make_cmap12_map ctx acc c12 =
	List.iter (fun group ->
		let rec loop cc gi =
			Hashtbl.replace acc cc gi;
			if cc < (Int32.to_int group.c12g_end_char_code) then loop (cc + 1) (gi + 1)
		in
		loop (Int32.to_int group.c12g_start_char_code) (Int32.to_int group.c12g_start_glyph_code)
	) c12.c12_groups

let bi v = if v then 1 else 0

let int_from_langcode lc =
	match lc with
	| LCNone -> 0
	| LCLatin -> 1
	| LCJapanese -> 2
	| LCKorean -> 3
	| LCSimplifiedChinese -> 4
	| LCTraditionalChinese -> 5

let write_font2 ch b f2 =
	IO.write_bits b 1 (bi true);
	IO.write_bits b 1 (bi f2.font_shift_jis);
	IO.write_bits b 1 (bi f2.font_is_small);
	IO.write_bits b 1 (bi f2.font_is_ansi);
	IO.write_bits b 1 (bi f2.font_wide_offsets);
	IO.write_bits b 1 (bi f2.font_wide_codes);
	IO.write_bits b 1 (bi f2.font_is_italic);
	IO.write_bits b 1 (bi f2.font_is_bold);
	IO.write_byte ch (int_from_langcode f2.font_language);
	IO.write_byte ch (String.length f2.font_name);
	IO.nwrite ch f2.font_name;
	IO.write_ui16 ch (Array.length f2.font_glyphs);
	let glyph_offset = ref (((Array.length f2.font_glyphs) * 4)+4) in
	Array.iter (fun g ->
		IO.write_i32 ch !glyph_offset;
		glyph_offset := !glyph_offset + SwfParser.font_shape_records_length g.font_shape;
	)f2.font_glyphs;
	IO.write_i32 ch !glyph_offset;
	Array.iter (fun g -> SwfParser.write_shape_without_style ch g.font_shape;) f2.font_glyphs;
	Array.iter (fun g -> IO.write_ui16 ch g.font_char_code; )f2.font_glyphs;
	IO.write_i16 ch f2.font_layout.font_ascent;
	IO.write_i16 ch f2.font_layout.font_descent;
	IO.write_i16 ch f2.font_layout.font_leading;
	Array.iter (fun g ->
		let fa = ref g.font_advance in
		if (!fa) <  -32767 then fa := -32768;(* fix or check *)
		if (!fa) > 32766 then fa := 32767;
		IO.write_i16 ch !fa;) f2.font_layout.font_glyphs_layout;
	Array.iter (fun g -> SwfParser.write_rect ch g.font_bounds;) f2.font_layout.font_glyphs_layout;
	IO.write_ui16 ch 0 (* TODO: optional FontKerningTable *)

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

let write_swf ttf range_str =
	let lut = Hashtbl.create 0 in
	Hashtbl.add lut 0 0;
	Hashtbl.add lut 1 1;
	Hashtbl.add lut 2 2;
	let ctx = {
		ttf = ttf;
		add_character = if range_str = "" then
			fun k v -> Hashtbl.replace lut k v
		else begin
			let range = parse_range_str range_str in
			fun k v -> if Hashtbl.mem range k then Hashtbl.replace lut k v
		end;
	} in
	List.iter (fun st -> match st.cs_def with
		| Cmap0 c0 ->
			Array.iteri (fun i c -> ctx.add_character i (int_of_char c)) c0.c0_glyph_index_array;
		| Cmap4 c4 ->
			make_cmap4_map ctx c4;
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
	) ctx.ttf.ttf_cmap.cmap_subtables;

	let glyfs = Hashtbl.fold (fun k v acc -> (k,ctx.ttf.ttf_glyfs.(v)) :: acc) lut [] in
	let glyfs = List.stable_sort (fun a b -> compare (fst a) (fst b)) glyfs in
	let glyfs = List.map (fun (k,g) -> Printf.printf "Char id: %i\n" k; write_glyph ctx k g) glyfs in
	let glyfs_font_layout = write_font_layout ctx lut in
	let glyfs = Array.of_list glyfs in
	{
		font_shift_jis = false;
		font_is_small = false;
		font_is_ansi = false;
		font_wide_offsets = true;
		font_wide_codes = true;
		font_is_italic = false;
		font_is_bold = false;
		font_language = LCNone;
		font_name = ttf.ttf_font_name;
		font_glyphs = glyfs;
		font_layout = glyfs_font_layout;
	}
;;
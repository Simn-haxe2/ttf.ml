if Array.length Sys.argv < 2 then failwith "Usage: ttf [font name] [range str = \"\"]";
let fontname = Sys.argv.(1) in
let range_str = if Array.length Sys.argv < 3 then "" else Sys.argv.(2) in
let f2 = TTFSwfWriter.write_swf (TTFParser.parse (open_in_bin (fontname ^ ".ttf"))) range_str in
let ch = (IO.output_channel (open_out_bin (fontname ^ ".dat"))) in
let b = IO.output_bits ch in
IO.write_i16 ch 1;
TTFSwfWriter.write_font2 ch b f2;
IO.close_out ch;
let xml = "<?xml version=\"1.0\" ?>
<swf>
	<FileAttributes/>
	<Custom tagId=\"75\" file=\"" ^ fontname ^ ".dat\" comment=\"DefineFont3\"/>
	<SymbolClass id=\"1\" class=\"TestFont\" base=\"flash.text.Font\"/>
	<DefineABC file=\"Main.swf\" isBoot=\"true\"/>
	<ShowFrame/>
</swf>"
in
let ch = open_out_bin (fontname ^ ".fxml") in
Pervasives.output_string ch xml;
Pervasives.close_out ch;
if Sys.command "haxe -main Main -swf main.swf" <> 0 then failwith "Could not execute haxe";
if Sys.command ("hxswfml xml2swf " ^ fontname ^ ".fxml " ^ fontname ^ ".swf -no-strict") <> 0 then failwith "Could not execute hxswfml";;
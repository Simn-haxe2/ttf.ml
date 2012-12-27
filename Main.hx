extern class TestFont extends flash.text.Font{}

class Main extends flash.display.Sprite
{
	public function new()
	{
		super();
		var font = new TestFont();
		var fmt = new flash.text.TextFormat();
		fmt.size = 50;
		fmt.font = font.fontName;

		var tf = new flash.text.TextField();
		tf.embedFonts = true;
		tf.defaultTextFormat = fmt;
		tf.wordWrap = true;
		tf.multiline = true;
		tf.width = flash.Lib.current.stage.stageWidth;
		tf.height = flash.Lib.current.stage.stageHeight;
		var all = [];
		var nlAfter = new IntHash();
		nlAfter.set("9".code, true);
		nlAfter.set("Z".code, true);
		nlAfter.set("z".code, true);
		for (i in 33...0xFF)
			all.push(i);
			
		var buf = new StringBuf();
		function add(i) {
			buf.addChar(i);
			all.remove(i);
			if (nlAfter.exists(i)) {
				buf.addChar(10);
			}
		}
		for (i in '0'.code...'9'.code + 1) 
			add(i);			
		for (i in 'a'.code...'z'.code + 1) 
			add(i);
		for (i in 'A'.code...'Z'.code + 1) 
			add(i);
		for (i in all)
			add(i);
		tf.text = buf.toString();
		addChild(tf);
	}

	public static function main()
	{
		flash.Lib.current.addChild(new Main());
	}
}
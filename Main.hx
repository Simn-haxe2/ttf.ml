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
		tf.text = "Hello World\nSome more blah";
		tf.x = tf.y = 50;
		tf.autoSize = flash.text.TextFieldAutoSize.LEFT;
		//tf.border = true;
		addChild(tf);
	}

	public static function main()
	{
		flash.Lib.current.addChild(new Main());
	}
}
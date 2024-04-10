
// EXAMPLE 1

let button = chain(Button::blank)
	.chain(Tooltip::wrap)
	.chain(CursorChanger::wrap)
	.done();

let widget = CursorChanger::wrap(Tooltip::wrap(Button::blank()));

parent.insert(widget);

widget.inner.inner.text.set("my value");
widget.inner.tooltip.text.set("nice info");
widget.cc.cursor.set(BasicCursor::Question);

// EXAMPLE 2

struct MyWidget {
	button: Ws<Button>,
	tooltip: Ws<Tooltip>,
	cc: Ws<CursorChanger>,
}

let widget = MyWidget {
	button: Button::new(),
	tooltip: Tooltip::new(),
	cc: CursorChanger::new()
};

let wrapped = chain(&widget.button)
	.chain(&widget.tooltip)
	.chain(&widget.cc)

parent.insert(wrapped);

widget.button.text.set("my text");
widget.tooltip.text.set("nice ways");
widget.cc.cursor.set(BasicCursor::Question);

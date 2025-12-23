
use skrifa::{outline::{DrawSettings, OutlinePen}, MetadataProvider};
use swash::{shape::{cluster::Glyph, ShapeContext}, text::Script};

use std::{pin::Pin, slice};

pub struct Font<'a> {
    // SAFETY:
    // 1. fontref must be dropped before data
    // 2. data's allocation must never be moved
    fontref: swash::FontRef<'a>,
    skrifa: skrifa::FontRef<'a>,
    data: Pin<Box<[u8]>>,
}

impl<'a> Font<'a> {
    pub fn new(binary: &[u8]) -> Result<Self, InvalidFont> {

        // allocate the data into a buffer that won't ever move
        let data = Pin::new(Vec::from(binary).into_boxed_slice());

        // get a reference to the data which is gonna be valid forever.
        // then we create a persistent FontRef from that
        let magic = unsafe { slice::from_raw_parts(data.as_ptr(), data.len()) };
        let fontref = swash::FontRef::from_index(magic, 0).ok_or(InvalidFont)?;
        let skrifa = skrifa::FontRef::from_index(magic, 0).map_err(|_| InvalidFont)?;

        // we keep a FontRef to heap allocated data that doesn't move,
        // instead of always creating a new FontRef. Save's some lines of code :b

        Ok(Self {
            fontref,
            skrifa,
            data,
        })

    }
}

#[derive(Debug)]
pub struct InvalidFont;

pub struct TextShaper<'a> {
    fonts: Vec<Font<'a>>,
    context: ShapeContext,
}

impl<'a> TextShaper<'a> {

    pub fn new(fonts: Vec<Font<'a>>) -> Self {
        assert!(fonts.len() > 0, "must load at least one font");
        Self {
            fonts,
            context: ShapeContext::new(),
        }
    }

    pub fn shape(&mut self, input: &str) {

        let font = &self.fonts[0];
        let mut shaper = self.context.builder(font.fontref)
            .size(30.)
            .script(Script::Latin)
            .build();

        let mut glyphs: Vec<Glyph> = Vec::new();

        shaper.add_str(input);
        shaper.shape_with(|glyph| {
            glyphs.extend(glyph.glyphs);
        });

        // glyphs[0].

        use skrifa::prelude::*;

        let outlines = font.skrifa.outline_glyphs();

        #[derive(Default)]
        struct SvgPath(String);

        // Implement the OutlinePen trait for this type. This emits the appropriate
        // SVG path commands for each element type.
        impl OutlinePen for SvgPath {
            fn move_to(&mut self, x: f32, y: f32) {
                self.0.push_str(&format!("M{x:.1},{y:.1}\n"));
            }

            fn line_to(&mut self, x: f32, y: f32) {
                self.0.push_str(&format!("L{x:.1},{y:.1}\n"));
            }

            fn quad_to(&mut self, cx0: f32, cy0: f32, x: f32, y: f32) {
                self.0
                    .push_str(&format!("Q{cx0:.1},{cy0:.1} {x:.1},{y:.1}\n"));
            }

            fn curve_to(&mut self, cx0: f32, cy0: f32, cx1: f32, cy1: f32, x: f32, y: f32) {
                self.0.push_str(&format!(
                    "C{cx0:.1},{cy0:.1} {cx1:.1},{cy1:.1} {x:.1},{y:.1}\n"
                ));
            }

            fn close(&mut self) {
                self.0.push_str("Z\n\n");
            }
        }
        // Now, construct an instance of our pen.
        let mut svg_path = SvgPath::default();

        for glyph in glyphs {

            let outline = outlines.get(skrifa::GlyphId::new(glyph.id as u32)).expect("TODO: handle error");
            let settings = DrawSettings::unhinted(Size::new(30.0), LocationRef::default());

            // And draw the glyph!
            outline.draw(settings, &mut svg_path).unwrap();

            // See what we've drawn.
            println!("{}", svg_path.0);
        }

        // let outlines = font.fontref.ou();

    }

}

#[test]
fn test() {
    const DATA: &[u8] = include_bytes!("/usr/share/fonts/google-noto/NotoSans-Regular.ttf");
    let font = Font::new(DATA).expect("invalid font");
    let mut shaper = TextShaper::new(Vec::from([font]));
    shaper.shape("B");
}


use crate::*;
use common::*;
use std::ops::Range;

/// Contains the layouting algorithm for the texture atlas,
/// so it can be seperate from the actual GPU calls.
struct AtlasLayout {
    /// The current size of the atlas.
    size: PhysicalSize,
    /// The height of the current row.
    rh: i16,
    /// The position after the current slot.
    cursor: PhysicalPair,
}

impl AtlasLayout {

    pub fn new(size: PhysicalSize) -> Self {
        Self {
            size,
            rh: 0,
            cursor: PhysicalPair::ZERO,
        }
    }

    /// Find the point before the next slot of `size`. If the current
    /// layout is not big enough it will return `None`.
    ///
    /// # Errors
    /// If the slot doesn't fit, returns the overshoot. It is not specified
    /// wether the overshoot occured sideways or upwards.
    pub fn advance(&mut self, size: PhysicalSize) -> Result<PhysicalPair, i16> {

        let PhysicalSize { x, y } = size;

        if self.cursor.x + x > self.size.x {

            // If we overshoot sideways, we need to grow upwards.

            let incr = self.rh as i16;

            // Move the cursor one row up.
            self.cursor.y += incr;
            self.cursor.x = 0;
            self.rh = 0;

            if self.cursor.x + x > self.size.x {
                // If we overshoot sideways again, the object is to large.
                Err(self.cursor.x + x - self.size.x)
            } else if self.cursor.y + y > self.size.y {
                // If we go out of bounds upwards, there is no space left.
                Err(self.cursor.y + y - self.size.y)
            } else {

                // We made space.

                let result = self.cursor;

                self.cursor.x += x;
                self.rh = self.rh.max(y);

                Ok(result)

            }

        } else if self.cursor.y + y > self.size.y {
            // If we overshoot upwards immediatly, the object is to large.
            Err(self.cursor.y + y - self.size.y)
        } else {

            // This is the simple case, where we actually have space.

            let result = self.cursor;

            self.cursor.x += x;
            self.rh = self.rh.max(y);

            Ok(result)

        }

    }

}

#[test]
fn atlas_layout() {

    let mut layout = AtlasLayout::new(PhysicalSize::new(100, 100));

    // 1. 10x10 squares, all at y=0

    for idx in 0..10 {

        let pt = layout.advance(PhysicalSize::new(10, 10))
            .expect("must have enough space");

        assert_eq!(pt.y, 0, "y level must be 0");
        assert_eq!(pt.x, idx as i16 * 10, "x must increase in steps of 10");

    }

    // 2. large bar, 80x10, above the sqaures at y=10

    let pt2 = layout.advance(PhysicalSize::new(80, 10))
        .expect("must have enough space (2)");

    assert_eq!(pt2.y, 10, "large bar should be above the squares");
    assert_eq!(pt2.x, 0, "large bar should be at the start of the row");

    // 3. misc squares, above the bar at y=20

    let pt3 = layout.advance(PhysicalSize::new(30, 30)).unwrap();
    assert_eq!(pt3.y, 20);
    assert_eq!(pt3.x, 0);

    let pt4 = layout.advance(PhysicalSize::new(50, 50)).unwrap();
    assert_eq!(pt4.y, 20);
    assert_eq!(pt4.x, 30);

    let pt5 = layout.advance(PhysicalSize::new(20, 20)).unwrap();
    assert_eq!(pt5.y, 20);
    assert_eq!(pt5.x, 80);

    // 3. large bar, above the squares at y=70 (20+50)

    let pt6 = layout.advance(PhysicalSize::new(100, 20)).unwrap();
    assert_eq!(pt6.y, 70);
    assert_eq!(pt6.x, 0);

    // 4. something that shouldn't fit :b

    let inv7 = layout.advance(PhysicalSize::new(55, 15));
    assert_eq!(inv7, Err(5), "large object should not fit");

    let mut layout2 = AtlasLayout::new(PhysicalSize::new(100, 100));
    let pt21 = layout2.advance(PhysicalSize::new(100, 100))
        .expect("100x100 should fit on a 100x100 layout");
    assert_eq!(pt21.y, 0);
    assert_eq!(pt21.x, 0);

    let mut layout3 = AtlasLayout::new(PhysicalSize::new(100, 100));
    let inv31 = layout3.advance(PhysicalSize::new(101, 101));
    assert_eq!(inv31, Err(1), "101x101 should not fit on a 100x100 layout");

}

/// Used to manage textures.
///
/// Before using a texture with the renderer you have to upload it
/// through this interface.
pub struct TextureAtlas {
    /// A 2D texture storing the images.
    texture: graphics::Texture,
    /// The current layout, used to place new slots.
    layout: AtlasLayout,
    /// Which size (as a quad) we can't exceed.
    maxsize: i16,
    /// Which images we are currently storing.
    entries: Vec<TextureEntry>,
    /// This associates a `TextureIndex` with an actual
    /// position inside `entries`. We use a mapping since
    /// `entries` is reordered when upsizing the atlas.
    mapping: Vec<u16>,
}

impl TextureAtlas {

    const MININCR: i16 = 256;

    pub fn new(renderer: &Renderer) -> Self {

        let mut this = Self {
            layout: AtlasLayout::new(PhysicalSize::ZERO),
            texture: graphics::Texture::new(&renderer.gp, PhysicalSize::new(1, 1), None),
            maxsize: graphics::Texture::maxsize(&renderer.gp) as i16,
            entries: Vec::new(),
            mapping: Vec::new(),
        };

        this.upsize(renderer, Self::MININCR);

        this

    }

    pub fn texture(&self) -> &graphics::Texture {
        &self.texture
    }

    /// Write an image into the atlas.
    ///
    /// There is no concept of releasing a single image inside an atlas,
    /// so if you want to release memory you have to drop the whole atlas.
    ///
    /// However, you can update a texture once uploaded. See [`GlTextureAtlas::update`].
    ///
    /// # Panic
    /// Panics if data length and `size` don't match up.
    #[track_caller]
    pub fn upload(&mut self, renderer: &Renderer, source: &impl IsAtlasSource, size: PhysicalSize) -> TextureIndex  {

        let (index, rect) = self.alloc(renderer, size);
        source.write(&mut self.texture, rect);

        index

    }

    fn alloc(&mut self, renderer: &Renderer, size: PhysicalSize) -> (TextureIndex, PhysicalRect)  {

        // Find a slot or return an error.

        let slot = loop {
            match self.layout.advance(size) {
                Ok(slot) => break slot,
                Err(overshoot) => {
                    let incr = overshoot.max(Self::MININCR);
                    if self.layout.size.x + incr > self.maxsize ||
                       self.layout.size.y + incr > self.maxsize {
                        panic!("The texture-atlas is full.")
                   } else {
                       self.upsize(renderer, incr);
                   }
                }
            }
        };

        // Add the slot to our state and return it.

        let mapping = self.mapping.len() as u16;
        let ientry = self.entries.len() as u16;

        let (index, rect) = (
            TextureIndex { inner: mapping as u16 },
            PhysicalRect { point: slot, size }
        );

        self.entries.push(TextureEntry { rect, mapping });
        self.mapping.push(ientry);

        (index, rect)

    }

    /// Overwrite the same texture with a new image of the same size.
    ///
    /// This is not only useful, but also required in some cases, to
    /// not overuse memory. If you are e.g. playing a video or updating
    /// animated frames, they should all use the same spot in the atlas.
    #[track_caller]
    pub fn update(&mut self, index: TextureIndex, source: impl IsAtlasSource) {

        let orig = self.entries[self.mapping[index.inner as usize] as usize].rect;
        source.write(&mut self.texture, orig);

    }

    /// Copy the atlas' texture from the GPU over to the CPU.
    ///
    /// The color format is RGBA-8.
    pub fn inspect(&mut self) -> Vec<u8> {
        self.texture.inspect()
    }

    /// Get the texture coordinates for a specific index relative
    /// to the atlas texture. These coordinates are in a range from
    /// 0..5000 which map to OpenGL's 0.0 .. 1.0 texture cordinates.
    ///
    /// Also: Why the FUCK are "clipspace" and "texture" coordinates
    /// using two different coordinate systems.
    pub(crate) fn get(&self, index: TextureIndex) -> PhysicalRect {

        let orig = self.entries[self.mapping[index.inner as usize] as usize].rect;

        let x_range = 0f64 .. self.layout.size.x as f64;
        let y_range = 0f64 .. self.layout.size.x as f64;

        const TARGET_RANGE: Range<f64> = 0f64 .. 5000f64;

        PhysicalRect::new2(
            maprange(orig.point.x  as f64, x_range.clone(), TARGET_RANGE) as i16,
            maprange(orig.point.y  as f64, y_range.clone(), TARGET_RANGE) as i16,
            maprange(orig.size.x as f64, x_range.clone(), TARGET_RANGE) as i16,
            maprange(orig.size.y as f64, y_range.clone(), TARGET_RANGE) as i16
        )

    }

    fn upsize(&mut self, renderer: &Renderer, incr: i16) {

        // Create a new, bigger texture.

        let mut layout = AtlasLayout::new(PhysicalSize::new(
            self.layout.size.x + incr,
            self.layout.size.y + incr,
        ));

        let mut new = graphics::Texture::new(&renderer.gp, layout.size, None);

        // We use this chance to sort the entries, for a more
        // efficient spacial layout. We also need to update the mapping.

        self.entries.sort_unstable_by(|lhs, rhs| {
            let ls = lhs.rect.size.x as usize * lhs.rect.size.y as usize;
            let rs = rhs.rect.size.x as usize * rhs.rect.size.y as usize;
            ls.cmp(&rs)
        });

        for (idx, entry) in self.entries.iter().enumerate() {
            self.mapping[entry.mapping as usize] = idx as u16;
        }

        // Copy over the old images to the new texture.

        for entry in self.entries.iter_mut() {
            let newpos = layout.advance(entry.rect.size)
                .expect("layout must be valid, since the new entry was not added yet");
            // Copy from the original rect, still stored in the rect to the new position `newpos`.
            new.fromtex(&self.texture, entry.rect, PhysicalRect::new(newpos, entry.rect.size));
            // Make sure to update the position of the entry accordingly.
            entry.rect.point = newpos;
        }

        // After this the atlas is fully present in the new texture.

        self.texture = new;
        self.layout = layout;

    }

}

pub trait IsAtlasSource {
    fn write(&self, target: &mut graphics::Texture, dstrect: PhysicalRect);
}

impl IsAtlasSource for [u8] {
    #[track_caller]
    fn write(&self, target: &mut graphics::Texture, dstrect: PhysicalRect) {
        // Write ourself to the texture at `dstrect`.
        target.frombuf(self, dstrect);
    }
}

impl IsAtlasSource for graphics::Texture {
    #[track_caller]
        fn write(&self, target: &mut graphics::Texture, dstrect: PhysicalRect) {
        target.fromtex(self, PhysicalRect::new(PhysicalPair::ZERO, dstrect.size), dstrect);
    }
}

#[derive(Debug, Clone)]
struct TextureEntry {
    /// The position inside the atlas texture.
    pub rect: PhysicalRect,
    /// Which `mapping` stores our index. Used to update
    /// the mapping accordingly after sorting the entries.
    pub mapping: u16,
}

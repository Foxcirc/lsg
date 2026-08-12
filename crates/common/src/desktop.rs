
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub enum MouseButton {
    Left,
    Right,
    Middle,
    X1,
    X2,
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)] // TODO: derive the right traits to all those classes
pub enum Key {
    Special(SpecialKey),
    Char(char), // a-z, A-Z, 1-9, + special chars
    DeadChar(char),
    Unknown(u32)
}

impl Key {
    pub fn ismodifier(&self) -> bool {
        if let Self::Special(key) = self {
            matches!(key,
                SpecialKey::Shift | SpecialKey::Control | SpecialKey::CapsLock |
                SpecialKey::Alt   | SpecialKey::AltGr   | SpecialKey::Super
            )
        } else {
            false
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpecialKey {
    Escape,
    Tab,
    CapsLock,
    Shift,
    Control,
    Alt,
    AltGr,
    /// Windows key.
    Super,
    /// Application menu key.
    AppMenu,
    Return,
    Backspace,
    Space,
    ArrowUp,
    ArrowDown,
    ArrowLeft,
    ArrowRight,
    F1, F2, F3, F4, F5, F6,
    F7, F8, F9, F10, F11, F12,
}

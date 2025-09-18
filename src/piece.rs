
#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub enum Color {
    White,
    Black,
}

impl Color {
    pub fn opposite(&self) -> Self {
        // crate::span::Span
        match self {
            Color::White => Color::Black,
            Color::Black => Color::White,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum PieceKind {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Piece {
    pub kind: PieceKind,
    pub color: Color,
}
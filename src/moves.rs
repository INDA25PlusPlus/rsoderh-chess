//! Contains functions for calculating the valid destination positions of a
//! piece given that a starting position and that the board is empty.

use std::iter;

use crate::{Color, Piece, PieceKind, Position};

/// Given a list of translations and a source position, returns a list of
/// positions of the translations applied to the source, with invalid positions
/// filtered out.
fn translated_positions<T>(source: Position, translations: T) -> impl Iterator<Item = Position>
where
    T: IntoIterator<Item = (i8, i8)>,
{
    translations
        .into_iter()
        .filter_map(move |translation| source.translated(translation))
}

fn white_pawn_moves(from: Position) -> Vec<Position> {
    let mut positions: Vec<_> = translated_positions(from, [(0, 1), (-1, 1), (1, 1)]).collect();
    if from.row() == 1
        && let Some(position) = from.translated((0, 2)) {
            positions.push(position);
        }

    positions
}

fn white_knight_moves(from: Position) -> impl Iterator<Item = Position> {
    // Offsets:
    //    -2-10 1 2
    //  2   XX  XX
    //  1 XX      XX
    //  0     Kn
    // -1 XX      XX
    // -2   XX  XX
    translated_positions(
        from,
        [
            (1, 2),
            (2, 1),
            (2, -1),
            (1, -2),
            (-1, -2),
            (-2, -1),
            (-2, 1),
            (-1, 2),
        ],
    )
}

fn white_bishop_moves(from: Position) -> impl Iterator<Item = Position> {
    let column = from.column();
    let row = from.row();

    // Moves can be divided into four groups, each being a diagonal line:
    // Line 1: +x, +y
    let positions_north_east = (column + 1..8).zip(row + 1..8);
    // Line 2: +x, -y
    let positions_south_east = ((column + 1)..8).zip((0..row).rev());
    // Line 3: -x, -y
    let positions_south_west = (0..column).rev().zip((0..row).rev());
    // // Line 4: -x, +y
    let positions_north_west = (0..column).rev().zip(row + 1..8);

    iter::empty()
        .chain(positions_north_east)
        .chain(positions_south_east)
        .chain(positions_south_west)
        .chain(positions_north_west)
        .map(|(column, row)| {
            Position::new(column, row)
                .expect("position calculations only produce positions within the board")
        })
}

fn white_rook_moves(from: Position) -> impl Iterator<Item = Position> {
    let column = from.column();
    let row = from.row();

    // Moves can be divided into four groups, each being a horizontal/vertical line:
    // Line 3: -y
    let positions_south = iter::repeat(column).zip(0..row);
    // Line 1: +y
    let positions_north = iter::repeat(column).zip(row + 1..8);
    // Line 4: -x
    let positions_west = (0..column).zip(iter::repeat(row));
    // Line 2: +x
    let positions_east = (column + 1..8).zip(iter::repeat(row));

    iter::empty()
        .chain(positions_north)
        .chain(positions_south)
        .chain(positions_west)
        .chain(positions_east)
        .map(|(column, row)| {
            Position::new(column, row)
                .expect("position calculations only produce positions within the board")
        })
}

fn white_queen_moves(from: Position) -> impl Iterator<Item = Position> {
    white_rook_moves(from).chain(white_bishop_moves(from))
}

fn white_king_moves(from: Position) -> impl Iterator<Item = Position> {
    // Every combination where column and row is one of -1, 0, and 1:
    // (Excluding (0, 0))
    let mut positions = translated_positions(
        from,
        [
            (1, 1),
            (1, 0),
            (1, -1),
            (0, -1),
            (-1, -1),
            (-1, 0),
            (-1, 1),
            (0, 1),
        ],
    ).collect::<Vec<_>>();
    
    if from.column() == 4 && from.row() == 0 {
        positions.push(Position::new(2, 0).unwrap());
        positions.push(Position::new(6, 0).unwrap());
    }
    
    positions.into_iter()
}

/// Return "naive" list of moves a piece is allowed to do from the given position assuming the board
/// is otherwise empty.
pub fn naive_moves_from_piece(piece: Piece, from: Position) -> Vec<Position> {
    // Consider position as from white.
    let from = if piece.color == Color::Black {
        from.as_other_color()
    } else {
        from
    };

    let mut positions = match piece.kind {
        PieceKind::Pawn => white_pawn_moves(from),
        PieceKind::Knight => white_knight_moves(from).collect(),
        PieceKind::Bishop => white_bishop_moves(from).collect(),
        PieceKind::Rook => white_rook_moves(from).collect(),
        PieceKind::Queen => white_queen_moves(from).collect(),
        PieceKind::King => white_king_moves(from).collect(),
    };

    if piece.color == Color::Black {
        for position in positions.iter_mut() {
            *position = position.as_other_color();
        }
    }

    positions
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use super::*;

    fn assert_moves_eq<A, B>(a: A, b: B)
    where
        A: IntoIterator<Item = Position>,
        B: IntoIterator<Item = Position>,
    {
        let a: BTreeSet<Position> = a.into_iter().collect();
        let b: BTreeSet<Position> = b.into_iter().collect();

        assert_eq!(a, b);
    }

    #[test]
    fn test_pawn_white() {
        assert_moves_eq(
            naive_moves_from_piece(
                Piece {
                    kind: PieceKind::Pawn,
                    color: Color::White,
                },
                Position::parse("d4").unwrap(),
            ),
            [
                Position::parse("c5").unwrap(),
                Position::parse("d5").unwrap(),
                Position::parse("e5").unwrap(),
            ],
        );
    }

    #[test]
    fn test_pawn_black() {
        assert_moves_eq(
            naive_moves_from_piece(
                Piece {
                    kind: PieceKind::Pawn,
                    color: Color::Black,
                },
                Position::parse("d4").unwrap(),
            ),
            [
                Position::parse("c3").unwrap(),
                Position::parse("d3").unwrap(),
                Position::parse("e3").unwrap(),
            ],
        );
    }

    #[test]
    fn test_pawn_start_white() {
        assert_moves_eq(
            naive_moves_from_piece(
                Piece {
                    kind: PieceKind::Pawn,
                    color: Color::White,
                },
                Position::parse("a2").unwrap(),
            ),
            [
                Position::parse("a3").unwrap(),
                Position::parse("a4").unwrap(),
                Position::parse("b3").unwrap(),
            ],
        );
    }

    #[test]
    fn test_pawn_end_white() {
        assert_moves_eq(
            naive_moves_from_piece(
                Piece {
                    kind: PieceKind::Pawn,
                    color: Color::White,
                },
                Position::new(0, 7).unwrap(),
            ),
            [],
        );
    }

    #[test]
    fn test_knight_white() {
        assert_moves_eq(
            naive_moves_from_piece(
                Piece {
                    kind: PieceKind::Knight,
                    color: Color::White,
                },
                Position::parse("g5").unwrap(),
            ),
            [
                Position::parse("h3").unwrap(),
                Position::parse("f3").unwrap(),
                Position::parse("e4").unwrap(),
                Position::parse("e6").unwrap(),
                Position::parse("f7").unwrap(),
                Position::parse("h7").unwrap(),
            ],
        );
        assert_moves_eq(
            naive_moves_from_piece(
                Piece {
                    kind: PieceKind::Knight,
                    color: Color::White,
                },
                Position::parse("e4").unwrap(),
            ),
            [
                Position::parse("d2").unwrap(),
                Position::parse("f2").unwrap(),
                Position::parse("c3").unwrap(),
                Position::parse("g3").unwrap(),
                Position::parse("c5").unwrap(),
                Position::parse("g5").unwrap(),
                Position::parse("d6").unwrap(),
                Position::parse("f6").unwrap(),
            ],
        );
    }

    #[test]
    fn test_bishop_white() {
        assert_moves_eq(
            naive_moves_from_piece(
                Piece {
                    kind: PieceKind::Bishop,
                    color: Color::White,
                },
                Position::parse("e5").unwrap(),
            ),
            [
                Position::parse("a1").unwrap(),
                Position::parse("b2").unwrap(),
                Position::parse("c3").unwrap(),
                Position::parse("d4").unwrap(),
                Position::parse("f6").unwrap(),
                Position::parse("g7").unwrap(),
                Position::parse("h8").unwrap(),
                Position::parse("b8").unwrap(),
                Position::parse("c7").unwrap(),
                Position::parse("d6").unwrap(),
                Position::parse("f4").unwrap(),
                Position::parse("g3").unwrap(),
                Position::parse("h2").unwrap(),
            ],
        );
    }

    #[test]
    fn test_bishop_corner_black() {
        assert_moves_eq(
            naive_moves_from_piece(
                Piece {
                    kind: PieceKind::Bishop,
                    color: Color::Black,
                },
                Position::parse("a8").unwrap(),
            ),
            [
                Position::parse("b7").unwrap(),
                Position::parse("c6").unwrap(),
                Position::parse("d5").unwrap(),
                Position::parse("e4").unwrap(),
                Position::parse("f3").unwrap(),
                Position::parse("g2").unwrap(),
                Position::parse("h1").unwrap(),
            ],
        );
    }

    #[test]
    fn test_rook_white() {
        assert_moves_eq(
            naive_moves_from_piece(
                Piece {
                    kind: PieceKind::Rook,
                    color: Color::White,
                },
                Position::parse("b3").unwrap(),
            ),
            [
                Position::parse("a3").unwrap(),
                Position::parse("c3").unwrap(),
                Position::parse("d3").unwrap(),
                Position::parse("e3").unwrap(),
                Position::parse("f3").unwrap(),
                Position::parse("g3").unwrap(),
                Position::parse("h3").unwrap(),
                Position::parse("b1").unwrap(),
                Position::parse("b2").unwrap(),
                Position::parse("b4").unwrap(),
                Position::parse("b5").unwrap(),
                Position::parse("b6").unwrap(),
                Position::parse("b7").unwrap(),
                Position::parse("b8").unwrap(),
            ],
        );
    }

    #[test]
    fn test_rook_edge_white() {
        assert_moves_eq(
            naive_moves_from_piece(
                Piece {
                    kind: PieceKind::Rook,
                    color: Color::White,
                },
                Position::parse("h1").unwrap(),
            ),
            [
                Position::parse("a1").unwrap(),
                Position::parse("b1").unwrap(),
                Position::parse("c1").unwrap(),
                Position::parse("d1").unwrap(),
                Position::parse("e1").unwrap(),
                Position::parse("f1").unwrap(),
                Position::parse("g1").unwrap(),
                Position::parse("h2").unwrap(),
                Position::parse("h3").unwrap(),
                Position::parse("h4").unwrap(),
                Position::parse("h5").unwrap(),
                Position::parse("h6").unwrap(),
                Position::parse("h7").unwrap(),
                Position::parse("h8").unwrap(),
            ],
        );
    }

    #[test]
    fn test_queen_white() {
        assert_moves_eq(
            naive_moves_from_piece(
                Piece {
                    kind: PieceKind::Queen,
                    color: Color::White,
                },
                Position::parse("e4").unwrap(),
            ),
            [
                Position::parse("a4").unwrap(),
                Position::parse("b4").unwrap(),
                Position::parse("c4").unwrap(),
                Position::parse("d4").unwrap(),
                Position::parse("f4").unwrap(),
                Position::parse("g4").unwrap(),
                Position::parse("h4").unwrap(),
                Position::parse("e1").unwrap(),
                Position::parse("e2").unwrap(),
                Position::parse("e3").unwrap(),
                Position::parse("e5").unwrap(),
                Position::parse("e6").unwrap(),
                Position::parse("e7").unwrap(),
                Position::parse("e8").unwrap(),
                Position::parse("b1").unwrap(),
                Position::parse("c2").unwrap(),
                Position::parse("d3").unwrap(),
                Position::parse("f5").unwrap(),
                Position::parse("g6").unwrap(),
                Position::parse("h7").unwrap(),
                Position::parse("a8").unwrap(),
                Position::parse("b7").unwrap(),
                Position::parse("c6").unwrap(),
                Position::parse("d5").unwrap(),
                Position::parse("f3").unwrap(),
                Position::parse("g2").unwrap(),
                Position::parse("h1").unwrap(),
            ],
        );
    }

    #[test]
    fn test_king_white() {
        assert_moves_eq(
            naive_moves_from_piece(
                Piece {
                    kind: PieceKind::King,
                    color: Color::White,
                },
                Position::parse("e4").unwrap(),
            ),
            [
                Position::parse("d3").unwrap(),
                Position::parse("e3").unwrap(),
                Position::parse("f3").unwrap(),
                Position::parse("d4").unwrap(),
                Position::parse("f4").unwrap(),
                Position::parse("d5").unwrap(),
                Position::parse("e5").unwrap(),
                Position::parse("f5").unwrap(),
            ],
        );
    }

    #[test]
    fn test_king_edge_black() {
        assert_moves_eq(
            naive_moves_from_piece(
                Piece {
                    kind: PieceKind::King,
                    color: Color::Black,
                },
                Position::parse("a1").unwrap(),
            ),
            [
                Position::parse("b1").unwrap(),
                Position::parse("a2").unwrap(),
                Position::parse("b2").unwrap(),
            ],
        );
    }
    
    #[test]
    fn king_castling_white() {
        assert_moves_eq(
            naive_moves_from_piece(
                Piece {
                    kind: PieceKind::King,
                    color: Color::White,
                },
                Position::parse("e1").unwrap(),
            ),
            [
                Position::parse("d1").unwrap(),
                Position::parse("d2").unwrap(),
                Position::parse("e2").unwrap(),
                Position::parse("f2").unwrap(),
                Position::parse("f1").unwrap(),
                Position::parse("c1").unwrap(),
                Position::parse("g1").unwrap(),
            ],
        );
    }
    
    #[test]
    fn king_castling_black() {
        assert_moves_eq(
            naive_moves_from_piece(
                Piece {
                    kind: PieceKind::King,
                    color: Color::Black,
                },
                Position::parse("e8").unwrap(),
            ),
            [
                Position::parse("d8").unwrap(),
                Position::parse("d7").unwrap(),
                Position::parse("e7").unwrap(),
                Position::parse("f7").unwrap(),
                Position::parse("f8").unwrap(),
                Position::parse("c8").unwrap(),
                Position::parse("g8").unwrap(),
            ],
        );
    }
}

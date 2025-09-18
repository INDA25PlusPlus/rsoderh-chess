use std::{
    fmt::{Debug, Write},
    hash::Hash,
};

use crate::{Color, Piece, PieceKind, Position, moves, span::PositionSpan};

#[cfg(test)]
mod tests;

/// Represents which pieces are attacking a certain position.
#[derive(Debug, PartialEq, Eq)]
pub struct AttackedPosition {
    /// Position of the piece which is attacked.
    pub piece: Position,
    /// List of the pieces which are attacking.
    pub attackers: Box<[Position]>,
}

// Represents the check state of a single king.
#[derive(Debug, PartialEq, Eq)]
pub enum CheckState {
    /// The king at the specified position is safe.
    Safe(Position),
    Check(AttackedPosition),
    Checkmate(AttackedPosition),
}

#[derive(Debug, PartialEq, Eq)]
pub enum IllegalMoveType {
    NoPiece,
    Color,
    /// That the piece can't move to the destination, either since that
    /// piece can't move in that direction, a piece was blocking the way, or
    /// if a friendly (or enemy in the case of pawns) piece is placed at the destination.
    Position,
    /// The move would have resulted in check mate for the current player.
    Checkmate(AttackedPosition),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Slot {
    Empty,
    Occupied(Piece),
}

impl Slot {
    pub fn as_piece(&self) -> Option<&Piece> {
        match self {
            Slot::Empty => None,
            Slot::Occupied(piece) => Some(piece),
        }
    }
    pub fn as_piece_mut(&mut self) -> Option<&mut Piece> {
        match self {
            Slot::Empty => None,
            Slot::Occupied(piece) => Some(piece),
        }
    }
    pub fn into_piece(self) -> Option<Piece> {
        match self {
            Slot::Empty => None,
            Slot::Occupied(piece) => Some(piece),
        }
    }
}

impl Debug for Slot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Empty => write!(f, "Empty"),
            Self::Occupied(piece) => write!(f, "{:?}", piece),
        }
    }
}

// About the representation of the board slots:
// They are stored in an array of rows, represented as an array of columns. These are stored so that
// lower row and column indices are earlier in the array. Note that this implies that when written
// out as a literal, the rows will appear inverted.

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Board([[Slot; 8]; 8]);

impl Board {
    pub fn new_empty() -> Self {
        Self([[Slot::Empty; 8]; 8])
    }

    // Returns a board with the standard starting position.
    pub fn new_standard() -> Self {
        Board([
            [
                Slot::Occupied(Piece::new(PieceKind::Rook, Color::White)),
                Slot::Occupied(Piece::new(PieceKind::Knight, Color::White)),
                Slot::Occupied(Piece::new(PieceKind::Bishop, Color::White)),
                Slot::Occupied(Piece::new(PieceKind::Queen, Color::White)),
                Slot::Occupied(Piece::new(PieceKind::King, Color::White)),
                Slot::Occupied(Piece::new(PieceKind::Bishop, Color::White)),
                Slot::Occupied(Piece::new(PieceKind::Knight, Color::White)),
                Slot::Occupied(Piece::new(PieceKind::Rook, Color::White)),
            ],
            [
                Slot::Occupied(Piece::new(PieceKind::Pawn, Color::White)),
                Slot::Occupied(Piece::new(PieceKind::Pawn, Color::White)),
                Slot::Occupied(Piece::new(PieceKind::Pawn, Color::White)),
                Slot::Occupied(Piece::new(PieceKind::Pawn, Color::White)),
                Slot::Occupied(Piece::new(PieceKind::Pawn, Color::White)),
                Slot::Occupied(Piece::new(PieceKind::Pawn, Color::White)),
                Slot::Occupied(Piece::new(PieceKind::Pawn, Color::White)),
                Slot::Occupied(Piece::new(PieceKind::Pawn, Color::White)),
            ],
            [Slot::Empty; 8],
            [Slot::Empty; 8],
            [Slot::Empty; 8],
            [Slot::Empty; 8],
            [
                Slot::Occupied(Piece::new(PieceKind::Pawn, Color::Black)),
                Slot::Occupied(Piece::new(PieceKind::Pawn, Color::Black)),
                Slot::Occupied(Piece::new(PieceKind::Pawn, Color::Black)),
                Slot::Occupied(Piece::new(PieceKind::Pawn, Color::Black)),
                Slot::Occupied(Piece::new(PieceKind::Pawn, Color::Black)),
                Slot::Occupied(Piece::new(PieceKind::Pawn, Color::Black)),
                Slot::Occupied(Piece::new(PieceKind::Pawn, Color::Black)),
                Slot::Occupied(Piece::new(PieceKind::Pawn, Color::Black)),
            ],
            [
                Slot::Occupied(Piece::new(PieceKind::Rook, Color::Black)),
                Slot::Occupied(Piece::new(PieceKind::Knight, Color::Black)),
                Slot::Occupied(Piece::new(PieceKind::Bishop, Color::Black)),
                Slot::Occupied(Piece::new(PieceKind::Queen, Color::Black)),
                Slot::Occupied(Piece::new(PieceKind::King, Color::Black)),
                Slot::Occupied(Piece::new(PieceKind::Bishop, Color::Black)),
                Slot::Occupied(Piece::new(PieceKind::Knight, Color::Black)),
                Slot::Occupied(Piece::new(PieceKind::Rook, Color::Black)),
            ],
        ])
    }

    /// Parses string representation of a chess board. String is trimmed before parsing.
    /// The string must start and end with a line containing "+--+--+--+--+--+--+--+--+".
    /// There should be eight lines starting with optional whitespace and starting and ending with
    /// "|". Within these there must be 8 cells separated by "|". Each cell either contains two
    /// spaces if empty or two characters where the first letter in each cell signifies the color
    /// - "w": white
    /// - "b": black
    ///
    /// and the second signifies the type
    /// - "p": pawn
    /// - "n": knight
    /// - "b": bishop
    /// - "r": rook
    /// - "q": queen
    /// - "k": king
    ///
    /// Example of the syntax:
    /// ```
    /// use rsoderh_chess::Board;
    /// assert_eq!(
    ///     Board::parse_str(
    ///         "
    ///          +--+--+--+--+--+--+--+--+
    ///         8|br|bn|bb|bq|bk|bb|bn|br|
    ///         7|bp|bp|bp|bp|bp|bp|bp|bp|
    ///         6|  |  |  |  |  |  |  |  |
    ///         5|  |  |  |  |  |  |  |  |
    ///         4|  |  |  |  |  |  |  |  |
    ///         3|  |  |  |  |  |  |  |  |
    ///         2|wp|wp|wp|wp|wp|wp|wp|wp|
    ///         1|wr|wn|wb|wq|wk|wb|wn|wr|
    ///          +--+--+--+--+--+--+--+--+
    ///            a  b  c  d  e  f  g  h
    ///         ",
    ///     ),
    ///     Some(Board::new_standard())
    /// );
    /// ```
    pub fn parse_str(string: &str) -> Option<Self> {
        let string = string.trim();
        // Check that string starts with the right prefix
        let string = string.strip_prefix("+--+--+--+--+--+--+--+--+")?.trim();
        // Check that string ends with the right suffix.
        let string = string.strip_suffix(" a  b  c  d  e  f  g  h")?.trim();
        let string = string.strip_suffix("+--+--+--+--+--+--+--+--+")?.trim();

        let slots: [[Slot; 8]; 8] = string
            .lines()
            .rev()
            .enumerate()
            .map(|(index, line)| {
                let line_number = index + 1;
                let line_prefix = char::from_digit(line_number as u32, 10)?.to_string();

                let line = line
                    .trim()
                    .strip_prefix(&line_prefix)?
                    .strip_prefix("|")?
                    .strip_suffix("|")?;
                line.split("|")
                    .map(|cell_str| {
                        if cell_str == "  " {
                            return Some(Slot::Empty);
                        }

                        let [first_char, second_char] = cell_str.chars().collect::<Box<[_]>>()[..]
                        else {
                            return None;
                        };

                        Some(Slot::Occupied(Piece {
                            color: match first_char {
                                'w' => Color::White,
                                'b' => Color::Black,
                                _ => return None,
                            },
                            kind: match second_char {
                                'p' => PieceKind::Pawn,
                                'n' => PieceKind::Knight,
                                'b' => PieceKind::Bishop,
                                'r' => PieceKind::Rook,
                                'q' => PieceKind::Queen,
                                'k' => PieceKind::King,
                                _ => return None,
                            },
                        }))
                    })
                    .collect::<Option<Vec<_>>>()?
                    .try_into()
                    .ok()
            })
            .collect::<Option<Vec<_>>>()?
            .try_into()
            .ok()?;

        Some(Self(slots))
    }

    /// Mirror board along the y-axis and flip the colors of the pieces, as if
    /// it was the opposite players turn.
    pub fn to_inverted(&self) -> Self {
        let mut result = self.clone();
        // let mut new_board = result.positions;

        for (row, positions) in result.0.iter_mut().enumerate() {
            // Flip row order
            *positions = self.0[7 - row];

            // Swap colors.
            for slot in positions.iter_mut() {
                match slot {
                    Slot::Empty => {}
                    Slot::Occupied(piece) => {
                        piece.color = piece.color.opposite();
                    }
                }
            }
        }
        result
    }

    pub fn positioned_slots(&self) -> impl Iterator<Item = (Position, &Slot)> {
        self.0.iter().enumerate().flat_map(|(row, row_slots)| {
            row_slots.iter().enumerate().map(move |(column, slot)| {
                (
                    Position::new(column as u8, row as u8).expect("indices are in 0..8"),
                    slot,
                )
            })
        })
    }

    /// Checks if a move is valid, without calculating the check state, returning a populated turn
    /// object if it is, or the reason if it isn't. This is like `is_valid_move`, except it
    /// considers moves which result in check mate as illegal.
    pub fn is_valid_move_ignoring_check(
        &self,
        from: Position,
        to: Position,
        turn: Color,
        turn_history: &[Turn],
    ) -> Result<Turn, IllegalMoveType> {
        // Things which must be true:
        // 1. Piece at source location
        // 2. Piece has current player's color
        // 3. Position in valid "direction" (i.e. it must be in included in
        //    `naive_moves_from_piece`)
        // 4. No pieces in the way between source and destination
        // 5. There isn't a friendly piece at destination
        // 6. If pawn and moving diagonally, that there is an enemy piece there OR that a pawn just
        //    moved two spaces last turn skipping over that position (i.e. en passant).

        // misc
        // TODO: En passant isn't valid.
        // TODO: Castling is currently considered an invalid move.
        // TODO: Apparently the position which the king skips over when castling can't be attacked
        //   for whatever reason.
        // TODO: Castling isn't valid if the king leaves, goes over, or moves into an attacked square.

        // Invert if black so we can assume that white is the current player.
        let (board, from, to) = match turn {
            Color::White => (self, from, to),
            Color::Black => (
                &self.to_inverted(),
                from.as_other_color(),
                to.as_other_color(),
            ),
        };

        let source_slot = board.at_position(from);
        let dest_slot = board.at_position(to);

        // For most valid moves, a capture is made if the destination slot has a piece.
        let mut capture = dest_slot.as_piece().map(|piece| (to, piece.kind));

        // Check 1.
        let Slot::Occupied(piece) = source_slot else {
            return Err(IllegalMoveType::NoPiece);
        };

        // Check 2.
        if piece.color != Color::White {
            return Err(IllegalMoveType::Color);
        }

        // Check 3.
        let naive_valid_moves = moves::naive_moves_from_piece(piece, from);
        if !naive_valid_moves.contains(&to) {
            return Err(IllegalMoveType::Position);
        }

        // Check 4.
        match piece.kind {
            PieceKind::Pawn => {
                // If moving straight, check that there isn't a piece at dest.
                if to.column() == from.column() && dest_slot != Slot::Empty {
                    return Err(IllegalMoveType::Position);
                }

                // If moving two places forward from start row, check that there isn't a piece in
                // the way.
                if from.translated((0, 2)) == Some(to)
                    && board.at_position(
                        from.translated((0, 1))
                            .expect("TODO: explain why this is guaranteed"),
                    ) != Slot::Empty
                {
                    return Err(IllegalMoveType::Position);
                }
            }
            PieceKind::Knight => {} // Knight can't be blocked.
            PieceKind::Bishop | PieceKind::Rook | PieceKind::Queen => {
                // Assumption: Check 3. has already guaranteed that `from` and `to` are either in
                //   the same column, row, or diagnonal axis.
                for position in PositionSpan::new(from, to)
                    .expect("positions to lie in the same row, column, or diagonal axis")
                    // Skip the source position, as that contains the source piece.
                    .skip(1)
                {
                    if board.at_position(position) != Slot::Empty {
                        return Err(IllegalMoveType::Position);
                    }
                }
            }
            PieceKind::King => {} // There are no positions between `from` and `to`
        }

        // Check 5.
        if board.color_at_position(to) == Some(Color::White) {
            // Destination contains a friendly piece.
            return Err(IllegalMoveType::Position);
        }

        // Check 6.
        if piece.kind == PieceKind::Pawn
            // Move is diagnonal.
            && from.column() != to.column()
            && (board.color_at_position(to) != Some(Color::Black))
        {
            if let Some(dest_above) = to.translated((0, 1))
                && let Some(dest_below) = to.translated((0, -1))
                && let Some(Turn {
                    player,
                    half_move:
                        HalfMove::Standard {
                            source: turn_source,
                            dest: turn_dest,
                            capture: _,
                        },
                }) = turn_history.last()
                && (*player == Color::Black)
                && *turn_source == dest_above
                && *turn_dest == dest_below
                && board.at_position(*turn_dest).into_piece()
                    == Some(Piece::new(PieceKind::Pawn, Color::Black))
            {
                // Pawn captured enemy pawn en passant.
                eprintln!("en passant");
                capture = Some((
                    to.translated((0, -1))
                        .expect("the outer guard can't pass unless this translation succeeded"),
                    PieceKind::Pawn,
                ))
            } else {
                return Err(IllegalMoveType::Position);
            }
        }

        // Invert back if black
        let (from, to, capture) = match turn {
            Color::White => (from, to, capture),
            Color::Black => (
                from.as_other_color(),
                to.as_other_color(),
                capture.map(|(position, kind)| (position.as_other_color(), kind)),
            ),
        };

        Ok(Turn {
            player: turn,
            half_move: HalfMove::Standard {
                source: from,
                dest: to,
                capture,
            },
        })
    }

    /// Checks if a move is valid for the specified player, returning a populated turn object if it
    /// is, or the reason if it isn't.
    pub fn is_valid_move(
        &self,
        from: Position,
        to: Position,
        turn_player: Color,
        turn_history: &[Turn],
    ) -> Result<Turn, IllegalMoveType> {
        // - If king that destination isn't check mate
        // - If source protected king
        // - If in check, then source must be king
        //   ^ can be combined as check for check mate after move.
        // - That the move isn't repeated
        let turn = self.is_valid_move_ignoring_check(from, to, turn_player, turn_history)?;

        // Create clone with move applied, and calculate if it's in check (yes, this feels very
        // ugly).
        let mut moved_board = self.clone();
        *moved_board.at_position_mut(to) = moved_board.at_position(from);
        *moved_board.at_position_mut(from) = Slot::Empty;

        if let Some(attacked_position) = moved_board
            .get_check_state_for_color(turn_player, turn_history)
            .filter_map(|state| match state {
                CheckState::Checkmate(attacked_position) => Some(attacked_position),
                // Note: `get_check_state_for_color` will return check if the current player can
                //   move a piece to block the attacker, but since the current player has just
                //   moved, and the other player is next to move we need to consider this case as
                //   checkmate as well.
                CheckState::Check(attacked_position) => Some(attacked_position),
                _ => None,
            })
            .next()
        {
            return Err(IllegalMoveType::Checkmate(attacked_position));
        }

        Ok(turn)
    }

    pub fn valid_moves_ignoring_check_from(
        &self,
        position: Position,
        turn: Color,
        turn_history: &[Turn],
    ) -> Option<impl Iterator<Item = Position>> {
        let piece = self.at_position(position).into_piece()?;

        Some(
            moves::naive_moves_from_piece(piece, position)
                .into_iter()
                .filter(move |destination| {
                    self.is_valid_move_ignoring_check(position, *destination, turn, turn_history)
                        .is_ok()
                }),
        )
    }

    // Returns iterator of all valid move destinations for piece at position. If there isn't a piece
    // there `None` is returned.
    pub fn valid_moves_from(
        &self,
        position: Position,
        turn: Color,
        turn_history: &[Turn],
    ) -> Option<impl Iterator<Item = Position>> {
        let piece = self.at_position(position).into_piece()?;

        Some(
            moves::naive_moves_from_piece(piece, position)
                .into_iter()
                .filter(move |destination| {
                    self.is_valid_move(position, *destination, turn, turn_history)
                        .is_ok()
                }),
        )
    }

    // Return iterator of positions containing pieces of the given color which are attacking the
    // position.
    pub fn pieces_attacking_position(
        &self,
        position: Position,
        color: Color,
        turn_history: &[Turn],
    ) -> impl Iterator<Item = Position> {
        self.positioned_slots()
            .filter_map(move |(attacker_position, slot)| match slot {
                Slot::Occupied(piece) if piece.color == color => {
                    let mut move_destinations = self
                        // TODO: I'm pretty sure it isn't valid to skip checking the check state
                        //   here, but checking it creates infinite recursion, which I don't know
                        //   how to avoid.
                        .valid_moves_ignoring_check_from(attacker_position, color, turn_history)
                        .expect("position to be occupied");

                    if move_destinations.any(|dest| dest == position) {
                        Some(attacker_position)
                    } else {
                        None
                    }
                }
                _ => None,
            })
    }

    /// For each king on the board, calculates if they are safe, in check, or in check mate,
    /// assuming that it's the king's turn, meaning it can move.
    ///
    /// Note that this has to be returned as an iterator since the board representation allows
    /// multiple kings of the same color on the board. These can of course have different check states.
    ///
    /// If you know that you have a standard board where there is always a single king of either
    /// color, then you can get that kings check state like this:
    /// `self.get_check_state_for_color(...).next().unwrap()`
    ///
    pub fn get_check_state_for_color(
        &self,
        player: Color,
        turn_history: &[Turn],
    ) -> impl Iterator<Item = CheckState> {
        // Positions with pieces attacking the king.
        self.positioned_slots()
            .filter_map(move |(position, slot)| match slot {
                Slot::Occupied(Piece {
                    kind: PieceKind::King,
                    color,
                }) if *color == player => {
                    Some(self.get_king_check_state(position, player, turn_history))
                }
                _ => None,
            })
    }

    pub fn get_king_check_state(
        &self,
        position: Position,
        color: Color,
        turn_history: &[Turn],
    ) -> CheckState {
        let attackers = self
            .pieces_attacking_position(position, color.opposite(), turn_history)
            .collect::<Box<[_]>>();

        if attackers.is_empty() {
            return CheckState::Safe(position);
        }

        // Possibilities which prevent a check from being a check mate:
        // 1. There is only one attacking enemy piece and it is itself attacked by friendly pieces.
        // 2. A friendly piece can be moved to block the attacking piece (which there is only one
        //    of).
        // 3. The king can be moved to a non-attacked position.

        // 1. and 2.
        match attackers.as_ref() {
            [attacker] => {
                // 1.
                if self
                    .pieces_attacking_position(*attacker, color, turn_history)
                    .next()
                    .is_some()
                {
                    // A friendly piece can capture the piece attacking the king.
                    return CheckState::Check(AttackedPosition {
                        piece: position,
                        attackers,
                    });
                }

                // 2.
                // Note: Castling can't possible result in the attacking piece being blocked.
                // TODO: Get a list of spaces the attacking piece would have to cross over, and
                //   check if any friendly pieces could move in to block them.
            }
            [_, ..] => {
                // There are more than one attacker
                return CheckState::Checkmate(AttackedPosition {
                    piece: position,
                    attackers,
                });
            }
            [] => panic!("attackers has been checked to contain > 0 elements"),
        }

        // 3.
        for dest in moves::naive_moves_from_piece(
            Piece {
                kind: PieceKind::King,
                color,
            },
            position,
        ) {
            // Check that potential position is empty, and that no enemy piece is attacking it.
            if self.at_position(dest) == Slot::Empty
                && self
                    .pieces_attacking_position(dest, color.opposite(), turn_history)
                    .next()
                    .is_none()
            {
                // There is a valid move for the king which isn't attacked.
                return CheckState::Check(AttackedPosition {
                    piece: position,
                    attackers,
                });
            }
        }

        CheckState::Checkmate(AttackedPosition {
            piece: position,
            attackers,
        })
    }

    // Access the slot at `position`.
    pub fn at_position(&self, position: Position) -> Slot {
        self.0[position.row.get() as usize][position.column.get() as usize]
    }

    // Access mutable reference to the slot at `position`.
    pub fn at_position_mut(&mut self, position: Position) -> &mut Slot {
        &mut self.0[position.row.get() as usize][position.column.get() as usize]
    }

    // Get the color of the piece at `position`, if there is a piece there.
    pub fn color_at_position(&self, position: Position) -> Option<Color> {
        self.at_position(position)
            .as_piece()
            .map(|piece| piece.color)
    }
}

impl Debug for Board {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("\n     +--+--+--+--+--+--+--+--+")?;
        for (i, row) in self.0.iter().enumerate().rev() {
            write!(f, "\n    {}|", i + 1)?;

            for slot in row.iter() {
                match slot {
                    Slot::Empty => {
                        f.write_str("  |")?;
                    }
                    Slot::Occupied(piece) => {
                        f.write_char(match piece.color {
                            Color::Black => 'b',
                            Color::White => 'w',
                        })?;
                        f.write_char(match piece.kind {
                            PieceKind::Pawn => 'p',
                            PieceKind::Knight => 'n',
                            PieceKind::Bishop => 'b',
                            PieceKind::Rook => 'r',
                            PieceKind::Queen => 'q',
                            PieceKind::King => 'k',
                        })?;
                        f.write_char('|')?;
                    }
                }
            }
        }
        f.write_str("\n     +--+--+--+--+--+--+--+--+")?;
        f.write_str("\n       a  b  c  d  e  f  g  h\n")?;

        Ok(())
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum CastleType {
    Kingside,
    Queenside,
}

/// Represents a half move by a player of arbitrary color.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HalfMove {
    Standard {
        /// The position the piece started at.
        source: Position,
        /// The position the piece was moved to.
        dest: Position,
        /// If an enemy piece was captured, this field contains it's position and type.
        /// Note: We need to store the position, as en passant captures pieces at a position other
        ///   than dest.
        capture: Option<(Position, PieceKind)>,
    },
    Castle(CastleType),
}

/// Represents a half move performed by a player. This contains all the necessary information to
/// reconstruct the board before or after the turn.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Turn {
    /// The player which performed the turn
    pub player: Color,
    /// What occurred during the turn.
    pub half_move: HalfMove,
}

impl Turn {
    /// The position the moved piece started at.
    pub fn source(&self) -> Position {
        match self.half_move {
            HalfMove::Standard { source, .. } => source,
            HalfMove::Castle(_) => match self.player {
                Color::White => Position::new(4, 0).expect("4 and 0 are within 0...8"),
                Color::Black => Position::new(4, 7).expect("4 and 7 are within 0...8"),
            },
        }
    }
    /// The position the piece was moved to.
    pub fn dest(&self) -> Position {
        match self.half_move {
            HalfMove::Standard { dest, .. } => dest,
            HalfMove::Castle(type_) => Position::new(
                match type_ {
                    CastleType::Kingside => 6,
                    CastleType::Queenside => 2,
                },
                match self.player {
                    Color::White => 0,
                    Color::Black => 7,
                },
            )
            .expect("6, 2, 0, and 7 are within 0..8"),
        }
    }

    /// If an enemy piece was captured during the move, this gives it's position and type.
    pub fn capture(&self) -> Option<(Position, PieceKind)> {
        match self.half_move {
            HalfMove::Standard { capture, .. } => capture,
            // Castling can't capture any pieces.
            HalfMove::Castle(_) => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum GameResult {
    /// At least one of the other player's kings is in check mate.
    Checkmate {
        winner: Color,
        /// The king which was in checkmate. If there were multiple then one is picked arbitrarily.
        attacked_king: AttackedPosition,
    },
    // /// The other players king isn't in check, but has no valid moves.
    // Stalemate,
    // TODO: Represent resignation
    // TODO: Represent manual draw
    // TODO: Represent draw due to repeated moves.
    // TODO: Represent draw due to repeated moves.
}

/// Represents whether a move, which didn't end the game, resulted in a check. If so, which pieces
/// are attacking the king is included.
#[derive(Debug, PartialEq, Eq)]
pub enum CheckOutcome {
    /// None of the other player's king are in check.
    Safe,
    /// The move occurred as wanted, and at least one of the other players kings is in check.
    /// Includes which pieces are attacking the king. If multiple enemy kings are in check (i.e. due
    /// to a custom board setup), one is picked arbitrarily.
    Check(AttackedPosition),
}

#[derive(Debug, PartialEq, Eq)]
pub enum MoveResult {
    /// The move was successfully applied. The new game state is included, and information about if
    /// the other player's king(s) is in check. Note that checkmate isn't included as a case, as
    /// that would imply that the game is finished.
    Ongoing(Game, CheckOutcome),
    /// The move resulted in a game over state. The final finshed game state is included.
    /// Information about why it finished and who won is included as part of the included struct
    /// (TODO: reference the method), i.e. checkmate, stalemate, etc.
    Finished(FinishedGame),
    /// The move wasn't legal. The previous game state is included.
    Illegal(Game, IllegalMoveType),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Game {
    board: Board,
    turn: Color,
    history: Vec<Turn>,
}

impl Game {
    /// Create a new game from a strating position, with the given player starting.
    pub fn new(board: Board, starting_player: Color) -> Self {
        Self {
            board,
            turn: starting_player,
            history: Vec::new(),
        }
    }

    /// Access the current game board.
    pub fn board(&self) -> &Board {
        &self.board
    }

    /// Access the history of half moves.
    pub fn history(&self) -> &[Turn] {
        &self.history
    }

    pub fn perform_move(mut self, from: Position, to: Position) -> MoveResult {
        // Check if the move is valid.
        let turn = match self
            .board
            .is_valid_move(from, to, self.turn, self.history())
        {
            Ok(turn) => turn,
            Err(error) => return MoveResult::Illegal(self, error),
        };

        // Add the turn to the history
        self.history.push(turn);

        // Move the piece on the board.
        *self.board.at_position_mut(to) = self.board.at_position(from);
        *self.board.at_position_mut(from) = Slot::Empty;

        // Calculate the check state of the new board.
        let check_state = self
            .board
            .get_check_state_for_color(self.turn.opposite(), self.history())
            .max_by_key(|check_state| match check_state {
                CheckState::Safe(_) => 0,
                CheckState::Check(_) => 1,
                CheckState::Checkmate(_) => 2,
            });

        // If the game hasn't ended, specifies if the other player's king(s) is in check.
        let check_state_ongoing = match check_state {
            // There are no kings on the board which can't occur unless the game started without any
            // kings. Just say that no kings are in check.
            None => CheckOutcome::Safe,
            Some(CheckState::Safe(_)) => CheckOutcome::Safe,
            Some(CheckState::Check(attacked_position)) => CheckOutcome::Check(attacked_position),
            Some(CheckState::Checkmate(attacked_position)) => {
                return MoveResult::Finished(FinishedGame {
                    board: self.board,
                    history: self.history,
                    result: GameResult::Checkmate {
                        winner: self.turn,
                        attacked_king: attacked_position,
                    },
                });
            }
        };

        // Flip the current turn.
        self.turn = self.turn.opposite();

        MoveResult::Ongoing(self, check_state_ongoing)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct FinishedGame {
    board: Board,
    history: Vec<Turn>,
    result: GameResult,
}

impl FinishedGame {
    /// Access the final game board.
    pub fn board(&self) -> &Board {
        &self.board
    }

    /// History of half moves applied to board.
    pub fn history(&self) -> &[Turn] {
        &self.history
    }

    /// Access the final game result.
    pub fn result(&self) -> &GameResult {
        &self.result
    }
}

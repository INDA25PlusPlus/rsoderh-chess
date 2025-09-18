use std::hash::Hash;

use crate::{moves, span::PositionSpan, Color, Piece, PieceKind, Position};

pub enum IllegalMoveType {
    NoPiece,
    Color,
    /// That the piece can't move to the destination, either since that
    /// piece can't move in that direction, a piece was blocking the way, or
    /// if a friendly (or enemy in the case of pawns) piece is placed at the destination.
    Position,
    /// The move would have resulted in check mate for the current player.
    CheckMate,
}

pub enum CheckState {
    Safe,
    Check(Box<[Position]>),
    CheckMate(Box<[Position]>),
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

#[derive(Clone, Hash)]
pub struct Board {
    positions: [[Slot; 8]; 8],
    turn: Color,
}

impl Board {
    pub fn new_empty() -> Self {
        todo!()
        // Self {
        //     positions: [[Slot::Empty; 8]; 8],
        //     turn: Color::White,
        // }
    }
    /// Mirror board along the y-axis and flip the colors of the pieces, as if
    /// it was the opposite players turn.
    pub fn to_inverted(&self) -> Self {
        let mut result = self.clone();
        // let mut new_board = result.positions;

        for (row, positions) in result.positions.iter_mut().enumerate() {
            // Flip row order
            *positions = self.positions[7 - row];

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
        result.turn = self.turn.opposite();
        result
    }

    pub fn positioned_slots(&self) -> impl Iterator<Item = (Position, &Slot)> {
        self.positions
            .iter()
            .enumerate()
            .flat_map(|(row, row_slots)| {
                row_slots.iter().enumerate().map(move |(column, slot)| {
                    (
                        Position::new(column as u8, row as u8).expect("indices are in 0..8"),
                        slot,
                    )
                })
            })
    }

    /// Checks if a move is valid, without calculating the check state, returning the reason if it
    /// isn't. This is like `is_valid_move`, except it considers move which result in check mate as
    /// illegal.
    pub fn is_valid_move_ignoring_check(
        &self,
        from: Position,
        to: Position,
    ) -> Result<(), IllegalMoveType> {
        // Things which must be true:
        // 1. Piece at source location
        // 2. Piece has current player's color
        // 3. Position in valid "direction" (i.e. it must be in included in
        //    `naive_moves_from_piece`)
        // 4. No pieces in the way between source and destination
        // 5. There isn't a friendly piece at destination
        // 6. If pawn and moving diagonally, that there is an enemy piece there

        // misc
        // TODO: En passant isn't valid.
        // TODO: Castling is currently considered an invalid move.
        // TODO: Apparently the position which the king skips over when castling can't be attacked
        //   for whatever reason.
        // TODO: Castling isn't valid if the king leaves, goes over, or moves into an attacked square.

        // Invert if black so we can assume that white is the current player.
        let (board, from, to) = match self.turn {
            Color::White => (self, from, to),
            Color::Black => (
                &self.to_inverted(),
                from.as_other_color(),
                to.as_other_color(),
            ),
        };

        let source_slot = board.at_position(from);
        let dest_slot = board.at_position(to);

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
        // Assumption: Check 4. guarantees that if dest has an enemy piece, that
        if from.column() != to.column()
            // Move is diagnonal.
            && self.color_at_position(to) != Some(Color::Black)
        {
            return Err(IllegalMoveType::Position);
        }

        Ok(())
    }

    /// Checks if a move is valid for the current player, returning the reason if it isn't.
    pub fn is_valid_move(&self, from: Position, to: Position) -> Result<(), IllegalMoveType> {
        // - If king that destination isn't check mate
        // - If source protected king
        // - If in check, then source must be king
        //   ^ can be combined as check for check mate after move.
        // - That the move isn't repeated
        self.is_valid_move_ignoring_check(from, to)?;
        if self
            .get_check_state_for_color(self.turn)
            .any(|(_, state)| matches!(state, CheckState::CheckMate(_)))
        {
            return Err(IllegalMoveType::CheckMate);
        }

        Ok(())
    }

    pub fn valid_moves_ignoring_check_from(
        &self,
        position: Position,
    ) -> Option<impl Iterator<Item = Position>> {
        let piece = self.at_position(position).into_piece()?;

        Some(
            moves::naive_moves_from_piece(piece, position)
                .into_iter()
                .filter(move |destination| self.is_valid_move(position, *destination).is_ok()),
        )
    }

    // Returns iterator of all valid move destinations for piece at position. If there isn't a piece
    // there `None` is returned.
    pub fn valid_moves_from(&self, position: Position) -> Option<impl Iterator<Item = Position>> {
        let piece = self.at_position(position).into_piece()?;

        Some(
            moves::naive_moves_from_piece(piece, position)
                .into_iter()
                .filter(move |destination| self.is_valid_move(position, *destination).is_ok()),
        )
    }

    // Return iterator of positions containing pieces of the given color which are attacking the
    // position.
    pub fn pieces_attacking_position(
        &self,
        position: Position,
        color: Color,
    ) -> impl Iterator<Item = Position> {
        self.positioned_slots()
            .filter_map(move |(attacker_position, slot)| match slot {
                Slot::Occupied(piece) if piece.color == color => {
                    let mut move_destinations = self
                        .valid_moves_from(attacker_position)
                        .expect("position to be occupied");

                    if move_destinations.any(|dest| dest == position) {
                        Some(position)
                    } else {
                        None
                    }
                }
                _ => None,
            })
    }

    /// Calculates if there are any kings of the given player which are currently in check or check
    /// mate, returning each instance as pair of its position and if check state.
    /// Note that this has to be returned as a list since there could be multiple kings of the same
    /// color on the board with different check states.
    ///
    /// The representation means that an empty list is returned if no kings are in check.
    /// For a standard game where there is only a single king, you could check if a king is in check
    /// as follows:
    pub fn get_check_state_for_color(
        &self,
        player: Color,
    ) -> impl Iterator<Item = (Position, CheckState)> {
        // Positions with pieces attacking the king.
        self.positioned_slots()
            .filter_map(move |(position, slot)| match slot {
                Slot::Occupied(Piece {
                    kind: PieceKind::King,
                    color,
                }) if *color == player => {
                    Some((position, self.get_king_check_state(position, player)))
                }
                _ => None,
            })
    }

    pub fn get_king_check_state(&self, position: Position, color: Color) -> CheckState {
        let attackers = self
            .pieces_attacking_position(position, color.opposite())
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
                    .pieces_attacking_position(*attacker, color)
                    .next()
                    .is_some()
                {
                    // A friendly piece can capture the piece attacking the king.
                    return CheckState::Check(attackers);
                }

                // 2.
                // Note: Castling can't possible result in the attacking piece being blocked.
                // TODO: Get a list of spaces the attacking piece would have to cross over, and
                //   check if any friendly pieces could move in to block them.
            }
            [_, ..] => {
                // There are more than one attacker
                return CheckState::CheckMate(attackers);
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
                    .pieces_attacking_position(dest, color.opposite())
                    .next()
                    .is_some()
            {
                // There is a valid move for the king which isn't attacked.
                return CheckState::Check(attackers);
            }
        }

        CheckState::CheckMate(attackers)
    }

    // Access the slot at `position`.
    pub fn at_position(&self, position: Position) -> Slot {
        self.positions[position.column.get() as usize][position.row.get() as usize]
    }

    // Get the color of the piece at `position`, if there is a piece there.
    pub fn color_at_position(&self, position: Position) -> Option<Color> {
        self.at_position(position)
            .as_piece()
            .map(|piece| piece.color)
    }

    pub fn perform_move(
        mut self,
        from: Position,
        to: Position,
    ) -> (Self, Result<CheckState, IllegalMoveType>) {
        todo!()
    }
}

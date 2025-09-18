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

// Based on source: https://stackoverflow.com/a/58126569/15507414
macro_rules! assert_match {
    ($value:expr, $pattern:pat $(,)?) => {{
        let value = &$value;

        if let $pattern = value {
        } else {
            panic!(
                r#"assertion `matches!(value, pattern)` failed:
   value: {:?},
 pattern: {}"#,
                value,
                stringify!($pattern)
            )
        }
    }};
}

impl Game {
    /// Apply moves as pairs of cell coordinate strings, like "d4". The `MoveResult` of the final
    /// move is returned. Panics if any of the strings aren't valid positions, if any result in
    /// invalid moves, or if moves doesn't contain at least one element.
    ///
    /// Useful for writing tests, don't use otherwise!
    fn apply_moves<'a>(
        mut self,
        moves: impl IntoIterator<Item = (&'a str, &'a str)>,
    ) -> MoveResult {
        let mut iter = moves.into_iter().peekable();
        while let Some((source, dest)) = iter.next() {
            let source = Position::parse(source).expect("string should be valid chess coordinate");
            let dest = Position::parse(dest).expect("string should be valid chess coordinate");

            let is_last_move = iter.peek().is_none();

            match self.perform_move(source, dest) {
                result if is_last_move => return result,
                MoveResult::Illegal(_, ref move_type) => {
                    panic!(
                        "Move {:?} -> {:?} is invalid: {:?}.",
                        source, dest, move_type
                    );
                }
                MoveResult::Ongoing(game, _) => {
                    self = game;
                }
                result @ MoveResult::Finished(_) => {
                    if !is_last_move {
                        panic!(
                            "Move {:?} -> {:?} resulted in finished game, while there are {} moves left.",
                            source,
                            dest,
                            iter.count()
                        );
                    }
                    return result;
                }
            }
        }

        panic!("Tried to apply no moves.")
    }
}

fn assert_game_result<'a>(
    start_board_str: &'a str,
    start_player: Color,
    moves: impl IntoIterator<Item = (&'a str, &'a str)>,
    end_board_str: &'a str,
    game_result: GameResult,
) {
    let game = Game::new(
        Board::parse_str(start_board_str).expect("valid test start board"),
        start_player,
    );
    let finished_game = match game.apply_moves(moves) {
        MoveResult::Finished(game) => game,
        result => {
            panic!("Moves did not result in a finished game: {:?}", result);
        }
    };

    assert_eq!(
        finished_game.board,
        Board::parse_str(end_board_str).expect("valid test end board")
    );
    assert_eq!(finished_game.result, game_result);
}

#[test]
fn board_display() {
    let expected = "
     +--+--+--+--+--+--+--+--+
    8|br|bn|bb|bq|bk|bb|bn|br|
    7|bp|bp|bp|bp|bp|bp|bp|bp|
    6|  |  |  |  |  |  |  |  |
    5|  |  |  |  |  |  |  |  |
    4|  |  |  |  |  |  |  |  |
    3|  |  |  |  |  |  |  |  |
    2|wp|wp|wp|wp|wp|wp|wp|wp|
    1|wr|wn|wb|wq|wk|wb|wn|wr|
     +--+--+--+--+--+--+--+--+
       a  b  c  d  e  f  g  h
";
    assert_eq!(format!("{:?}", Board::new_standard()), expected.to_owned());
}

#[test]
fn simple_game() {
    eprintln!("Running test");

    let game = Game::new(Board::new_standard(), Color::White);

    let result = game.apply_moves([("d2", "d4"), ("e7", "e5"), ("c1", "f4")]);

    match result {
        MoveResult::Ongoing(
            Game {
                turn: Color::Black,
                board,
                history: _,
            },
            CheckOutcome::Safe,
        ) => {
            assert_eq!(
                board,
                Board::parse_str(
                    "
                     +--+--+--+--+--+--+--+--+
                    8|br|bn|bb|bq|bk|bb|bn|br|
                    7|bp|bp|bp|bp|  |bp|bp|bp|
                    6|  |  |  |  |  |  |  |  |
                    5|  |  |  |  |bp|  |  |  |
                    4|  |  |  |wp|  |wb|  |  |
                    3|  |  |  |  |  |  |  |  |
                    2|wp|wp|wp|  |wp|wp|wp|wp|
                    1|wr|wn|  |wq|wk|wb|wn|wr|
                     +--+--+--+--+--+--+--+--+
                       a  b  c  d  e  f  g  h
                    "
                )
                .unwrap()
            )
        }
        result => {
            panic!("Unexpected game result: {:?}", result);
        }
    }
}

#[test]
fn simple_complete_game() {
    assert_game_result(
        "
         +--+--+--+--+--+--+--+--+
        8|br|bn|bb|bq|bk|bb|bn|br|
        7|bp|bp|bp|bp|bp|bp|bp|bp|
        6|  |  |  |  |  |  |  |  |
        5|  |  |  |  |  |  |  |  |
        4|  |  |  |  |  |  |  |  |
        3|  |  |  |  |  |  |  |  |
        2|wp|wp|wp|wp|wp|wp|wp|wp|
        1|wr|wn|wb|wq|wk|wb|wn|wr|
         +--+--+--+--+--+--+--+--+
           a  b  c  d  e  f  g  h
        ",
        Color::White,
        [("f2", "f3"), ("e7", "e6"), ("g2", "g4"), ("d8", "h4")],
        "
         +--+--+--+--+--+--+--+--+
        8|br|bn|bb|  |bk|bb|bn|br|
        7|bp|bp|bp|bp|  |bp|bp|bp|
        6|  |  |  |  |bp|  |  |  |
        5|  |  |  |  |  |  |  |  |
        4|  |  |  |  |  |  |wp|bq|
        3|  |  |  |  |  |wp|  |  |
        2|wp|wp|wp|wp|wp|  |  |wp|
        1|wr|wn|wb|wq|wk|wb|wn|wr|
         +--+--+--+--+--+--+--+--+
           a  b  c  d  e  f  g  h
        ",
        GameResult::Checkmate {
            winner: Color::Black,
            attacked_king: AttackedPosition {
                piece: Position::parse("e1").unwrap(),
                attackers: vec![Position::parse("h4").unwrap()].into(),
            },
        },
    );
}

#[test]
fn check() {
    assert_eq!(
        Board::parse_str(
            "
             +--+--+--+--+--+--+--+--+
            8|  |  |  |  |  |  |  |  |
            7|  |  |  |  |  |  |  |  |
            6|  |  |  |  |  |  |  |  |
            5|  |  |  |  |  |  |  |  |
            4|  |  |  |  |  |  |  |bq|
            3|  |  |  |  |  |  |  |  |
            2|  |  |  |wp|wp|  |  |  |
            1|  |  |  |wp|wk|wp|  |  |
             +--+--+--+--+--+--+--+--+
               a  b  c  d  e  f  g  h
            "
        )
        .unwrap()
        .get_check_state_for_color(Color::White, &[])
        .collect::<Vec<_>>(),
        vec![CheckState::Checkmate(AttackedPosition {
            piece: Position::parse("e1").unwrap(),
            attackers: [Position::parse("h4").unwrap()].into()
        })],
    );
    assert_eq!(
        Board::parse_str(
            "
             +--+--+--+--+--+--+--+--+
            8|  |  |  |  |  |  |  |  |
            7|  |  |  |  |  |  |  |  |
            6|  |  |  |  |  |  |  |  |
            5|  |  |  |  |  |  |  |  |
            4|  |  |  |  |wq|  |  |bq|
            3|  |  |  |  |  |  |  |  |
            2|  |  |  |  |  |  |  |  |
            1|  |  |  |  |wk|  |  |  |
             +--+--+--+--+--+--+--+--+
               a  b  c  d  e  f  g  h
            "
        )
        .unwrap()
        .get_check_state_for_color(Color::White, &[])
        .collect::<Vec<_>>(),
        vec![CheckState::Check(AttackedPosition {
            piece: Position::parse("e1").unwrap(),
            attackers: [Position::parse("h4").unwrap()].into()
        })],
    );
}

#[test]
fn misc_valid_moves_ignoring_check() {
    let board = Board::parse_str(
        "
         +--+--+--+--+--+--+--+--+
        8|  |  |  |  |  |  |  |  |
        7|  |  |  |  |  |  |  |  |
        6|  |  |  |  |  |  |  |  |
        5|  |  |  |  |  |  |  |  |
        4|  |  |  |  |  |  |  |  |
        3|  |  |  |  |  |  |  |  |
        2|  |wp|  |  |  |  |  |  |
        1|wr|wn|wb|wq|wk|  |  |  |
         +--+--+--+--+--+--+--+--+
           a  b  c  d  e  f  g  h
        ",
    )
    .unwrap();

    assert_moves_eq(
        board
            .valid_moves_ignoring_check_from(Position::parse("a1").unwrap(), Color::White, &[])
            .unwrap(),
        ["a2", "a3", "a4", "a5", "a6", "a7", "a8"].map(|string| Position::parse(string).unwrap()),
    );
    assert_moves_eq(
        board
            .valid_moves_ignoring_check_from(Position::parse("b1").unwrap(), Color::White, &[])
            .unwrap(),
        ["a3", "c3", "d2"].map(|string| Position::parse(string).unwrap()),
    );
    assert_moves_eq(
        board
            .valid_moves_ignoring_check_from(Position::parse("b2").unwrap(), Color::White, &[])
            .unwrap(),
        ["b3", "b4"].map(|string| Position::parse(string).unwrap()),
    );
    assert_moves_eq(
        board
            .valid_moves_ignoring_check_from(Position::parse("c1").unwrap(), Color::White, &[])
            .unwrap(),
        ["d2", "e3", "f4", "g5", "h6"].map(|string| Position::parse(string).unwrap()),
    );
    assert_moves_eq(
        board
            .valid_moves_ignoring_check_from(Position::parse("d1").unwrap(), Color::White, &[])
            .unwrap(),
        [
            "a4", "b3", "c2", "e2", "f3", "g4", "h5", "d2", "d3", "d4", "d5", "d6", "d7", "d8",
        ]
        .map(|string| Position::parse(string).unwrap()),
    );
    assert_moves_eq(
        board
            .valid_moves_ignoring_check_from(Position::parse("e1").unwrap(), Color::White, &[])
            .unwrap(),
        ["d2", "e2", "f2", "f1"].map(|string| Position::parse(string).unwrap()),
    );
}

#[test]
fn pawn_moves() {
    let board = Board::parse_str(
        "
         +--+--+--+--+--+--+--+--+
        8|  |  |  |  |  |  |  |  |
        7|  |  |  |  |bp|  |  |  |
        6|  |  |  |wp|  |  |  |  |
        5|  |  |  |  |  |  |  |  |
        4|  |  |  |  |  |  |  |  |
        3|  |  |wp|  |  |  |  |  |
        2|  |wp|  |  |  |  |  |  |
        1|  |  |  |  |  |  |  |  |
         +--+--+--+--+--+--+--+--+
           a  b  c  d  e  f  g  h
        ",
    )
    .unwrap();

    assert_match!(
        board.is_valid_move(
            Position::parse("b2").unwrap(),
            Position::parse("b4").unwrap(),
            Color::White,
            &[],
        ),
        Ok(_),
    );
    assert_match!(
        board.is_valid_move(
            Position::parse("c3").unwrap(),
            Position::parse("c4").unwrap(),
            Color::White,
            &[],
        ),
        Ok(_),
    );
    assert_match!(
        board.is_valid_move(
            Position::parse("c3").unwrap(),
            Position::parse("c5").unwrap(),
            Color::White,
            &[],
        ),
        Err(IllegalMoveType::Position),
    );
    assert_match!(
        board.is_valid_move(
            Position::parse("d6").unwrap(),
            Position::parse("e7").unwrap(),
            Color::White,
            &[],
        ),
        Ok(_),
    );
}

#[test]
fn basic_illegal_moves() {
    // let board = Game::new(
    //     Board::parse_str(
    //         "
    //      +--+--+--+--+--+--+--+--+
    //     8|  |  |  |  |  |  |  |  |
    //     7|  |  |  |  |bp|  |  |  |
    //     6|  |  |  |wp|  |  |  |  |
    //     5|  |  |  |  |  |  |  |  |
    //     4|  |  |  |  |  |  |  |  |
    //     3|  |  |wp|  |  |  |  |  |
    //     2|  |wp|  |  |  |  |  |  |
    //     1|  |  |  |  |  |  |  |  |
    //      +--+--+--+--+--+--+--+--+
    //        a  b  c  d  e  f  g  h
    //     ",
    //     )
    //     .unwrap(),
    //     Color::White,
    // );
    let board = Board::parse_str(
        "
         +--+--+--+--+--+--+--+--+
        8|  |  |  |  |  |  |  |  |
        7|  |  |  |  |bp|  |  |  |
        6|  |  |  |  |  |  |  |  |
        5|  |  |  |  |  |  |  |  |
        4|  |  |  |  |  |  |  |  |
        3|  |wp|  |  |  |wq|  |  |
        2|  |  |  |  |  |  |  |  |
        1|  |  |  |  |  |  |  |  |
         +--+--+--+--+--+--+--+--+
           a  b  c  d  e  f  g  h
        ",
    )
    .unwrap();

    assert_eq!(
        board.is_valid_move(
            Position::parse("a1").unwrap(),
            Position::parse("a2").unwrap(),
            Color::White,
            &[],
        ),
        Err(IllegalMoveType::NoPiece),
    );
    assert_eq!(
        board.is_valid_move(
            Position::parse("e7").unwrap(),
            Position::parse("e6").unwrap(),
            Color::White,
            &[],
        ),
        Err(IllegalMoveType::Color),
    );
    assert_eq!(
        board.is_valid_move(
            Position::parse("b3").unwrap(),
            Position::parse("b5").unwrap(),
            Color::White,
            &[],
        ),
        Err(IllegalMoveType::Position),
    );
}

#[test]
fn illegal_move_check_mate_move_into() {
    let board = Board::parse_str(
        "
         +--+--+--+--+--+--+--+--+
        8|  |  |  |  |  |  |  |  |
        7|  |  |  |br|  |  |  |  |
        6|  |  |  |  |  |  |  |  |
        5|  |  |  |  |  |  |  |  |
        4|  |  |  |  |wk|  |  |  |
        3|  |  |  |  |  |  |  |  |
        2|  |  |  |  |  |  |  |  |
        1|  |  |  |  |  |  |  |  |
         +--+--+--+--+--+--+--+--+
           a  b  c  d  e  f  g  h
        ",
    )
    .unwrap();

    assert_eq!(
        board.is_valid_move(
            Position::parse("e4").unwrap(),
            Position::parse("d4").unwrap(),
            Color::White,
            &[],
        ),
        Err(IllegalMoveType::Checkmate(AttackedPosition {
            piece: Position::parse("d4").unwrap(),
            attackers: vec![Position::parse("d7").unwrap()].into(),
        })),
    );
}

#[test]
fn illegal_move_check_mate_not_move_away() {
    let board = Board::parse_str(
        "
         +--+--+--+--+--+--+--+--+
        8|  |  |  |  |  |  |  |  |
        7|  |  |  |br|  |  |  |  |
        6|  |  |  |  |  |  |  |  |
        5|  |  |  |  |  |  |  |  |
        4|  |  |  |wk|  |  |wp|  |
        3|  |  |  |  |  |  |  |  |
        2|  |  |  |  |  |  |  |  |
        1|  |  |  |  |  |  |  |  |
         +--+--+--+--+--+--+--+--+
           a  b  c  d  e  f  g  h
        ",
    )
    .unwrap();

    assert_eq!(
        board.is_valid_move(
            Position::parse("g4").unwrap(),
            Position::parse("g5").unwrap(),
            Color::White,
            &[],
        ),
        Err(IllegalMoveType::Checkmate(AttackedPosition {
            piece: Position::parse("d4").unwrap(),
            attackers: vec![Position::parse("d7").unwrap()].into(),
        })),
    );
}

#[test]
fn illegal_move_check_mate_not_capture() {
    let board = Board::parse_str(
        "
         +--+--+--+--+--+--+--+--+
        8|  |  |  |  |  |  |  |  |
        7|  |  |  |br|  |wq|  |  |
        6|  |  |  |  |  |  |  |  |
        5|  |  |  |  |  |  |  |  |
        4|  |  |  |wk|  |  |  |  |
        3|  |  |  |  |  |  |  |  |
        2|  |  |  |  |  |  |  |  |
        1|  |  |  |  |  |  |  |  |
         +--+--+--+--+--+--+--+--+
           a  b  c  d  e  f  g  h
        ",
    )
    .unwrap();

    assert_eq!(
        board.is_valid_move(
            Position::parse("f7").unwrap(),
            Position::parse("f6").unwrap(),
            Color::White,
            &[],
        ),
        Err(IllegalMoveType::Checkmate(AttackedPosition {
            piece: Position::parse("d4").unwrap(),
            attackers: vec![Position::parse("d7").unwrap()].into(),
        })),
    );
}

#[test]
fn history() {
    let game = Game::new(
        Board::parse_str(
            "
             +--+--+--+--+--+--+--+--+
            8|  |  |  |  |  |  |  |  |
            7|  |  |  |  |  |  |  |  |
            6|  |  |  |  |  |  |  |  |
            5|  |  |  |  |  |  |  |  |
            4|  |  |  |wn|  |  |  |  |
            3|  |  |  |  |  |  |  |  |
            2|  |br|  |  |  |  |  |  |
            1|  |  |  |  |  |  |  |  |
             +--+--+--+--+--+--+--+--+
               a  b  c  d  e  f  g  h
            ",
        )
        .unwrap(),
        Color::White,
    );

    let game = match game.apply_moves([("d4", "b5"), ("b2", "b5")]) {
        MoveResult::Ongoing(game, CheckOutcome::Safe) => game,
        result => panic!("Unexpected move result: {:?}", result),
    };

    assert_eq!(
        game.history(),
        [
            Turn {
                player: Color::White,
                half_move: HalfMove::Standard {
                    source: Position::parse("d4").unwrap(),
                    dest: Position::parse("b5").unwrap(),
                    capture: None
                }
            },
            Turn {
                player: Color::Black,
                half_move: HalfMove::Standard {
                    source: Position::parse("b2").unwrap(),
                    dest: Position::parse("b5").unwrap(),
                    capture: Some((Position::parse("b5").unwrap(), PieceKind::Knight)),
                }
            }
        ]
    )
}

#[test]
fn en_passant() {
    let game = Game::new(
        Board::parse_str(
            "
             +--+--+--+--+--+--+--+--+
            8|  |  |  |  |  |  |  |  |
            7|bp|  |  |  |  |  |  |  |
            6|  |  |  |  |  |  |  |  |
            5|  |wp|  |  |  |  |  |  |
            4|  |  |  |  |  |  |  |  |
            3|  |  |  |  |  |  |  |  |
            2|  |  |  |  |  |  |  |  |
            1|  |  |  |  |  |  |  |  |
             +--+--+--+--+--+--+--+--+
               a  b  c  d  e  f  g  h
            ",
        )
        .unwrap(),
        Color::Black,
    );

    match game.apply_moves([("a7", "a5"), ("b5", "a6")]) {
        MoveResult::Ongoing(_, CheckOutcome::Safe) => {}
        result => panic!("Unexpected move result: {:?}", result),
    }
}
#[test]
fn en_passant_illegal_pawn() {
    let game = Game::new(
        Board::parse_str(
            "
            +--+--+--+--+--+--+--+--+
            8|  |  |  |  |  |  |  |  |
            7|  |  |  |  |  |  |  |  |
            6|bp|  |  |  |  |  |  |  |
            5|  |wp|  |  |  |  |  |  |
            4|  |  |  |  |  |  |  |  |
            3|  |  |  |  |  |  |  |  |
            2|  |  |  |  |  |  |  |  |
            1|  |  |  |  |  |  |  |  |
             +--+--+--+--+--+--+--+--+
               a  b  c  d  e  f  g  h
            ",
        )
        .unwrap(),
        Color::Black,
    );

    match game.apply_moves([("a6", "a5"), ("b5", "a6")]) {
        MoveResult::Illegal(_, IllegalMoveType::Position) => {}
        result => panic!("Unexpected move result: {:?}", result),
    }
}

#[test]
fn en_passant_illegal_rook() {
    let game = Game::new(
        Board::parse_str(
            "
            +--+--+--+--+--+--+--+--+
            8|  |  |  |  |  |  |  |  |
            7|  |  |  |  |  |  |  |  |
            6|br|  |  |  |  |  |  |  |
            5|  |wp|  |  |  |  |  |  |
            4|  |  |  |  |  |  |  |  |
            3|  |  |  |  |  |  |  |  |
            2|  |  |  |  |  |  |  |  |
            1|  |  |  |  |  |  |  |  |
             +--+--+--+--+--+--+--+--+
               a  b  c  d  e  f  g  h
            ",
        )
        .unwrap(),
        Color::Black,
    );

    match game.apply_moves([("a6", "a5"), ("b5", "a6")]) {
        MoveResult::Illegal(_, IllegalMoveType::Position) => {}
        result => panic!("Unexpected move result: {:?}", result),
    }
}

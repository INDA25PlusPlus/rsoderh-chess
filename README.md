# rsoderh_chess

A library with a complete backend covering all the rules* of chess, to be used when creating a chess game.

_*conditions apply_

## API

On a high level, you use this library by creating an instance of the `Game` struct. But before we explain this, we should first explain how positions are represented.

### `Position`

This is a struct which represents some position on the board, containing zero-indexed column and row indices. It is designed in a way so that its coordinates are guaranteed to lie within the board, i.e. be within 0..7. This means that creating an instance returns an Option, in case the provided integers are out of range. Note that when debug formatted the position is pretty-printed, like "b4".

> *Tip*: A value is debug formatted when using a format specifying like `"{:?}"`.

Here is an example of creating a `Position`.
```rust
use rsoderh_chess::Position;

fn main() {
    let position = Position::new(1, 5).unwrap();
    println!("Position: {:?}", position);
    // Position: b4
}
```

Note that you can also create them by parsing a coordinate string (convenient when debugging):
```rust
use rsoderh_chess::Position;

fn main() {
    let position = Position::parse("b4").unwrap();
    println!("Position: {:?}", position);
    // Position: b4
}
```

### `Game`

This struct represents a certain turn of an ongoing game. You use it by issuing half moves using `Board::perform_move`, specifying a source and destination position. (Note that in chess a "half move" refers to a single piece being moved. A full move is made up of two such turns. However, since I was lazy when naming things most places in this library refers to half moves as just moves.) It validates the move, and if valid returns a new `Game` value with the half move applied. If the half move resulted in checkmate another struct, `FinishedGame`, is returned instead. The fact that the previous `Game` value is consumed and that a new instance is returned means that the API is functional! :)

The `Game` struct contains a field with a `Board` instance, which you can access using `game.board()`. See below for its details. For now the important part is that when printed is pretty-printed as an ASCII art table.

Here is an example of a complete game showcasing the fool's mate, the fastest possible checkmate at just two full moves! Note that the final call to `Game::perform_move` returned the `MoveResult::Finished` enum variant with a `FinishedGame` instance instead of a `Game`. It's very similar to the `Game` struct, except that you can't call `Board::perform_move` or `Board::valid_moves`.

```rust
use rsoderh_chess::{FinishedGame, Game, MoveResult, Position};

fn main() {
    let game: Game = Game::new_standard();
    println!("{:?}", game.board());
    /*
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
    */
    // White's turn
    // Here we assign the "newly created" game value.
    let game: Game = match game.perform_move(
        Position::parse("f2").unwrap(),
        Position::parse("f3").unwrap(),
    ) {
        MoveResult::Ongoing(game, check_outcome) => {
            println!("check: {:?}", check_outcome);
            // check: Safe
            game
        }
        _ => unreachable!(),
    };
    
    // Black's turn
    let game: Game = match game.perform_move(
        Position::parse("e7").unwrap(),
        Position::parse("e6").unwrap(),
    ) {
        MoveResult::Ongoing(game, _) => game,
        _ => unreachable!(),
    };
    
    // White's turn
    let game: Game = match game.perform_move(
        Position::parse("g2").unwrap(),
        Position::parse("g4").unwrap(),
    ) {
        MoveResult::Ongoing(game, _) => game,
        _ => unreachable!(),
    };
    
    // Black's turn
    let finished_game: FinishedGame = match game.perform_move(
        Position::parse("d8").unwrap(),
        Position::parse("h4").unwrap(),
    ) {
        MoveResult::Finished(finished_game) => finished_game,
        _ => unreachable!(),
    };
    
    println!("final board: {:?}", finished_game.board());
    /*
    final board: 
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
    */
    println!("result: {:?}", finished_game.result());
    // result: Checkmate { winner: Black, attacked_king: AttackedPosition { piece: e1, attackers: [h4] } }
}
```

To provide a nice user experience you may want to highlight the valid destination squares when a user selects a piece. This can be achieved via `Board::valid_moves`, which returns a list of valid moves for the pieces at a given position. The list is returned as an option, with `None` specifying that the specified location is empty.

```rust
use rsoderh_chess::{Board, Color, Game, Position};

fn main() {
    let game = Game::new(
        Board::parse_str(
            "
             +--+--+--+--+--+--+--+--+
            8|br|bn|bb|bq|bk|bb|bn|br|
            7|bp|bp|bp|bp|  |bp|bp|bp|
            6|  |  |  |  |  |  |  |  |
            5|  |  |  |  |  |  |  |  |
            4|  |  |  |  |  |  |  |  |
            3|  |  |  |  |  |  |  |  |
            2|wp|wp|wp|wp|wp|wp|wp|wp|
            1|wr|wn|wb|wq|wk|wb|wn|wr|
             +--+--+--+--+--+--+--+--+
               a  b  c  d  e  f  g  h
            ",
        )
        .unwrap(),
        Color::Black,
    );

    println!(
        "moves: {:?}",
        game.valid_moves(Position::parse("d8").unwrap())
    )
    // moves: Some([e7, f6, g5, h4])
}
```

### `Board`

Represents the certain chess position. It can be accessed by `Board::board()`, and when debug printed it is pretty-printed. See the example below. Like `Position`, a `Board` can also be parsed from a string using `Board::parse_str`. It follows the same format used when pretty-printing boards.

```rust
use rsoderh_chess::Board;

fn main() {
    assert_eq!(
        Board::parse_str(
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
        ).unwrap(),
        Board::new_standard(),
    );
}
```

### All the other stuff

There are of course more datatypes and methods than I've explained here, but hopefully these examples should contain everything you need for a basic chess game. A lot of the types and methods have docstrings, and are (hopefully) pretty self-explanatory. If not I guess you can just poke me and ask me what the hell I was thinking. :)

## Missing features/known bugs

Unfortunately the following features aren't supported:
- Promotion
- Stalemate
- Manual draw
- Draw due to repeated moves

Also, there is a known bug where certain checks will be considered as checkmate. This occurs when a king is in check, and a friendly piece could move in between the king and the attacker and block it. The code just doesn't check for this currently, and therefore flags it as a checkmate.

See `check_blockable_attacker` in [board/tests.rs](./src/board/tests.rs) for an example.

use std::fmt::Debug;

/// Represents a coordinate on a chess board. Wrapper around u8 guaranteed to be within 0..8
/// (exclusive).
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct PositionIndex(u8);

impl PositionIndex {
    pub fn new(value: u8) -> Option<Self> {
        match value {
            0..8 => Some(Self(value)),
            _ => None,
        }
    }

    pub fn parse(string: &str) -> Option<Self> {
        let char_ = string.chars().next()?;
        match char_ {
            column_char @ ('a'..='h' | 'A'..='H') => {
                Self::new((column_char.to_digit(18)? - 10) as u8)
            }
            row_char @ ('1'..='8') => Self::new((row_char.to_digit(10)? - 1) as u8),
            _ => None,
        }
    }

    pub fn get(&self) -> u8 {
        self.0
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Position {
    pub column: PositionIndex,
    pub row: PositionIndex,
}

impl Position {
    pub fn new(column: u8, row: u8) -> Option<Self> {
        Self::from_pair((column, row))
    }
    pub fn from_pair(pair: (u8, u8)) -> Option<Self> {
        Some(Position {
            column: PositionIndex::new(pair.0)?,
            row: PositionIndex::new(pair.1)?,
        })
    }
    pub fn as_other_color(self) -> Self {
        Self {
            row: PositionIndex::new(7 - self.row.0).unwrap(),
            ..self
        }
    }
    pub fn column(self) -> u8 {
        self.column.0
    }
    pub fn row(self) -> u8 {
        self.row.0
    }
    pub fn translated(self, translation: (i8, i8)) -> Option<Self> {
        Self::new(
            (self.column.0 as i8 + translation.0) as u8,
            (self.row.0 as i8 + translation.1) as u8,
        )
    }

    /// Parse Position from string like "a1". Is case insensitive.
    /// ```
    /// use rsoderh_chess::Position;
    ///
    /// assert_eq!(Position::parse("a1").unwrap(), Position::new(0, 0).unwrap());
    /// ```
    pub fn parse(string: &str) -> Option<Self> {
        let chars: Vec<char> = string.chars().collect();
        match chars[..] {
            [
                column_char @ ('a'..='h' | 'A'..='H'),
                row_char @ ('1'..='8'),
            ] => Self::new(
                (column_char.to_digit(18)? - 10) as u8,
                (row_char.to_digit(10)? - 1) as u8,
            ),
            _ => None,
        }
    }
}

impl Debug for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let column = char::from_digit((self.column.0 as u32) + 10, 18)
            .expect("Position to hold value in valid range");
        let row = char::from_digit(self.row.0 as u32 + 1, 10)
            .expect("Position to hold value in valid range");

        write!(f, "{}{}", column, row)
    }
}

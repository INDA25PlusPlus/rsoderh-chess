use std::cmp::Ordering;

use crate::{Position, PositionIndex};

/// An iterator which iterates over the values between `start` and `end`, where
/// `end` may be smaller than `start`, meaning that the numbers are returned in
/// descending order. `start` is the first value returned, and `end` isn't
/// returned (so it's non-inclusive).
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Span {
    start: u8,
    end: u8,
}

impl Span {
    pub fn new(start: u8, end: u8) -> Self {
        Self { start, end }
    }
}

impl Iterator for Span {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        match self.start.cmp(&self.end) {
            Ordering::Less => {
                self.start += 1;
                Some(self.start - 1)
            }
            Ordering::Greater => {
                self.start -= 1;
                Some(self.start + 1)
            }
            Ordering::Equal => None,
        }
    }
}

pub struct SpanInclusive {
    start: u8,
    end: u8,
    exhausted: bool,
}

impl SpanInclusive {
    pub fn new(start: u8, end: u8) -> Self {
        Self {
            start,
            end,
            exhausted: false,
        }
    }
}

impl Iterator for SpanInclusive {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        match self.start.cmp(&self.end) {
            Ordering::Less => {
                self.start += 1;
                Some(self.start - 1)
            }
            Ordering::Greater => {
                self.start -= 1;
                Some(self.start + 1)
            }
            Ordering::Equal if !self.exhausted => {
                self.exhausted = true;
                Some(self.start)
            }
            Ordering::Equal => None,
        }
    }
}

/// An iterator which iterates over the positions between `start` and `end`,
/// where the two positions either lie in the same column, same row, or on the
/// same diagonal axis. `start` is the first value returned, and `end` isn't
/// included (so it's non-inclusive).
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum PositionSpan {
    Diagonal { start: Position, end: Position },
    Horizontal { column: Span, row: PositionIndex },
    Vertical { column: PositionIndex, row: Span },
}

impl PositionSpan {
    pub fn new(start: Position, end: Position) -> Option<Self> {
        if start.row == end.row {
            Some(Self::Horizontal {
                column: Span::new(start.column(), end.column()),
                row: start.row,
            })
        } else if start.column == end.column {
            Some(Self::Vertical {
                column: start.column,
                row: Span::new(start.row(), end.row()),
            })
        } else if start.column().abs_diff(end.column()) == start.row().abs_diff(end.row()) {
            Some(Self::Diagonal { start, end })
        } else {
            None
        }
        
    }
}

impl Iterator for PositionSpan {
    type Item = Position;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Diagonal { start, end } => {
                if start == end {
                    None
                } else {
                    let offset = (
                        if start.column() < end.column() { 1 } else { -1 },
                        if start.row() < end.row() { 1 } else { -1 },
                    );

                    let result = *start;
                    *start = start
                        .translated(offset)
                        .expect("point between two position to be within board");
                    Some(result)
                }
            }
            Self::Horizontal { column, row } => column.next().map(|column| Position {
                column: PositionIndex::new(column).expect("span should only produce valid indices"),
                row: *row,
            }),
            Self::Vertical { column, row } => row.next().map(|row| Position {
                column: *column,
                row: PositionIndex::new(row).expect("span should only produce valid indices"),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::span::{PositionSpan, Span, SpanInclusive};

    #[test]
    fn span() {
        assert_eq!(Span::new(0, 5).collect::<Vec<_>>(), vec![0, 1, 2, 3, 4],);
    }

    #[test]
    fn span_reverse() {
        assert_eq!(Span::new(5, 2).collect::<Vec<_>>(), vec![5, 4, 3],);
    }

    #[test]
    fn span_inclusive() {
        assert_eq!(
            SpanInclusive::new(0, 5).collect::<Vec<_>>(),
            vec![0, 1, 2, 3, 4, 5],
        );
    }

    #[test]
    fn span_inclusive_reverse() {
        assert_eq!(
            SpanInclusive::new(5, 0).collect::<Vec<_>>(),
            vec![5, 4, 3, 2, 1, 0]
        );
    }

    #[test]
    fn span_positions_horizontal() {
        assert_eq!(
            PositionSpan::new(Position::new(0, 0).unwrap(), Position::new(3, 0).unwrap())
                .unwrap()
                .collect::<Vec<_>>(),
            vec![
                Position::new(0, 0).unwrap(),
                Position::new(1, 0).unwrap(),
                Position::new(2, 0).unwrap(),
            ]
        );
        assert_eq!(
            PositionSpan::new(Position::new(3, 0).unwrap(), Position::new(0, 0).unwrap())
                .unwrap()
                .collect::<Vec<_>>(),
            vec![
                Position::new(3, 0).unwrap(),
                Position::new(2, 0).unwrap(),
                Position::new(1, 0).unwrap(),
            ]
        );
    }

    #[test]
    fn span_positions_vertical() {
        assert_eq!(
            PositionSpan::new(Position::new(0, 0).unwrap(), Position::new(0, 3).unwrap())
                .unwrap()
                .collect::<Vec<_>>(),
            vec![
                Position::new(0, 0).unwrap(),
                Position::new(0, 1).unwrap(),
                Position::new(0, 2).unwrap(),
            ]
        );
        assert_eq!(
            PositionSpan::new(Position::new(0, 3).unwrap(), Position::new(0, 0).unwrap())
                .unwrap()
                .collect::<Vec<_>>(),
            vec![
                Position::new(0, 3).unwrap(),
                Position::new(0, 2).unwrap(),
                Position::new(0, 1).unwrap(),
            ]
        );
    }

    #[test]
    fn span_positions_diagonal() {
        assert_eq!(
            PositionSpan::new(Position::new(0, 0).unwrap(), Position::new(3, 3).unwrap())
                .unwrap()
                .collect::<Vec<_>>(),
            vec![
                Position::new(0, 0).unwrap(),
                Position::new(1, 1).unwrap(),
                Position::new(2, 2).unwrap(),
            ]
        );
        assert_eq!(
            PositionSpan::new(Position::new(3, 3).unwrap(), Position::new(0, 0).unwrap())
                .unwrap()
                .collect::<Vec<_>>(),
            vec![
                Position::new(3, 3).unwrap(),
                Position::new(2, 2).unwrap(),
                Position::new(1, 1).unwrap(),
            ]
        );
    }
    #[test]
    fn span_positions_invalid() {
        assert_eq!(
            PositionSpan::new(Position::new(0, 0).unwrap(), Position::new(1, 2).unwrap()),
            None
        );
    }
}

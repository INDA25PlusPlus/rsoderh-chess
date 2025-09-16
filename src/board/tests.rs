use super::*;

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

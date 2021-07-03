
use std::fmt::Debug;
use std::cell::Cell;
use std::rc::Rc;
use std::ops::{Deref, Range, RangeFrom, RangeFull, RangeTo};
use std::str::{Chars, CharIndices};
use nom::{Err, FindSubstring, Compare, InputTake, Parser, Needed, IResult, CompareResult, InputIter, InputLength, Offset, UnspecializedInput, Slice};
use nom::error::{ErrorKind, ParseError};
use nom::bytes::complete::{take, take_while, take_until, take_while1, tag};
use nom::combinator::recognize;
use nom::multi::{many0, many1};
use nom::branch::alt;
use nom::sequence::{delimited, preceded, terminated};
use crate::frontend::ast::Position;

#[derive(Debug)]
pub struct Error<'a> {
    pub input: Input<'a>,
    pub code: ErrorKind,
}

impl<'a> ParseError<Input<'a>> for Error<'a> {
    fn from_error_kind(input: Input<'a>, kind: ErrorKind) -> Self {
        let len = input.input_len();
        let mut state = input.max_parsed.get();
        if state.1 > len || state.0.is_none() {
            state.0 = Some(kind);
            state.1 = len;
        }
        input.max_parsed.set(state);
        Error { input, code: kind }
    }

    fn append(_: Input<'a>, _: ErrorKind, other: Self) -> Self {
        let len = other.input.input_len();
        let mut state = other.input.max_parsed.get();
        if state.1 > len || state.0.is_none() {
            state.0 = Some(other.code);
            state.1 = len;
        }
        other.input.max_parsed.set(state);
        other
    }
}

#[derive(Clone, Debug)]
pub struct Input<'a> {
    data: &'a str,
    max_parsed: Rc<Cell<(Option<ErrorKind>, usize)>>
}

impl<'a> Input<'a> {
    pub fn new(data: &'a str) -> Self {
        Input { data: data, max_parsed: Rc::new(Cell::new((None, 0))) }
    }
    pub fn position(self: &Self) -> Position {
        self.input_len() as Position
    }
    pub fn max_parsed(self: &Self) -> (Option<ErrorKind>, usize) {
        self.max_parsed.get()
    }
    fn from_str(self: &Self, data: &'a str) -> Self {
        Input { data: data, max_parsed: self.max_parsed.clone() }
    }
}

impl<'a> Deref for Input<'a> {
    type Target = &'a str;
    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<'a> PartialEq for Input<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}

impl<'a> UnspecializedInput for Input<'a> { }

impl<'a> InputLength for Input<'a> {
    #[inline]
    fn input_len(&self) -> usize {
      self.data.len()
    }
  }

impl<'a> InputTake for Input<'a> {
    #[inline]
    fn take(&self, count: usize) -> Self {
        self.from_str(&self.data[..count])
    }
    #[inline]
    fn take_split(&self, count: usize) -> (Self, Self) {
        (self.from_str(&self.data[count..]), self.from_str(&self.data[..count]))
    }
}

impl<'a> InputIter for Input<'a> {
    type Item = char;
    type Iter = CharIndices<'a>;
    type IterElem = Chars<'a>;
    #[inline]
    fn iter_indices(&self) -> Self::Iter {
        self.data.char_indices()
    }
    #[inline]
    fn iter_elements(&self) -> Self::IterElem {
        self.data.chars()
    }
    fn position<P>(&self, predicate: P) -> Option<usize> where P: Fn(Self::Item) -> bool {
        self.data.position(predicate)
    }
    #[inline]
    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        self.data.slice_index(count)
    }
}

impl<'a, 'b> Compare<&'b str> for Input<'a> {
    #[inline(always)]
    fn compare(&self, t: &'b str) -> CompareResult {
        self.data.compare(t)
    }
    #[inline(always)]
    fn compare_no_case(&self, t: &'b str) -> CompareResult {
        self.data.compare_no_case(t)
    }
}

impl<'a, 'b> Compare<Input<'b>> for Input<'a> {
    #[inline(always)]
    fn compare(&self, t: Input<'b>) -> CompareResult {
        self.data.compare(&t)
    }
    #[inline(always)]
    fn compare_no_case(&self, t: Input<'b>) -> CompareResult {
        self.data.compare_no_case(&t)
    }
}

impl<'a, 'b> FindSubstring<Input<'b>> for Input<'a> {
    fn find_substring(&self, substr: Input<'b>) -> Option<usize> {
        self.data.find_substring(substr.data)
    }
}

impl<'a, 'b> FindSubstring<&'b str> for Input<'a> {
    fn find_substring(&self, substr: &'b str) -> Option<usize> {
        self.data.find_substring(substr)
    }
}

impl<'a> Offset for Input<'a> {
    fn offset(&self, second: &Self) -> usize {
        self.data.offset(second.data)
    }
}

impl<'a> Slice<Range<usize>> for Input<'a> {
    fn slice(&self, range: Range<usize>) -> Self {
        self.from_str(&self.data[range])
    }
}

impl<'a> Slice<RangeTo<usize>> for Input<'a> {
    fn slice(&self, range: RangeTo<usize>) -> Self {
        self.from_str(&self.data[range])
    }
}

impl<'a> Slice<RangeFrom<usize>> for Input<'a> {
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        self.from_str(&self.data[range])
    }
}

impl<'a> Slice<RangeFull> for Input<'a> {
    fn slice(&self, range: RangeFull) -> Self {
        self.from_str(&self.data[range])
    }
}

pub type Output<'a, O> = nom::IResult<Input<'a>, O, Error<'a>>;

/// Utility combinator to debug nom functions.
#[allow(dead_code)]
pub fn trace<'a, I, F, O, E>(context: &'static str, mut f: F) -> impl FnMut(I) -> IResult<I, O, E>
where
    F: FnMut(I) -> IResult<I, O, E>,
    I: Debug,
    E: Debug
{
    move |i: I| match f(i) {
        Err(e) => {
            println!("{}: Error({:?})", context, e);
            Err(e)
        }
        a => {
            println!("{}: Ok", context);
            a
        },
    }
}

#[allow(unused_macros)]
macro_rules! trace {
    ($x:expr) => {
        trace(stringify!($x), $x)
    };
}

/// fold_many0 implementation that doesn't suck.
pub fn fold_many0<I, O, E, F, G, R>(mut f: F, init: R, mut g: G) -> impl FnOnce(I) -> IResult<I, R, E>
where
    I: Clone + PartialEq,
    F: Parser<I, O, E>,
    G: FnMut(R, O) -> R,
    E: nom::error::ParseError<I>
{
    move |i: I| {
        let mut res = init;
        let mut input = i;

        loop {
            let i_ = input.clone();
            match f.parse(i_) {
                Ok((i, o)) => {
                    // loop trip must always consume (otherwise infinite loops)
                    if i == input {
                        return Err(Err::Error(E::from_error_kind(input, ErrorKind::Many0)));
                    }

                    res = g(res, o);
                    input = i;
                }
                Err(Err::Error(_)) => {
                    return Ok((input, res));
                }
                Err(e) => {
                    return Err(e);
                }
            }
        }
    }
}

/// Allow optional whitespace before and after matching the inner parser.
pub fn ws<'a, F: 'a, O, E: 'a + nom::error::ParseError<Input<'a>>>(p: F) -> impl FnMut(Input<'a>) -> IResult<Input<'a>, O, E>
where
    F: FnMut(Input<'a>) -> IResult<Input<'a>, O, E>,
    E: Debug
{
    delimited(
        space0,
        p,
        space0
    )
}

/// Expect at least one whitespace before matching the inner parser.
pub fn sepl<'a, F: 'a, O, E: 'a + nom::error::ParseError<Input<'a>>>(p: F) -> impl FnMut(Input<'a>) -> IResult<Input<'a>, O, E>
where
    F: FnMut(Input<'a>) -> IResult<Input<'a>, O, E>,
    E: Debug
{
    preceded(
        space1,
        p
    )
}

/// Expect at least one whitespace after matching the inner parser.
pub fn sepr<'a, F: 'a, O, E: 'a + nom::error::ParseError<Input<'a>>>(p: F) -> impl FnMut(Input<'a>) -> IResult<Input<'a>, O, E>
where
    F: FnMut(Input<'a>) -> IResult<Input<'a>, O, E>,
    E: Debug
{
    terminated(
        p,
        space1
    )
}

/// Restores function removed from nom 5
fn take_until_and_consume<T, I, E>(tag: T) -> impl FnMut(I) -> nom::IResult<I, I, E>
where
    E: nom::error::ParseError<I>,
    I: nom::InputTake
        + nom::FindSubstring<T>
        + nom::Slice<std::ops::RangeFrom<usize>>
        + nom::InputIter<Item = char>
        + nom::InputLength,
    T: nom::InputLength + Clone,
{
    move |input| terminated(take_until(tag.clone()), take(tag.input_len()))(input)
}

pub fn space0<'a, I: 'a, E: nom::error::ParseError<I>>(input: I) -> IResult<I, I, E>
where
    E: nom::error::ParseError<I> + Debug,
    I: nom::InputTake
        + nom::FindSubstring<I>
        + nom::Slice<std::ops::RangeFrom<usize>>
        + nom::Slice<std::ops::RangeTo<usize>>
        + nom::InputIter<Item = char>
        + nom::InputLength
        + nom::InputTakeAtPosition<Item = char>
        + nom::Offset
        + nom::Compare<&'a str>
        + nom::FindSubstring<&'a str>
        + Clone
        + PartialEq
        + Debug
{
    recognize(many0(alt((
        preceded(tag("//"), take_while(not_eol)),
        preceded(tag("/*"), take_until_and_consume("*/")), // TODO: check if this proceeds
        take_while1(is_whitespace)
    ))))(input)
}

pub fn space1<'a, I: 'a, E: nom::error::ParseError<I>>(input: I) -> IResult<I, I, E>
where
    E: nom::error::ParseError<I> + Debug,
    I: nom::InputTake
        + nom::FindSubstring<I>
        + nom::Slice<std::ops::RangeFrom<usize>>
        + nom::Slice<std::ops::RangeTo<usize>>
        + nom::InputIter<Item = char>
        + nom::InputLength
        + nom::InputTakeAtPosition<Item = char>
        + nom::Offset
        + nom::Compare<&'a str>
        + nom::FindSubstring<&'a str>
        + Clone
        + PartialEq
        + Debug
{
    recognize(many1(alt((
        preceded(tag("//"), take_while(not_eol)),
        preceded(tag("/*"), take_until_and_consume("*/")), // TODO: check if this proceeds
        take_while1(is_whitespace)
    ))))(input)
}

/// returns true if given character is a whitespace character
fn is_whitespace(chr: char) -> bool {
    //println!("is_whitespace");
    chr == ' ' || chr == '\t' || chr == '\r' || chr == '\n'
}

/// returns true if given character is not an end of line character
fn not_eol(chr: char) -> bool {
    //println!("noteol");
    chr != '\r' && chr != '\n'
}

/// Splits numerical value from its type suffix (if it has any)
pub fn splits_numerical_suffix(n: Input<'_>) -> (&str, Option<&str>) {
    let tail = if n.input_len() > 3 {
        let tail = &n[n.input_len() - 3 ..];
        if &tail[0..1] != "u" && &tail[0..1] != "i" && &tail[0..1] != "f" {
            &tail[1..]
        } else {
            tail
        }
    } else if n.input_len() > 2 {
        &n[n.input_len() - 2 ..]
    } else {
        ""
    };

    if tail.input_len() == 2 && (tail == "u8" || tail == "i8") {
        (&n[0..n.input_len() - 2], Some(tail))
    } else if tail.input_len() == 3 && (&tail[0..1] == "u" || &tail[0..1] == "i" || &tail[0..1] == "f") && (&tail[1..] == "16" || &tail[1..] == "32" || &tail[1..] == "64") && tail != "f16" {
        (&n[0..n.input_len() - 3], Some(tail))
    } else {
        (*n, None)
    }
}

/// Returns whether the given signed numerical is within range of the named type.
pub fn check_signed_range(num: i64, type_name: Option<&str>) -> bool {
    match type_name {
        Some("i8")  => num >= i8::MIN as i64 && num <= i8::MAX as i64,
        Some("i16") => num >= i16::MIN as i64 && num <= i16::MAX as i64,
        Some("i32") => num >= i32::MIN as i64 && num <= i32::MAX as i64,
        Some("i64") => true,
        None        => true,
        _           => false,
    }
}

/// Returns whether the given unsigned numerical is within range of the named type.
pub fn check_unsigned_range(num: u64, type_name: Option<&str>) -> bool {
    match type_name {
        Some("u8")  => num >= u8::MIN as u64 && num <= u8::MAX as u64,
        Some("u16") => num >= u16::MIN as u64 && num <= u16::MAX as u64,
        Some("u32") => num >= u32::MIN as u64 && num <= u32::MAX as u64,
        Some("u64") => true,
        None        => true,
        _           => false,
    }
}
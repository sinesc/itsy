use std::ops::{Range, RangeFrom, RangeFull, RangeTo};
use std::str::{Chars, CharIndices};
use nom::bytes::complete::{take, take_while, take_until, take_while1, tag};
use nom::combinator::recognize;
use nom::multi::{many0, many1};
use nom::branch::alt;
use nom::sequence::{delimited, preceded, terminated};
use crate::prelude::Debug;
use crate::frontend::parser::{Input, Failure, error::ParseErrorKind};

impl<'a> nom::error::ParseError<Input<'a>> for Failure<'a> {
    fn from_error_kind(input: Input<'a>, _: nom::error::ErrorKind) -> Self {
        use nom::InputLength;
        let len = input.input_len();
        input.max_parsed_mut(|max_parsed| {
            if max_parsed.1 > len || max_parsed.0.is_none() {
                max_parsed.0 = Some(ParseErrorKind::SyntaxError);
                max_parsed.1 = len;
            }
        });
        Failure { input, kind: ParseErrorKind::SyntaxError }
    }
    fn append(_: Input<'a>, _: nom::error::ErrorKind, other: Self) -> Self {
        use nom::InputLength;
        let len = other.input.input_len();
        other.input.max_parsed_mut(|max_parsed| {
            if max_parsed.1 > len || max_parsed.0.is_none() {
                max_parsed.0 = Some(ParseErrorKind::SyntaxError);
                max_parsed.1 = len;
            }
        });
        other
    }
}

impl<'a> nom::UnspecializedInput for Input<'a> { }

impl<'a> nom::InputLength for Input<'a> {
    #[inline]
    fn input_len(&self) -> usize {
        self.data.len()
    }
  }

impl<'a> nom::InputTake for Input<'a> {
    #[inline]
    fn take(&self, count: usize) -> Self {
        self.from_str(&self.data[..count])
    }
    #[inline]
    fn take_split(&self, count: usize) -> (Self, Self) {
        (self.from_str(&self.data[count..]), self.from_str(&self.data[..count]))
    }
}

impl<'a> nom::InputIter for Input<'a> {
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
    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        self.data.slice_index(count)
    }
}

impl<'a, 'b> nom::Compare<&'b str> for Input<'a> {
    #[inline(always)]
    fn compare(&self, t: &'b str) -> nom::CompareResult {
        self.data.compare(t)
    }
    #[inline(always)]
    fn compare_no_case(&self, t: &'b str) -> nom::CompareResult {
        self.data.compare_no_case(t)
    }
}

impl<'a, 'b> nom::Compare<Input<'b>> for Input<'a> {
    #[inline(always)]
    fn compare(&self, t: Input<'b>) -> nom::CompareResult {
        self.data.compare(t.data)
    }
    #[inline(always)]
    fn compare_no_case(&self, t: Input<'b>) -> nom::CompareResult {
        self.data.compare_no_case(t.data)
    }
}

impl<'a, 'b> nom::FindSubstring<Input<'b>> for Input<'a> {
    fn find_substring(&self, substr: Input<'b>) -> Option<usize> {
        self.data.find_substring(substr.data)
    }
}

impl<'a, 'b> nom::FindSubstring<&'b str> for Input<'a> {
    fn find_substring(&self, substr: &'b str) -> Option<usize> {
        self.data.find_substring(substr)
    }
}

impl<'a> nom::Offset for Input<'a> {
    fn offset(&self, second: &Self) -> usize {
        self.data.offset(second.data)
    }
}

impl<'a> nom::Slice<Range<usize>> for Input<'a> {
    fn slice(&self, range: Range<usize>) -> Self {
        self.from_str(&self.data[range])
    }
}

impl<'a> nom::Slice<RangeTo<usize>> for Input<'a> {
    fn slice(&self, range: RangeTo<usize>) -> Self {
        self.from_str(&self.data[range])
    }
}

impl<'a> nom::Slice<RangeFrom<usize>> for Input<'a> {
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        self.from_str(&self.data[range])
    }
}

impl<'a> nom::Slice<RangeFull> for Input<'a> {
    fn slice(&self, range: RangeFull) -> Self {
        self.from_str(&self.data[range])
    }
}

/// Utility combinator to debug nom functions.
#[allow(dead_code)]
pub(super) fn trace<'a, I, F, O, E>(context: &'static str, mut f: F) -> impl FnMut(I) -> nom::IResult<I, O, E>
where
    F: FnMut(I) -> nom::IResult<I, O, E>,
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

/// Fixes non-provided fold_many0 implementation to have less strict bounds.
pub(super) fn fold_many0_mut<I, O, E, F, G, R>(mut f: F, init: R, mut g: G) -> impl FnOnce(I) -> nom::IResult<I, R, E>
where
    I: Clone + PartialEq,
    F: nom::Parser<I, O, E>,
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
                        return Err(nom::Err::Error(E::from_error_kind(input, nom::error::ErrorKind::Many0)));
                    }

                    res = g(res, o);
                    input = i;
                }
                Err(nom::Err::Error(_)) => {
                    return Ok((input, res));
                }
                Err(e) => {
                    return Err(e);
                }
            }
        }
    }
}

/// Fixes nom-provided map_res function to return Err::Failure instead of Err::Err, remove need for some additional trait
pub(super) fn map_result<'a, I: Clone, O1, O2, P, F>(mut parser: P, mut f: F) -> impl FnMut(I) -> nom::IResult<I, O2, Failure<'a>>
where
    P: nom::Parser<I, O1, Failure<'a>>,
    F: FnMut(O1) -> Result<O2, Failure<'a>>,
{
    move |input: I| {
        let (input, o1) = parser.parse(input)?;
        match f(o1) {
            Ok(o2) => Ok((input, o2)),
            Err(e) => Err(nom::Err::Failure(e)),
        }
    }
}

/// Allow optional whitespace before and after matching the inner parser.
pub(super) fn ws<'a, F: 'a, O, E: 'a + nom::error::ParseError<Input<'a>>>(p: F) -> impl FnMut(Input<'a>) -> nom::IResult<Input<'a>, O, E>
where
    F: FnMut(Input<'a>) -> nom::IResult<Input<'a>, O, E>,
    E: Debug
{
    delimited(
        space0,
        p,
        space0
    )
}

/// Expect at least one whitespace before matching the inner parser.
pub(super) fn sepl<'a, F: 'a, O, E: 'a + nom::error::ParseError<Input<'a>>>(p: F) -> impl FnMut(Input<'a>) -> nom::IResult<Input<'a>, O, E>
where
    F: FnMut(Input<'a>) -> nom::IResult<Input<'a>, O, E>,
    E: Debug
{
    preceded(
        space1,
        p
    )
}

/// Expect at least one whitespace after matching the inner parser.
pub(super) fn sepr<'a, F: 'a, O, E: 'a + nom::error::ParseError<Input<'a>>>(p: F) -> impl FnMut(Input<'a>) -> nom::IResult<Input<'a>, O, E>
where
    F: FnMut(Input<'a>) -> nom::IResult<Input<'a>, O, E>,
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

pub(super) fn space0<'a, I: 'a, E: nom::error::ParseError<I>>(input: I) -> nom::IResult<I, I, E>
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

pub(super) fn space1<'a, I: 'a, E: nom::error::ParseError<I>>(input: I) -> nom::IResult<I, I, E>
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
    chr == ' ' || chr == '\t' || chr == '\r' || chr == '\n'
}

/// returns true if given character is not an end of line character
fn not_eol(chr: char) -> bool {
    chr != '\r' && chr != '\n'
}

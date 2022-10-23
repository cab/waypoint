use axum::http::StatusCode;
use axum::routing::get;
use axum::{Json, Router};
use std::borrow::{Borrow, Cow};
use std::collections::HashMap;
use std::convert::Infallible;
use std::net::SocketAddr;
use std::str::FromStr;
use tracing::warn;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt::init();

    let app = Router::new().route("/", get(redirect));
    let addr = SocketAddr::from(([127, 0, 0, 1], 3000));
    tracing::debug!("listening on {}", addr);
    axum::Server::bind(&addr)
        .serve(app.into_make_service())
        .await
        .unwrap();

    Ok(())
}

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error("todo")]
    Todo,
}

impl From<Error> for StatusCode {
    fn from(err: Error) -> Self {
        // TODO
        warn!("{}", err);
        Self::INTERNAL_SERVER_ERROR
    }
}

async fn redirect() -> Result<Json<Link<'static>>, StatusCode> {
    let route = Link {
        path: LinkPath::parse("test"),
    };

    Ok(Json(route))
}

#[derive(Debug, Clone, serde::Serialize)]
struct Link {
    path: LinkPath,
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize)]
struct LinkPath {
    segments: Vec<LinkPathSegment>,
}

#[derive(Debug, PartialEq, Eq)]
struct LiteralPath<'a> {
    segments: Vec<LiteralPathSegment<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum LiteralPathSegment<'a> {
    Literal(&'a str),
    Separator,
    Empty,
}

impl<'a> LiteralPath<'a> {
    fn parse(s: &'a str) -> Self {
        let mut segments = parse_segments(
            s,
            |s| LiteralPathSegment::Separator,
            |s| LiteralPathSegment::Literal(s),
        );
        // reverse so we can pop from the "front"
        segments.reverse();
        Self { segments }
    }

    fn separator_count(&self) -> usize {
        self.segments
            .iter()
            .filter(|s| matches!(s, LiteralPathSegment::Separator))
            .count()
    }

    fn pop(&mut self) -> Option<LiteralPathSegment> {
        self.segments.pop()
    }

    //     fn get(&self, index: usize) -> Cow<'a, LiteralPathSegment> {
    //         self.segments
    //             .get(index)
    //             .map(Cow::Borrowed)
    //             .unwrap_or_else(|| Cow::Owned(LiteralPathSegment::Empty))
    //     }
}

fn parse_segments<'a, S>(
    s: &'a str,
    parse_separator: impl Fn(&'a str) -> S,
    parse_ident: impl Fn(&'a str) -> S,
) -> Vec<S> {
    let mut segments = Vec::new();
    let mut chars = s.char_indices().peekable();
    while let Some((index, ch)) = chars.peek() {
        if *ch == '/' {
            segments.push(parse_separator(&s[*index..(*index + 1)]));
            chars.next();
        } else {
            let start_index = *index;
            // let mut lit = vec![ch];
            chars.next();
            let mut end_index = start_index;
            while let Some((_, ch)) = chars.peek() {
                if *ch == '/' {
                    break;
                } else {
                    // lit.push(chars.next().unwrap());
                    end_index = chars.next().unwrap().0;
                }
            }
            let s = &s[start_index..(end_index + 1)];
            segments.push(parse_ident(s));
        }
    }
    segments
}

impl<'a> LinkPath<'a> {
    fn parse(s: &'a str) -> Self {
        Self {
            segments: parse_segments(
                s,
                |s| LinkPathSegment::Separator,
                |s| LinkPathSegment::parse(s),
            ),
        }
    }

    fn separator_count(&self) -> usize {
        self.segments
            .iter()
            .filter(|s| matches!(s, LinkPathSegment::Separator))
            .count()
    }

    fn matches<'b: 'a>(&self, path: &'b str) -> SegmentMatchResult<HashMap<String, &'b str>> {
        let mut path: LiteralPath<'b> = LiteralPath::parse(path);
        // TODO exit early if a segment doesn't match
        let (matches, extract) = self.segments.iter().enumerate().fold(
            (true, HashMap::new()),
            move |(matches, mut extract), (index, s)| {
                let p = path.pop().unwrap_or(LiteralPathSegment::Empty);
                eprintln!("{:?} vs {:?}", p, s);
                let result: SegmentMatchResult<&'b str> = s.matches(p);
                let matches = if matches { result.is_match } else { matches };
                if let Some(extract_value) = result.extract {
                    extract.insert("todo".to_string(), extract_value);
                }
                (matches, extract)
            },
        );
        SegmentMatchResult {
            is_match: matches,
            extract: Some(extract),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize)]
enum LinkPathSegment<'a> {
    Literal(&'a str),
    Wildcard(&'a str),
    Separator,
}

impl<'a> LinkPathSegment<'a> {
    fn parse(s: &'a str) -> Self {
        if s == "/" {
            Self::Separator
        } else if s.starts_with("%") && s.ends_with("%") {
            // TODO what if the string is "%%"
            let name = &s[1..(s.len() - 1)];
            Self::Wildcard(name)
        } else {
            Self::Literal(s)
        }
    }

    fn matches<'b>(&self, path: LiteralPathSegment<'b>) -> SegmentMatchResult<&'b str> {
        match (self, path) {
            (Self::Separator, LiteralPathSegment::Separator) => SegmentMatchResult {
                is_match: true,
                extract: None,
            },
            (Self::Literal(value), LiteralPathSegment::Literal(path)) => SegmentMatchResult {
                is_match: *value == path,
                extract: None,
            },
            (Self::Wildcard(name), LiteralPathSegment::Literal(value)) => SegmentMatchResult {
                is_match: true,
                extract: Some(value),
            },
            _ => SegmentMatchResult {
                is_match: false,
                extract: None,
            },
        }
    }
}

#[derive(Debug)]
struct SegmentMatchResult<E> {
    is_match: bool,
    extract: Option<E>,
}

#[cfg(test)]
mod test {
    use std::str::FromStr;

    use crate::{LinkPath, LinkPathSegment, LiteralPath, LiteralPathSegment};

    #[test]
    fn it_parses_literalpath() {
        let path = LiteralPath::parse("/");
        assert_eq!(
            path,
            LiteralPath {
                segments: vec![LiteralPathSegment::Separator]
            }
        );
        let path = LiteralPath::parse("test/");
        assert_eq!(
            path,
            LiteralPath {
                segments: vec![
                    LiteralPathSegment::Literal("test"),
                    LiteralPathSegment::Separator
                ]
            }
        );
    }

    #[test]
    fn it_parses_wildcards() {
        let path = LinkPathSegment::parse("%test%");
        assert_eq!(path, LinkPathSegment::Wildcard("test"));
    }

    #[test]
    fn it_parses_wildcards_in_paths() {
        let path = LinkPath::parse("test/%test%");
        assert_eq!(
            path,
            LinkPath {
                segments: vec![
                    LinkPathSegment::Literal("test"),
                    LinkPathSegment::Separator,
                    LinkPathSegment::Wildcard("test")
                ]
            }
        );
    }

    #[test]
    fn it_parses_separators() {
        let path = LinkPathSegment::parse("/");
        assert_eq!(path, LinkPathSegment::Separator);
    }

    #[test]
    fn it_parses_literals() {
        let path = LinkPathSegment::parse("test");
        assert_eq!(path, LinkPathSegment::Literal("test"));
    }

    #[test]
    fn it_parses_segments() {
        let path = LinkPath::parse("test/");
        assert_eq!(
            path,
            LinkPath {
                segments: vec![LinkPathSegment::Literal("test"), LinkPathSegment::Separator]
            }
        );
    }

    #[test]
    fn it_matches_paths() {
        let path = LinkPath::parse("test");
        assert_eq!(true, path.matches("test").is_match);
        assert_eq!(false, path.matches("testx").is_match);
        assert_eq!(false, path.matches("testtest").is_match);

        let path = LinkPath::parse("test/foo");
        assert_eq!(true, path.matches("test/foo").is_match);
        assert_eq!(false, path.matches("testx/foo").is_match);
        assert_eq!(false, path.matches("test/testx").is_match);
    }

    #[test]
    fn it_matches_separators() {
        let path = LinkPath::parse("/");
        assert_eq!(true, path.matches("/").is_match);
        let path = LinkPath::parse("test/");
        assert_eq!(true, path.matches("test/").is_match);
    }

    #[test]
    fn it_matches_wildcard_paths() {
        let path = LinkPath::parse("test/%w%");
        assert_eq!(false, path.matches("test").is_match);
        assert_eq!(true, path.matches("test/").is_match);
        assert_eq!(true, path.matches("test/foo").is_match);
    }
}

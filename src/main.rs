use axum::extract::Path;
use axum::http::StatusCode;
use axum::routing::get;
use axum::{Extension, Json, Router};
use std::borrow::{Borrow, Cow};
use std::collections::HashMap;
use std::convert::Infallible;
use std::fs::File;
use std::net::SocketAddr;
use std::str::FromStr;
use std::sync::Arc;
use tracing::{debug, info, warn};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt::init();

    let config = Config::load()?;
    debug!("using configuration {:#?}", config);

    let links = Arc::new(config.links.into_iter().map(Link::from).collect::<Vec<_>>());

    let app = Router::new()
        .route("/*route", get(redirect))
        .layer(Extension(links));
    let addr = SocketAddr::from(([127, 0, 0, 1], 3000));
    info!("listening on {}", addr);
    axum::Server::bind(&addr)
        .serve(app.into_make_service())
        .await
        .unwrap();

    Ok(())
}

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error(transparent)]
    Io(#[from] std::io::Error),
    #[error("invalid configuration")]
    Config(#[from] serde_yaml::Error),
}

impl From<Error> for StatusCode {
    fn from(err: Error) -> Self {
        // TODO
        warn!("{}", err);
        Self::INTERNAL_SERVER_ERROR
    }
}

async fn redirect(
    Path(path): Path<String>,
    Extension(links): Extension<Arc<Vec<Link>>>,
) -> Result<Json<Link>, StatusCode> {
    debug!("path {:?}", path);
    
    // TODO make this more efficient. 
    // store a trie for links starting with literals?
    let matched_link = links.iter().find(predicate)

    Ok(Json(route))
}

#[derive(Debug, Clone, serde::Serialize)]
struct Link {
    path: LinkPath,
}

#[derive(Debug, serde::Deserialize)]
struct Config {
    links: Vec<ConfigLink>,
}

#[derive(Debug, serde::Deserialize)]
struct ConfigLink(String);

impl From<ConfigLink> for Link {
    fn from(c: ConfigLink) -> Self {
        Self {
            path: LinkPath::parse(&c.0),
        }
    }
}

impl Config {
    fn load() -> Result<Self, Error> {
        let file = File::open("config.yaml")?;
        Ok(serde_yaml::from_reader(file)?)
    }
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
    fn parse(s: &'a str) -> LiteralPath<'a> {
        Self {
            segments: parse_segments(
                s,
                |s| LiteralPathSegment::Separator,
                |s| LiteralPathSegment::Literal(s),
            ),
        }
    }

    fn separator_count(&self) -> usize {
        self.segments
            .iter()
            .filter(|s| matches!(s, LiteralPathSegment::Separator))
            .count()
    }
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

impl LinkPath {
    fn parse(s: &str) -> Self {
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

    fn matches<'b>(&self, path: &'b str) -> SegmentMatchResult<HashMap<String, &'b str>> {
        let mut literal_path: LiteralPath<'b> = LiteralPath::parse(path);
        let (matches, extract) = self
            .segments
            .iter()
            .zip(
                literal_path
                    .segments
                    .into_iter()
                    // if the link path is longer, match aganst Empty
                    .chain(std::iter::repeat(LiteralPathSegment::Empty)),
            )
            .fold((true, HashMap::new()), |(matches, mut extract), (s, p)| {
                let result = s.matches(p);
                let matches = if matches { result.is_match } else { matches };
                if let Some(extract_value) = result.extract {
                    extract.insert("todo".to_string(), extract_value);
                }
                (matches, extract)
            });

        SegmentMatchResult {
            is_match: matches,
            extract: Some(extract),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize)]
enum LinkPathSegment {
    Literal(String),
    Wildcard(String),
    Separator,
}

impl LinkPathSegment {
    fn parse(s: &str) -> Self {
        if s == "/" {
            Self::Separator
        } else if s.starts_with("%") && s.ends_with("%") {
            // TODO what if the string is "%%"
            let name = &s[1..(s.len() - 1)];
            Self::Wildcard(name.to_string())
        } else {
            Self::Literal(s.to_string())
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
            (Self::Wildcard(name), LiteralPathSegment::Empty) => SegmentMatchResult {
                is_match: true,
                extract: None,
            },
            (Self::Wildcard(name), LiteralPathSegment::Separator) => SegmentMatchResult {
                is_match: true,
                extract: None,
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
        assert_eq!(path, LinkPathSegment::Wildcard("test".to_string()));
    }

    #[test]
    fn it_parses_wildcards_in_paths() {
        let path = LinkPath::parse("test/%test%");
        assert_eq!(
            path,
            LinkPath {
                segments: vec![
                    LinkPathSegment::Literal("test".to_string()),
                    LinkPathSegment::Separator,
                    LinkPathSegment::Wildcard("test".to_string())
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
        assert_eq!(path, LinkPathSegment::Literal("test".to_string()));
    }

    #[test]
    fn it_parses_segments() {
        let path = LinkPath::parse("test/");
        assert_eq!(
            path,
            LinkPath {
                segments: vec![
                    LinkPathSegment::Literal("test".to_string()),
                    LinkPathSegment::Separator
                ]
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
        assert_eq!(true, path.matches("test//").is_match);
        assert_eq!(true, path.matches("test/foo").is_match);
        assert_eq!(true, path.matches("test/foo/bar").is_match);
        assert_eq!(true, path.matches("test///").is_match);
        assert_eq!(true, path.matches("test///foo").is_match);

        let path = LinkPath::parse("test/%w%/f");
        assert_eq!(false, path.matches("test").is_match);
        assert_eq!(false, path.matches("test/").is_match);
        assert_eq!(false, path.matches("test//f").is_match);
        assert_eq!(true, path.matches("test///f").is_match);
        assert_eq!(true, path.matches("test/foo/f").is_match);
        assert_eq!(false, path.matches("test/foo/bar/f").is_match);
    }
}

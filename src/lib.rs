//! A tiny library to handle GFA file.
//! # Example
//! To create a graph with two nodes and a connecting edge between them;
//! ```rust
//! let header = Content::Header(Header::default());
//! let node1 = Content::Seg(Segment::from("seq1".to_string(), 1_000, None));
//! let node2 = Content::Seg(Segment::from("seq2".to_string(), 1_000, None));
//! let edge = Content::Edge(Edge::from(
//!     None,
//!     RefID::from("seq1", true),
//!     RefID::from("seq2", true),
//!     Position::from(1_000, false),
//!     Position::from(1_000, true),
//!     Position::from(0, false),
//!     Position::from(0, false),
//!     None,
//! ));
//! let records = [header, node1, node2, edge].map(|c| Record::from_contents(c, vec![]));
//! GFA::from_records(records.to_vec());
//!```
//! Of course, this is rather messy. As human-friendly API, I write `Graph` module.
pub mod graph;
#[derive(Debug, Clone)]
pub struct GFA {
    inner: Vec<Record>,
}

impl GFA {
    pub fn from_records(inner: Vec<Record>) -> Self {
        Self { inner }
    }
    pub fn from_reader<R: std::io::Read>(rdr: R) -> Self {
        use std::io::{BufRead, BufReader};
        let inner: Vec<_> = BufReader::new(rdr)
            .lines()
            .filter_map(|e| e.ok())
            .filter_map(|line| Record::from_line(&line))
            .collect();
        Self::from_records(inner)
    }
    pub fn iter(&self) -> std::slice::Iter<'_, Record> {
        self.inner.iter()
    }
    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, Record> {
        self.inner.iter_mut()
    }
}

#[derive(Debug, Clone)]
pub struct SamTags(Vec<SamTag>);
impl std::convert::From<Vec<SamTag>> for SamTags {
    fn from(tags: Vec<SamTag>) -> Self {
        SamTags(tags)
    }
}
impl SamTags {
    pub fn iter(&self) -> std::slice::Iter<'_, SamTag> {
        self.0.iter()
    }
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    pub fn find(&self, key: &str) -> Option<(&str, &str)> {
        self.0.iter().find_map(|tag| {
            let mut tag = tag.inner.split(':');
            if tag.next()? == key {
                let val_type = tag.next()?;
                let value = tag.next()?;
                Some((val_type, value))
            } else {
                None
            }
        })
    }
}

#[derive(Debug, Clone)]
pub struct Record {
    pub content: Content,
    pub tags: SamTags,
}

impl Record {
    pub fn from_line(line: &str) -> Option<Self> {
        let line: Vec<&str> = line.split('\t').collect();
        match line[0] {
            "H" => Header::new(&line[1..]).map(|(header, tags)| {
                let content = Content::Header(header);
                Self { content, tags }
            }),
            "S" => Segment::new(&line[1..]).map(|(segment, tags)| {
                let content = Content::Seg(segment);
                Self { content, tags }
            }),
            "F" => Fragment::new(&line[1..]).map(|(fragment, tags)| {
                let content = Content::Frag(fragment);
                Self { content, tags }
            }),
            "E" => Edge::new(&line[1..]).map(|(edge, tags)| {
                let content = Content::Edge(edge);
                Self { content, tags }
            }),
            "G" => Gap::new(&line[1..]).map(|(gap, tags)| {
                let content = Content::Gap(gap);
                Self { content, tags }
            }),
            "O" => OrderedGroup::new(&line[1..]).map(|(group, tags)| {
                let content = Content::Group(Group::Path(group));
                Self { content, tags }
            }),
            "U" => UnorderedGroup::new(&line[1..]).map(|(group, tags)| {
                let content = Content::Group(Group::Set(group));
                Self { content, tags }
            }),
            _ => None,
        }
    }
    pub fn from_contents(content: Content, tags: SamTags) -> Self {
        Self { content, tags }
    }
}

#[derive(Debug, Clone)]
pub enum Content {
    Header(Header),
    Seg(Segment),
    Frag(Fragment),
    Edge(Edge),
    Gap(Gap),
    Group(Group),
}

#[derive(Debug, Clone)]
pub struct Header {
    pub version: Option<String>,
    pub trace: Option<String>,
}

impl Header {
    pub fn new(line: &[&str]) -> Option<(Self, SamTags)> {
        let (version, trace, rest) = match line {
            ["VN:Z:2.0", x, rest @ ..] if x.starts_with("TS:i:") => {
                (Some("VN:Z:2.0".to_string()), Some(x.to_string()), rest)
            }
            ["VN:Z:2.0", rest @ ..] => (Some("VN:Z:2.0".to_string()), None, rest),
            [x, rest @ ..] if x.starts_with("TS:i:") => (None, Some(x.to_string()), rest),
            x => (None, None, x),
        };
        let tags = to_tags(rest);
        let header = Self { version, trace };
        Some((header, tags))
    }
}
impl std::default::Default for Header {
    fn default() -> Self {
        Self {
            version: Some("VN:Z:2.0".to_string()),
            trace: None,
        }
    }
}

fn to_tags(tags: &[&str]) -> SamTags {
    tags.iter()
        .map(|s| SamTag {
            inner: s.to_string(),
        })
        .collect::<Vec<_>>()
        .into()
}

#[derive(Debug, Clone, Default)]
pub struct Segment {
    pub sid: String,
    pub slen: u64,
    pub sequence: Option<String>,
}

impl Segment {
    pub fn from(sid: String, slen: usize, sequence: Option<String>) -> Self {
        let slen = slen as u64;
        Self {
            sid,
            slen,
            sequence,
        }
    }
    pub fn new(line: &[&str]) -> Option<(Self, SamTags)> {
        if line.len() < 3 {
            None
        } else {
            let sid = line[0].to_string();
            let slen: u64 = match line[1].parse() {
                Ok(res) => res,
                Err(res) => panic!("{:?}\t{:?}", res, line),
            };
            let sequence = if line[2] == "*" {
                None
            } else {
                Some(line[2].to_string())
            };
            let tags = if line.len() > 3 {
                to_tags(&line[3..])
            } else {
                vec![].into()
            };
            let segment = Self {
                sid,
                slen,
                sequence,
            };
            Some((segment, tags))
        }
    }
}

#[derive(Debug, Clone)]
pub struct Fragment {
    pub fid: String,
    pub external: RefID,
    pub sbeg: Position,
    pub send: Position,
    pub fbeg: Position,
    pub fend: Position,
    pub alignment: Option<Alignment>,
}

impl Fragment {
    pub fn new(line: &[&str]) -> Option<(Self, SamTags)> {
        if line.len() < 7 {
            None
        } else {
            let frag = Self {
                fid: line[0].to_string(),
                external: RefID::new(&line[1])?,
                sbeg: Position::new(&line[2])?,
                send: Position::new(&line[3])?,
                fbeg: Position::new(&line[4])?,
                fend: Position::new(&line[5])?,
                alignment: if line[6] == "*" {
                    None
                } else {
                    Alignment::new(&line[6])
                },
            };
            let tags = if line.len() > 7 {
                to_tags(&line[7..])
            } else {
                vec![].into()
            };
            Some((frag, tags))
        }
    }
}

#[derive(Debug, Clone)]
pub struct Edge {
    pub eid: Option<String>,
    pub sid1: RefID,
    pub sid2: RefID,
    pub beg1: Position,
    pub end1: Position,
    pub beg2: Position,
    pub end2: Position,
    pub alignment: Option<Alignment>,
}

impl Edge {
    pub fn new(line: &[&str]) -> Option<(Self, SamTags)> {
        if line.len() < 8 {
            None
        } else {
            let edge = Self {
                eid: if line[0] == "*" {
                    None
                } else {
                    Some(line[0].to_string())
                },
                sid1: RefID::new(&line[1])?,
                sid2: RefID::new(&line[2])?,
                beg1: Position::new(&line[3])?,
                end1: Position::new(&line[4])?,
                beg2: Position::new(&line[5])?,
                end2: Position::new(&line[6])?,
                alignment: if line[7] == "*" {
                    None
                } else {
                    Alignment::new(&line[7])
                },
            };
            let tags = if line.len() > 8 {
                to_tags(&line[8..])
            } else {
                vec![].into()
            };
            Some((edge, tags))
        }
    }
    #[allow(clippy::too_many_arguments)]
    pub fn from(
        eid: Option<String>,
        sid1: RefID,
        sid2: RefID,
        beg1: Position,
        end1: Position,
        beg2: Position,
        end2: Position,
        alignment: Option<Alignment>,
    ) -> Self {
        Self {
            eid,
            sid1,
            sid2,
            beg1,
            end1,
            beg2,
            end2,
            alignment,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Gap {
    pub gid: Option<String>,
    pub sid1: RefID,
    pub sid2: RefID,
    pub dist: i32,
    pub var: Option<i32>,
}

impl Gap {
    pub fn new(line: &[&str]) -> Option<(Self, SamTags)> {
        if line.len() < 5 {
            None
        } else {
            let gap = Self {
                gid: if line[0] == "*" {
                    None
                } else {
                    Some(line[0].to_string())
                },
                sid1: RefID::new(line[1])?,
                sid2: RefID::new(line[2])?,
                dist: line[3].parse().ok()?,
                var: if line[4] == "*" {
                    None
                } else {
                    line[4].parse().ok()
                },
            };
            let tags = if line.len() > 5 {
                to_tags(&line[5..])
            } else {
                vec![].into()
            };
            Some((gap, tags))
        }
    }
}

#[derive(Debug, Clone)]
pub enum Group {
    Set(UnorderedGroup),
    Path(OrderedGroup),
}

impl Group {
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
    pub fn len(&self) -> usize {
        match self {
            Group::Set(ug) => ug.ids.len(),
            Group::Path(og) => og.ids.len(),
        }
    }
    pub fn set(&self) -> Option<&UnorderedGroup> {
        match self {
            Group::Set(ug) => Some(ug),
            _ => None,
        }
    }
    pub fn path(&self) -> Option<&OrderedGroup> {
        match self {
            Group::Path(og) => Some(og),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct UnorderedGroup {
    pub uid: Option<String>,
    pub ids: Vec<String>,
}

impl UnorderedGroup {
    pub fn new(line: &[&str]) -> Option<(Self, SamTags)> {
        if line.len() < 2 {
            None
        } else {
            let uid = if line[0] == "*" {
                None
            } else {
                Some(line[0].to_string())
            };
            let ids = line[1].split_whitespace().map(|x| x.to_string()).collect();
            let group = Self { uid, ids };
            let tags = if line.len() > 2 {
                to_tags(&line[2..])
            } else {
                vec![].into()
            };
            Some((group, tags))
        }
    }
    pub fn iter(&self) -> std::slice::Iter<'_, String> {
        self.ids.iter()
    }
}

#[derive(Debug, Clone)]
pub struct OrderedGroup {
    pub oid: Option<String>,
    pub ids: Vec<RefID>,
}

impl OrderedGroup {
    pub fn iter(&self) -> std::slice::Iter<'_, RefID> {
        self.ids.iter()
    }
    pub fn new(line: &[&str]) -> Option<(Self, SamTags)> {
        if line.len() < 2 {
            None
        } else {
            let oid = if line[0] == "*" {
                None
            } else {
                Some(line[0].to_string())
            };
            let ids: Vec<_> = line[1]
                .split_whitespace()
                .filter_map(|x| RefID::new(x))
                .collect();
            let group = Self { oid, ids };
            let tags = if line.len() > 2 {
                to_tags(&line[2..])
            } else {
                vec![].into()
            };
            Some((group, tags))
        }
    }
}

#[derive(Debug, Clone)]
pub struct SamTag {
    pub inner: String,
}

impl SamTag {
    pub fn key(&self) -> Option<&str> {
        self.inner.split(':').next()
    }
    pub fn value_type(&self) -> Option<&str> {
        self.inner.split(':').nth(1)
    }
    pub fn value(&self) -> Option<&str> {
        self.inner.split(':').nth(2)
    }
    pub fn new(seq: String) -> Self {
        if seq.chars().filter(|&x| x == ':').count() != 2 {
            panic!("{} is invalid SamTag.", seq);
        } else {
            Self { inner: seq }
        }
    }
}

#[derive(Debug, Clone)]
pub struct RefID {
    pub direction: Direction,
    pub id: String,
}

impl RefID {
    pub fn new(seq: &str) -> Option<Self> {
        let mut id: String = seq.to_string();
        let direction = id.pop()?;
        let direction = if direction == '+' {
            Direction::Forward
        } else if direction == '-' {
            Direction::Reverse
        } else {
            return None;
        };
        Some(Self { direction, id })
    }
    pub fn from(id: &str, is_forward: bool) -> Self {
        Self {
            id: id.to_string(),
            direction: if is_forward {
                Direction::Forward
            } else {
                Direction::Reverse
            },
        }
    }
    pub fn is_forward(&self) -> bool {
        match self.direction {
            Direction::Forward => true,
            Direction::Reverse => false,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Direction {
    Forward,
    Reverse,
}

#[derive(Debug, Clone, Copy)]
pub struct Position {
    pub pos: usize,
    pub is_last: bool,
}

impl Position {
    pub fn new(seq: &str) -> Option<Self> {
        if seq.ends_with('$') {
            let mut seq: String = seq.to_string();
            seq.pop();
            seq.parse().ok().map(|pos| Self { pos, is_last: true })
        } else {
            let is_last = false;
            seq.parse().ok().map(|pos| Self { pos, is_last })
        }
    }
    pub fn from(pos: usize, is_last: bool) -> Self {
        Self { pos, is_last }
    }
}

#[derive(Debug, Clone)]
pub enum Alignment {
    Trace(Trace),
    Cigar(Cigar),
}

impl Alignment {
    pub fn new(seq: &str) -> Option<Self> {
        if seq.chars().any(|c| "MDIP".contains(c)) {
            let mut ops = vec![];
            let mut num = 0;
            for x in seq.chars() {
                if x.is_ascii_digit() {
                    num = 10 * num + x.to_digit(10)? as usize;
                } else {
                    if let Some(res) = CigarOp::from(num, x) {
                        ops.push(res);
                    }
                    num = 0;
                }
            }
            Some(Alignment::Cigar(Cigar { ops }))
        } else if seq.contains(',') {
            let inner: Vec<i32> = seq.split(',').filter_map(|x| x.parse().ok()).collect();
            Some(Alignment::Trace(Trace { inner }))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub struct Trace {
    pub inner: Vec<i32>,
}

#[derive(Debug, Clone)]
pub struct Cigar {
    pub ops: Vec<CigarOp>,
}

#[derive(Debug, Clone)]
pub enum CigarOp {
    Match(usize),
    Deletion(usize),
    Insertion(usize),
    Padding(usize),
}

impl CigarOp {
    pub fn from(num: usize, op: char) -> Option<Self> {
        match op {
            'M' => Some(Self::Match(num)),
            'I' => Some(Self::Insertion(num)),
            'D' => Some(Self::Deletion(num)),
            'P' => Some(Self::Padding(num)),
            _ => None,
        }
    }
}

impl std::fmt::Display for GFA {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let lines: Vec<_> = self.inner.iter().map(|c| format!("{}", c)).collect();
        write!(f, "{}", lines.join("\n"))
    }
}

impl std::fmt::Display for Record {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.tags.is_empty() {
            write!(f, "{}", self.content)
        } else {
            let tags: Vec<_> = self.tags.iter().map(|t| format!("{}", t)).collect();
            let tags = tags.join("\t");
            write!(f, "{}\t{}", self.content, tags)
        }
    }
}

impl std::fmt::Display for Content {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Content::Header(h) => write!(f, "{}", h),
            Content::Seg(s) => write!(f, "{}", s),
            Content::Frag(fr) => write!(f, "{}", fr),
            Content::Edge(e) => write!(f, "{}", e),
            Content::Gap(g) => write!(f, "{}", g),
            Content::Group(g) => write!(f, "{}", g),
        }
    }
}

impl std::fmt::Display for Header {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "H")?;
        if let Some(v) = &self.version {
            write!(f, "\t{}", v)?;
        }
        if let Some(t) = &self.trace {
            write!(f, "\t{}", t)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for Segment {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.sequence {
            Some(res) => write!(f, "S\t{}\t{}\t{}", self.sid, self.slen, res),
            None => write!(f, "S\t{}\t{}\t*", self.sid, self.slen),
        }
    }
}
impl std::fmt::Display for Fragment {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "F\t{}\t{}\t", self.fid, self.external)?;
        write!(
            f,
            "{}\t{}\t{}\t{}\t",
            self.sbeg, self.send, self.fbeg, self.fbeg
        )?;
        match &self.alignment {
            Some(aln) => write!(f, "{}", aln),
            None => write!(f, "*"),
        }
    }
}
impl std::fmt::Display for Edge {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.eid {
            Some(res) => write!(f, "E\t{}\t", res)?,
            None => write!(f, "E\t*\t")?,
        };
        write!(f, "{}\t{}\t", self.sid1, self.sid2)?;
        write!(
            f,
            "{}\t{}\t{}\t{}\t",
            self.beg1, self.end1, self.beg2, self.end2
        )?;
        match &self.alignment {
            Some(res) => write!(f, "{}", res),
            None => write!(f, "*"),
        }
    }
}
impl std::fmt::Display for Gap {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.gid {
            Some(res) => write!(f, "G\t{}\t", res)?,
            None => write!(f, "G\t*\t")?,
        };
        write!(f, "{}\t{}\t{}\t", self.sid1, self.sid2, self.dist)?;
        match &self.gid {
            Some(res) => write!(f, "{}", res),
            None => write!(f, "*"),
        }
    }
}
impl std::fmt::Display for Group {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Group::Set(s) => write!(f, "{}", s),
            Group::Path(p) => write!(f, "{}", p),
        }
    }
}

impl std::fmt::Display for UnorderedGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.uid {
            Some(res) => write!(f, "U\t{}\t", res)?,
            None => write!(f, "U\t*\t")?,
        };
        let ids: Vec<_> = self.ids.iter().map(|id| id.to_string()).collect();
        write!(f, "{}", ids.join(" "))
    }
}
impl std::fmt::Display for OrderedGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.oid {
            Some(res) => write!(f, "O\t{}\t", res)?,
            None => write!(f, "O\t*\t")?,
        }
        let ids: Vec<_> = self.ids.iter().map(|id| format!("{}", id)).collect();
        write!(f, "{}", ids.join(" "))
    }
}
impl std::fmt::Display for RefID {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}{}", self.id, self.direction)
    }
}
impl std::fmt::Display for Direction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Direction::Forward => write!(f, "+"),
            Direction::Reverse => write!(f, "-"),
        }
    }
}

impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.is_last {
            write!(f, "{}$", self.pos)
        } else {
            write!(f, "{}", self.pos)
        }
    }
}

impl std::fmt::Display for Alignment {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Alignment::Trace(t) => write!(f, "{}", t),
            Alignment::Cigar(c) => write!(f, "{}", c),
        }
    }
}

impl std::fmt::Display for Trace {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let inner: Vec<_> = self.inner.iter().map(|x| format!("{}", x)).collect();
        write!(f, "{}", inner.join(","))
    }
}

impl std::fmt::Display for Cigar {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for op in &self.ops {
            write!(f, "{}", op)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for CigarOp {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            CigarOp::Deletion(d) => write!(f, "{}D", d),
            CigarOp::Insertion(i) => write!(f, "{}I", i),
            CigarOp::Match(m) => write!(f, "{}M", m),
            CigarOp::Padding(p) => write!(f, "{}P", p),
        }
    }
}

impl std::fmt::Display for SamTag {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.inner)
    }
}

impl std::iter::IntoIterator for GFA {
    type Item = Record;
    type IntoIter = std::vec::IntoIter<Self::Item>;
    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}

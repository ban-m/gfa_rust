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
}

#[derive(Debug, Clone)]
pub struct Record {
    pub content: Content,
    pub tags: Vec<SamTag>,
}

impl Record {
    pub fn from_line(line: &str) -> Option<Self> {
        let line: Vec<&str> = line.split_whitespace().collect();
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
    pub fn from_contents(content: Content, tags: Vec<SamTag>) -> Self {
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
    pub fn new(line: &[&str]) -> Option<(Self, Vec<SamTag>)> {
        let (version, trace, rest) = match line {
            ["VN:Z:2.0", x, rest @ ..] if x.starts_with("TS:i:") => {
                (Some("VN:Z:2.0".to_string()), Some(x.to_string()), rest)
            }
            ["VN:Z:2.0", rest @ ..] => (Some("VN:Z:2.0".to_string()), None, rest),
            [x, rest @ ..] if x.starts_with("TS:i:") => (None, Some(x.to_string()), rest),
            [rest @ ..] => (None, None, rest),
        };
        let tags = to_tags(rest);
        let header = Self { version, trace };
        Some((header, tags))
    }
}

fn to_tags(tags: &[&str]) -> Vec<SamTag> {
    tags.iter()
        .map(|s| SamTag {
            inner: s.to_string(),
        })
        .collect()
}

#[derive(Debug, Clone)]
pub struct Segment {
    pub sid: String,
    pub slen: u64,
    pub sequence: Option<String>,
}

impl Segment {
    pub fn new(line: &[&str]) -> Option<(Self, Vec<SamTag>)> {
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
                vec![]
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
    pub fn new(line: &[&str]) -> Option<(Self, Vec<SamTag>)> {
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
                vec![]
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
    pub fn new(line: &[&str]) -> Option<(Self, Vec<SamTag>)> {
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
                vec![]
            };
            Some((edge, tags))
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
    pub fn new(line: &[&str]) -> Option<(Self, Vec<SamTag>)> {
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
                vec![]
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

#[derive(Debug, Clone)]
pub struct UnorderedGroup {
    pub uid: Option<String>,
    pub ids: Vec<RefID>,
}

impl UnorderedGroup {
    pub fn new(line: &[&str]) -> Option<(Self, Vec<SamTag>)> {
        if line.len() < 2 {
            None
        } else {
            let uid = if line[0] == "*" {
                None
            } else {
                Some(line[0].to_string())
            };
            let (refs, tags): (Vec<&str>, Vec<_>) =
                line[1..].iter().partition(|x| RefID::new(x).is_some());
            let ids: Vec<_> = refs.iter().filter_map(|x| RefID::new(x)).collect();
            let group = Self { uid, ids };
            let tags: Vec<_> = to_tags(&tags);
            Some((group, tags))
        }
    }
}

#[derive(Debug, Clone)]
pub struct OrderedGroup {
    pub oid: Option<String>,
    pub ids: Vec<RefID>,
}

impl OrderedGroup {
    pub fn new(line: &[&str]) -> Option<(Self, Vec<SamTag>)> {
        if line.len() < 2 {
            None
        } else {
            let oid = if line[0] == "*" {
                None
            } else {
                Some(line[0].to_string())
            };
            let (refs, tags): (Vec<&str>, Vec<_>) =
                line[1..].iter().partition(|x| RefID::new(x).is_some());
            let ids: Vec<_> = refs.iter().filter_map(|x| RefID::new(x)).collect();
            let group = Self { oid, ids };
            let tags: Vec<_> = to_tags(&tags);
            Some((group, tags))
        }
    }
}

#[derive(Debug, Clone)]
pub struct SamTag {
    pub inner: String,
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
}

#[derive(Debug, Clone)]
pub enum Direction {
    Forward,
    Reverse,
}

#[derive(Debug, Clone)]
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
            let inner: Vec<i32> = seq.split(",").filter_map(|x| x.parse().ok()).collect();
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
        let ids: Vec<_> = self.ids.iter().map(|id| format!("{}", id)).collect();
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

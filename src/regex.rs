use crate::{Error, Result, grammar::ValidGrammar};
use indexmap::IndexSet;
use std::{cell::OnceCell, collections::BTreeMap, io::Write};

use roaring::RoaringBitmap;
use ustr::Ustr;

use crate::grammar::{Expr, ExprId, HumanSpan};

// Serves as RegexInputId
pub type Position = u32;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RegexInput {
    Literal {
        literal: Ustr,
        description: Option<Ustr>,
        fallback_level: usize,
        span: HumanSpan,
    },
    Nonterminal {
        nonterm: Ustr,
        fallback_level: usize,
        span: HumanSpan,
    },
    Command {
        cmd: Ustr,
        zsh_compadd: bool,
        fallback_level: usize,
        span: HumanSpan,
    },
    Subword {
        subword_regex_id: RegexId,
        fallback_level: usize,
        span: HumanSpan,
    },
}

impl RegexInput {
    fn is_star_subword(&self) -> bool {
        match self {
            Self::Nonterminal { .. } => true,
            Self::Literal { .. } | Self::Command { .. } => false,
            Self::Subword { .. } => unreachable!(),
        }
    }

    pub fn get_span(&self) -> HumanSpan {
        match self {
            Self::Literal { span, .. } => *span,
            Self::Subword { span, .. } => *span,
            Self::Nonterminal { span, .. } => *span,
            Self::Command { span, .. } => *span,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct RegexNodeId(usize);

impl std::fmt::Display for RegexNodeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::ops::Index<RegexNodeId> for Vec<RegexNode> {
    type Output = RegexNode;

    fn index(&self, index: RegexNodeId) -> &Self::Output {
        &self[index.0]
    }
}

impl std::ops::Index<RegexNodeId> for [RegexNode] {
    type Output = RegexNode;

    fn index(&self, index: RegexNodeId) -> &Self::Output {
        &self[index.0]
    }
}

fn alloc(arena: &mut Vec<RegexNode>, elem: RegexNode) -> RegexNodeId {
    let id = arena.len();
    arena.push(elem);
    RegexNodeId(id)
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum RegexNode {
    Epsilon,

    Terminal(Position),
    Nonterminal(Position),
    Command(Position),
    Subword(Position),
    EndMarker(Position),

    Cat(Vec<RegexNodeId>),
    Or(Vec<RegexNodeId>),
    Star(RegexNodeId),
}

impl std::fmt::Debug for RegexNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Subword(position) => f.write_fmt(format_args!(r#"Subword({position})"#)),
            Self::Terminal(position) => f.write_fmt(format_args!(r#"Terminal({position})"#)),
            Self::Nonterminal(position) => f.write_fmt(format_args!(r#"Nonterminal({position})"#)),
            Self::Command(position) => f.write_fmt(format_args!(r#"Command({position})"#)),
            Self::Cat(children) => f.write_fmt(format_args!(r#"Cat({children:?})"#)),
            Self::Or(children) => f.write_fmt(format_args!(r#"Or(vec!{children:?})"#)),
            Self::Star(child) => f.write_fmt(format_args!(r#"Star({child:?})"#)),
            Self::EndMarker(position) => f.write_fmt(format_args!(r#"EndMarker({position})"#)),
            Self::Epsilon => f.write_fmt(format_args!(r#"Epsilon"#)),
        }
    }
}

fn do_firstpos(re: &RegexNode, arena: &[RegexNode], result: &mut RoaringBitmap) {
    match re {
        RegexNode::Epsilon => {}
        RegexNode::Subword(position) => {
            result.insert(*position);
        }
        RegexNode::Terminal(position) => {
            result.insert(*position);
        }
        RegexNode::Nonterminal(position) => {
            result.insert(*position);
        }
        RegexNode::Command(position) => {
            result.insert(*position);
        }
        RegexNode::Or(children) => {
            for child_id in children {
                let child = &arena[*child_id];
                do_firstpos(child, arena, result);
            }
        }
        RegexNode::Cat(children) => {
            for child_id in children {
                let child = &arena[*child_id];
                do_firstpos(child, arena, result);
                if !child.nullable(arena) {
                    break;
                }
            }
        }
        RegexNode::Star(child_id) => {
            let child = &arena[*child_id];
            do_firstpos(child, arena, result);
        }
        RegexNode::EndMarker(position) => {
            result.insert(*position);
        }
    }
}

fn do_lastpos(re: &RegexNode, arena: &[RegexNode], result: &mut RoaringBitmap) {
    match re {
        RegexNode::Epsilon => {}
        RegexNode::Subword(position) => {
            result.insert(*position);
        }
        RegexNode::Terminal(position) => {
            result.insert(*position);
        }
        RegexNode::Nonterminal(position) => {
            result.insert(*position);
        }
        RegexNode::Command(position) => {
            result.insert(*position);
        }
        RegexNode::Or(children) => {
            for child_id in children {
                let child = &arena[*child_id];
                do_lastpos(child, arena, result);
            }
        }
        RegexNode::Cat(children) => {
            for child_id in children.iter().rev() {
                let child = &arena[*child_id];
                do_lastpos(child, arena, result);
                if !child.nullable(arena) {
                    break;
                }
            }
        }
        RegexNode::Star(child_id) => {
            let child = &arena[*child_id];
            do_lastpos(child, arena, result);
        }
        RegexNode::EndMarker(position) => {
            result.insert(*position);
        }
    }
}

fn do_followpos(
    re: &RegexNode,
    arena: &[RegexNode],
    result: &mut BTreeMap<Position, RoaringBitmap>,
) {
    match re {
        RegexNode::Epsilon => {}
        RegexNode::Subword(..) => {}
        RegexNode::Terminal(..) => {}
        RegexNode::Nonterminal(..) => {}
        RegexNode::Command(..) => {}
        RegexNode::Or(subregexes) => {
            for subreid in subregexes {
                let subre = &arena[*subreid];
                do_followpos(subre, arena, result);
            }
        }
        RegexNode::Cat(children) => {
            for child_id in children {
                let child = &arena[*child_id];
                do_followpos(child, arena, result);
            }

            for i in 0..children.len() {
                let left = &arena[children[i]];
                let mut j = i + 1;
                let mut heads = RoaringBitmap::default();
                while j < children.len() {
                    let right = &arena[children[j]];
                    heads |= right.firstpos(arena);
                    if !right.nullable(arena) {
                        break;
                    }
                    j += 1;
                }
                for tail in left.lastpos(arena) {
                    for head in &heads {
                        result.entry(tail).or_default().insert(head);
                    }
                }
            }
        }
        RegexNode::Star(subregexid) => {
            let subregex = &arena[*subregexid];
            let first = subregex.firstpos(arena);
            let last = subregex.lastpos(arena);
            for i in last {
                for j in &first {
                    result.entry(i).or_default().insert(j);
                }
            }
        }
        RegexNode::EndMarker(_) => {}
    }
}

impl RegexNode {
    fn nullable(&self, arena: &[RegexNode]) -> bool {
        match self {
            RegexNode::Epsilon => true,
            RegexNode::Subword(..) => false,
            RegexNode::Terminal(..) => false,
            RegexNode::Nonterminal(..) => false,
            RegexNode::Command(..) => false,
            RegexNode::Or(children) => children
                .iter()
                .any(|child_id| arena[*child_id].nullable(arena)),
            RegexNode::Cat(children) => children
                .iter()
                .all(|child_id| arena[*child_id].nullable(arena)),
            RegexNode::Star(_) => true,
            RegexNode::EndMarker(_) => true,
        }
    }

    fn firstpos(&self, arena: &[RegexNode]) -> RoaringBitmap {
        let mut result: RoaringBitmap = Default::default();
        do_firstpos(self, arena, &mut result);
        result
    }

    fn lastpos(&self, arena: &[RegexNode]) -> RoaringBitmap {
        let mut result: RoaringBitmap = Default::default();
        do_lastpos(self, arena, &mut result);
        result
    }

    fn followpos(&self, arena: &[RegexNode]) -> BTreeMap<Position, RoaringBitmap> {
        let mut result: BTreeMap<Position, RoaringBitmap> = Default::default();
        do_followpos(self, arena, &mut result);
        result
    }
}

fn do_from_expr(
    expr_id: ExprId,
    expr_arena: &[Expr],
    node_arena: &mut Vec<RegexNode>,
    input_from_position: &mut Vec<RegexInput>,
    subwords: &mut RegexInternPool,
) -> Result<RegexNodeId> {
    match &expr_arena[expr_id] {
        Expr::Terminal {
            term,
            descr: description,
            fallback: level,
            span,
        } => {
            let result = RegexNode::Terminal(input_from_position.len() as Position);
            let input = RegexInput::Literal {
                literal: *term,
                description: *description,
                fallback_level: *level,
                span: *span,
            };
            input_from_position.push(input.clone());
            Ok(alloc(node_arena, result))
        }
        Expr::Subword {
            root_id,
            fallback,
            span,
        } => {
            let subword_regex = Regex::from_expr(*root_id, expr_arena, subwords)?;
            let subword_regex_id = subwords.intern(subword_regex);
            let result = RegexNode::Subword(input_from_position.len() as Position);
            let input = RegexInput::Subword {
                subword_regex_id,
                fallback_level: *fallback,
                span: *span,
            };
            input_from_position.push(input.clone());
            Ok(alloc(node_arena, result))
        }
        Expr::NontermRef {
            nonterm,
            fallback,
            span,
        } => {
            let result = RegexNode::Nonterminal(input_from_position.len() as Position);
            let input = RegexInput::Nonterminal {
                nonterm: *nonterm,
                fallback_level: *fallback,
                span: *span,
            };
            input_from_position.push(input.clone());
            Ok(alloc(node_arena, result))
        }
        Expr::Command {
            cmd,
            zsh_compadd,
            fallback,
            span,
        } => {
            let result = RegexNode::Command(input_from_position.len() as Position);
            let input = RegexInput::Command {
                cmd: *cmd,
                zsh_compadd: *zsh_compadd,
                fallback_level: *fallback,
                span: *span,
            };
            input_from_position.push(input.clone());
            Ok(alloc(node_arena, result))
        }
        Expr::Sequence {
            children: subexprs, ..
        } => {
            let subregexes: Vec<RegexNodeId> = subexprs
                .iter()
                .map(|subexpr_id| {
                    do_from_expr(
                        *subexpr_id,
                        expr_arena,
                        node_arena,
                        input_from_position,
                        subwords,
                    )
                })
                .collect::<Result<_>>()?;

            let result = RegexNode::Cat(subregexes);
            let result_id = alloc(node_arena, result);
            Ok(result_id)
        }
        Expr::Alternative {
            children: subexprs, ..
        } => {
            let subregexes: Vec<RegexNodeId> = subexprs
                .iter()
                .map(|subexpr_id| {
                    do_from_expr(
                        *subexpr_id,
                        expr_arena,
                        node_arena,
                        input_from_position,
                        subwords,
                    )
                })
                .collect::<Result<_>>()?;

            let result = RegexNode::Or(subregexes);
            Ok(alloc(node_arena, result))
        }
        Expr::Optional { child: subexpr, .. } => {
            let subregex = do_from_expr(
                *subexpr,
                expr_arena,
                node_arena,
                input_from_position,
                subwords,
            )?;
            let epsid = alloc(node_arena, RegexNode::Epsilon);
            let result = RegexNode::Or(vec![subregex, epsid]);
            Ok(alloc(node_arena, result))
        }
        Expr::Many1 { child: subexpr, .. } => {
            let subregex_id = do_from_expr(
                *subexpr,
                expr_arena,
                node_arena,
                input_from_position,
                subwords,
            )?;
            let star = RegexNode::Star(subregex_id);
            let starid = alloc(node_arena, star);
            let result = RegexNode::Cat(vec![subregex_id, starid]);
            Ok(alloc(node_arena, result))
        }
        Expr::DistributiveDescription { .. } => unreachable!(),
        Expr::Fallback {
            children: subexprs, ..
        } => {
            let subregexes: Vec<RegexNodeId> = subexprs
                .iter()
                .map(|subexpr_id| {
                    do_from_expr(
                        *subexpr_id,
                        expr_arena,
                        node_arena,
                        input_from_position,
                        subwords,
                    )
                })
                .collect::<Result<_>>()?;

            let result = RegexNode::Or(subregexes);
            Ok(alloc(node_arena, result))
        }
    }
}

pub(crate) fn make_dot_string_constant(s: &str) -> String {
    let escaped = s.replace('\\', "\\\\").replace('"', "\\\"");
    format!(r#""{escaped}""#)
}

fn do_to_dot<W: Write>(
    output: &mut W,
    node_id: RegexNodeId,
    parent_dot_id: Option<&str>,
    arena: &[RegexNode],
    input_from_position: &Vec<RegexInput>,
    subword_regexes: &RegexInternPool,
    identifiers_prefix: &str,
    recursion_level: usize,
    visited_subwords: &mut RoaringBitmap,
) -> std::result::Result<(), std::io::Error> {
    let indentation = format!("\t{}", str::repeat("\t", recursion_level));
    let node_dot_id = format!("_{identifiers_prefix}{node_id}");
    match arena[node_id].clone() {
        RegexNode::Subword(pos) => {
            let RegexInput::Subword {
                subword_regex_id, ..
            } = input_from_position[pos as usize].clone()
            else {
                unreachable!();
            };
            if let Some(parent_dot_id) = parent_dot_id {
                writeln!(output, "{indentation}{parent_dot_id} -> {node_dot_id};")?;
            }
            writeln!(
                output,
                "{indentation}{node_dot_id}[label=\"{pos}: Subword {subword_regex_id}\"];"
            )?;
            let subword_regex = subword_regexes.lookup(subword_regex_id);
            writeln!(
                output,
                "{indentation}{node_dot_id} -> _{subword_regex_id}_{};",
                subword_regex.root_id
            )?;
            if visited_subwords.contains(subword_regex_id.0 as _) {
                return Ok(());
            }
            visited_subwords.insert(subword_regex_id.0 as _);
            writeln!(
                output,
                "{indentation}subgraph cluster_{subword_regex_id} {{"
            )?;
            writeln!(
                output,
                "{indentation}\tlabel=\"SUBWORD {subword_regex_id}\";"
            )?;
            writeln!(output, "{indentation}\tcolor=grey91;")?;
            writeln!(output, "{indentation}\tstyle=filled;")?;
            let subword_identifiers_prefix = &format!("{subword_regex_id}_");
            do_to_dot(
                output,
                subword_regex.root_id,
                None,
                &subword_regex.arena,
                &subword_regex.input_from_position,
                subword_regexes,
                subword_identifiers_prefix,
                recursion_level + 1,
                visited_subwords,
            )?;
            writeln!(output, "{indentation}}}")?;
        }
        RegexNode::Epsilon => {
            writeln!(output, r#"{indentation}{node_dot_id}[label="Epsilon"];"#)?;
            if let Some(parent_dot_id) = parent_dot_id {
                writeln!(output, r#"{indentation}{parent_dot_id} -> {node_dot_id};"#,)?;
            }
        }
        RegexNode::Terminal(pos) => {
            let RegexInput::Literal {
                literal,
                description,
                ..
            } = input_from_position[pos as usize].clone()
            else {
                unreachable!();
            };
            if let Some(description) = description {
                writeln!(
                    output,
                    r#"{indentation}{node_dot_id}[label="{pos}: \"{literal}\"\n\"{description}\""];"#
                )?;
            } else {
                writeln!(
                    output,
                    r#"{indentation}{node_dot_id}[label="{pos}: \"{literal}\""];"#
                )?;
            }
            if let Some(parent_dot_id) = parent_dot_id {
                writeln!(output, r#"{indentation}{parent_dot_id} -> {node_dot_id};"#,)?;
            }
        }
        RegexNode::Nonterminal(pos) => {
            let input = input_from_position[pos as usize].clone();
            let RegexInput::Nonterminal { nonterm, .. } = input else {
                unreachable!()
            };
            writeln!(
                output,
                r#"{indentation}{node_dot_id}[label="{pos}: <{nonterm}>"];"#
            )?;
            if let Some(parent_dot_id) = parent_dot_id {
                writeln!(output, r#"{indentation}{parent_dot_id} -> {node_dot_id};"#,)?;
            }
        }
        RegexNode::Command(pos) => {
            let RegexInput::Command { cmd, .. } = input_from_position[pos as usize] else {
                unreachable!()
            };
            writeln!(
                output,
                r#"{indentation}{node_dot_id}[label={}];"#,
                make_dot_string_constant(&format!("{pos}: {cmd}"))
            )?;
            if let Some(parent_dot_id) = parent_dot_id {
                writeln!(output, r#"{indentation}{parent_dot_id} -> {node_dot_id};"#,)?;
            }
        }
        RegexNode::Cat(children) => {
            writeln!(output, r#"{indentation}{node_dot_id}[label="Cat"];"#)?;
            for child_id in children {
                do_to_dot(
                    output,
                    child_id,
                    Some(&node_dot_id),
                    arena,
                    input_from_position,
                    subword_regexes,
                    identifiers_prefix,
                    recursion_level,
                    visited_subwords,
                )?;
            }
            if let Some(parent_dot_id) = parent_dot_id {
                writeln!(output, r#"{indentation}{parent_dot_id} -> {node_dot_id};"#,)?;
            }
        }
        RegexNode::Or(ors) => {
            writeln!(output, r#"{indentation}{node_dot_id}[label="Or"];"#)?;
            for child in ors {
                do_to_dot(
                    output,
                    child,
                    Some(&node_dot_id),
                    arena,
                    input_from_position,
                    subword_regexes,
                    identifiers_prefix,
                    recursion_level,
                    visited_subwords,
                )?;
            }
            if let Some(parent_dot_id) = parent_dot_id {
                writeln!(output, r#"{indentation}{parent_dot_id} -> {node_dot_id};"#,)?;
            }
        }
        RegexNode::Star(..) => {
            writeln!(output, r#"{indentation}{node_dot_id}[label="Star"];"#)?;
            if let Some(parent_dot_id) = parent_dot_id {
                writeln!(output, r#"{indentation}{parent_dot_id} -> {node_dot_id};"#,)?;
            }
        }
        RegexNode::EndMarker(pos) => {
            writeln!(
                output,
                r#"{indentation}{node_dot_id}[label="{pos}: EndMarker"];"#
            )?;
            if let Some(parent_dot_id) = parent_dot_id {
                writeln!(output, r#"{indentation}{parent_dot_id} -> {node_dot_id};"#,)?;
            }
        }
    }
    Ok(())
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct RegexId(pub(crate) usize);

impl std::fmt::Display for RegexId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Default)]
pub struct RegexInternPool {
    store: IndexSet<Regex>,
}

impl RegexInternPool {
    fn intern(&mut self, value: Regex) -> RegexId {
        let (id, _) = self.store.insert_full(value);
        RegexId(id)
    }

    pub(crate) fn lookup(&self, id: RegexId) -> &Regex {
        self.store.get_index(id.0).unwrap()
    }
}

#[derive(Debug, Clone)]
pub struct Regex {
    pub root_id: RegexNodeId,
    pub input_from_position: Vec<RegexInput>,
    pub endmarker_position: Position,
    pub arena: Vec<RegexNode>,
    follow_cache: OnceCell<BTreeMap<Position, RoaringBitmap>>,
}

// Ignore `follow_cache`
impl PartialEq for Regex {
    fn eq(&self, other: &Self) -> bool {
        let Self {
            root_id: self_root_id,
            input_from_position: self_input_from_position,
            endmarker_position: self_endmarker_position,
            arena: self_arena,
            follow_cache: _,
        } = self;

        let Self {
            root_id: other_root_id,
            input_from_position: other_input_from_position,
            endmarker_position: other_endmarker_position,
            arena: other_arena,
            follow_cache: _,
        } = other;

        self_root_id == other_root_id
            && self_input_from_position == other_input_from_position
            && self_endmarker_position == other_endmarker_position
            && self_arena == other_arena
    }
}

// Ignore `follow_cache`
impl Eq for Regex {}

// Ignore `follow_cache`
impl std::hash::Hash for Regex {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.root_id.hash(state);
        self.input_from_position.hash(state);
        self.endmarker_position.hash(state);
        self.arena.hash(state);
    }
}

impl Regex {
    pub fn from_valid_grammar(
        v: &ValidGrammar,
        subword_regexes: &mut RegexInternPool,
    ) -> Result<Self> {
        let regex = Self::from_expr(v.expr, &v.arena, subword_regexes)?;
        regex.check_ambiguities(subword_regexes)?;
        Ok(regex)
    }

    pub(crate) fn from_expr(
        root_expr_id: ExprId,
        expr_arena: &[Expr],
        subwords: &mut RegexInternPool,
    ) -> Result<Self> {
        let mut input_from_position: Vec<RegexInput> = Default::default();
        let mut node_arena: Vec<RegexNode> = Default::default();
        let regex = do_from_expr(
            root_expr_id,
            expr_arena,
            &mut node_arena,
            &mut input_from_position,
            subwords,
        )?;
        let endmarker_position = input_from_position.len() as Position;
        let endmarkerid = alloc(&mut node_arena, RegexNode::EndMarker(endmarker_position));
        let root = RegexNode::Cat(vec![regex, endmarkerid]);
        let root_id = alloc(&mut node_arena, root.clone());

        let retval = Self {
            root_id,
            endmarker_position,
            input_from_position,
            arena: node_arena,
            follow_cache: Default::default(),
        };
        Ok(retval)
    }

    fn do_check_ambiguous_inputs_tail_only_subword(
        &self,
        firstpos: &RoaringBitmap,
        followpos: &BTreeMap<Position, RoaringBitmap>,
        path_prev_ambiguous: Option<RegexInput>,
        visited: &mut RoaringBitmap,
    ) -> Result<()> {
        let inputs: Vec<RegexInput> = firstpos
            .iter()
            .filter(|pos| *pos != self.endmarker_position)
            .map(|pos| self.input_from_position[pos as usize].clone())
            .collect();

        let mut prev_ambiguous: Option<RegexInput> = None;
        for inp in inputs {
            if let Some(ref prev_inp) = path_prev_ambiguous {
                return Err(Error::UnboundedMatchable(
                    prev_inp.get_span(),
                    inp.get_span(),
                ));
            }

            if inp.is_star_subword() {
                prev_ambiguous = Some(inp);
            }
        }

        for pos in firstpos {
            if visited.contains(pos) {
                continue;
            }
            let Some(follow) = followpos.get(&pos) else {
                continue;
            };
            visited.insert(pos);
            self.do_check_ambiguous_inputs_tail_only_subword(
                follow,
                followpos,
                path_prev_ambiguous.clone().or(prev_ambiguous.clone()),
                visited,
            )?;
        }
        Ok(())
    }

    fn check_ambiguous_inputs_tail_only_subword(
        &self,
        firstpos: &RoaringBitmap,
        followpos: &BTreeMap<Position, RoaringBitmap>,
    ) -> Result<()> {
        let mut visited: RoaringBitmap = Default::default();
        visited.insert(self.endmarker_position);
        self.do_check_ambiguous_inputs_tail_only_subword(firstpos, followpos, None, &mut visited)
    }

    fn check_subwords(
        &self,
        firstpos: &RoaringBitmap,
        followpos: &BTreeMap<Position, RoaringBitmap>,
        subword_regex_pool: &RegexInternPool,
        visited: &mut RoaringBitmap,
        checked_subword_regexes: &mut RoaringBitmap,
    ) -> Result<()> {
        let subword_regexes: Vec<RegexId> = firstpos
            .iter()
            .filter(|pos| *pos != self.endmarker_position)
            .map(|pos| self.input_from_position[pos as usize].clone())
            .filter_map(|input| match input {
                RegexInput::Literal { .. } => None,
                RegexInput::Nonterminal { .. } => None,
                RegexInput::Command { .. } => None,
                RegexInput::Subword {
                    subword_regex_id, ..
                } => Some(subword_regex_id),
            })
            .filter(|subword_regex_id| !checked_subword_regexes.contains(subword_regex_id.0 as u32))
            .collect();

        for subword_regex_id in subword_regexes {
            let subword_regex = subword_regex_pool.lookup(subword_regex_id);
            let subword_firstpos = RoaringBitmap::from_iter(&subword_regex.firstpos());
            let subword_followpos = subword_regex.followpos();
            subword_regex
                .check_ambiguous_inputs_tail_only_subword(&subword_firstpos, subword_followpos)?;
            checked_subword_regexes.insert(subword_regex_id.0 as u32);
        }

        for pos in firstpos {
            if visited.contains(pos) {
                continue;
            }
            let Some(follow) = followpos.get(&pos) else {
                continue;
            };
            visited.insert(pos);
            self.check_subwords(
                follow,
                followpos,
                subword_regex_pool,
                visited,
                checked_subword_regexes,
            )?;
        }
        Ok(())
    }

    pub(crate) fn check_ambiguities(&self, subword_regexes: &RegexInternPool) -> Result<()> {
        let firstpos = RoaringBitmap::from_iter(&self.firstpos());
        let followpos = self.followpos();
        let mut visited: RoaringBitmap = Default::default();
        visited.insert(self.endmarker_position);
        let mut checked_subword_regexes: RoaringBitmap = Default::default();
        self.check_subwords(
            &firstpos,
            followpos,
            subword_regexes,
            &mut visited,
            &mut checked_subword_regexes,
        )?;
        Ok(())
    }

    fn get_root(&self) -> &RegexNode {
        &self.arena[self.root_id]
    }

    pub fn firstpos(&self) -> RoaringBitmap {
        self.get_root().firstpos(&self.arena)
    }

    pub(crate) fn followpos(&self) -> &BTreeMap<Position, RoaringBitmap> {
        self.follow_cache
            .get_or_init(|| self.get_root().followpos(&self.arena))
    }

    pub fn to_dot<W: Write>(
        &self,
        output: &mut W,
        subword_regexes: &RegexInternPool,
    ) -> std::result::Result<(), std::io::Error> {
        let mut visited_subwords = RoaringBitmap::default();
        writeln!(output, "digraph rx {{")?;
        do_to_dot(
            output,
            self.root_id,
            None,
            &self.arena,
            &self.input_from_position,
            subword_regexes,
            "",
            0,
            &mut visited_subwords,
        )?;
        writeln!(output, "}}")?;
        Ok(())
    }

    #[allow(dead_code)]
    pub fn to_dot_file<P: AsRef<std::path::Path>>(
        &self,
        path: P,
        subword_regexes: &RegexInternPool,
    ) -> std::result::Result<(), std::io::Error> {
        let mut file = std::fs::File::create(path)?;
        self.to_dot(&mut file, subword_regexes)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::grammar::Shell;
    use crate::{Error, Result};

    fn make_sample_star_regex(arena: &mut Vec<RegexNode>) -> RegexNodeId {
        let t1 = alloc(arena, RegexNode::Terminal(1));
        let t2 = alloc(arena, RegexNode::Terminal(2));
        let or_ = RegexNode::Or(vec![t1, t2]);
        let or_id = alloc(arena, or_);
        let star = RegexNode::Star(or_id);
        let star_id = alloc(arena, star);
        star_id
    }

    fn make_sample_regex(arena: &mut Vec<RegexNode>) -> RegexNode {
        // (a|b)*a
        let t = alloc(arena, RegexNode::Terminal(3));
        RegexNode::Cat(vec![make_sample_star_regex(arena), t])
    }

    #[test]
    fn nullable() {
        let mut arena = Default::default();
        let star_regex_id = make_sample_star_regex(&mut arena);
        let star_regex = &arena[star_regex_id];
        assert!(star_regex.nullable(&arena));

        let regex = make_sample_regex(&mut arena);
        assert!(!regex.nullable(&arena));
    }

    #[test]
    fn firstpos() {
        let mut arena = Default::default();
        let regex = make_sample_regex(&mut arena);
        assert_eq!(regex.firstpos(&arena), RoaringBitmap::from_iter([1, 2, 3]));
    }

    #[test]
    fn lastpos() {
        let mut arena = Default::default();
        let regex = make_sample_regex(&mut arena);
        assert_eq!(regex.lastpos(&arena), RoaringBitmap::from_iter([3]));
    }

    fn make_followpos_regex(arena: &mut Vec<RegexNode>) -> RegexNode {
        use RegexNode::*;

        let c0 = make_sample_star_regex(arena);
        let c1 = alloc(arena, Terminal(3));
        let c2 = alloc(arena, Cat(vec![c0, c1]));
        let c3 = alloc(arena, Terminal(4));
        let c4 = alloc(arena, Cat(vec![c2, c3]));
        let c5 = alloc(arena, Terminal(5));

        // (a|b)*abb#
        RegexNode::Cat(vec![
            alloc(arena, Cat(vec![c4, c5])),
            alloc(arena, EndMarker(6)),
        ])
    }

    #[test]
    fn followpos() {
        let mut arena = Default::default();
        let regex = make_followpos_regex(&mut arena);
        let fp = regex.followpos(&mut arena);
        assert_eq!(fp.get(&6), None);
        assert_eq!(fp.get(&5), Some(&RoaringBitmap::from_iter([6])));
        assert_eq!(fp.get(&4), Some(&RoaringBitmap::from_iter([5])));
        assert_eq!(fp.get(&3), Some(&RoaringBitmap::from_iter([4])));
        assert_eq!(fp.get(&2), Some(&RoaringBitmap::from_iter([1, 2, 3])));
        assert_eq!(fp.get(&1), Some(&RoaringBitmap::from_iter([1, 2, 3])));
    }

    fn get_validated_grammar(input: &str) -> Result<crate::grammar::ValidGrammar> {
        let g = crate::grammar::Grammar::parse(input)
            .map_err(|e| e.to_string())
            .unwrap();
        let validated = crate::grammar::ValidGrammar::from_grammar(g, Shell::Bash)?;
        let mut subword_regexes = RegexInternPool::default();
        let _ = Regex::from_valid_grammar(&validated, &mut subword_regexes)?;
        Ok(validated)
    }

    #[test]
    fn lsof_bug() {
        const GRAMMAR: &str = "
lsf -s<PROTOCOL>:<STATE-SPEC>[,<STATE-SPEC>]...;
<PROTOCOL> = TCP | UDP;
<STATE-SPEC> = [^]<STATE>;
<STATE> = LISTEN | CLOSED;
";

        assert!(matches!(get_validated_grammar(GRAMMAR), Ok(_)));
    }

    #[test]
    fn detects_nontail_command_subword() {
        assert!(matches!(
            get_validated_grammar(r#"cmd {{{ git tag }}}..{{{ git tag }}};"#),
            Ok(_)
        ));
        assert!(matches!(
            get_validated_grammar(r#"cmd --option={{{ echo foo }}};"#),
            Ok(_)
        ));
        assert!(matches!(
            get_validated_grammar(r#"cmd {{{ echo foo }}}{{{ echo bar }}};"#),
            Ok(_)
        ));

        // https://github.com/adaszko/complgen/issues/49
        assert!(matches!(
            get_validated_grammar(
                r#"build <PLATFORM>-(amd64|arm64); <PLATFORM> = {{{ echo foo }}};"#
            ),
            Ok(_)
        ));
        assert!(matches!(
            get_validated_grammar(
                r#"build <PLATFORM>[-(amd64|arm64)]; <PLATFORM> = {{{ echo foo }}};"#
            ),
            Ok(_)
        ));

        // https://github.com/adaszko/complgen/issues/53
        assert!(matches!(
            get_validated_grammar(r#"cmd <DUMMY>,<DUMMY>; <DUMMY@bash> = {{{ echo dummy }}};"#),
            Ok(_)
        ));
        assert!(matches!(
            get_validated_grammar(r#"cmd --option=<FOO>; <FOO@bash> = {{{ echo bash }}};"#),
            Ok(_)
        ));
        assert!(matches!(
            get_validated_grammar(
                r#"
duf -hide-fs <FS>[,<FS>]...;
<FS@fish> = {{{ string split ' ' --fields 3 </proc/mounts | sort --unique }}};
"#
            ),
            Err(Error::UnboundedMatchable(_, _))
        ));
    }
}

use crate::{Error, Result, grammar::ValidGrammar, make_dot_string_constant};
use hashbrown::HashSet;
use indexmap::IndexSet;
use std::{
    cell::OnceCell,
    collections::{BTreeMap, BTreeSet},
    io::Write,
};

use roaring::RoaringBitmap;
use ustr::Ustr;

use crate::grammar::{Expr, ExprId, HumanSpan, Shell};

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
    Subword(Position),
    Terminal(Ustr, usize, Position), // terminal, fallback level, position
    Nonterminal(Position),
    Command(Ustr, Position),
    Cat(RegexNodeId, RegexNodeId),
    Or(Box<[RegexNodeId]>, bool), // is fallback
    Star(RegexNodeId),
    EndMarker(Position),
}

impl std::fmt::Debug for RegexNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Subword(position) => f.write_fmt(format_args!(r#"Subword({position})"#)),
            Self::Terminal(term, level, position) => f.write_fmt(format_args!(
                r#"Terminal({term:?}.to_string(), {level}, {position})"#
            )),
            Self::Nonterminal(position) => f.write_fmt(format_args!(r#"Nonterminal({position})"#)),
            Self::Command(code, position) => {
                f.write_fmt(format_args!(r#"Command({code:?}.to_string(), {position})"#))
            }
            Self::Cat(left, right) => f.write_fmt(format_args!(r#"Cat({left:?}, {right:?})"#)),
            Self::Or(children, is_fallback) => {
                f.write_fmt(format_args!(r#"Or(vec!{children:?}, {is_fallback})"#))
            }
            Self::Star(child) => f.write_fmt(format_args!(r#"Star({child:?})"#)),
            Self::EndMarker(position) => f.write_fmt(format_args!(r#"EndMarker({position})"#)),
            Self::Epsilon => f.write_fmt(format_args!(r#"Epsilon"#)),
        }
    }
}

fn do_firstpos(re: &RegexNode, arena: &[RegexNode], result: &mut BTreeSet<Position>) {
    match re {
        RegexNode::Epsilon => {}
        RegexNode::Subword(position) => {
            result.insert(*position);
        }
        RegexNode::Terminal(_, _, position) => {
            result.insert(*position);
        }
        RegexNode::Nonterminal(position) => {
            result.insert(*position);
        }
        RegexNode::Command(_, position) => {
            result.insert(*position);
        }
        RegexNode::Or(subregexes, _) => {
            for subreid in subregexes {
                let subre = &arena[*subreid];
                do_firstpos(subre, arena, result);
            }
        }
        RegexNode::Cat(leftid, rightid) => {
            let left = &arena[*leftid];
            let right = &arena[*rightid];
            if left.nullable(arena) {
                do_firstpos(left, arena, result);
                do_firstpos(right, arena, result);
            } else {
                do_firstpos(left, arena, result);
            }
        }
        RegexNode::Star(subregexid) => {
            let subregex = &arena[*subregexid];
            do_firstpos(subregex, arena, result);
        }
        RegexNode::EndMarker(position) => {
            result.insert(*position);
        }
    }
}

fn do_lastpos(re: &RegexNode, arena: &[RegexNode], result: &mut HashSet<Position>) {
    match re {
        RegexNode::Epsilon => {}
        RegexNode::Subword(position) => {
            result.insert(*position);
        }
        RegexNode::Terminal(_, _, position) => {
            result.insert(*position);
        }
        RegexNode::Nonterminal(position) => {
            result.insert(*position);
        }
        RegexNode::Command(_, position) => {
            result.insert(*position);
        }
        RegexNode::Or(subregexes, _) => {
            for subreid in subregexes {
                let subre = &arena[*subreid];
                do_lastpos(subre, arena, result);
            }
        }
        RegexNode::Cat(leftid, rightid) => {
            let left = &arena[*leftid];
            let right = &arena[*rightid];
            if right.nullable(arena) {
                do_lastpos(right, arena, result);
                do_lastpos(left, arena, result);
            } else {
                do_lastpos(right, arena, result);
            }
        }
        RegexNode::Star(subregexid) => {
            let subregex = &arena[*subregexid];
            do_lastpos(subregex, arena, result);
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
        RegexNode::Or(subregexes, _) => {
            for subreid in subregexes {
                let subre = &arena[*subreid];
                do_followpos(subre, arena, result);
            }
        }
        RegexNode::Cat(leftid, rightid) => {
            let left = &arena[*leftid];
            let right = &arena[*rightid];
            do_followpos(left, arena, result);
            do_followpos(right, arena, result);
            let fp = right.firstpos(arena);
            for i in left.lastpos(arena) {
                for j in &fp {
                    result.entry(i).or_default().insert(*j);
                }
            }
        }
        RegexNode::Star(subregexid) => {
            let subregex = &arena[*subregexid];
            let first = subregex.firstpos(arena);
            let last = subregex.lastpos(arena);
            for i in last {
                for j in &first {
                    result.entry(i).or_default().insert(*j);
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
            RegexNode::Or(children, _) => children.iter().any(|child_id| {
                let child = &arena[*child_id];
                child.nullable(arena)
            }),
            RegexNode::Cat(leftid, rightid) => {
                let left = &arena[*leftid];
                let right = &arena[*rightid];
                left.nullable(arena) && right.nullable(arena)
            }
            RegexNode::Star(_) => true,
            RegexNode::EndMarker(_) => true,
        }
    }

    fn firstpos(&self, arena: &[RegexNode]) -> BTreeSet<Position> {
        let mut result: BTreeSet<Position> = Default::default();
        do_firstpos(self, arena, &mut result);
        result
    }

    fn lastpos(&self, arena: &[RegexNode]) -> HashSet<Position> {
        let mut result: HashSet<Position> = Default::default();
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
    e: ExprId,
    expr_arena: &[Expr],
    shell: Shell,
    arena: &mut Vec<RegexNode>,
    input_from_position: &mut Vec<RegexInput>,
    subword_regexes: &mut RegexInternPool,
) -> Result<RegexNodeId> {
    match &expr_arena[e] {
        Expr::Terminal {
            term,
            descr: description,
            fallback: level,
            span,
        } => {
            let result = RegexNode::Terminal(*term, *level, input_from_position.len() as Position);
            let input = RegexInput::Literal {
                literal: *term,
                description: *description,
                fallback_level: *level,
                span: *span,
            };
            input_from_position.push(input.clone());
            Ok(alloc(arena, result))
        }
        Expr::Subword {
            root_id,
            fallback,
            span,
        } => {
            let subword_regex = Regex::from_expr(*root_id, expr_arena, shell, subword_regexes)?;
            let subword_regex_id = subword_regexes.intern(subword_regex);
            let result = RegexNode::Subword(input_from_position.len() as Position);
            let input = RegexInput::Subword {
                subword_regex_id,
                fallback_level: *fallback,
                span: *span,
            };
            input_from_position.push(input.clone());
            Ok(alloc(arena, result))
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
            Ok(alloc(arena, result))
        }
        Expr::Command {
            cmd,
            zsh_compadd,
            fallback,
            span,
        } => {
            let result = RegexNode::Command(*cmd, input_from_position.len() as Position);
            let input = RegexInput::Command {
                cmd: *cmd,
                zsh_compadd: *zsh_compadd,
                fallback_level: *fallback,
                span: *span,
            };
            input_from_position.push(input.clone());
            Ok(alloc(arena, result))
        }
        Expr::Sequence {
            children: subexprs, ..
        } => {
            let mut left_regex_id = do_from_expr(
                subexprs[0],
                expr_arena,
                shell,
                arena,
                input_from_position,
                subword_regexes,
            )?;
            for right_expr in &subexprs[1..] {
                let right_regex_id = do_from_expr(
                    *right_expr,
                    expr_arena,
                    shell,
                    arena,
                    input_from_position,
                    subword_regexes,
                )?;
                let left_regex = RegexNode::Cat(left_regex_id, right_regex_id);
                left_regex_id = alloc(arena, left_regex);
            }
            Ok(left_regex_id)
        }
        Expr::Alternative {
            children: subexprs, ..
        } => {
            let mut subregexes: Vec<RegexNodeId> = Default::default();
            for e in subexprs {
                let subregex = do_from_expr(
                    *e,
                    expr_arena,
                    shell,
                    arena,
                    input_from_position,
                    subword_regexes,
                )?;
                subregexes.push(subregex);
            }
            let result = RegexNode::Or(subregexes.into_boxed_slice(), false);
            Ok(alloc(arena, result))
        }
        Expr::Optional { child: subexpr, .. } => {
            let subregex = do_from_expr(
                *subexpr,
                expr_arena,
                shell,
                arena,
                input_from_position,
                subword_regexes,
            )?;
            let epsid = alloc(arena, RegexNode::Epsilon);
            let result = RegexNode::Or(Box::new([subregex, epsid]), false);
            Ok(alloc(arena, result))
        }
        Expr::Many1 { child: subexpr, .. } => {
            let subregex_id = do_from_expr(
                *subexpr,
                expr_arena,
                shell,
                arena,
                input_from_position,
                subword_regexes,
            )?;
            let star = RegexNode::Star(subregex_id);
            let starid = alloc(arena, star);
            let result = RegexNode::Cat(subregex_id, starid);
            Ok(alloc(arena, result))
        }
        Expr::DistributiveDescription { .. } => unreachable!(
            "DistributiveDescription Expr type should have been erased before compilation to regex"
        ),
        Expr::Fallback {
            children: subexprs, ..
        } => {
            let mut subregexes: Vec<RegexNodeId> = Default::default();
            for e in subexprs {
                let subregex = do_from_expr(
                    *e,
                    expr_arena,
                    shell,
                    arena,
                    input_from_position,
                    subword_regexes,
                )?;
                subregexes.push(subregex);
            }
            let result = RegexNode::Or(subregexes.into_boxed_slice(), true);
            Ok(alloc(arena, result))
        }
    }
}

fn do_to_dot<W: Write>(
    output: &mut W,
    node_id: RegexNodeId,
    arena: &[RegexNode],
    input_from_position: &Vec<RegexInput>,
    subword_regexes: &RegexInternPool,
    recursion_level: usize,
) -> std::result::Result<(), std::io::Error> {
    let indentation = format!("\t{}", str::repeat("\t", recursion_level));
    match arena[node_id].clone() {
        RegexNode::Epsilon => {
            writeln!(output, r#"{indentation}_{node_id}[label="Epsilon"];"#)?;
        }
        RegexNode::Subword(pos) => {
            let RegexInput::Subword {
                subword_regex_id, ..
            } = input_from_position[pos as usize].clone()
            else {
                unreachable!();
            };
            writeln!(output, "{indentation}_{node_id} -> _{subword_regex_id};")?;
            writeln!(output, "{indentation}subgraph cluster_{node_id} {{")?;
            writeln!(output, "{indentation}\tlabel=\"SUBWORD\";")?;
            writeln!(output, "{indentation}\tcolor=grey91;")?;
            writeln!(output, "{indentation}\tstyle=filled;")?;
            writeln!(
                output,
                "{indentation}\t_{node_id}[label=\"{pos}: Subword\"];"
            )?;
            let subword_regex = subword_regexes.lookup(subword_regex_id);
            do_to_dot(
                output,
                subword_regex.root_id,
                &subword_regex.arena,
                input_from_position,
                subword_regexes,
                recursion_level + 1,
            )?;
            writeln!(output, "{indentation}}}")?;
        }
        RegexNode::Terminal(term, _, pos) => {
            writeln!(
                output,
                r#"{indentation}_{node_id}[label="{pos}: \"{term}\""];"#
            )?;
        }
        RegexNode::Nonterminal(pos) => {
            let input = input_from_position[pos as usize].clone();
            let RegexInput::Nonterminal { nonterm, .. } = input else {
                unreachable!()
            };
            writeln!(
                output,
                r#"{indentation}_{node_id}[label="{pos}: <{nonterm}>"];"#
            )?;
        }
        RegexNode::Command(cmd, pos) => {
            writeln!(
                output,
                r#"{indentation}_{node_id}[label={}];"#,
                make_dot_string_constant(&format!("{pos}: {cmd}"))
            )?;
        }
        RegexNode::Cat(lhs, rhs) => {
            writeln!(
                output,
                r#"{indentation}_{node_id}[label="Cat"]; _{node_id} -> _{lhs}; _{node_id} -> _{rhs};"#
            )?;
            do_to_dot(
                output,
                lhs,
                arena,
                input_from_position,
                subword_regexes,
                recursion_level,
            )?;
            do_to_dot(
                output,
                rhs,
                arena,
                input_from_position,
                subword_regexes,
                recursion_level,
            )?;
        }
        RegexNode::Or(ors, _) => {
            writeln!(output, r#"{indentation}_{node_id}[label="Or"];"#)?;
            for child in &ors {
                writeln!(output, r#"{indentation}_{node_id} -> _{child};"#)?;
            }
            for child in ors {
                do_to_dot(
                    output,
                    child,
                    arena,
                    input_from_position,
                    subword_regexes,
                    recursion_level,
                )?;
            }
        }
        RegexNode::Star(child) => {
            writeln!(
                output,
                r#"{indentation}_{node_id}[label="Star"]; _{node_id} -> _{child};"#
            )?;
        }
        RegexNode::EndMarker(pos) => {
            writeln!(
                output,
                r#"{indentation}_{node_id}[label="{pos}: EndMarker"];"#
            )?;
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
        shell: Shell,
        subword_regexes: &mut RegexInternPool,
    ) -> Result<Self> {
        let regex = Self::from_expr(v.expr, &v.arena, shell, subword_regexes)?;
        regex.check_ambiguities(subword_regexes)?;
        Ok(regex)
    }

    pub(crate) fn from_expr(
        e: ExprId,
        expr_arena: &[Expr],
        shell: Shell,
        subword_regexes: &mut RegexInternPool,
    ) -> Result<Self> {
        let mut input_from_position: Vec<RegexInput> = Default::default();
        let mut arena: Vec<RegexNode> = Default::default();
        let regex = do_from_expr(
            e,
            expr_arena,
            shell,
            &mut arena,
            &mut input_from_position,
            subword_regexes,
        )?;
        let endmarker_position = input_from_position.len() as Position;
        let endmarkerid = alloc(&mut arena, RegexNode::EndMarker(endmarker_position));
        let root = RegexNode::Cat(regex, endmarkerid);
        let root_id = alloc(&mut arena, root.clone());

        let retval = Self {
            root_id,
            endmarker_position,
            input_from_position,
            arena,
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
                .check_ambiguous_inputs_tail_only_subword(&subword_firstpos, &subword_followpos)?;
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
            &followpos,
            subword_regexes,
            &mut visited,
            &mut checked_subword_regexes,
        )?;
        Ok(())
    }

    fn get_root(&self) -> &RegexNode {
        &self.arena[self.root_id]
    }

    pub fn firstpos(&self) -> BTreeSet<Position> {
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
        writeln!(output, "digraph rx {{")?;
        do_to_dot(
            output,
            self.root_id,
            &self.arena,
            &self.input_from_position,
            subword_regexes,
            0,
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
    use crate::{Error, Result};
    use ustr::ustr;

    fn make_sample_star_regex(arena: &mut Vec<RegexNode>) -> RegexNodeId {
        let t1 = alloc(arena, RegexNode::Terminal(ustr("a"), 0, 1));
        let t2 = alloc(arena, RegexNode::Terminal(ustr("b"), 0, 2));
        let or_ = RegexNode::Or(Box::new([t1, t2]), false);
        let or_id = alloc(arena, or_);
        let star = RegexNode::Star(or_id);
        let star_id = alloc(arena, star);
        star_id
    }

    fn make_sample_regex(arena: &mut Vec<RegexNode>) -> RegexNode {
        // (a|b)*a
        let t = alloc(arena, RegexNode::Terminal(ustr("a"), 0, 3));
        RegexNode::Cat(make_sample_star_regex(arena), t)
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
        assert_eq!(regex.firstpos(&arena), BTreeSet::from([1, 2, 3]));
    }

    #[test]
    fn lastpos() {
        let mut arena = Default::default();
        let regex = make_sample_regex(&mut arena);
        assert_eq!(regex.lastpos(&arena), HashSet::from([3]));
    }

    fn make_followpos_regex(arena: &mut Vec<RegexNode>) -> RegexNode {
        use RegexNode::*;

        let c0 = make_sample_star_regex(arena);
        let c1 = alloc(arena, Terminal(ustr("a"), 0, 3));
        let c2 = alloc(arena, Cat(c0, c1));
        let c3 = alloc(arena, Terminal(ustr("b"), 0, 4));
        let c4 = alloc(arena, Cat(c2, c3));
        let c5 = alloc(arena, Terminal(ustr("b"), 0, 5));

        // (a|b)*abb#
        RegexNode::Cat(alloc(arena, Cat(c4, c5)), alloc(arena, EndMarker(6)))
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
        let _ = Regex::from_valid_grammar(&validated, Shell::Bash, &mut subword_regexes)?;
        Ok(validated)
    }

    #[test]
    fn lsof_bug() {
        const GRAMMAR: &str = "
lsf -s<PROTOCOL>:<STATE-SPEC>[,<STATE-SPEC>]...;
<PROTOCOL> ::= TCP | UDP;
<STATE-SPEC> ::= [^]<STATE>;
<STATE> ::= LISTEN | CLOSED;
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
                r#"build <PLATFORM>-(amd64|arm64); <PLATFORM> ::= {{{ echo foo }}};"#
            ),
            Ok(_)
        ));
        assert!(matches!(
            get_validated_grammar(
                r#"build <PLATFORM>[-(amd64|arm64)]; <PLATFORM> ::= {{{ echo foo }}};"#
            ),
            Ok(_)
        ));

        // https://github.com/adaszko/complgen/issues/53
        assert!(matches!(
            get_validated_grammar(r#"cmd <DUMMY>,<DUMMY>; <DUMMY@bash> ::= {{{ echo dummy }}};"#),
            Ok(_)
        ));
        assert!(matches!(
            get_validated_grammar(r#"cmd --option=<FOO>; <FOO@bash> ::= {{{ echo bash }}};"#),
            Ok(_)
        ));
        assert!(matches!(
            get_validated_grammar(
                r#"
duf -hide-fs <FS>[,<FS>]...;
<FS@fish> ::= {{{ string split ' ' --fields 3 </proc/mounts | sort --unique }}};
"#
            ),
            Err(Error::UnboundedMatchable(_, _))
        ));
    }
}

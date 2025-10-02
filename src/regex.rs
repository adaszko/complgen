use crate::{Error, Result, grammar::ValidGrammar};
use hashbrown::HashSet;
use indexmap::IndexMap;
use std::{
    collections::{BTreeMap, BTreeSet},
    io::Write,
};

use roaring::RoaringBitmap;
use ustr::{Ustr, UstrMap};

use crate::grammar::{
    CmdRegex, DFAId, Expr, ExprId, HumanSpan, Shell, Specialization, SubwordCompilationPhase,
};

pub type Position = u32;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Input {
    Literal {
        literal: Ustr,
        description: Option<Ustr>,
        fallback_level: usize,
        span: Option<HumanSpan>,
    },
    Subword {
        subdfa: DFAId,
        fallback_level: usize,
        span: Option<HumanSpan>,
    },
    Nonterminal {
        nonterm: Ustr,
        spec: Option<Specialization>,
        fallback_level: usize,
        span: Option<HumanSpan>,
    },
    Command {
        cmd: Ustr,
        regex: Option<CmdRegex>,
        fallback_level: usize,
        span: Option<HumanSpan>,
    },
}

impl Input {
    pub fn is_ambiguous(&self, shell: Shell) -> bool {
        match self {
            Self::Literal { .. } => false,
            Self::Subword { .. } => false, // XXX inaccurate
            Self::Nonterminal { .. } => true,
            Self::Command { regex: None, .. } => true,
            Self::Command {
                regex: Some(regex), ..
            } => regex.matches_anything(shell),
        }
    }

    pub fn get_fallback_level(&self) -> usize {
        match self {
            Self::Literal {
                fallback_level: level,
                ..
            } => *level,
            Self::Subword {
                fallback_level: level,
                ..
            } => *level,
            Self::Nonterminal {
                fallback_level: level,
                ..
            } => *level,
            Self::Command {
                fallback_level: level,
                ..
            } => *level,
        }
    }

    pub fn get_span(&self) -> Option<HumanSpan> {
        match self {
            Self::Literal { span, .. } => *span,
            Self::Subword { span, .. } => *span,
            Self::Nonterminal { span, .. } => *span,
            Self::Command { span, .. } => *span,
        }
    }
}

pub fn diagnostic_display_input<W: std::fmt::Write>(w: &mut W, input: &Inp) -> crate::Result<()> {
    match input {
        Inp::Literal { literal, .. } => write!(w, r#"{literal}"#)?,
        Inp::Nonterminal { nonterm, .. } => write!(w, r#"<{nonterm}>"#)?,
        Inp::Command { cmd, .. } => write!(w, r#"{{{{{{ {cmd} }}}}}}"#)?,
        Inp::Subword { .. } => unreachable!(),
    }
    Ok(())
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Inp {
    Literal {
        literal: Ustr,
        description: Option<Ustr>,
        fallback_level: usize,
    },
    Subword {
        subdfa: DFAId,
        fallback_level: usize,
    },
    Nonterminal {
        nonterm: Ustr,
        spec: Option<Specialization>,
        fallback_level: usize,
    },
    Command {
        cmd: Ustr,
        regex: Option<CmdRegex>,
        fallback_level: usize,
    },
}

impl Inp {
    pub fn from_input(input: &Input) -> Self {
        match input.clone() {
            Input::Literal {
                literal,
                description,
                fallback_level,
                ..
            } => Self::Literal {
                literal,
                description,
                fallback_level,
            },
            Input::Subword {
                subdfa,
                fallback_level,
                ..
            } => Self::Subword {
                subdfa,
                fallback_level,
            },
            Input::Nonterminal {
                nonterm,
                spec,
                fallback_level,
                ..
            } => Self::Nonterminal {
                nonterm,
                spec,
                fallback_level,
            },
            Input::Command {
                cmd,
                regex,
                fallback_level,
                ..
            } => Self::Command {
                cmd,
                regex,
                fallback_level,
            },
        }
    }

    pub fn is_ambiguous(&self, shell: Shell) -> bool {
        match self {
            Self::Literal { .. } => false,
            Self::Subword { .. } => false, // XXX inaccurate
            Self::Nonterminal { .. } => true,
            Self::Command { regex: None, .. } => true,
            Self::Command {
                regex: Some(regex), ..
            } => regex.matches_anything(shell),
        }
    }

    pub fn get_fallback_level(&self) -> usize {
        match self {
            Self::Literal {
                fallback_level: level,
                ..
            } => *level,
            Self::Subword {
                fallback_level: level,
                ..
            } => *level,
            Self::Nonterminal {
                fallback_level: level,
                ..
            } => *level,
            Self::Command {
                fallback_level: level,
                ..
            } => *level,
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

#[derive(Clone, PartialEq)]
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

    pub fn firstpos(&self, arena: &[RegexNode]) -> BTreeSet<Position> {
        let mut result: BTreeSet<Position> = Default::default();
        do_firstpos(self, arena, &mut result);
        result
    }

    fn lastpos(&self, arena: &[RegexNode]) -> HashSet<Position> {
        let mut result: HashSet<Position> = Default::default();
        do_lastpos(self, arena, &mut result);
        result
    }

    pub fn followpos(&self, arena: &[RegexNode]) -> BTreeMap<Position, RoaringBitmap> {
        let mut result: BTreeMap<Position, RoaringBitmap> = Default::default();
        do_followpos(self, arena, &mut result);
        result
    }
}

fn do_from_expr(
    e: ExprId,
    expr_arena: &[Expr],
    specs: &UstrMap<Specialization>,
    arena: &mut Vec<RegexNode>,
    input_from_position: &mut Vec<Input>,
) -> RegexNodeId {
    match &expr_arena[e] {
        Expr::Terminal {
            term,
            descr: description,
            fallback: level,
            span,
        } => {
            let result = RegexNode::Terminal(
                *term,
                *level,
                Position::try_from(input_from_position.len()).unwrap(),
            );
            let input = Input::Literal {
                literal: *term,
                description: *description,
                fallback_level: *level,
                span: *span,
            };
            input_from_position.push(input.clone());
            alloc(arena, result)
        }
        Expr::Subword {
            phase,
            fallback,
            span,
        } => {
            let result = RegexNode::Subword(Position::try_from(input_from_position.len()).unwrap());
            let dfa = match phase {
                SubwordCompilationPhase::DFA(dfa) => dfa,
                SubwordCompilationPhase::Expr(_) => unreachable!(),
            };
            let input = Input::Subword {
                subdfa: *dfa,
                fallback_level: *fallback,
                span: *span,
            };
            input_from_position.push(input.clone());
            alloc(arena, result)
        }
        Expr::NontermRef {
            nonterm,
            fallback,
            span,
        } => {
            let result =
                RegexNode::Nonterminal(Position::try_from(input_from_position.len()).unwrap());
            let specialization = specs.get(nonterm);
            let input = Input::Nonterminal {
                nonterm: *nonterm,
                spec: specialization.copied(),
                fallback_level: *fallback,
                span: *span,
            };
            input_from_position.push(input.clone());
            alloc(arena, result)
        }
        Expr::Command {
            cmd,
            regex,
            fallback,
            span,
        } => {
            let result =
                RegexNode::Command(*cmd, Position::try_from(input_from_position.len()).unwrap());
            let input = Input::Command {
                cmd: *cmd,
                regex: *regex,
                fallback_level: *fallback,
                span: *span,
            };
            input_from_position.push(input.clone());
            alloc(arena, result)
        }
        Expr::Sequence(subexprs) => {
            let mut left_regex_id =
                do_from_expr(subexprs[0], expr_arena, specs, arena, input_from_position);
            for right_expr in &subexprs[1..] {
                let right_regex_id =
                    do_from_expr(*right_expr, expr_arena, specs, arena, input_from_position);
                let left_regex = RegexNode::Cat(left_regex_id, right_regex_id);
                left_regex_id = alloc(arena, left_regex);
            }
            left_regex_id
        }
        Expr::Alternative(subexprs) => {
            let mut subregexes: Vec<RegexNodeId> = Default::default();
            for e in subexprs {
                let subregex = do_from_expr(*e, expr_arena, specs, arena, input_from_position);
                subregexes.push(subregex);
            }
            let result = RegexNode::Or(subregexes.into_boxed_slice(), false);
            alloc(arena, result)
        }
        Expr::Optional(subexpr) => {
            let subregex = do_from_expr(*subexpr, expr_arena, specs, arena, input_from_position);
            let epsid = alloc(arena, RegexNode::Epsilon);
            let result = RegexNode::Or(Box::new([subregex, epsid]), false);
            alloc(arena, result)
        }
        Expr::Many1(subexpr) => {
            let subregex_id = do_from_expr(*subexpr, expr_arena, specs, arena, input_from_position);
            let star = RegexNode::Star(subregex_id);
            let starid = alloc(arena, star);
            let result = RegexNode::Cat(subregex_id, starid);
            alloc(arena, result)
        }
        Expr::DistributiveDescription { child: _, descr: _ } => unreachable!(
            "DistributiveDescription Expr type should have been erased before compilation to regex"
        ),
        Expr::Fallback(subexprs) => {
            let mut subregexes: Vec<RegexNodeId> = Default::default();
            for e in subexprs {
                let subregex = do_from_expr(*e, expr_arena, specs, arena, input_from_position);
                subregexes.push(subregex);
            }
            let result = RegexNode::Or(subregexes.into_boxed_slice(), true);
            alloc(arena, result)
        }
    }
}

#[derive(Debug)]
pub struct Regex {
    pub root_id: RegexNodeId,
    pub input_from_position: Vec<Input>,
    pub endmarker_position: Position,
    pub arena: Vec<RegexNode>,
}

/*

Ambiguous matching arises from:

1) ({{{ ... }}} | foo), i.e. commands at non-tail position; (foo | {{{ ... }}}) is fine
2) Same with <NONTERM>, where <NONTERM> is undefined, because it matches anything then

*/
fn do_ensure_ambiguous_inputs_tail_only(
    firstpos: &RoaringBitmap,
    followpos: &BTreeMap<Position, RoaringBitmap>,
    endmarker_position: Position,
    input_from_position: &Vec<Input>,
    shell: Shell,
    visited: &mut RoaringBitmap,
) -> Result<()> {
    let unvisited = firstpos - visited.clone();
    if unvisited.is_empty() {
        return Ok(());
    }

    let inputs: Vec<Input> = firstpos
        .iter()
        .filter(|pos| *pos != endmarker_position)
        .map(|pos| input_from_position[pos as usize].clone())
        .collect();

    let mut prev_ambiguous: Option<Input> = None;
    for inp in inputs {
        if inp.is_ambiguous(shell) {
            if let Some(prev_inp) = prev_ambiguous {
                return Err(Error::AmbiguousMatchable(Box::new(prev_inp), Box::new(inp)));
            }
            prev_ambiguous = Some(inp);
        }
    }

    for pos in unvisited {
        let Some(follow) = followpos.get(&pos) else {
            continue;
        };
        visited.insert(pos);
        do_ensure_ambiguous_inputs_tail_only(
            follow,
            followpos,
            endmarker_position,
            input_from_position,
            shell,
            visited,
        )?;
    }
    Ok(())
}

// Subword ambiguity checker is stricter than the word-based one.  Any matches_anything() input
// followed by any input is ambiguous since we can't tell where the matches_anything() one ends.
fn do_ensure_ambiguous_inputs_tail_only_subword(
    firstpos: &RoaringBitmap,
    followpos: &BTreeMap<Position, RoaringBitmap>,
    input_from_position: &Vec<Input>,
    shell: Shell,
    path_prev_ambiguous: Option<Input>,
    visited: &mut RoaringBitmap,
) -> Result<()> {
    let unvisited = firstpos - visited.clone();
    if unvisited.is_empty() {
        return Ok(());
    }

    let inputs: Vec<Input> = unvisited
        .iter()
        .filter_map(|pos| input_from_position.get(pos as usize).cloned())
        .collect();

    let mut prev_ambiguous: Option<Input> = None;
    for inp in inputs {
        if let Some(ref prev_inp) = path_prev_ambiguous {
            return Err(Error::AmbiguousMatchable(
                Box::new(prev_inp.clone()),
                Box::new(inp),
            ));
        }

        if let Some(prev_inp) = prev_ambiguous {
            return Err(Error::AmbiguousMatchable(Box::new(prev_inp), Box::new(inp)));
        }
        if inp.is_ambiguous(shell) {
            prev_ambiguous = Some(inp);
        }
    }

    for pos in unvisited {
        let Some(follow) = followpos.get(&pos) else {
            continue;
        };
        visited.insert(pos);
        do_ensure_ambiguous_inputs_tail_only_subword(
            follow,
            followpos,
            input_from_position,
            shell,
            path_prev_ambiguous.clone().or(prev_ambiguous.clone()),
            visited,
        )?;
    }
    Ok(())
}

fn do_check_clashing_variants(
    firstpos: &RoaringBitmap,
    followpos: &BTreeMap<Position, RoaringBitmap>,
    input_from_position: &Vec<Input>,
    visited: &mut RoaringBitmap,
) -> Result<()> {
    let unvisited = firstpos - visited.clone();
    if unvisited.is_empty() {
        return Ok(());
    }

    let mut inputs: Vec<(Ustr, Option<Ustr>, Option<HumanSpan>)> = unvisited
        .iter()
        .filter_map(|pos| input_from_position.get(pos as usize).cloned())
        .filter_map(|inp| match inp {
            Input::Literal {
                literal,
                description,
                span,
                ..
            } => Some((literal, description, span)),
            _ => None,
        })
        .collect();

    inputs.sort_by_key(|(literal, _, _)| *literal);
    inputs.dedup_by_key(|(literal, description, _)| (*literal, *description));

    for slice in inputs.windows(2) {
        let [
            (left_literal, left_description, left_span),
            (right_literal, right_description, right_span),
        ] = slice
        else {
            unreachable!()
        };

        if left_literal != right_literal {
            continue;
        }

        if left_description == right_description {
            continue;
        }

        return Err(Error::ClashingVariants(*left_span, *right_span));
    }

    for pos in unvisited {
        let Some(follow) = followpos.get(&pos) else {
            continue;
        };
        visited.insert(pos);
        do_check_clashing_variants(follow, followpos, input_from_position, visited)?;
    }
    Ok(())
}

fn do_to_dot<W: Write>(
    output: &mut W,
    node_id: RegexNodeId,
    arena: &[RegexNode],
    input_from_position: &Vec<Input>,
) -> std::result::Result<(), std::io::Error> {
    match arena[node_id].clone() {
        RegexNode::Epsilon => {
            writeln!(output, r#"  _{node_id}[label="Epsilon"];"#)?;
        }
        RegexNode::Subword(pos) => {
            writeln!(output, r#"  _{node_id}[label="{pos}: Subword"];"#)?;
        }
        RegexNode::Terminal(term, _, pos) => {
            writeln!(output, r#"  _{node_id}[label="{pos}: \"{term}\""];"#)?;
        }
        RegexNode::Nonterminal(pos) => {
            let input = input_from_position[pos as usize].clone();
            let Input::Nonterminal { nonterm, .. } = input else {
                unreachable!()
            };
            writeln!(output, r#"  _{node_id}[label="{pos}: <{nonterm}>"];"#)?;
        }
        RegexNode::Command(cmd, pos) => {
            writeln!(output, r#"  _{node_id}[label="{pos}: {cmd}"];"#)?;
        }
        RegexNode::Cat(lhs, rhs) => {
            writeln!(
                output,
                r#"  _{node_id}[label="Cat"]; _{node_id} -> _{lhs}; _{node_id} -> _{rhs};"#
            )?;
            do_to_dot(output, lhs, arena, input_from_position)?;
            do_to_dot(output, rhs, arena, input_from_position)?;
        }
        RegexNode::Or(ors, _) => {
            writeln!(output, r#"  _{node_id}[label="Or"];"#)?;
            for child in &ors {
                writeln!(output, r#"  _{node_id} -> _{child};"#)?;
            }
            for child in ors {
                do_to_dot(output, child, arena, input_from_position)?;
            }
        }
        RegexNode::Star(child) => {
            writeln!(
                output,
                r#"  _{node_id}[label="Star"]; _{node_id} -> _{child};"#
            )?;
        }
        RegexNode::EndMarker(pos) => {
            writeln!(output, r#"  _{node_id}[label="{pos}: EndMarker"];"#)?;
        }
    }
    Ok(())
}

impl Regex {
    pub fn from_valid_grammar(v: &ValidGrammar, shell: Shell) -> Result<Self> {
        let regex = Self::from_expr(v.expr, &v.arena, &v.specializations)?;
        regex.ensure_ambiguous_inputs_tail_only(shell)?;
        regex.check_clashing_variants()?;
        Ok(regex)
    }

    pub fn from_expr(
        e: ExprId,
        expr_arena: &[Expr],
        specs: &UstrMap<Specialization>,
    ) -> Result<Self> {
        let mut input_from_position: Vec<Input> = Default::default();
        let mut arena: Vec<RegexNode> = Default::default();
        let regex = do_from_expr(e, expr_arena, specs, &mut arena, &mut input_from_position);
        let endmarker_position = input_from_position.len() as Position;
        let endmarkerid = alloc(&mut arena, RegexNode::EndMarker(endmarker_position));
        let root = RegexNode::Cat(regex, endmarkerid);
        let root_id = alloc(&mut arena, root.clone());

        let retval = Self {
            root_id,
            endmarker_position,
            input_from_position,
            arena,
        };
        Ok(retval)
    }

    pub fn get_unique_inputs(&self) -> IndexMap<Input, Position> {
        let mut result: IndexMap<Input, Position> = Default::default();
        for (id, input) in self.input_from_position.iter().enumerate() {
            result.entry(input.clone()).or_insert(id as Position);
        }
        result
    }

    pub fn ensure_ambiguous_inputs_tail_only(&self, shell: Shell) -> Result<()> {
        let mut visited: RoaringBitmap = Default::default();
        do_ensure_ambiguous_inputs_tail_only(
            &RoaringBitmap::from_iter(&self.firstpos()),
            &self.followpos(),
            self.endmarker_position,
            &self.input_from_position,
            shell,
            &mut visited,
        )
    }

    pub fn ensure_ambiguous_inputs_tail_only_subword(&self, shell: Shell) -> Result<()> {
        let mut visited: RoaringBitmap = Default::default();
        visited.insert(self.endmarker_position);
        do_ensure_ambiguous_inputs_tail_only_subword(
            &RoaringBitmap::from_iter(&self.firstpos()),
            &self.followpos(),
            &self.input_from_position,
            shell,
            None,
            &mut visited,
        )
    }

    // e.g. git (subcommand "description" | subcommand --option);
    pub fn check_clashing_variants(&self) -> Result<()> {
        let mut visited: RoaringBitmap = Default::default();
        visited.insert(self.endmarker_position);
        do_check_clashing_variants(
            &RoaringBitmap::from_iter(&self.firstpos()),
            &self.followpos(),
            &self.input_from_position,
            &mut visited,
        )
    }

    pub fn get_root(&self) -> &RegexNode {
        &self.arena[self.root_id]
    }

    pub fn firstpos(&self) -> BTreeSet<Position> {
        self.get_root().firstpos(&self.arena)
    }

    pub fn followpos(&self) -> BTreeMap<Position, RoaringBitmap> {
        self.get_root().followpos(&self.arena)
    }

    pub fn to_dot<W: Write>(&self, output: &mut W) -> std::result::Result<(), std::io::Error> {
        writeln!(output, "digraph rx {{")?;
        do_to_dot(output, self.root_id, &self.arena, &self.input_from_position)?;
        writeln!(output, "}}")?;
        Ok(())
    }

    #[allow(dead_code)]
    pub fn to_dot_file<P: AsRef<std::path::Path>>(
        &self,
        path: P,
    ) -> std::result::Result<(), std::io::Error> {
        let mut file = std::fs::File::create(path)?;
        self.to_dot(&mut file)?;
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
        let specs = UstrMap::default();
        let regex = Regex::from_expr(validated.expr, &validated.arena, &specs).unwrap();
        regex.ensure_ambiguous_inputs_tail_only(Shell::Bash)?;
        regex.check_clashing_variants()?;
        Ok(validated)
    }

    #[test]
    fn detects_nontail_command_subword() {
        assert!(matches!(
            get_validated_grammar(r#"cmd {{{ git tag }}}..{{{ git tag }}};"#),
            Err(Error::AmbiguousMatchable(_, _))
        ));
        assert!(matches!(
            get_validated_grammar(r#"cmd --option={{{ echo foo }}};"#),
            Ok(_)
        ));
        assert!(matches!(
            get_validated_grammar(r#"cmd {{{ echo foo }}}{{{ echo bar }}};"#),
            Err(Error::AmbiguousMatchable(_, _))
        ));

        // https://github.com/adaszko/complgen/issues/49
        assert!(matches!(
            get_validated_grammar(
                r#"build <PLATFORM>-(amd64|arm64); <PLATFORM> ::= {{{ echo foo }}};"#
            ),
            Err(Error::AmbiguousMatchable(_, _))
        ));
        assert!(matches!(
            get_validated_grammar(
                r#"build <PLATFORM>[-(amd64|arm64)]; <PLATFORM> ::= {{{ echo foo }}};"#
            ),
            Err(Error::AmbiguousMatchable(_, _))
        ));

        // https://github.com/adaszko/complgen/issues/53
        assert!(matches!(
            get_validated_grammar(r#"cmd <DUMMY>,<DUMMY>; <DUMMY@bash> ::= {{{ echo dummy }}};"#),
            Err(Error::AmbiguousMatchable(_, _))
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
            Err(Error::AmbiguousMatchable(_, _))
        ));
    }
}

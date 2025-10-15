use crate::{
    Error, Result,
    grammar::{DFAInternPool, ValidGrammar},
};
use hashbrown::HashSet;
use std::{
    collections::{BTreeMap, BTreeSet},
    io::Write,
};

use roaring::RoaringBitmap;
use ustr::Ustr;

use crate::grammar::{DFAId, Expr, ExprId, HumanSpan, Shell, SubwordCompilationPhase};

pub type Position = u32;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Input {
    Literal {
        literal: Ustr,
        description: Option<Ustr>,
        fallback_level: usize,
        span: HumanSpan,
    },
    Subword {
        subdfa: DFAId,
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
        regex: Option<Ustr>,
        zsh_compadd: bool,
        fallback_level: usize,
        span: HumanSpan,
    },
}

impl Input {
    fn is_star(&self, subdfas: &DFAInternPool) -> bool {
        match self {
            Self::Literal { .. } => false,
            Self::Subword { subdfa: id, .. } => {
                let subdfa = subdfas.lookup(*id);
                for inp_id in subdfa.iter_leaders() {
                    let inp = subdfa.get_input(inp_id);
                    if inp.is_star(subdfas) {
                        return true;
                    }
                }
                false
            }
            Self::Nonterminal { .. } => true,
            Self::Command { regex: None, .. } => true,
            Self::Command { .. } => false,
        }
    }

    fn is_star_subword(&self) -> bool {
        match self {
            Self::Literal { .. } => false,
            Self::Nonterminal { .. } => true,
            Self::Command { regex: None, .. } => true,
            Self::Command { .. } => false,
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

pub fn diagnostic_display_input<W: std::fmt::Write>(w: &mut W, input: &Inp) -> Result<()> {
    match input {
        Inp::Literal { literal, .. } => write!(w, r#"{literal}"#)?,
        Inp::Star => write!(w, r#"*"#)?,
        Inp::Command {
            cmd,
            regex: None,
            zsh_compadd,
            ..
        } => write!(
            w,
            r#"{{{{{{ {cmd} }}}}}}{}"#,
            if *zsh_compadd { "compadd" } else { "" }
        )?,
        Inp::Command {
            cmd,
            regex: Some(regex),
            zsh_compadd,
            ..
        } => write!(
            w,
            r#"{{{{{{ {cmd} }}}}}}@shell"{regex}"{}"#,
            if *zsh_compadd { "compadd" } else { "" }
        )?,
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
    Command {
        cmd: Ustr,
        regex: Option<Ustr>,
        zsh_compadd: bool,
        fallback_level: usize,
    },
    Star,
}

impl Inp {
    pub(crate) fn from_input(input: &Input) -> Self {
        match input.clone() {
            Input::Literal {
                literal,
                description,
                fallback_level,
                span: _,
            } => Self::Literal {
                literal,
                description,
                fallback_level,
            },
            Input::Subword {
                subdfa,
                fallback_level,
                span: _,
            } => Self::Subword {
                subdfa,
                fallback_level,
            },
            Input::Nonterminal { .. } => Self::Star,
            Input::Command {
                cmd,
                regex,
                fallback_level,
                zsh_compadd,
                span: _,
            } => Self::Command {
                cmd,
                regex,
                zsh_compadd,
                fallback_level,
            },
        }
    }

    pub(crate) fn is_star(&self, subdfas: &DFAInternPool) -> bool {
        match self {
            Self::Literal { .. } => false,
            Self::Subword { subdfa: id, .. } => {
                let subdfa = subdfas.lookup(*id);
                for inp_id in subdfa.iter_leaders() {
                    let inp = subdfa.get_input(inp_id);
                    if inp.is_star(subdfas) {
                        return true;
                    }
                }
                false
            }
            Self::Star => true,
            Self::Command { regex: None, .. } => true,
            Self::Command {
                regex: Some(..), ..
            } => false,
        }
    }

    pub(crate) fn get_fallback_level(&self) -> Option<usize> {
        match self {
            Self::Literal {
                fallback_level: level,
                ..
            } => Some(*level),
            Self::Subword {
                fallback_level: level,
                ..
            } => Some(*level),
            Self::Star => None,
            Self::Command {
                fallback_level: level,
                ..
            } => Some(*level),
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
            let input = Input::Nonterminal {
                nonterm: *nonterm,
                fallback_level: *fallback,
                span: *span,
            };
            input_from_position.push(input.clone());
            alloc(arena, result)
        }
        Expr::Command {
            cmd,
            bash_regex,
            fish_regex,
            zsh_regex,
            zsh_compadd,
            fallback,
            span,
        } => {
            let result =
                RegexNode::Command(*cmd, Position::try_from(input_from_position.len()).unwrap());
            let rx = match shell {
                Shell::Bash => bash_regex,
                Shell::Fish => fish_regex,
                Shell::Zsh => zsh_regex,
            };
            let input = Input::Command {
                cmd: *cmd,
                regex: *rx,
                zsh_compadd: *zsh_compadd,
                fallback_level: *fallback,
                span: *span,
            };
            input_from_position.push(input.clone());
            alloc(arena, result)
        }
        Expr::Sequence(subexprs) => {
            let mut left_regex_id =
                do_from_expr(subexprs[0], expr_arena, shell, arena, input_from_position);
            for right_expr in &subexprs[1..] {
                let right_regex_id =
                    do_from_expr(*right_expr, expr_arena, shell, arena, input_from_position);
                let left_regex = RegexNode::Cat(left_regex_id, right_regex_id);
                left_regex_id = alloc(arena, left_regex);
            }
            left_regex_id
        }
        Expr::Alternative(subexprs) => {
            let mut subregexes: Vec<RegexNodeId> = Default::default();
            for e in subexprs {
                let subregex = do_from_expr(*e, expr_arena, shell, arena, input_from_position);
                subregexes.push(subregex);
            }
            let result = RegexNode::Or(subregexes.into_boxed_slice(), false);
            alloc(arena, result)
        }
        Expr::Optional(subexpr) => {
            let subregex = do_from_expr(*subexpr, expr_arena, shell, arena, input_from_position);
            let epsid = alloc(arena, RegexNode::Epsilon);
            let result = RegexNode::Or(Box::new([subregex, epsid]), false);
            alloc(arena, result)
        }
        Expr::Many1(subexpr) => {
            let subregex_id = do_from_expr(*subexpr, expr_arena, shell, arena, input_from_position);
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
                let subregex = do_from_expr(*e, expr_arena, shell, arena, input_from_position);
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
        let regex = Self::from_expr(v.expr, &v.arena, shell)?;
        regex.check_ambiguities(&v.subdfas)?;
        Ok(regex)
    }

    pub(crate) fn from_expr(e: ExprId, expr_arena: &[Expr], shell: Shell) -> Result<Self> {
        let mut input_from_position: Vec<Input> = Default::default();
        let mut arena: Vec<RegexNode> = Default::default();
        let regex = do_from_expr(e, expr_arena, shell, &mut arena, &mut input_from_position);
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

    // Fail if there's a state with 2 outgoing transitions, where both transitions have "star"
    // inputs.  Meaning it's not possible to tell which transition to take next at matching time.
    fn do_check_transitions_unambiguous(
        &self,
        firstpos: &RoaringBitmap,
        followpos: &BTreeMap<Position, RoaringBitmap>,
        subdfas: &DFAInternPool,
        visited: &mut RoaringBitmap,
    ) -> Result<()> {
        let stars: Vec<Input> = firstpos
            .iter()
            .filter(|pos| *pos != self.endmarker_position)
            .map(|pos| self.input_from_position[pos as usize].clone())
            .filter(|input| input.is_star(subdfas))
            .collect();

        if stars.len() >= 2 {
            return Err(Error::AmbiguousMatchable(
                stars[0].get_span(),
                stars[1].get_span(),
            ));
        }

        let unvisited = firstpos - visited.clone();
        for pos in unvisited {
            let Some(follow) = followpos.get(&pos) else {
                continue;
            };
            visited.insert(pos);
            self.do_check_transitions_unambiguous(follow, followpos, subdfas, visited)?;
        }
        Ok(())
    }

    fn check_transitions_unambiguous(
        &self,
        firstpos: &RoaringBitmap,
        followpos: &BTreeMap<Position, RoaringBitmap>,
        subdfas: &DFAInternPool,
    ) -> Result<()> {
        let mut visited: RoaringBitmap = Default::default();
        self.do_check_transitions_unambiguous(firstpos, followpos, subdfas, &mut visited)
    }

    fn do_check_transitions_unambiguous_subword(
        &self,
        firstpos: &RoaringBitmap,
        followpos: &BTreeMap<Position, RoaringBitmap>,
        visited: &mut RoaringBitmap,
    ) -> Result<()> {
        let stars: Vec<Input> = firstpos
            .iter()
            .filter(|pos| *pos != self.endmarker_position)
            .map(|pos| self.input_from_position[pos as usize].clone())
            .filter(|input| input.is_star_subword())
            .collect();

        if stars.len() >= 2 {
            return Err(Error::AmbiguousMatchable(
                stars[0].get_span(),
                stars[1].get_span(),
            ));
        }

        let unvisited = firstpos - visited.clone();
        for pos in unvisited {
            let Some(follow) = followpos.get(&pos) else {
                continue;
            };
            visited.insert(pos);
            self.do_check_transitions_unambiguous_subword(follow, followpos, visited)?;
        }
        Ok(())
    }

    fn check_transitions_unambiguous_subword(
        &self,
        firstpos: &RoaringBitmap,
        followpos: &BTreeMap<Position, RoaringBitmap>,
    ) -> Result<()> {
        let mut visited: RoaringBitmap = Default::default();
        visited.insert(self.endmarker_position);
        self.do_check_transitions_unambiguous_subword(firstpos, followpos, &mut visited)
    }

    fn do_check_ambiguous_inputs_tail_only_subword(
        &self,
        firstpos: &RoaringBitmap,
        followpos: &BTreeMap<Position, RoaringBitmap>,
        path_prev_ambiguous: Option<Input>,
        visited: &mut RoaringBitmap,
    ) -> Result<()> {
        let inputs: Vec<Input> = firstpos
            .iter()
            .filter(|pos| *pos != self.endmarker_position)
            .map(|pos| self.input_from_position[pos as usize].clone())
            .collect();

        let mut prev_ambiguous: Option<Input> = None;
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

        let unvisited = firstpos - visited.clone();
        for pos in unvisited {
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

    fn do_check_descr_no_descr_clashes(
        &self,
        firstpos: &RoaringBitmap,
        followpos: &BTreeMap<Position, RoaringBitmap>,
        visited: &mut RoaringBitmap,
    ) -> Result<()> {
        let mut inputs: Vec<(Ustr, Option<Ustr>, HumanSpan)> = firstpos
            .iter()
            .filter(|pos| *pos != self.endmarker_position)
            .map(|pos| self.input_from_position[pos as usize].clone())
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

        let unvisited = firstpos - visited.clone();
        for pos in unvisited {
            let Some(follow) = followpos.get(&pos) else {
                continue;
            };
            visited.insert(pos);
            self.do_check_descr_no_descr_clashes(follow, followpos, visited)?;
        }
        Ok(())
    }

    // e.g. git (subcommand "description" | subcommand --option);
    fn check_descr_no_descr_clashes(
        &self,
        firstpos: &RoaringBitmap,
        followpos: &BTreeMap<Position, RoaringBitmap>,
    ) -> Result<()> {
        let mut visited: RoaringBitmap = Default::default();
        visited.insert(self.endmarker_position);
        self.do_check_descr_no_descr_clashes(firstpos, followpos, &mut visited)
    }

    fn do_check_clashing_subword_leaders(
        &self,
        firstpos: &RoaringBitmap,
        followpos: &BTreeMap<Position, RoaringBitmap>,
        subdfas: &DFAInternPool,
        visited: &mut RoaringBitmap,
    ) -> Result<()> {
        let mut leaders: Vec<(Ustr, HumanSpan)> = firstpos
            .iter()
            .filter(|pos| *pos != self.endmarker_position)
            .map(|pos| self.input_from_position[pos as usize].clone())
            .filter_map(|inp| match inp {
                Input::Subword {
                    subdfa: id, span, ..
                } => {
                    let subdfa = subdfas.lookup(id);
                    let literals: Vec<Ustr> = subdfa
                        .iter_leaders()
                        .map(|inp_id| subdfa.get_input(inp_id).clone())
                        .filter_map(|inp| match inp {
                            Inp::Literal { literal, .. } => Some(literal),
                            Inp::Subword { .. } => None,
                            Inp::Command { .. } => None,
                            Inp::Star => None,
                        })
                        .collect();
                    Some((literals, span))
                }
                Input::Literal { .. } | Input::Nonterminal { .. } | Input::Command { .. } => None,
            })
            .flat_map(|(literals, span)| {
                literals
                    .iter()
                    .map(|lit| (*lit, span))
                    .collect::<Vec<(Ustr, HumanSpan)>>()
            })
            .collect();

        leaders.sort_by_key(|(literal, _)| *literal);

        for slice in leaders.windows(2) {
            let [(left_literal, left_span), (right_literal, right_span)] = slice else {
                unreachable!()
            };

            if left_literal != right_literal {
                continue;
            }

            return Err(Error::ClashingSubwordLeaders(*left_span, *right_span));
        }

        let unvisited = firstpos - visited.clone();
        for pos in unvisited {
            let Some(follow) = followpos.get(&pos) else {
                continue;
            };
            visited.insert(pos);
            self.do_check_clashing_subword_leaders(follow, followpos, subdfas, visited)?;
        }
        Ok(())
    }

    // e.g. git (subcommand "description" | subcommand --option);
    fn check_clashing_subword_leaders(
        &self,
        firstpos: &RoaringBitmap,
        followpos: &BTreeMap<Position, RoaringBitmap>,
        subdfas: &DFAInternPool,
    ) -> Result<()> {
        let mut visited: RoaringBitmap = Default::default();
        visited.insert(self.endmarker_position);
        self.do_check_clashing_subword_leaders(firstpos, followpos, subdfas, &mut visited)
    }

    pub(crate) fn check_ambiguities(&self, subdfas: &DFAInternPool) -> Result<()> {
        let firstpos = RoaringBitmap::from_iter(&self.firstpos());
        let followpos = self.followpos();
        self.check_transitions_unambiguous(&firstpos, &followpos, subdfas)?;
        self.check_descr_no_descr_clashes(&firstpos, &followpos)?;
        self.check_clashing_subword_leaders(&firstpos, &followpos, subdfas)?;
        Ok(())
    }

    pub(crate) fn check_ambiguities_subword(&self) -> Result<()> {
        let firstpos = RoaringBitmap::from_iter(&self.firstpos());
        let followpos = self.followpos();
        self.check_transitions_unambiguous_subword(&firstpos, &followpos)?;
        self.check_ambiguous_inputs_tail_only_subword(&firstpos, &followpos)?;
        self.check_descr_no_descr_clashes(&firstpos, &followpos)?;
        Ok(())
    }

    fn get_root(&self) -> &RegexNode {
        &self.arena[self.root_id]
    }

    pub fn firstpos(&self) -> BTreeSet<Position> {
        self.get_root().firstpos(&self.arena)
    }

    pub(crate) fn followpos(&self) -> BTreeMap<Position, RoaringBitmap> {
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
        let _ = Regex::from_valid_grammar(&validated, Shell::Bash)?;
        Ok(validated)
    }

    #[test]
    fn detects_nontail_command_subword() {
        assert!(matches!(
            get_validated_grammar(r#"cmd {{{ git tag }}}..{{{ git tag }}};"#),
            Err(Error::UnboundedMatchable(_, _))
        ));
        assert!(matches!(
            get_validated_grammar(r#"cmd --option={{{ echo foo }}};"#),
            Ok(_)
        ));
        assert!(matches!(
            get_validated_grammar(r#"cmd {{{ echo foo }}}{{{ echo bar }}};"#),
            Err(Error::UnboundedMatchable(_, _))
        ));

        // https://github.com/adaszko/complgen/issues/49
        assert!(matches!(
            get_validated_grammar(
                r#"build <PLATFORM>-(amd64|arm64); <PLATFORM> ::= {{{ echo foo }}};"#
            ),
            Err(Error::UnboundedMatchable(_, _))
        ));
        assert!(matches!(
            get_validated_grammar(
                r#"build <PLATFORM>[-(amd64|arm64)]; <PLATFORM> ::= {{{ echo foo }}};"#
            ),
            Err(Error::UnboundedMatchable(_, _))
        ));

        // https://github.com/adaszko/complgen/issues/53
        assert!(matches!(
            get_validated_grammar(r#"cmd <DUMMY>,<DUMMY>; <DUMMY@bash> ::= {{{ echo dummy }}};"#),
            Err(Error::UnboundedMatchable(_, _))
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

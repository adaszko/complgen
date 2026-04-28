use std::debug_assert;

use hashbrown::HashSet;

use crate::{
    Error, Result,
    parse::{
        BuiltinSpec, Expr, ExprId, Grammar, HumanSpan, NontermDefn, Shell, UserSpec, alloc,
        flatten_expr,
    },
};
use ustr::{Ustr, UstrMap, UstrSet, ustr};

#[derive(Debug)]
pub struct ValidGrammar {
    pub command: Ustr,
    pub expr: ExprId,
    pub arena: Vec<Expr>,
    pub undefined_nonterminals: UstrMap<HumanSpan>,
    pub unused_nonterminals: UstrMap<HumanSpan>,
    pub unused_specializations: UstrMap<HumanSpan>,
}

// Contrary to flatten_expr(), preserves the topmost subword node.
fn collapse_subwords(arena: &mut Vec<Expr>, expr_id: ExprId) -> ExprId {
    match arena[expr_id].clone() {
        Expr::Subword {
            root_id,
            fallback,
            span,
        } => {
            let new_root = flatten_expr(arena, root_id);
            alloc(
                arena,
                Expr::Subword {
                    root_id: new_root,
                    fallback,
                    span,
                },
            )
        }
        Expr::Terminal { .. } | Expr::NontermRef { .. } | Expr::Command { .. } => expr_id,
        Expr::Sequence { children, span } => {
            let new_children: Vec<ExprId> = children
                .iter()
                .map(|e| collapse_subwords(arena, *e))
                .collect();
            if children == new_children {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Sequence {
                        children: new_children,
                        span,
                    },
                )
            }
        }
        Expr::Alternative { children, span } => {
            let new_children: Vec<ExprId> = children
                .iter()
                .map(|e| collapse_subwords(arena, *e))
                .collect();
            if children == new_children {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Alternative {
                        children: new_children,
                        span,
                    },
                )
            }
        }
        Expr::Optional { child, span } => {
            let new_child = collapse_subwords(arena, child);
            if child == new_child {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Optional {
                        child: new_child,
                        span,
                    },
                )
            }
        }
        Expr::Many1 { child, span } => {
            let new_child = collapse_subwords(arena, child);
            if child == new_child {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Many1 {
                        child: new_child,
                        span,
                    },
                )
            }
        }
        Expr::DistributiveDescription {
            child,
            descr: description,
            span,
        } => {
            let new_child = collapse_subwords(arena, child);
            if child == new_child {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::DistributiveDescription {
                        child: new_child,
                        descr: description,
                        span,
                    },
                )
            }
        }
        Expr::Fallback { children, span } => {
            let new_children: Vec<ExprId> = children
                .iter()
                .map(|e| collapse_subwords(arena, *e))
                .collect();
            if children == new_children {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Fallback {
                        children: new_children,
                        span,
                    },
                )
            }
        }
    }
}

// Move descriptions to their corresponding terminals.
fn do_distribute_descriptions(
    arena: &mut Vec<Expr>,
    expr_id: ExprId,
    description: &mut Option<Ustr>,
) -> ExprId {
    match arena[expr_id].clone() {
        Expr::DistributiveDescription { child, descr, .. } => {
            let new_child = do_distribute_descriptions(arena, child, &mut Some(descr));
            if child == new_child { child } else { new_child }
        }
        Expr::Terminal {
            term,
            descr: None,
            fallback: level,
            span,
        } if description.is_some() => {
            let result = Expr::Terminal {
                term,
                descr: *description,
                fallback: level,
                span,
            };
            *description = None; // spend it
            alloc(arena, result)
        }
        Expr::Terminal { .. } => expr_id,
        Expr::NontermRef { .. } | Expr::Command { .. } => expr_id,
        Expr::Sequence { children, span } => {
            let new_children: Vec<ExprId> = children
                .iter()
                .map(|e| do_distribute_descriptions(arena, *e, description))
                .collect();
            if children == new_children {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Sequence {
                        children: new_children,
                        span,
                    },
                )
            }
        }
        Expr::Alternative { children, span } => {
            let new_children: Vec<ExprId> = children
                .iter()
                .map(|e| do_distribute_descriptions(arena, *e, &mut description.clone()))
                .collect();
            if children == new_children {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Alternative {
                        children: new_children,
                        span,
                    },
                )
            }
        }
        Expr::Optional { child, span } => {
            let new_child = do_distribute_descriptions(arena, child, description);
            if child == new_child {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Optional {
                        child: new_child,
                        span,
                    },
                )
            }
        }
        Expr::Many1 { child, span } => {
            let new_child = do_distribute_descriptions(arena, child, description);
            if child == new_child {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Many1 {
                        child: new_child,
                        span,
                    },
                )
            }
        }
        Expr::Subword {
            root_id: child,
            fallback,
            span,
        } => {
            let new_child = do_distribute_descriptions(arena, child, description);
            if child == new_child {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Subword {
                        root_id: new_child,
                        fallback,
                        span,
                    },
                )
            }
        }
        Expr::Fallback { children, span } => {
            let new_children: Vec<ExprId> = children
                .iter()
                .map(|e| do_distribute_descriptions(arena, *e, description))
                .collect();
            if children == new_children {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Fallback {
                        children: new_children,
                        span,
                    },
                )
            }
        }
    }
}

fn distribute_descriptions(arena: &mut Vec<Expr>, expr_id: ExprId) -> ExprId {
    let mut description = None;
    do_distribute_descriptions(arena, expr_id, &mut description)
}

// Propagate fallback levels of fallback alternatives down to literals.
fn do_propagate_fallback_levels(
    arena: &mut Vec<Expr>,
    expr_id: ExprId,
    fallback_level: usize,
) -> ExprId {
    match arena[expr_id].clone() {
        Expr::Terminal { fallback, .. } if fallback == fallback_level => expr_id,
        Expr::Terminal {
            term, descr, span, ..
        } => alloc(
            arena,
            Expr::Terminal {
                term,
                descr,
                fallback: fallback_level,
                span,
            },
        ),
        Expr::Fallback { children, span } => {
            let new_children: Vec<ExprId> = children
                .iter()
                .enumerate()
                .map(|(i, e)| do_propagate_fallback_levels(arena, *e, i))
                .collect();
            if children == new_children {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Fallback {
                        children: new_children,
                        span,
                    },
                )
            }
        }
        Expr::NontermRef { .. } | Expr::Command { .. } => expr_id,
        Expr::Sequence { children, span } => {
            let new_children: Vec<ExprId> = children
                .iter()
                .map(|e| do_propagate_fallback_levels(arena, *e, fallback_level))
                .collect();
            if children == new_children {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Sequence {
                        children: new_children,
                        span,
                    },
                )
            }
        }
        Expr::Alternative { children, span } => {
            let new_children: Vec<ExprId> = children
                .iter()
                .map(|e| do_propagate_fallback_levels(arena, *e, fallback_level))
                .collect();
            if children == new_children {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Alternative {
                        children: new_children,
                        span,
                    },
                )
            }
        }
        Expr::Optional { child, span } => {
            let new_child = do_propagate_fallback_levels(arena, child, fallback_level);
            if child == new_child {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Optional {
                        child: new_child,
                        span,
                    },
                )
            }
        }
        Expr::Many1 { child, span } => {
            let new_child = do_propagate_fallback_levels(arena, child, fallback_level);
            if child == new_child {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Many1 {
                        child: new_child,
                        span,
                    },
                )
            }
        }
        Expr::Subword {
            root_id: child,
            fallback: _,
            span,
        } => {
            let new_child = do_propagate_fallback_levels(arena, child, fallback_level);
            if child == new_child {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Subword {
                        root_id: new_child,
                        fallback: fallback_level,
                        span,
                    },
                )
            }
        }
        Expr::DistributiveDescription { .. } => unreachable!(),
    }
}

fn propagate_fallback_levels(arena: &mut Vec<Expr>, expr_id: ExprId) -> ExprId {
    do_propagate_fallback_levels(arena, expr_id, 0)
}

fn stable_dedup_by<T, F: Fn(&T) -> D, D: std::hash::Hash + Eq + Copy>(f: F, v: &mut Vec<T>) {
    let mut set = HashSet::new();
    v.retain(|x| set.insert(f(x)));
}

fn is_valid_command_name(command: &str) -> bool {
    if command.contains('/') {
        return false;
    }
    true
}

impl ValidGrammar {
    pub fn from_grammar(mut grammar: Grammar, shell: Shell) -> Result<Self> {
        // 1. Ensure the grammar defines completions for at most one command.
        let (command, command_span) = {
            let mut commands: Vec<(Ustr, HumanSpan)> = grammar
                .iter_call_variants()
                .map(|(cmd, span, _)| (cmd, span))
                .collect();

            if commands.is_empty() {
                return Err(Error::MissingCallVariants);
            }

            stable_dedup_by(|(name, _)| *name, &mut commands);

            if commands.len() > 1 {
                return Err(Error::VaryingCommandNames(
                    commands.into_iter().map(|(_, span)| span).collect(),
                ));
            }
            commands[0]
        };

        if !is_valid_command_name(&command) {
            return Err(crate::Error::InvalidCommandName(command_span));
        }

        // 2. Aggregate all call variants into a single alternative expression
        let expr = {
            let call_variants: Vec<ExprId> = grammar
                .iter_call_variants()
                .map(|(_, _, expr_id)| expr_id)
                .collect();
            if call_variants.len() == 1 {
                call_variants[0]
            } else {
                let span = *grammar.arena[call_variants[0]].get_span();
                alloc(
                    &mut grammar.arena,
                    Expr::Alternative {
                        children: call_variants,
                        span,
                    },
                )
            }
        };

        // 3. Collect nonterminal definitions for expansion
        let mut nonterminal_definitions: UstrMap<NontermDefn> = {
            let mut nonterminal_definitions: UstrMap<NontermDefn> = Default::default();
            for defn in grammar.iter_nonterm_defns() {
                if defn.shell.is_some() {
                    continue;
                }
                if let Some(dup) = nonterminal_definitions.get(&defn.lhs_name) {
                    return Err(Error::DuplicateNonterminalDefinition(
                        dup.lhs_span,
                        defn.lhs_span,
                    ));
                }
                nonterminal_definitions.insert(defn.lhs_name, defn.clone());
            }
            nonterminal_definitions
        };

        // 4. Expand distributive descriptions
        for (_, defn) in nonterminal_definitions.iter_mut() {
            defn.rhs_expr_id = distribute_descriptions(&mut grammar.arena, defn.rhs_expr_id);
        }
        let expr = distribute_descriptions(&mut grammar.arena, expr);

        let (mut user_specs, fallback_specs) = grammar.get_specializations(shell)?;
        let builtin_specs = make_builtin_specializations(shell);

        let mut unused_nonterminals: UstrMap<HumanSpan> = nonterminal_definitions
            .iter()
            .map(|(nonterm, defn)| (*nonterm, defn.lhs_span))
            .collect();
        for (_, defn) in nonterminal_definitions.iter_mut() {
            defn.rhs_expr_id = specialize_nonterminals(
                defn.rhs_expr_id,
                &mut grammar.arena,
                shell,
                &mut user_specs,
                &builtin_specs,
                &fallback_specs,
                &mut unused_nonterminals,
            );
        }

        let expr = specialize_nonterminals(
            expr,
            &mut grammar.arena,
            shell,
            &mut user_specs,
            &builtin_specs,
            &fallback_specs,
            &mut unused_nonterminals,
        );

        let unused_specializations = user_specs
            .iter()
            .filter(|(_, spec)| !spec.used)
            .map(|(name, spec)| (*name, spec.span))
            .collect();

        for nonterminal in
            get_nonterminals_resolution_order(&grammar.arena, &nonterminal_definitions)?
        {
            let e = nonterminal_definitions
                .get(&nonterminal)
                .unwrap()
                .rhs_expr_id;
            let new_e = resolve_nonterminals(
                &mut grammar.arena,
                e,
                &nonterminal_definitions,
                &mut unused_nonterminals,
            );
            nonterminal_definitions
                .get_mut(&nonterminal)
                .unwrap()
                .rhs_expr_id = new_e;
        }

        check_subword_spaces(&grammar.arena, expr, &nonterminal_definitions)?;

        let expr = resolve_nonterminals(
            &mut grammar.arena,
            expr,
            &nonterminal_definitions,
            &mut unused_nonterminals,
        );

        // Handle nested subwords originating in nonterminals expansion
        let expr = collapse_subwords(&mut grammar.arena, expr);

        let expr = propagate_fallback_levels(&mut grammar.arena, expr);

        // Whatever nonterminals remained in the expression tree after nonterminal expansion,
        // they're undefined.
        let undefined_nonterminals = get_nonterm_refs(&grammar.arena, expr);

        let g = ValidGrammar {
            arena: grammar.arena,
            command,
            expr,
            undefined_nonterminals,
            unused_nonterminals,
            unused_specializations,
        };
        Ok(g)
    }
}

fn expr_get_head(arena: &[Expr], expr_id: ExprId) -> ExprId {
    match &arena[expr_id] {
        Expr::Terminal { .. }
        | Expr::NontermRef { .. }
        | Expr::Command { .. }
        | Expr::Alternative { .. }
        | Expr::Fallback { .. }
        | Expr::Optional { .. }
        | Expr::Many1 { .. } => expr_id,
        Expr::Sequence { children, .. } => expr_get_head(arena, *children.first().unwrap()),
        Expr::Subword { root_id, .. } => expr_get_head(arena, *root_id),
        Expr::DistributiveDescription { .. } => {
            unreachable!("wrong compilation phases order")
        }
    }
}

fn expr_get_tail(arena: &[Expr], expr_id: ExprId) -> ExprId {
    match &arena[expr_id] {
        Expr::Terminal { .. }
        | Expr::NontermRef { .. }
        | Expr::Command { .. }
        | Expr::Alternative { .. }
        | Expr::Fallback { .. }
        | Expr::Optional { .. }
        | Expr::Many1 { .. } => expr_id,
        Expr::Sequence { children, .. } => expr_get_tail(arena, *children.last().unwrap()),
        Expr::Subword { root_id, .. } => expr_get_tail(arena, *root_id),
        Expr::DistributiveDescription { .. } => {
            unreachable!("wrong compilation phases order")
        }
    }
}

fn check_subword_spaces(
    arena: &[Expr],
    expr_id: ExprId,
    nonterms: &UstrMap<NontermDefn>,
) -> Result<()> {
    let mut nonterm_expn_trace: Vec<HumanSpan> = Default::default();
    do_check_subword_spaces(arena, expr_id, nonterms, &mut nonterm_expn_trace, false)
}

// Disallows spaces in subword expressions, e.g.
//
// aerc :<COMMAND>;
// <COMMAND> = quit -f;
//
// On deeply nested <NONTERM>s, this can lead to [surprising spaces removal](https://github.com/adaszko/complgen/issues/63) so forbid it completely.
fn do_check_subword_spaces(
    arena: &[Expr],
    expr_id: ExprId,
    nonterms: &UstrMap<NontermDefn>,
    nonterm_expn_trace: &mut Vec<HumanSpan>,
    within_subword: bool,
) -> Result<()> {
    match &arena[expr_id] {
        Expr::Sequence { children, .. } if within_subword => {
            for child in children {
                do_check_subword_spaces(
                    arena,
                    *child,
                    nonterms,
                    nonterm_expn_trace,
                    within_subword,
                )?;
            }
            // Error out on two adjacent Expr::Terminal()s
            for pair in children.windows(2) {
                let [left, right] = pair else { unreachable!() };
                let left_tail = expr_get_tail(arena, *left);
                let right_head = expr_get_head(arena, *right);
                if let (
                    Expr::Terminal {
                        span: left_span, ..
                    },
                    Expr::Terminal {
                        span: right_span, ..
                    },
                ) = (&arena[left_tail], &arena[right_head])
                {
                    return Err(Error::SubwordSpaces(
                        left_span.to_owned(),
                        right_span.to_owned(),
                        nonterm_expn_trace.clone().into_boxed_slice(),
                    ));
                }
            }
            Ok(())
        }
        Expr::Sequence { children, .. } => {
            for child in children {
                do_check_subword_spaces(
                    arena,
                    *child,
                    nonterms,
                    nonterm_expn_trace,
                    within_subword,
                )?;
            }
            Ok(())
        }
        Expr::Terminal { .. } => Ok(()),
        Expr::NontermRef { nonterm, span, .. } => {
            let Some(expn) = nonterms.get(nonterm) else {
                return Ok(());
            };
            nonterm_expn_trace.push(*span);
            do_check_subword_spaces(
                arena,
                expn.rhs_expr_id,
                nonterms,
                nonterm_expn_trace,
                within_subword,
            )?;
            nonterm_expn_trace.pop();
            Ok(())
        }
        Expr::Command { .. } => Ok(()),
        Expr::Subword { root_id: child, .. } => {
            do_check_subword_spaces(arena, *child, nonterms, nonterm_expn_trace, true)
        }
        Expr::Alternative { children, .. } => {
            for child in children {
                do_check_subword_spaces(
                    arena,
                    *child,
                    nonterms,
                    nonterm_expn_trace,
                    within_subword,
                )?;
            }
            Ok(())
        }
        Expr::Fallback { children, .. } => {
            for child in children {
                do_check_subword_spaces(
                    arena,
                    *child,
                    nonterms,
                    nonterm_expn_trace,
                    within_subword,
                )?;
            }
            Ok(())
        }
        Expr::Optional { child, .. } => {
            do_check_subword_spaces(arena, *child, nonterms, nonterm_expn_trace, within_subword)
        }
        Expr::Many1 { child, .. } => {
            do_check_subword_spaces(arena, *child, nonterms, nonterm_expn_trace, within_subword)
        }
        Expr::DistributiveDescription { .. } => {
            unreachable!("wrong compilation phases order")
        }
    }
}

fn specialize_nonterminals(
    expr_id: ExprId,
    expr_arena: &mut Vec<Expr>,
    shell: Shell,
    user_specs: &mut UstrMap<UserSpec>,
    builtin_specs: &UstrMap<BuiltinSpec>,
    fallback_specs: &UstrMap<(Ustr, HumanSpan)>,
    unused_nonterminals: &mut UstrMap<HumanSpan>,
) -> ExprId {
    match expr_arena[expr_id].clone() {
        Expr::Terminal { .. } | Expr::Command { .. } => expr_id,
        Expr::NontermRef {
            nonterm,
            fallback,
            span,
        } => {
            unused_nonterminals.remove(&nonterm);

            let (cmd, zsh_compadd) = if let Some(ref mut spec) = user_specs.get_mut(&nonterm) {
                spec.used = true;
                (spec.cmd, true)
            } else if let Some(BuiltinSpec { cmd }) = builtin_specs.get(&nonterm) {
                (*cmd, true)
            } else if let Some((cmd, _)) = fallback_specs.get(&nonterm) {
                (*cmd, false)
            } else {
                return expr_id;
            };

            let new_node = match shell {
                Shell::Bash => Expr::Command {
                    cmd,
                    zsh_compadd: false,
                    fallback,
                    span,
                },
                Shell::Fish => Expr::Command {
                    cmd,
                    zsh_compadd: false,
                    fallback,
                    span,
                },
                Shell::Zsh => Expr::Command {
                    cmd,
                    zsh_compadd,
                    fallback,
                    span,
                },
                Shell::Pwsh => Expr::Command {
                    cmd,
                    zsh_compadd: false,
                    fallback,
                    span,
                },
            };

            alloc(expr_arena, new_node)
        }
        Expr::Subword {
            root_id: child,
            fallback,
            span,
        } => {
            let new_child = specialize_nonterminals(
                child,
                expr_arena,
                shell,
                user_specs,
                builtin_specs,
                fallback_specs,
                unused_nonterminals,
            );
            if child == new_child {
                expr_id
            } else {
                alloc(
                    expr_arena,
                    Expr::Subword {
                        root_id: new_child,
                        fallback,
                        span,
                    },
                )
            }
        }
        Expr::Sequence { children, span } => {
            let new_children: Vec<ExprId> = children
                .iter()
                .map(|child| {
                    specialize_nonterminals(
                        *child,
                        expr_arena,
                        shell,
                        user_specs,
                        builtin_specs,
                        fallback_specs,
                        unused_nonterminals,
                    )
                })
                .collect();

            if children == new_children {
                expr_id
            } else {
                alloc(
                    expr_arena,
                    Expr::Sequence {
                        children: new_children,
                        span,
                    },
                )
            }
        }
        Expr::Alternative { children, span } => {
            let new_children: Vec<ExprId> = children
                .iter()
                .map(|child| {
                    specialize_nonterminals(
                        *child,
                        expr_arena,
                        shell,
                        user_specs,
                        builtin_specs,
                        fallback_specs,
                        unused_nonterminals,
                    )
                })
                .collect();

            if children == new_children {
                expr_id
            } else {
                alloc(
                    expr_arena,
                    Expr::Alternative {
                        children: new_children,
                        span,
                    },
                )
            }
        }
        Expr::Optional { child, span } => {
            let new_child = specialize_nonterminals(
                child,
                expr_arena,
                shell,
                user_specs,
                builtin_specs,
                fallback_specs,
                unused_nonterminals,
            );
            if child == new_child {
                expr_id
            } else {
                alloc(
                    expr_arena,
                    Expr::Optional {
                        child: new_child,
                        span,
                    },
                )
            }
        }
        Expr::Many1 { child, span } => {
            let new_child = specialize_nonterminals(
                child,
                expr_arena,
                shell,
                user_specs,
                builtin_specs,
                fallback_specs,
                unused_nonterminals,
            );
            if child == new_child {
                expr_id
            } else {
                alloc(
                    expr_arena,
                    Expr::Many1 {
                        child: new_child,
                        span,
                    },
                )
            }
        }
        Expr::DistributiveDescription { .. } => unreachable!(),
        Expr::Fallback { children, span } => {
            let new_children: Vec<ExprId> = children
                .iter()
                .map(|child| {
                    specialize_nonterminals(
                        *child,
                        expr_arena,
                        shell,
                        user_specs,
                        builtin_specs,
                        fallback_specs,
                        unused_nonterminals,
                    )
                })
                .collect();

            if children == new_children {
                expr_id
            } else {
                alloc(
                    expr_arena,
                    Expr::Fallback {
                        children: new_children,
                        span,
                    },
                )
            }
        }
    }
}

fn resolve_nonterminals(
    arena: &mut Vec<Expr>,
    expr_id: ExprId,
    vars: &UstrMap<NontermDefn>,
    unused_nonterminals: &mut UstrMap<HumanSpan>,
) -> ExprId {
    match arena[expr_id].clone() {
        Expr::Terminal { .. } | Expr::Command { .. } => expr_id,
        Expr::Subword {
            root_id: child,
            fallback,
            span,
        } => {
            let new_child = resolve_nonterminals(arena, child, vars, unused_nonterminals);
            if child == new_child {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Subword {
                        root_id: new_child,
                        fallback,
                        span,
                    },
                )
            }
        }
        Expr::NontermRef { nonterm: name, .. } => match vars.get(&name) {
            Some(replacement) => {
                unused_nonterminals.remove(&name);
                replacement.rhs_expr_id
            }
            None => expr_id,
        },
        Expr::Sequence { children, span } => {
            let new_children: Vec<ExprId> = children
                .iter()
                .map(|child| resolve_nonterminals(arena, *child, vars, unused_nonterminals))
                .collect();

            if children == new_children {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Sequence {
                        children: new_children,
                        span,
                    },
                )
            }
        }
        Expr::Alternative { children, span } => {
            let new_children: Vec<ExprId> = children
                .iter()
                .map(|child| resolve_nonterminals(arena, *child, vars, unused_nonterminals))
                .collect();

            if children == new_children {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Alternative {
                        children: new_children,
                        span,
                    },
                )
            }
        }
        Expr::Optional { child, span } => {
            let new_child = resolve_nonterminals(arena, child, vars, unused_nonterminals);
            if child == new_child {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Optional {
                        child: new_child,
                        span,
                    },
                )
            }
        }
        Expr::Many1 { child, span } => {
            let new_child = resolve_nonterminals(arena, child, vars, unused_nonterminals);
            if child == new_child {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Many1 {
                        child: new_child,
                        span,
                    },
                )
            }
        }
        Expr::DistributiveDescription { .. } => unreachable!(
            "Expr::DistributiveDescription should have been erased by the time nonterminals are being resolved"
        ),
        Expr::Fallback { children, span } => {
            let new_children: Vec<ExprId> = children
                .iter()
                .map(|child| resolve_nonterminals(arena, *child, vars, unused_nonterminals))
                .collect();

            if children == new_children {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Fallback {
                        children: new_children,
                        span,
                    },
                )
            }
        }
    }
}

fn do_get_nonterm_refs(arena: &[Expr], expr_id: ExprId, refs: &mut UstrMap<HumanSpan>) {
    match &arena[expr_id] {
        Expr::Terminal { .. } | Expr::Command { .. } => {}
        Expr::Subword {
            root_id: subexpr, ..
        } => {
            do_get_nonterm_refs(arena, *subexpr, refs);
        }
        Expr::NontermRef { nonterm, span, .. } => {
            refs.insert(*nonterm, *span);
        }
        Expr::Sequence { children, .. } => {
            for child in children {
                do_get_nonterm_refs(arena, *child, refs);
            }
        }
        Expr::Alternative { children, .. } => {
            for child in children {
                do_get_nonterm_refs(arena, *child, refs);
            }
        }
        Expr::Optional { child, .. } => {
            do_get_nonterm_refs(arena, *child, refs);
        }
        Expr::Many1 { child, .. } => {
            do_get_nonterm_refs(arena, *child, refs);
        }
        Expr::DistributiveDescription { .. } => (),
        Expr::Fallback { children, .. } => {
            for child in children {
                do_get_nonterm_refs(arena, *child, refs);
            }
        }
    }
}

fn get_nonterm_refs(arena: &[Expr], expr_id: ExprId) -> UstrMap<HumanSpan> {
    let mut result: UstrMap<HumanSpan> = Default::default();
    do_get_nonterm_refs(arena, expr_id, &mut result);
    result
}

fn get_not_depended_on_nonterminals(dependency_graph: &UstrMap<UstrMap<HumanSpan>>) -> UstrSet {
    let num_depending_nonterminals = {
        let mut num_depending_nonterminals: UstrMap<usize> =
            dependency_graph.keys().map(|vertex| (*vertex, 0)).collect();
        for (_, nonterminal_depependencies) in dependency_graph.iter() {
            for dep in nonterminal_depependencies.keys() {
                *num_depending_nonterminals.get_mut(dep).unwrap() += 1;
            }
        }
        num_depending_nonterminals
    };

    let vertices_without_incoming_edges: UstrSet = num_depending_nonterminals
        .into_iter()
        .filter(|(_, indegree)| *indegree == 0)
        .map(|(vertex, _)| vertex)
        .collect();

    vertices_without_incoming_edges
}

fn traverse_nonterminal_dependencies_dfs(
    vertex: Ustr,
    graph: &UstrMap<UstrMap<HumanSpan>>,
    path: &mut Vec<(Ustr, HumanSpan)>,
    visited: &mut UstrSet,
    result: &mut Vec<Ustr>,
) -> Result<()> {
    visited.insert(vertex);
    let dummy = UstrMap::default();
    for (child, span) in graph.get(&vertex).unwrap_or(&dummy) {
        if path.iter().any(|(chld, _)| chld == child) {
            path.push((vertex, *span));
            return Err(Error::NonterminalDefinitionsCycle(
                path.iter().map(|(_, span)| *span).collect(),
            ));
        }
        if visited.contains(child) {
            continue;
        }
        path.push((*child, *span));
        traverse_nonterminal_dependencies_dfs(*child, graph, path, visited, result)?;
        path.pop().unwrap();
        result.push(*child);
    }
    Ok(())
}

// A topological order but without the initial nonterminals that don't depend on any other
// nonterminals.
fn get_nonterminals_resolution_order(
    arena: &[Expr],
    nonterminal_definitions: &UstrMap<NontermDefn>,
) -> Result<Vec<Ustr>> {
    if nonterminal_definitions.is_empty() {
        return Ok(Vec::default());
    }

    let mut dependency_graph: UstrMap<UstrMap<HumanSpan>> = Default::default();
    for (varname, defn) in nonterminal_definitions {
        let mut refs = get_nonterm_refs(arena, defn.rhs_expr_id);
        refs.retain(|var, _| nonterminal_definitions.contains_key(var));
        dependency_graph.insert(*varname, refs);
    }

    let mut visited: UstrSet = Default::default();
    let mut result: Vec<Ustr> = Default::default();
    let mut path: Vec<(Ustr, HumanSpan)> = Default::default();

    let not_depended_on_vars = get_not_depended_on_nonterminals(&dependency_graph);
    if not_depended_on_vars.is_empty() {
        // Take any vertex and compute a sample cycle to illustrate to the user
        let any_vertex = dependency_graph.keys().next().unwrap();
        path.push((
            *any_vertex,
            nonterminal_definitions.get(any_vertex).unwrap().lhs_span,
        ));
        traverse_nonterminal_dependencies_dfs(
            *any_vertex,
            &dependency_graph,
            &mut path,
            &mut visited,
            &mut result,
        )?;
        unreachable!();
    }

    for vertex in not_depended_on_vars {
        debug_assert!(!visited.contains(&vertex));
        path.push((
            vertex,
            nonterminal_definitions.get(&vertex).unwrap().lhs_span,
        ));
        traverse_nonterminal_dependencies_dfs(
            vertex,
            &dependency_graph,
            &mut path,
            &mut visited,
            &mut result,
        )?;
        path.clear();
        result.push(vertex);
        debug_assert!(path.is_empty());
    }

    // Filter out nonterminals that don't depend on any other as they are already fully resolved.
    result.retain(|vertex| {
        dependency_graph
            .get(vertex)
            .map(|children| !children.is_empty())
            .unwrap_or(true)
    });

    Ok(result)
}

fn make_builtin_specializations(shell: Shell) -> UstrMap<BuiltinSpec> {
    let mut specializations: UstrMap<BuiltinSpec> = Default::default();

    let path_spec = match shell {
        Shell::Bash => BuiltinSpec {
            cmd: ustr(r#"compgen -A file -- "$1""#),
        },
        Shell::Fish => BuiltinSpec {
            cmd: ustr(r#"__fish_complete_path "$1""#),
        },
        Shell::Zsh => BuiltinSpec {
            cmd: ustr(r#"_path_files"#),
        },
        Shell::Pwsh => BuiltinSpec {
            // Preserve directory prefix: extract parent dir from prefix and join it back with filename
            cmd: ustr(r#"Get-ChildItem | ForEach-Object { $_.Name }"#),
        },
    };
    specializations.entry(ustr("PATH")).insert_entry(path_spec);

    let directory_spec = match shell {
        Shell::Bash => BuiltinSpec {
            cmd: ustr(r#"compgen -A directory -- "$1""#),
        },
        Shell::Fish => BuiltinSpec {
            cmd: ustr(r#"__fish_complete_directories "$1""#),
        },
        Shell::Zsh => BuiltinSpec {
            cmd: ustr(r#"_path_files -/"#),
        },
        Shell::Pwsh => BuiltinSpec {
            // Preserve directory prefix: extract parent dir from prefix and join it back with dirname
            cmd: ustr(r#"Get-ChildItem -Directory | ForEach-Object { $_.Name }"#),
        },
    };
    specializations
        .entry(ustr("DIRECTORY"))
        .insert_entry(directory_spec);

    specializations
}

#[cfg(test)]
pub mod tests {
    use crate::parse::{Span, Statement, expr};

    use super::*;
    use crate::parse::tests::teq;
    use Expr::*;

    #[test]
    fn nonterminal_resolution_order_detects_trivial_cycle() {
        let mut arena: Vec<Expr> = vec![];
        let foo_expr = Expr::nontermref("BAR");
        let foo_id = alloc(&mut arena, foo_expr);
        let bar_expr = Expr::nontermref("FOO");
        let bar_id = alloc(&mut arena, bar_expr);
        let nonterminal_definitions = UstrMap::from_iter([
            (
                ustr("FOO"),
                NontermDefn {
                    lhs_name: ustr("FOO"),
                    lhs_span: HumanSpan::default(),
                    shell: None,
                    rhs_expr_id: foo_id,
                },
            ),
            (
                ustr("BAR"),
                NontermDefn {
                    lhs_name: ustr("BAR"),
                    lhs_span: HumanSpan::default(),
                    shell: None,
                    rhs_expr_id: bar_id,
                },
            ),
        ]);
        assert!(matches!(
            get_nonterminals_resolution_order(&arena, &nonterminal_definitions),
            Err(Error::NonterminalDefinitionsCycle(_))
        ));
    }

    #[test]
    fn nonterminal_resolution_order_detects_simple_cycle() {
        let mut arena: Vec<Expr> = vec![];
        let foo_expr = Expr::nontermref("BAR");
        let foo_id = alloc(&mut arena, foo_expr);
        let bar_expr = Expr::nontermref("BAR");
        let bar_id = alloc(&mut arena, bar_expr);
        let nonterminal_definitions = UstrMap::from_iter([
            (
                ustr("FOO"),
                NontermDefn {
                    lhs_name: ustr("FOO"),
                    lhs_span: HumanSpan::default(),
                    shell: None,
                    rhs_expr_id: foo_id,
                },
            ),
            (
                ustr("BAR"),
                NontermDefn {
                    lhs_name: ustr("BAR"),
                    lhs_span: HumanSpan::default(),
                    shell: None,
                    rhs_expr_id: bar_id,
                },
            ),
        ]);
        assert!(matches!(
            &get_nonterminals_resolution_order(&arena, &nonterminal_definitions),
            Err(Error::NonterminalDefinitionsCycle(_))
        ));
    }

    #[test]
    fn computes_nonterminals_resolution_order() {
        let mut arena: Vec<Expr> = vec![];
        let always_id = alloc(&mut arena, Expr::term("always"));
        let never_id = alloc(&mut arena, Expr::term("never"));
        let auto_id = alloc(&mut arena, Expr::term("auto"));
        let when_id = alloc(
            &mut arena,
            Alternative {
                children: vec![always_id, never_id, auto_id],
                span: Default::default(),
            },
        );
        let foo_id = alloc(&mut arena, Expr::nontermref("WHEN"));
        let color_id = alloc(&mut arena, Expr::term("--color"));
        let option_foo_ref_id = alloc(&mut arena, Expr::nontermref("FOO"));
        let option_id = alloc(
            &mut arena,
            Sequence {
                children: vec![color_id, option_foo_ref_id],
                span: Default::default(),
            },
        );
        let nonterminal_definitions = UstrMap::from_iter([
            (
                ustr("WHEN"),
                NontermDefn {
                    lhs_name: ustr("WHEN"),
                    lhs_span: HumanSpan::default(),
                    shell: None,
                    rhs_expr_id: when_id,
                },
            ),
            (
                ustr("FOO"),
                NontermDefn {
                    lhs_name: ustr("FOO"),
                    lhs_span: HumanSpan::default(),
                    shell: None,
                    rhs_expr_id: foo_id,
                },
            ),
            (
                ustr("OPTION"),
                NontermDefn {
                    lhs_name: ustr("OPTION"),
                    lhs_span: HumanSpan::default(),
                    shell: None,
                    rhs_expr_id: option_id,
                },
            ),
        ]);
        assert_eq!(
            get_nonterminals_resolution_order(&arena, &nonterminal_definitions).unwrap(),
            vec![ustr("FOO"), ustr("OPTION")]
        );
    }

    #[test]
    fn detects_duplicated_nonterminals() {
        const INPUT: &str = r#"
grep [<OPTION>]... <PATTERNS> [<FILE>]...;
<OPTION> = --color <WHEN>;
<OPTION> = always | never | auto;
"#;
        let g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert!(matches!(
            ValidGrammar::from_grammar(g, Shell::Bash),
            Err(Error::DuplicateNonterminalDefinition(..))
        ));
    }

    #[test]
    fn parses_specialized_nonterminals() {
        use Statement::*;
        const INPUT: &str = r#"
ls <FILE>;
<FILE@bash> = {{{ compgen -A file "$1" }}};
<FILE@fish> = {{{ __fish_complete_path "$1" }}};
"#;
        let mut g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(g.statements.len(), 3);

        let (head, expr_id) = match &g.statements[0] {
            CallVariant {
                name: head,
                expr,
                name_span: _,
            } => (head, expr),
            _ => panic!("Expected CallVariant"),
        };
        assert_eq!(*head, ustr("ls"));
        let expected_expr1_id = alloc(&mut g.arena, Expr::nontermref("FILE"));
        assert!(teq(*expr_id, expected_expr1_id, &g.arena));

        let (symbol, shell, expr_id) = match &g.statements[1] {
            NonterminalDefinition(NontermDefn {
                lhs_name: symbol,
                shell,
                rhs_expr_id: expr,
                lhs_span: _,
            }) => (symbol, shell, expr),
            _ => panic!("Expected NonterminalDefinition"),
        };
        assert_eq!(*symbol, ustr("FILE"));
        assert_eq!(shell.unwrap().0, "bash");
        let expected_expr2_id = alloc(
            &mut g.arena,
            Command {
                cmd: ustr(r#"compgen -A file "$1""#),
                zsh_compadd: false,
                fallback: 0,
                span: HumanSpan::default(),
            },
        );
        assert!(teq(*expr_id, expected_expr2_id, &g.arena));

        let (symbol, shell, expr_id) = match &g.statements[2] {
            NonterminalDefinition(NontermDefn {
                lhs_name: symbol,
                shell,
                rhs_expr_id: expr,
                lhs_span: _,
            }) => (symbol, shell, expr),
            _ => panic!("Expected NonterminalDefinition"),
        };
        assert_eq!(*symbol, ustr("FILE"));
        assert_eq!(shell.unwrap().0, "fish");
        let expected_expr3_id = alloc(
            &mut g.arena,
            Command {
                cmd: ustr(r#"__fish_complete_path "$1""#),
                zsh_compadd: false,
                fallback: 0,
                span: HumanSpan::default(),
            },
        );
        assert!(teq(*expr_id, expected_expr3_id, &g.arena));

        assert!(ValidGrammar::from_grammar(g, Shell::Bash).is_ok());
    }

    #[test]
    fn distributes_descriptions() {
        const INPUT: &str = r#"mygrep (--color=<WHEN> | --color <WHEN>) "use markers to highlight the matching strings""#;
        let mut arena: Vec<Expr> = Default::default();
        let (s, e) = expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let distributed_id = distribute_descriptions(&mut arena, e);

        let mygrep_id = alloc(&mut arena, Expr::term("mygrep"));
        let color_eq_id = alloc(
            &mut arena,
            Expr::term_descr("--color=", "use markers to highlight the matching strings"),
        );
        let when_id1 = alloc(&mut arena, Expr::nontermref("WHEN"));
        let subword_seq_id = alloc(
            &mut arena,
            Sequence {
                children: vec![color_eq_id, when_id1],
                span: Default::default(),
            },
        );
        let subword_id = alloc(&mut arena, Expr::subword(subword_seq_id));

        let color_id = alloc(
            &mut arena,
            Expr::term_descr("--color", "use markers to highlight the matching strings"),
        );
        let when_id2 = alloc(&mut arena, Expr::nontermref("WHEN"));
        let seq2_id = alloc(
            &mut arena,
            Sequence {
                children: vec![color_id, when_id2],
                span: Default::default(),
            },
        );

        let alt_id = alloc(
            &mut arena,
            Alternative {
                children: vec![subword_id, seq2_id],
                span: Default::default(),
            },
        );
        let expected_expr_id = alloc(
            &mut arena,
            Sequence {
                children: vec![mygrep_id, alt_id],
                span: Default::default(),
            },
        );

        assert!(teq(distributed_id, expected_expr_id, &arena));
    }

    #[test]
    fn spends_distributed_description() {
        const INPUT: &str = r#"mygrep --help | (--color=<WHEN> | --color <WHEN>) "use markers to highlight the matching strings""#;
        let mut arena: Vec<Expr> = Default::default();
        let (s, e) = expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let distributed_id = distribute_descriptions(&mut arena, e);

        let mygrep_id = alloc(&mut arena, Expr::term("mygrep"));
        let help_id = alloc(&mut arena, Expr::term("--help"));
        let seq1_id = alloc(
            &mut arena,
            Sequence {
                children: vec![mygrep_id, help_id],
                span: Default::default(),
            },
        );

        let color_eq_id = alloc(
            &mut arena,
            Expr::term_descr("--color=", "use markers to highlight the matching strings"),
        );
        let when_id1 = alloc(&mut arena, Expr::nontermref("WHEN"));
        let subword_seq_id = alloc(
            &mut arena,
            Sequence {
                children: vec![color_eq_id, when_id1],
                span: Default::default(),
            },
        );
        let subword_id = alloc(&mut arena, Expr::subword(subword_seq_id));

        let color_id = alloc(
            &mut arena,
            Expr::term_descr("--color", "use markers to highlight the matching strings"),
        );
        let when_id2 = alloc(&mut arena, Expr::nontermref("WHEN"));
        let seq2_id = alloc(
            &mut arena,
            Sequence {
                children: vec![color_id, when_id2],
                span: Default::default(),
            },
        );

        let alt2_id = alloc(
            &mut arena,
            Alternative {
                children: vec![subword_id, seq2_id],
                span: Default::default(),
            },
        );

        let expected_expr_id = alloc(
            &mut arena,
            Alternative {
                children: vec![seq1_id, alt2_id],
                span: Default::default(),
            },
        );

        assert!(teq(distributed_id, expected_expr_id, &arena));
    }

    #[test]
    fn spends_distributed_description2() {
        const INPUT: &str = r#"mygrep (--help | (--color=<WHEN> | --color <WHEN>) "use markers to highlight the matching strings")"#;
        let mut arena: Vec<Expr> = Default::default();
        let (s, e) = expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let distributed_id = distribute_descriptions(&mut arena, e);

        let mygrep_id = alloc(&mut arena, Expr::term("mygrep"));
        let help_id = alloc(&mut arena, Expr::term("--help"));

        let color_eq_id = alloc(
            &mut arena,
            Expr::term_descr("--color=", "use markers to highlight the matching strings"),
        );
        let when_id1 = alloc(&mut arena, Expr::nontermref("WHEN"));
        let subword_seq_id = alloc(
            &mut arena,
            Sequence {
                children: vec![color_eq_id, when_id1],
                span: Default::default(),
            },
        );
        let subword_id = alloc(&mut arena, Expr::subword(subword_seq_id));

        let color_id = alloc(
            &mut arena,
            Expr::term_descr("--color", "use markers to highlight the matching strings"),
        );
        let when_id2 = alloc(&mut arena, Expr::nontermref("WHEN"));
        let seq2_id = alloc(
            &mut arena,
            Sequence {
                children: vec![color_id, when_id2],
                span: Default::default(),
            },
        );

        let alt2_id = alloc(
            &mut arena,
            Alternative {
                children: vec![subword_id, seq2_id],
                span: Default::default(),
            },
        );

        let alt1_id = alloc(
            &mut arena,
            Alternative {
                children: vec![help_id, alt2_id],
                span: Default::default(),
            },
        );

        let expected_expr_id = alloc(
            &mut arena,
            Sequence {
                children: vec![mygrep_id, alt1_id],
                span: Default::default(),
            },
        );

        assert!(teq(distributed_id, expected_expr_id, &arena));
    }

    #[test]
    fn issue_71() {
        const INPUT: &str = r#"
srun --acctg-freq=<FREQ>;
<FREQ> = <DATATYPE>=<INTERVAL>;
<DATATYPE> = task | energy | network | filesystem;
<INTERVAL> = <NUM>[s|m|h];
"#;
        let g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert!(matches!(ValidGrammar::from_grammar(g, Shell::Bash), Ok(_)));
    }
}

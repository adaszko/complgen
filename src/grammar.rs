use std::{rc::Rc, debug_assert};

use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_while1, escaped, take_till, take_while},
    character::{complete::{char, multispace1, one_of}, is_alphanumeric},
    multi::many0,
    IResult, combinator::fail, error::context,
};

use complgen::{Error, Result};
use ustr::{Ustr, ustr, UstrMap, UstrSet};

// Can't use an arena here until proptest supports non-owned types: https://github.com/proptest-rs/proptest/issues/9
#[derive(Clone, PartialEq)]
pub enum Expr {
    Terminal(Ustr), // e.g. an option: "--help", or a command: "build"
    Nonterminal(Ustr), // e.g. <FILE>, <PATH>, <DIR>, etc.
    Command(Ustr), // e.g. { ls }
    Sequence(Vec<Rc<Expr>>),
    Alternative(Vec<Rc<Expr>>),
    Optional(Rc<Expr>),
    Many1(Rc<Expr>),
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Terminal(arg0) => f.write_fmt(format_args!(r#"Rc::new(Terminal(ustr("{}")))"#, arg0)),
            Self::Nonterminal(arg0) => f.write_fmt(format_args!(r#"Rc::new(Nonterminal(ustr("{}")))"#, arg0)),
            Self::Command(arg0) => f.write_fmt(format_args!(r#"Rc::new(Command(ustr("{}")))"#, arg0)),
            Self::Sequence(arg0) => f.write_fmt(format_args!(r#"Rc::new(Sequence(vec!{:?}))"#, arg0)),
            Self::Alternative(arg0) => f.write_fmt(format_args!(r#"Rc::new(Alternative(vec!{:?}))"#, arg0)),
            Self::Optional(arg0) => f.write_fmt(format_args!(r#"Rc::new(Optional({:?}))"#, arg0)),
            Self::Many1(arg0) => f.write_fmt(format_args!(r#"Rc::new(Many1({:?}))"#, arg0)),
        }
    }
}

fn do_to_railroad_diagram(expr: Rc<Expr>) -> Box<dyn railroad::Node> {
    match expr.as_ref() {
        Expr::Terminal(s) => Box::new(railroad::Terminal::new(s.as_str().to_string())),
        Expr::Nonterminal(s) => Box::new(railroad::NonTerminal::new(s.as_str().to_string())),
        Expr::Command(s) => Box::new(railroad::Comment::new(s.as_str().to_string())),
        Expr::Sequence(subexprs) => {
            let subnodes: Vec<Box<dyn railroad::Node>> = subexprs.iter().map(|e| do_to_railroad_diagram(Rc::clone(e))).collect();
            Box::new(railroad::Sequence::new(subnodes))
        },
        Expr::Alternative(subexprs) => {
            let subnodes: Vec<Box<dyn railroad::Node>> = subexprs.iter().map(|e| do_to_railroad_diagram(Rc::clone(e))).collect();
            Box::new(railroad::Choice::new(subnodes))
        },
        Expr::Optional(subexpr) => Box::new(railroad::Optional::new(do_to_railroad_diagram(Rc::clone(subexpr)))),
        Expr::Many1(subexpr) => {
            let subnode = do_to_railroad_diagram(Rc::clone(subexpr));
            Box::new(railroad::Repeat::new(subnode, Box::new(railroad::Empty)))
        },
    }
}

pub fn to_railroad_diagram<W: std::io::Write>(expr: Rc<Expr>, output: &mut W) -> std::result::Result<(), std::io::Error> {
    let root = {
        let node = do_to_railroad_diagram(expr);
        let mut seq: railroad::Sequence<Box<dyn railroad::Node>> = Default::default();
        seq.push(Box::new(railroad::Start));
        seq.push(node);
        seq.push(Box::new(railroad::End));
        seq
    };
    let mut dia = railroad::Diagram::new(root);
    dia.add_element(railroad::svg::Element::new("style").set("type", "text/css").text(railroad::DEFAULT_CSS));
    dia.write(output)
}

pub fn to_railroad_diagram_file<P: AsRef<std::path::Path>>(
    expr: Rc<Expr>,
    path: P,
) -> std::result::Result<(), std::io::Error> {
    let mut file = std::fs::File::create(path)?;
    to_railroad_diagram(expr, &mut file)?;
    Ok(())
}

fn comment(input: &str) -> IResult<&str, &str> {
    let (input, _) = char('#')(input)?;
    let (input, content) = take_till(|c| c == '\n')(input)?;
    Ok((input, content))
}

fn blanks(input: &str) -> IResult<&str, ()> {
    let (input, _) = alt((multispace1, comment))(input)?;
    Ok((input, ()))
}

fn multiblanks0(mut input: &str) -> IResult<&str, ()> {
    while let Ok((rest, _)) = blanks(input) {
        input = rest;
    }
    Ok((input, ()))
}

fn multiblanks1(input: &str) -> IResult<&str, ()> {
    let (input, _) = blanks(input)?;
    let (input, _) = multiblanks0(input)?;
    Ok((input, ()))
}

fn terminal(input: &str) -> IResult<&str, &str> {
    fn is_terminal_char(c: char) -> bool {
        c.is_ascii() && (is_alphanumeric(c as u8) || c == '-' || c == '+' || c == '_')
    }
    let (input, term) = escaped(take_while1(is_terminal_char), '\\', one_of(r#"()[]<>.|;"#))(input)?;
    if term.len() == 0 {
        return fail(input);
    }
    Ok((input, term))
}

fn terminal_expr(input: &str) -> IResult<&str, Expr> {
    let (input, literal) = context("terminal", terminal)(input)?;
    Ok((input, Expr::Terminal(ustr(literal))))
}

fn nonterminal(input: &str) -> IResult<&str, &str> {
    let (input, _) = char('<')(input)?;
    let (input, name) = is_not(">")(input)?;
    let (input, _) = char('>')(input)?;
    Ok((input, name))
}

fn nonterminal_expr(input: &str) -> IResult<&str, Expr> {
    let (input, nonterm) = context("symbol", nonterminal)(input)?;
    Ok((input, Expr::Nonterminal(ustr(nonterm))))
}

fn command(input: &str) -> IResult<&str, &str> {
    fn is_command_char(c: char) -> bool {
        c != '}'
    }

    let (input, _) = char('{')(input)?;
    let (input, cmd) = escaped(take_while(is_command_char), '\\', one_of("{}"))(input)?;
    let (input, _) = char('}')(input)?;
    Ok((input, cmd.trim()))
}

fn command_expr(input: &str) -> IResult<&str, Expr> {
    let (input, cmd) = command(input)?;
    Ok((input, Expr::Command(ustr(cmd))))
}

fn optional_expr(input: &str) -> IResult<&str, Expr> {
    let (input, _) = char('[')(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, expr) = expr(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, Expr::Optional(Rc::new(expr))))
}

fn parenthesized_expr(input: &str) -> IResult<&str, Expr> {
    let (input, _) = char('(')(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, e) = expr(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, _) = char(')')(input)?;
    Ok((input, e))
}

fn one_or_more_tag(input: &str) -> IResult<&str, ()> {
    let (input, _) = multiblanks0(input)?;
    let (input, _) = tag("...")(input)?;
    Ok((input, ()))
}

fn expr_no_alternative_no_sequence(input: &str) -> IResult<&str, Expr> {
    let (input, e) = alt((
        nonterminal_expr,
        optional_expr,
        parenthesized_expr,
        command_expr,
        terminal_expr,
    ))(input)?;

    if let Ok((input, ())) = one_or_more_tag(input) {
        return Ok((input, Expr::Many1(Rc::new(e))));
    }

    Ok((input, e))
}

fn sequence_expr(input: &str) -> IResult<&str, Expr> {
    fn do_sequence_expr(input: &str) -> IResult<&str, Expr> {
        let (input, _) = multiblanks0(input)?;
        let (input, right) = sequence_expr(input)?;
        Ok((input, right))
    }

    let (mut input, left) = expr_no_alternative_no_sequence(input)?;
    let mut factors: Vec<Expr> = vec![left];
    loop {
        let Ok((pos, right)) = do_sequence_expr(input) else { break };
        factors.push(right);
        input = pos;
    }
    let result = if factors.len() == 1 {
        factors.drain(..).next().unwrap()
    } else {
        Expr::Sequence(factors.into_iter().map(Rc::new).collect())
    };
    Ok((input, result))
}

fn alternative_expr(input: &str) -> IResult<&str, Expr> {
    fn do_alternative_expr(input: &str) -> IResult<&str, Expr> {
        let (input, _) = multiblanks0(input)?;
        let (input, _) = char('|')(input)?;
        let (input, _) = multiblanks0(input)?;
        let (input, right) = sequence_expr(input)?;
        Ok((input, right))
    }

    let (mut input, left) = sequence_expr(input)?;
    let mut elems: Vec<Expr> = vec![left];
    loop {
        let Ok((pos, right)) = do_alternative_expr(input) else { break };
        elems.push(right);
        input = pos;
    }
    let result = if elems.len() == 1 {
        elems.drain(..).next().unwrap()
    } else {
        Expr::Alternative(elems.into_iter().map(Rc::new).collect())
    };
    Ok((input, result))
}

fn expr(input: &str) -> IResult<&str, Expr> {
    alternative_expr(input)
}


#[derive(Debug, Clone, PartialEq)]
enum Statement {
    CallVariant {
        head: Ustr,
        expr: Rc<Expr>,
    },
    NonterminalDefinition {
        symbol: Ustr,
        expr: Rc<Expr>,
    },
}


fn call_variant(input: &str) -> IResult<&str, Statement> {
    let (input, name) = terminal(input)?;
    let (input, _) = multiblanks1(input)?;
    let (input, expr) = expr(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, _) = char(';')(input)?;

    let production = Statement::CallVariant {
        head: ustr(name),
        expr: Rc::new(expr),
    };

    Ok((input, production))
}

fn nonterminal_definition(input: &str) -> IResult<&str, Statement> {
    let (input, symbol) = nonterminal(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, _) = tag("::=")(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, e) = expr(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, _) = char(';')(input)?;

    let stmt = Statement::NonterminalDefinition {
        symbol: ustr(symbol),
        expr: Rc::new(e),
    };

    Ok((input, stmt))
}

fn statement(input: &str) -> IResult<&str, Statement> {
    let (input, stmt) = alt((call_variant, nonterminal_definition))(input)?;
    let (input, _) = multiblanks0(input)?;
    Ok((input, stmt))
}

fn grammar(input: &str) -> IResult<&str, Vec<Statement>> {
    let (input, _) = multiblanks0(input)?;
    let (input, statements) = many0(statement)(input)?;
    let (input, _) = multiblanks0(input)?;
    Ok((input, statements))
}


#[derive(Debug, PartialEq, Clone)]
pub struct Grammar {
    statements: Vec<Statement>,
}


#[derive(Debug, PartialEq, Clone)]
pub struct Validated {
    pub command: Ustr,
    pub expr: Rc<Expr>,
}


fn resolve_nonterminals(expr: Rc<Expr>, vars: &UstrMap<Rc<Expr>>) -> Rc<Expr> {
    match expr.as_ref() {
        Expr::Terminal(_) => Rc::clone(&expr),
        Expr::Nonterminal(name) => {
            match vars.get(&name) {
                Some(replacement) => {
                    Rc::clone(&replacement)
                },
                None => {
                    Rc::clone(&expr)
                },
            }
        },
        Expr::Command(_) => Rc::clone(&expr),
        Expr::Sequence(children) => {
            let mut new_children: Vec<Rc<Expr>> = Default::default();
            let mut any_child_replaced = false;
            for child in children {
                let new_child = resolve_nonterminals(Rc::clone(child), vars);
                if !Rc::ptr_eq(&child, &new_child) {
                    any_child_replaced = true;
                }
                new_children.push(new_child);
            }
            if any_child_replaced {
                Rc::new(Expr::Sequence(new_children))
            } else {
                Rc::clone(&expr)
            }
        },
        Expr::Alternative(children) => {
            let mut new_children: Vec<Rc<Expr>> = Default::default();
            let mut any_child_replaced = false;
            for child in children {
                let new_child = resolve_nonterminals(Rc::clone(child), vars);
                if !Rc::ptr_eq(&child, &new_child) {
                    any_child_replaced = true;
                }
                new_children.push(new_child);
            }
            if any_child_replaced {
                Rc::new(Expr::Alternative(new_children))
            } else {
                Rc::clone(&expr)
            }
        },
        Expr::Optional(child) => {
            let new_child = resolve_nonterminals(Rc::clone(child), vars);
            if Rc::ptr_eq(&child, &new_child) {
                Rc::clone(&expr)
            }
            else {
                Rc::new(Expr::Optional(new_child))
            }
        },
        Expr::Many1(child) => {
            let new_child = resolve_nonterminals(Rc::clone(child), vars);
            if Rc::ptr_eq(&child, &new_child) {
                Rc::clone(&expr)
            }
            else {
                Rc::new(Expr::Many1(new_child))
            }
        },
    }
}


fn do_get_expression_nonterminals(expr: Rc<Expr>, deps: &mut UstrSet) {
    match expr.as_ref() {
        Expr::Terminal(_) => {},
        Expr::Nonterminal(varname) => {
            deps.insert(*varname);
        },
        Expr::Command(_) => {},
        Expr::Sequence(children) => {
            for child in children {
                do_get_expression_nonterminals(Rc::clone(&child), deps);
            }
        },
        Expr::Alternative(children) => {
            for child in children {
                do_get_expression_nonterminals(Rc::clone(&child), deps);
            }
        },
        Expr::Optional(child) => { do_get_expression_nonterminals(Rc::clone(&child), deps); }
        Expr::Many1(child) => { do_get_expression_nonterminals(Rc::clone(&child), deps); }
    }
}


fn get_expression_nonterminals(expr: Rc<Expr>) -> UstrSet {
    let mut result: UstrSet = Default::default();
    do_get_expression_nonterminals(expr, &mut result);
    result
}


pub fn get_not_depended_on_nonterminals(dependency_graph: &UstrMap<UstrSet>) -> UstrSet {
    let num_depending_nonterminals = {
        let mut num_depending_nonterminals: UstrMap<usize> = dependency_graph.keys().map(|vertex| (*vertex, 0)).collect();
        for (_, nonterminal_depependencies) in dependency_graph.iter() {
            for dep in nonterminal_depependencies {
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


fn traverse_nonterminal_dependencies_dfs(vertex: Ustr, graph: &UstrMap<UstrSet>, path: &mut Vec<Ustr>, visited: &mut UstrSet, result: &mut Vec<Ustr>) -> Result<()> {
    visited.insert(vertex);
    let dummy = UstrSet::default();
    for child in graph.get(&vertex).unwrap_or_else(|| &dummy) {
        if path.contains(child) {
            let mut path = path.clone();
            path.push(vertex);
            return Err(Error::NonterminalDefinitionsCycle(Some(path)));
        }
        if visited.contains(child) {
            continue;
        }
        path.push(*child);
        traverse_nonterminal_dependencies_dfs(*child, graph, path, visited, result)?;
        path.pop().unwrap();
        result.push(*child);
    }
    Ok(())
}


// A topological order but without the initial nonterminals that don't depend on any other
// nonterminals.
fn get_nonterminals_resolution_order(variable_definitions: &UstrMap<Rc<Expr>>) -> Result<Vec<Ustr>> {
    if variable_definitions.is_empty() {
        return Ok(Vec::default());
    }

    let mut dependency_graph: UstrMap<UstrSet> = Default::default();
    for (varname, expr) in variable_definitions {
        let mut vars = get_expression_nonterminals(Rc::clone(&expr));
        vars.retain(|var| variable_definitions.contains_key(var));
        dependency_graph.insert(*varname, vars);
    }

    let not_depended_on_vars = get_not_depended_on_nonterminals(&dependency_graph);
    if not_depended_on_vars.is_empty() {
        return Err(Error::NonterminalDefinitionsCycle(None));
    }

    let mut visited: UstrSet = Default::default();
    let mut result: Vec<Ustr> = Default::default();
    let mut path: Vec<Ustr> = Default::default();
    for vertex in not_depended_on_vars {
        debug_assert!(!visited.contains(&vertex));
        traverse_nonterminal_dependencies_dfs(vertex, &dependency_graph, &mut path, &mut visited, &mut result)?;
        result.push(vertex);
        debug_assert!(path.is_empty());
    }

    // Filter out nonterminals that don't depend on any other as they are already fully resolved.
    result.retain(|vertex| dependency_graph.get(vertex).map(|children| !children.is_empty()).unwrap_or(true));

    log::debug!("nonterminals expansion order: {:?}", result);
    Ok(result)
}


impl Grammar {
    pub fn validate(&self) -> Result<Validated> {
        let command = {
            let mut commands: Vec<Ustr> = self.statements.iter().filter_map(|v|
                match v {
                    Statement::CallVariant { head: lhs, .. } => Some(*lhs),
                    Statement::NonterminalDefinition { .. } => None,
                }
            ).collect();

            if commands.is_empty() {
                return Err(Error::EmptyGrammar);
            }

            commands.sort_unstable();
            commands.dedup();

            if commands.len() > 1 {
                return Err(Error::VaryingCommandNames(
                    commands.into_iter().collect(),
                ));
            }
            commands[0]
        };

        let expr = {
            let call_variants: Vec<Rc<Expr>> = self.statements.iter().filter_map(|v|
                match v {
                    Statement::CallVariant { expr: rhs, .. } => Some(rhs.clone()),
                    Statement::NonterminalDefinition { .. } => None,
                }
            ).collect();

            if call_variants.len() == 1 {
                Rc::clone(&call_variants[0])
            }
            else {
                Rc::new(Expr::Alternative(call_variants))
            }
        };

        let mut variable_definitions: UstrMap<Rc<Expr>> = self.statements.iter().filter_map(|v|
            match v {
                Statement::CallVariant { .. } => None,
                Statement::NonterminalDefinition { symbol, expr: rhs } => Some((*symbol, Rc::clone(&rhs))),
            }
        ).collect();

        for variable in get_nonterminals_resolution_order(&variable_definitions)? {
            let e = Rc::clone(variable_definitions.get(&variable).unwrap());
            *variable_definitions.get_mut(&variable).unwrap() = resolve_nonterminals(e, &variable_definitions);
        }
        let expr = resolve_nonterminals(expr, &variable_definitions);

        let g = Validated {
            command,
            expr,
        };
        Ok(g)

    }
}


pub fn parse(input: &str) -> Result<Grammar> {
    let (input, statements) = match grammar(input) {
        Ok((input, statements)) => (input, statements),
        Err(e) => return Err(Error::ParsingError(e.to_string())),
    };

    if !input.is_empty() {
        return Err(Error::TrailingInput(input.to_owned()));
    }

    let g = Grammar {
        statements,
    };

    Ok(g)
}


#[cfg(test)]
pub mod tests {
    use std::{rc::Rc, ops::Rem};
    use proptest::{strategy::BoxedStrategy, test_runner::TestRng};
    use proptest::prelude::*;
    use ustr::ustr as u;
    use Expr::*;

    use super::*;

    fn arb_literal(inputs: Rc<Vec<Ustr>>) -> BoxedStrategy<Rc<Expr>> {
        (0..inputs.len()).prop_map(move |index| Rc::new(Terminal(ustr(&inputs[index])))).boxed()
    }

    fn arb_variable(nonterminals: Rc<Vec<Ustr>>) -> BoxedStrategy<Rc<Expr>> {
        (0..nonterminals.len()).prop_map(move |index| Rc::new(Nonterminal(ustr(&nonterminals[index])))).boxed()
    }

    fn arb_optional(inputs: Rc<Vec<Ustr>>, nonterminals: Rc<Vec<Ustr>>, remaining_depth: usize, max_width: usize) -> BoxedStrategy<Rc<Expr>> {
        arb_expr(inputs, nonterminals, remaining_depth-1, max_width).prop_map(|e| Rc::new(Optional(e))).boxed()
    }

    fn arb_many1(inputs: Rc<Vec<Ustr>>, nonterminals: Rc<Vec<Ustr>>, remaining_depth: usize, max_width: usize) -> BoxedStrategy<Rc<Expr>> {
        arb_expr(inputs, nonterminals, remaining_depth-1, max_width).prop_map(|e| Rc::new(Many1(e))).boxed()
    }

    fn arb_sequence(inputs: Rc<Vec<Ustr>>, nonterminals: Rc<Vec<Ustr>>, remaining_depth: usize, max_width: usize) -> BoxedStrategy<Rc<Expr>> {
        (2..max_width).prop_flat_map(move |width| {
            let e = arb_expr(inputs.clone(), nonterminals.clone(), remaining_depth-1, max_width);
            prop::collection::vec(e, width).prop_map(|v| Rc::new(Sequence(v)))
        }).boxed()
    }

    fn arb_alternative(inputs: Rc<Vec<Ustr>>, nonterminals: Rc<Vec<Ustr>>, remaining_depth: usize, max_width: usize) -> BoxedStrategy<Rc<Expr>> {
        (2..max_width).prop_flat_map(move |width| {
            let e = arb_expr(inputs.clone(), nonterminals.clone(), remaining_depth-1, max_width);
            prop::collection::vec(e, width).prop_map(|v| Rc::new(Alternative(v)))
        }).boxed()
    }

    pub fn arb_expr(inputs: Rc<Vec<Ustr>>, nonterminals: Rc<Vec<Ustr>>, remaining_depth: usize, max_width: usize) -> BoxedStrategy<Rc<Expr>> {
        if remaining_depth <= 1 {
            prop_oneof![
                arb_literal(Rc::clone(&inputs)),
                arb_variable(nonterminals),
            ].boxed()
        }
        else {
            prop_oneof![
                arb_literal(inputs.clone()),
                arb_variable(nonterminals.clone()),
                arb_optional(inputs.clone(), nonterminals.clone(), remaining_depth, max_width),
                arb_many1(inputs.clone(), nonterminals.clone(), remaining_depth, max_width),
                arb_sequence(inputs.clone(), nonterminals.clone(), remaining_depth, max_width),
                arb_alternative(inputs, nonterminals, remaining_depth, max_width),
            ].boxed()
        }
    }

    pub fn do_arb_match(e: Rc<Expr>, rng: &mut TestRng, max_width: usize, output: &mut Vec<Ustr>) {
        match e.as_ref() {
            Terminal(s) => output.push(*s),
            Nonterminal(_) => output.push(ustr("anything")),
            Command(_) => output.push(ustr("anything")),
            Sequence(v) => {
                for subexpr in v {
                    do_arb_match(Rc::clone(&subexpr), rng, max_width, output);
                }
            },
            Alternative(v) => {
                let chosen_alternative = usize::try_from(rng.next_u64().rem(u64::try_from(v.len()).unwrap())).unwrap();
                do_arb_match(Rc::clone(&v[chosen_alternative]), rng, max_width, output);
            },
            Optional(subexpr) => {
                if rng.next_u64() % 2 == 0 {
                    do_arb_match(Rc::clone(&subexpr), rng, max_width, output);
                }
            },
            Many1(subexpr) => {
                let n = rng.next_u64();
                let chosen_len = n % u64::try_from(max_width).unwrap() + 1;
                for _ in 0..chosen_len {
                    do_arb_match(Rc::clone(&subexpr), rng, max_width, output);
                }
            },
        }
    }

    pub fn arb_match(e: Rc<Expr>, mut rng: TestRng, max_width: usize) -> (Rc<Expr>, Vec<Ustr>) {
        let mut output: Vec<Ustr> = Default::default();
        do_arb_match(Rc::clone(&e), &mut rng, max_width, &mut output);
        (e, output)
    }

    // Produce an arbitrary sequence matching `e`.
    pub fn arb_expr_match(inputs: Rc<Vec<Ustr>>, nonterminals: Rc<Vec<Ustr>>, remaining_depth: usize, max_width: usize) -> BoxedStrategy<(Rc<Expr>, Vec<Ustr>)> {
        arb_expr(inputs, nonterminals, remaining_depth, max_width).prop_perturb(move |e, rng| arb_match(e, rng, max_width)).boxed()
    }


    #[test]
    fn parses_word_terminal() {
        const INPUT: &str = r#"foo"#;
        let ("", e) = terminal_expr(INPUT).unwrap() else { panic!("parsing error"); };
        assert_eq!(e, Terminal(u("foo")));
    }

    #[test]
    fn parses_short_option_terminal() {
        const INPUT: &str = r#"-f"#;
        let ("", e) = terminal_expr(INPUT).unwrap() else { panic!("parsing error"); };
        assert_eq!(e, Terminal(u("-f")));
    }

    #[test]
    fn parses_long_option_terminal() {
        const INPUT: &str = r#"--foo"#;
        let ("", e) = terminal_expr(INPUT).unwrap() else { panic!("parsing error"); };
        assert_eq!(e, Terminal(u("--foo")));
    }

    #[test]
    fn parses_symbol() {
        const INPUT: &str = "<FILE>";
        let ("", e) = nonterminal_expr(INPUT).unwrap() else { panic!("parsing error"); };
        assert_eq!(e, Nonterminal(u("FILE")));
    }

    #[test]
    fn parses_command() {
        const INPUT: &str = "{ rustup toolchain list | cut -d' ' -f1 }";
        let ("", e) = command_expr(INPUT).unwrap() else { panic!("parsing error"); };
        assert_eq!(e, Command(u(" rustup toolchain list | cut -d' ' -f1 ")));
    }

    #[test]
    fn parses_optional_expr() {
        const INPUT: &str = "[<foo>]";
        let ("", e) = expr(INPUT).unwrap() else { panic!("parsing error"); };
        assert_eq!(e, Optional(Rc::new(Nonterminal(u("foo")))));
    }

    #[test]
    fn parses_one_or_more_expr() {
        const INPUT: &str = "<foo>...";
        let ("", e) = expr(INPUT).unwrap() else { panic!("parsing error"); };
        assert_eq!(e, Many1(Rc::new(Nonterminal(u("foo")))));
    }

    #[test]
    fn parses_sequence_expr() {
        const INPUT: &str = "<first-symbol> <second symbol>";
        let ("", e) = expr(INPUT).unwrap() else { panic!("parsing error"); };
        assert_eq!(
            e,
            Sequence(vec![
                Rc::new(Nonterminal(u("first-symbol"))),
                Rc::new(Nonterminal(u("second symbol"))),
            ])
        );
    }

    #[test]
    fn parses_alternative_expr() {
        const INPUT: &str = "a b | c";
        let ("", e) = expr(INPUT).unwrap() else { panic!("parsing error"); };
        assert_eq!(
            e,
            Alternative(vec![
                Rc::new(Sequence(vec![Rc::new(Terminal(u("a"))), Rc::new(Terminal(u("b")))])),
                Rc::new(Terminal(u("c")))
            ])
        );
    }

    #[test]
    fn parses_parenthesised_expr() {
        const INPUT: &str = r#"a (b | c)"#;
        let ("", e) = expr(INPUT).unwrap() else { panic!("parsing error"); };
        assert_eq!(
            e,
            Sequence(vec![
                Rc::new(Terminal(u("a"))),
                Rc::new(Alternative(vec![Rc::new(Terminal(u("b"))), Rc::new(Terminal(u("c")))])),
            ])
        );
    }

    #[test]
    fn parses_variant() {
        const INPUT: &str = r#"foo bar;"#;
        let ("", v) = call_variant(INPUT).unwrap() else { panic!("parsing error"); };
        assert_eq!(
            v,
            Statement::CallVariant {
                head: u("foo"),
                expr: Rc::new(Terminal(u("bar")))
            }
        );
    }

    #[test]
    fn parses_grammar() {
        const INPUT: &str = r#"
foo bar;
foo baz;
"#;
        let g = parse(INPUT).unwrap();
        assert_eq!(
            g,
            Grammar {
                statements: vec![
                    Statement::CallVariant { head: u("foo"), expr: Rc::new(Terminal(u("bar"))) },
                    Statement::CallVariant { head: u("foo"), expr: Rc::new(Terminal(u("baz"))) }
                ],
            }
        );
    }

    #[test]
    fn bug1() {
        // Did not consider whitespace before ...
        const INPUT: &str = "darcs help ( ( -v | --verbose ) | ( -q | --quiet ) ) ... [<DARCS_COMMAND> [DARCS_SUBCOMMAND]]  ;";
        let g = parse(INPUT).unwrap();
        assert_eq!(
            g,
            Grammar {
                statements: vec![
                    Statement::CallVariant { head: u("darcs"), expr: Rc::new(Sequence(vec![
                    Rc::new(Terminal(u("help"))),
                    Rc::new(Sequence(vec![
                        Rc::new(Many1(Rc::new(Alternative(vec![
                            Rc::new(Alternative(vec![Rc::new(Terminal(u("-v"))), Rc::new(Terminal(u("--verbose")))])),
                            Rc::new(Alternative(vec![Rc::new(Terminal(u("-q"))), Rc::new(Terminal(u("--quiet")))])),
                        ],)),)),
                        Rc::new(Optional(Rc::new(Sequence(vec![
                            Rc::new(Nonterminal(u("DARCS_COMMAND"))),
                            Rc::new(Optional(Rc::new(Terminal(u("DARCS_SUBCOMMAND"))))),
                        ])))),
                    ])),
                ])) },
                ],
            }
        );
    }

    #[test]
    fn parses_darcs_grammar() {
        // Source: https://github.com/mbrubeck/compleat/blob/56dd9761cdbb07de674947b129192cd8043cda8a/examples/darcs.usage
        const INPUT: &str = r#"
darcs help ( ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... [<DARCS_COMMAND> [DARCS_SUBCOMMAND]]  ;
darcs add ( --boring | ( --case-ok | --reserved-ok ) | ( ( -r | --recursive ) | --not-recursive ) | ( --date-trick | --no-date-trick ) | --repodir <DIRECTORY> | --dry-run | --umask <UMASK> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... ( <FILE> | <DIRECTORY> )...;
darcs remove ( --repodir <DIRECTORY> | --umask <UMASK> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... ( <FILE> | <DIRECTORY> )...;
darcs move ( ( --case-ok | --reserved-ok ) | --repodir <DIRECTORY> | --umask <UMASK> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... <SOURCE> ... <DESTINATION>;
darcs replace ( --token-chars <"[CHARS]"> | ( ( -f | --force ) | --no-force ) | --repodir <DIRECTORY> | --ignore-times | --umask <UMASK> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... <OLD> <NEW> <FILE> ...;
darcs revert ( ( ( -a | --all ) | ( -i | --interactive ) ) | --repodir <DIRECTORY> | --ignore-times | --umask <UMASK> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... [ ( <FILE> | <DIRECTORY> ) ]...;
darcs unrevert ( --ignore-times | ( ( -a | --all ) | ( -i | --interactive ) ) | --repodir <DIRECTORY> | --umask <UMASK> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... ;
darcs whatsnew ( ( ( -s | --summary ) | --no-summary ) | ( -u | --unified ) | ( ( -l | --look-for-adds ) | --dont-look-for-adds ) | --repodir <DIRECTORY> | --ignore-times | --boring | --no-cache | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... [ ( <FILE> | <DIRECTORY> ) ]...;
darcs record ( ( -m <PATCHNAME> | --patch-name <PATCHNAME> ) | ( -A <EMAIL> | --author <EMAIL> ) | ( --no-test | --test ) | ( --leave-test-directory | --remove-test-directory ) | ( ( -a | --all ) | --pipe | ( -i | --interactive ) ) | ( --ask-deps | --no-ask-deps ) | ( --edit-long-comment | --skip-long-comment | --prompt-long-comment ) | ( ( -l | --look-for-adds ) | --dont-look-for-adds ) | --repodir <DIRECTORY> | --logfile <FILE> | --delete-logfile | ( --compress | --dont-compress ) | --ignore-times | --umask <UMASK> | ( --set-scripts-executable | --dont-set-scripts-executable ) | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... [ ( <FILE> | <DIRECTORY> ) ]...;
darcs unrecord ( ( --from-match <PATTERN> | --from-patch <REGEXP> | --from-tag <REGEXP> | --last <NUMBER> | --matches <PATTERN> | ( -p <REGEXP> | --patches <REGEXP> ) | ( -t <REGEXP> | --tags <REGEXP> ) ) | ( --no-deps | --dont-prompt-for-dependencies | --prompt-for-dependencies ) | ( ( -a | --all ) | ( -i | --interactive ) ) | --repodir <DIRECTORY> | ( --compress | --dont-compress ) | --umask <UMASK> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... ;
darcs amend-record ( ( --match <PATTERN> | ( -p <REGEXP> | --patch <REGEXP> ) | ( -n <N> | --index <N> ) ) | ( --no-test | --test ) | ( --leave-test-directory | --remove-test-directory ) | ( ( -a | --all ) | ( -i | --interactive ) ) | ( -A <EMAIL> | --author <EMAIL> ) | ( -m <PATCHNAME> | --patch-name <PATCHNAME> ) | ( --edit-long-comment | --skip-long-comment | --prompt-long-comment ) | ( ( -l | --look-for-adds ) | --dont-look-for-adds ) | --repodir <DIRECTORY> | ( --compress | --dont-compress ) | --ignore-times | --umask <UMASK> | ( --set-scripts-executable | --dont-set-scripts-executable ) | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... [ ( <FILE> | <DIRECTORY> ) ]...;
darcs mark-conflicts ( --ignore-times | --repodir <DIRECTORY> | --umask <UMASK> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... ;
darcs tag ( ( -m <PATCHNAME> | --patch-name <PATCHNAME> ) | ( -A <EMAIL> | --author <EMAIL> ) | ( --pipe | ( -i | --interactive ) ) | ( --edit-long-comment | --skip-long-comment | --prompt-long-comment ) | --repodir <DIRECTORY> | ( --compress | --dont-compress ) | --umask <UMASK> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... [TAGNAME];
darcs setpref ( --repodir <DIRECTORY> | --umask <UMASK> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... <PREF> <VALUE>;
darcs diff ( ( --to-match <PATTERN> | --to-patch <REGEXP> | --to-tag <REGEXP> | --from-match <PATTERN> | --from-patch <REGEXP> | --from-tag <REGEXP> | --match <PATTERN> | ( -p <REGEXP> | --patch <REGEXP> ) | --last <NUMBER> | ( -n <N-M> | --index <N-M> ) ) | --diff-command <COMMAND> | --diff-opts <OPTIONS> | ( -u | --unified ) | --repodir <DIRECTORY> | --store-in-memory | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... [ ( <FILE> | <DIRECTORY> ) ]...;
darcs changes ( ( --to-match <PATTERN> | --to-patch <REGEXP> | --to-tag <REGEXP> | --from-match <PATTERN> | --from-patch <REGEXP> | --from-tag <REGEXP> | --last <NUMBER> | ( -n <N-M> | --index <N-M> ) | --matches <PATTERN> | ( -p <REGEXP> | --patches <REGEXP> ) | ( -t <REGEXP> | --tags <REGEXP> ) ) | --max-count <NUMBER> | --only-to-files | ( --context | --xml-output | --human-readable | --number | --count ) | ( ( -s | --summary ) | --no-summary ) | --reverse | --repo <URL> | --repodir <DIRECTORY> | ( ( -a | --all ) | ( -i | --interactive ) ) | ( --ssh-cm | --no-ssh-cm ) | ( --http-pipelining | --no-http-pipelining ) | --no-cache | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... [ ( <FILE> | <DIRECTORY> ) ]...;
darcs annotate ( ( ( -s | --summary ) | --no-summary ) | ( -u | --unified ) | --human-readable | --xml-output | ( --match <PATTERN> | ( -p <REGEXP> | --patch <REGEXP> ) | ( -t <REGEXP> | --tag <REGEXP> ) | ( -n <N> | --index <N> ) ) | --creator-hash <HASH> | --repodir <DIRECTORY> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... [ ( <FILE> | <DIRECTORY> ) ]...;
darcs dist ( ( -d <DISTNAME> | --dist-name <DISTNAME> ) | --repodir <DIRECTORY> | ( --match <PATTERN> | ( -p <REGEXP> | --patch <REGEXP> ) | ( -t <REGEXP> | --tag <REGEXP> ) | ( -n <N> | --index <N> ) ) | --store-in-memory | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... ;
darcs trackdown ( --repodir <DIRECTORY> | ( --set-scripts-executable | --dont-set-scripts-executable ) | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... [[INITIALIZATION] COMMAND];
darcs show ( contents ( ( --match <PATTERN> | ( -p <REGEXP> | --patch <REGEXP> ) | ( -t <REGEXP> | --tag <REGEXP> ) | ( -n <N> | --index <N> ) ) | --repodir <DIRECTORY> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... [FILE]... | files ( ( --files | --no-files ) | ( --directories | --no-directories ) | ( --pending | --no-pending ) | ( -0 | --null ) | --repodir <DIRECTORY> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ...  | index ( ( --files | --no-files ) | ( --directories | --no-directories ) | ( -0 | --null ) | --repodir <DIRECTORY> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ...  | pristine ( ( --files | --no-files ) | ( --directories | --no-directories ) | ( -0 | --null ) | --repodir <DIRECTORY> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ...  | repo ( --repodir <DIRECTORY> | ( --files | --no-files ) | --xml-output | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ...  | authors ( --repodir <DIRECTORY> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ...  | tags ( --repodir <DIRECTORY> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ...  );
darcs pull ( ( --matches <PATTERN> | ( -p <REGEXP> | --patches <REGEXP> ) | ( -t <REGEXP> | --tags <REGEXP> ) ) | ( ( -a | --all ) | ( -i | --interactive ) ) | ( --mark-conflicts | --allow-conflicts | --dont-allow-conflicts | --skip-conflicts ) | --external-merge <COMMAND> | ( --test | --no-test ) | --dry-run | --xml-output | ( ( -s | --summary ) | --no-summary ) | ( --no-deps | --dont-prompt-for-dependencies | --prompt-for-dependencies ) | ( --set-default | --no-set-default ) | --repodir <DIRECTORY> | --ignore-unrelated-repos | ( --intersection | --union | --complement ) | ( --compress | --dont-compress ) | --nolinks | --ignore-times | --remote-repo <URL> | ( --set-scripts-executable | --dont-set-scripts-executable ) | --umask <UMASK> | ( --restrict-paths | --dont-restrict-paths ) | ( --ssh-cm | --no-ssh-cm ) | ( --http-pipelining | --no-http-pipelining ) | --no-cache | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... [REPOSITORY]...;
darcs obliterate ( ( --from-match <PATTERN> | --from-patch <REGEXP> | --from-tag <REGEXP> | --last <NUMBER> | --matches <PATTERN> | ( -p <REGEXP> | --patches <REGEXP> ) | ( -t <REGEXP> | --tags <REGEXP> ) ) | ( --no-deps | --dont-prompt-for-dependencies | --prompt-for-dependencies ) | ( ( -a | --all ) | ( -i | --interactive ) ) | --repodir <DIRECTORY> | ( ( -s | --summary ) | --no-summary ) | ( --compress | --dont-compress ) | --ignore-times | --umask <UMASK> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... ;
darcs rollback ( ( --from-match <PATTERN> | --from-patch <REGEXP> | --from-tag <REGEXP> | --last <NUMBER> | --matches <PATTERN> | ( -p <REGEXP> | --patches <REGEXP> ) | ( -t <REGEXP> | --tags <REGEXP> ) ) | ( ( -a | --all ) | ( -i | --interactive ) ) | ( -A <EMAIL> | --author <EMAIL> ) | ( -m <PATCHNAME> | --patch-name <PATCHNAME> ) | ( --edit-long-comment | --skip-long-comment | --prompt-long-comment ) | ( --no-test | --test ) | ( --leave-test-directory | --remove-test-directory ) | --repodir <DIRECTORY> | ( --compress | --dont-compress ) | --umask <UMASK> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... [ ( <FILE> | <DIRECTORY> ) ]...;
darcs push ( ( --matches <PATTERN> | ( -p <REGEXP> | --patches <REGEXP> ) | ( -t <REGEXP> | --tags <REGEXP> ) ) | ( --no-deps | --dont-prompt-for-dependencies | --prompt-for-dependencies ) | ( ( -a | --all ) | ( -i | --interactive ) ) | ( --sign | --sign-as <KEYID> | --sign-ssl <IDFILE> | --dont-sign ) | --dry-run | --xml-output | ( ( -s | --summary ) | --no-summary ) | --repodir <DIRECTORY> | ( --set-default | --no-set-default ) | --ignore-unrelated-repos | ( --apply-as <USERNAME> | --apply-as-myself ) | --nolinks | --remote-repo <URL> | ( --ssh-cm | --no-ssh-cm ) | ( --http-pipelining | --no-http-pipelining ) | --no-cache | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... [REPOSITORY];
darcs send ( ( --matches <PATTERN> | ( -p <REGEXP> | --patches <REGEXP> ) | ( -t <REGEXP> | --tags <REGEXP> ) ) | ( --no-deps | --dont-prompt-for-dependencies | --prompt-for-dependencies ) | ( ( -a | --all ) | ( -i | --interactive ) ) | --from <EMAIL> | ( -A <EMAIL> | --author <EMAIL> ) | --to <EMAIL> | --cc <EMAIL> | --subject <SUBJECT> | --in-reply-to <EMAIL> | ( -o <FILE> | --output <FILE> ) | ( -O [<DIRECTORY>] | --output-auto-name [<DIRECTORY>] ) | ( --sign | --sign-as <KEYID> | --sign-ssl <IDFILE> | --dont-sign ) | --dry-run | --xml-output | ( ( -s | --summary ) | --no-summary ) | ( --edit-description | --dont-edit-description ) | ( --set-default | --no-set-default ) | --repodir <DIRECTORY> | --sendmail-command <COMMAND> | --ignore-unrelated-repos | --logfile <FILE> | --delete-logfile | --remote-repo <URL> | --context <FILENAME> | ( --ssh-cm | --no-ssh-cm ) | ( --http-pipelining | --no-http-pipelining ) | --no-cache | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... [REPOSITORY];
darcs apply ( ( --verify <PUBRING> | --verify-ssl <KEYS> | --no-verify ) | ( ( -a | --all ) | ( -i | --interactive ) ) | --dry-run | --xml-output | ( --mark-conflicts | --allow-conflicts | --no-resolve-conflicts | --dont-allow-conflicts | --skip-conflicts ) | --external-merge <COMMAND> | ( --no-test | --test ) | ( --leave-test-directory | --remove-test-directory ) | --repodir <DIRECTORY> | --reply <FROM> | --cc <EMAIL> | --happy-forwarding | --sendmail-command <COMMAND> | --ignore-times | ( --compress | --dont-compress ) | ( --set-scripts-executable | --dont-set-scripts-executable ) | --umask <UMASK> | ( --restrict-paths | --dont-restrict-paths ) | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... <PATCHFILE>;
darcs get ( ( --repo-name <DIRECTORY> | --repodir <DIRECTORY> ) | ( --partial | --lazy | --ephemeral | --complete ) | ( --to-match <PATTERN> | --to-patch <REGEXP> | ( -t <REGEXP> | --tag <REGEXP> ) | --context <FILENAME> ) | ( --set-default | --no-set-default ) | ( --set-scripts-executable | --dont-set-scripts-executable ) | --nolinks | ( --hashed | --old-fashioned-inventory ) | ( --ssh-cm | --no-ssh-cm ) | ( --http-pipelining | --no-http-pipelining ) | --no-cache | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... <REPOSITORY> [<DIRECTORY>];
darcs put ( ( --to-match <PATTERN> | --to-patch <REGEXP> | ( -t <REGEXP> | --tag <REGEXP> ) | --context <FILENAME> ) | ( --set-scripts-executable | --dont-set-scripts-executable ) | ( --hashed | --old-fashioned-inventory ) | ( --set-default | --no-set-default ) | --repodir <DIRECTORY> | ( --apply-as <USERNAME> | --apply-as-myself ) | ( --ssh-cm | --no-ssh-cm ) | ( --http-pipelining | --no-http-pipelining ) | --no-cache | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... <NEW_REPOSITORY>;
darcs initialize ( ( --hashed | --darcs-2 | --old-fashioned-inventory ) | --repodir <DIRECTORY> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... ;
darcs optimize ( --repodir <DIRECTORY> | --reorder-patches | --sibling <URL> | --relink | --relink-pristine | --upgrade | --pristine | ( --compress | --dont-compress | --uncompress ) | --umask <UMASK> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... ;
darcs check ( ( --complete | --partial ) | ( --no-test | --test ) | ( --leave-test-directory | --remove-test-directory ) | --repodir <DIRECTORY> | --ignore-times | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... ;
darcs repair ( --repodir <DIRECTORY> | --umask <UMASK> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... ;
darcs convert ( ( --repo-name <DIRECTORY> | --repodir <DIRECTORY> ) | ( --set-scripts-executable | --dont-set-scripts-executable ) | ( --ssh-cm | --no-ssh-cm ) | ( --http-pipelining | --no-http-pipelining ) | --no-cache | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... <SOURCE> [<DESTINATION>];
"#;
        let _ = parse(INPUT).unwrap();
    }

    #[test]
    fn parses_variable_definition() {
        const INPUT: &str = r#"
grep [<OPTION>]... <PATTERNS> [<FILE>]...;
<OPTION> ::= --color <WHEN>;
<WHEN> ::= always | never | auto;
"#;
        let g = parse(INPUT).unwrap();
        assert_eq!(
            g,
            Grammar {
                statements: vec![
                    Statement::CallVariant { head: u("grep"), expr: Rc::new(Sequence(vec![Rc::new(Many1(Rc::new(Optional(Rc::new(Nonterminal(ustr("OPTION"))))))), Rc::new(Sequence(vec![Rc::new(Nonterminal(ustr("PATTERNS"))), Rc::new(Many1(Rc::new(Optional(Rc::new(Nonterminal(ustr("FILE")))))))]))])) },
                    Statement::NonterminalDefinition { symbol: u("OPTION"), expr: Rc::new(Sequence(vec![Rc::new(Terminal(ustr("--color"))), Rc::new(Nonterminal(ustr("WHEN")))])) },
                    Statement::NonterminalDefinition { symbol: u("WHEN"), expr: Rc::new(Alternative(vec![Rc::new(Terminal(ustr("always"))), Rc::new(Terminal(ustr("never"))), Rc::new(Terminal(ustr("auto")))])) },
                ],
            }
        );
    }

    #[test]
    fn skips_comment() {
        const INPUT: &str = r#"#foo"#;
        assert_eq!(comment(INPUT).unwrap(), ("", "foo"));
    }

    #[test]
    fn parses_comments() {
        const INPUT: &str = r#"
# sample comment
<OPTION> ::= --extended-regexp                      # "PATTERNS are extended regular expressions"
           | --fixed-strings                        # "PATTERNS are strings"
           | --basic-regexp                         # "PATTERNS are basic regular expressions"
           | --perl-regexp                          # "PATTERNS are Perl regular expressions"
# another comment
           ;
"#;
        let g = parse(INPUT).unwrap();
        assert_eq!(
            g,
            Grammar {
                statements: vec![
                    Statement::NonterminalDefinition { symbol: u("OPTION"), expr: Rc::new(Alternative(vec![
                        Rc::new(Terminal(u("--extended-regexp"))),
                        Rc::new(Terminal(u("--fixed-strings"))),
                        Rc::new(Terminal(u("--basic-regexp"))),
                        Rc::new(Terminal(u("--perl-regexp"))),
                    ]))},
                ],
            }
        );
    }

    #[test]
    fn variable_resolution_order_detects_trivial_cycle() {
        let variable_definitions = UstrMap::from_iter([
            (u("FOO"), Rc::new(Nonterminal(u("BAR")))),
            (u("BAR"), Rc::new(Nonterminal(u("FOO")))),
        ]);
        assert!(matches!(get_nonterminals_resolution_order(&variable_definitions), Err(Error::NonterminalDefinitionsCycle(None))));
    }

    #[test]
    fn variable_resolution_order_detects_simple_cycle() {
        let variable_definitions = UstrMap::from_iter([
            (u("FOO"), Rc::new(Nonterminal(u("BAR")))),
            (u("BAR"), Rc::new(Nonterminal(u("BAR")))),
        ]);
        assert!(matches!(&get_nonterminals_resolution_order(&variable_definitions), Err(Error::NonterminalDefinitionsCycle(Some(path))) if path == &[u("BAR"), u("BAR")]));
    }

    #[test]
    fn computes_nonterminals_resolution_order() {
        let variable_definitions = UstrMap::from_iter([
            (u("WHEN"), Rc::new(Alternative(vec![Rc::new(Terminal(u("always"))), Rc::new(Terminal(u("never"))), Rc::new(Terminal(u("auto")))]))),
            (u("FOO"), Rc::new(Nonterminal(u("WHEN")))),
            (u("OPTION"), Rc::new(Sequence(vec![Rc::new(Terminal(u("--color"))), Rc::new(Nonterminal(u("FOO")))]))),
        ]);
        assert_eq!(get_nonterminals_resolution_order(&variable_definitions).unwrap(), vec![u("FOO"), u("OPTION")]);
    }

    #[test]
    fn parses_inline_shell_command() {
        const INPUT: &str = r#"
cargo [+{ rustup toolchain list | cut -d' ' -f1 }] [<OPTIONS>] [<COMMAND>];
"#;
        let g = parse(INPUT).unwrap();
        assert_eq!(
            g,
            Grammar {
                statements: vec![
                    Statement::CallVariant {
                        head: u("cargo"),
                        expr: Rc::new(Sequence(vec![Rc::new(Optional(Rc::new(Sequence(vec![Rc::new(Terminal(ustr("+"))), Rc::new(Command(u(" rustup toolchain list | cut -d' ' -f1 ")))])))), Rc::new(Sequence(vec![Rc::new(Optional(Rc::new(Nonterminal(ustr("OPTIONS"))))), Rc::new(Optional(Rc::new(Nonterminal(ustr("COMMAND")))))]))])),
                    },
                ],
            }
        );
    }

    #[test]
    fn parses_shell_command_variable_definition() {
        const INPUT: &str = r#"
cargo [+<toolchain>] [<OPTIONS>] [<COMMAND>];
<toolchain> ::= { rustup toolchain list | cut -d' ' -f1 };
"#;
        let g = parse(INPUT).unwrap();
        assert_eq!(
            g,
            Grammar {
                statements: vec![
                    Statement::CallVariant {
                        head: u("cargo"),
                        expr: Rc::new(Sequence(vec![Rc::new(Optional(Rc::new(Sequence(vec![Rc::new(Terminal(ustr("+"))), Rc::new(Nonterminal(ustr("toolchain")))])))), Rc::new(Sequence(vec![Rc::new(Optional(Rc::new(Nonterminal(ustr("OPTIONS"))))), Rc::new(Optional(Rc::new(Nonterminal(ustr("COMMAND")))))]))])),
                    },
                    Statement::NonterminalDefinition {
                        symbol: u("toolchain"),
                        expr: Rc::new(Command(u(" rustup toolchain list | cut -d' ' -f1 "))),
                    },
                ],
            }
        );
    }
}

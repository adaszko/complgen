use std::io::Write;

use slice_group_by::GroupBy;
use ustr::{ustr, Ustr};

use crate::{
    aot::fish::{
        make_id_from_command_map, make_string_constant, validate_command_name,
        write_generic_subword_fn, write_match_fn, write_subword_fn, MATCH_FN_NAME,
    },
    dfa::DFA,
    grammar::Specialization,
    regex::Input,
};

use super::{
    get_subword_transitions, get_transitions, make_external_command_fn_name, make_subword_fn_name,
};

pub fn write_fish_completion_shell_code<W: Write>(
    completed_command: &str,
    dfa: &DFA,
    words_before_cursor: &[&str],
    entered_prefix: &str,
    output: &mut W,
    test_mode: bool,
) -> anyhow::Result<()> {
    validate_command_name(completed_command)?;

    let prefix_constant = make_string_constant(entered_prefix);

    let mut transitions = get_transitions(&dfa, &words_before_cursor);
    transitions.sort_unstable_by_key(|input| input.get_fallback_level());

    let id_from_cmd = make_id_from_command_map(dfa);
    for (cmd, id) in &id_from_cmd {
        write!(
            output,
            r#"function {}
    set 1 $argv[1]
    {cmd}
end

"#,
            make_external_command_fn_name(completed_command, *id)
        )?;
    }

    let id_from_dfa = get_subword_transitions(&transitions);

    if !id_from_dfa.is_empty() {
        write_generic_subword_fn(output, completed_command)?;
        writeln!(output)?;
    }
    for (dfa, id) in &id_from_dfa {
        write_subword_fn(output, completed_command, *id, dfa.as_ref(), &id_from_cmd)?;
        writeln!(output)?;
    }

    write_match_fn(output)?;
    writeln!(output)?;

    // Generate shell code for fallback levels in order, optionally calling shell functions defined
    // above.
    writeln!(
        output,
        r#"function __complgen_jit
    set candidates
"#
    )?;

    let groups: Vec<&[Input]> = transitions
        .linear_group_by_key(|t| t.get_fallback_level())
        .collect();
    for group in groups {
        let mut literals: Vec<Ustr> = Default::default();
        let mut literals_with_description: Vec<(Ustr, String)> = Default::default();
        for (literal, description) in group.iter().filter_map(|t| match t {
            Input::Literal(literal, description, ..) => Some((
                literal,
                description.unwrap_or(ustr("")).as_str().to_string(),
            )),
            _ => None,
        }) {
            if !description.is_empty() {
                literals_with_description.push((*literal, description));
            } else {
                literals.push(*literal);
            }
        }

        if !literals.is_empty() {
            writeln!(
                output,
                r#"    set --append candidates {}"#,
                itertools::join(literals.iter().map(|c| make_string_constant(c)), " ")
            )?;
        }
        for (literal, description) in literals_with_description {
            writeln!(
                output,
                r#"    set --append candidates "{literal}	{description}""#
            )?;
        }

        // An external command -- execute it and collect stdout lines as candidates
        for cmd in group.iter().filter_map(|t| match t {
            Input::Command(cmd, ..) => Some(*cmd),
            Input::Nonterminal(
                _,
                Some(Specialization {
                    fish: None,
                    generic: Some(cmd),
                    ..
                }),
                ..,
            ) => Some(*cmd),
            _ => None,
        }) {
            let command_id = id_from_cmd.get(&cmd).unwrap();
            let fn_name = make_external_command_fn_name(completed_command, *command_id);
            writeln!(
                output,
                r#"    set --append candidates ({fn_name} {prefix_constant})"#,
            )?;
        }

        for subdfa in group.iter().filter_map(|t| match t {
            Input::Subword(subdfa, ..) => Some(subdfa),
            _ => None,
        }) {
            let subdfa_id = id_from_dfa.get(subdfa).unwrap();
            writeln!(
                output,
                r#"    set --append candidates ({} complete {prefix_constant})"#,
                make_subword_fn_name(completed_command, *subdfa_id)
            )?;
        }
        for cmd in group.iter().filter_map(|t| match t {
            Input::Nonterminal(
                _,
                Some(Specialization {
                    fish: Some(cmd), ..
                }),
                ..,
            ) => Some(*cmd),
            _ => None,
        }) {
            let command_id = id_from_cmd.get(&cmd).unwrap();
            let fn_name = make_external_command_fn_name(completed_command, *command_id);
            writeln!(
                output,
                r#"    set --append candidates ({fn_name} {prefix_constant})"#,
            )?;
        }

        writeln!(
            output,
            r#"    printf '%s\n' $candidates | {MATCH_FN_NAME} {prefix_constant} && return 0"#
        )?;
    }
    writeln!(output, r#"end"#)?;

    if test_mode {
        return Ok(());
    }

    // Call the generated shell function.
    writeln!(output)?;
    writeln!(output, r#"__complgen_jit"#)?;

    Ok(())
}

use std::io::Write;

use slice_group_by::GroupBy;
use ustr::Ustr;

use crate::{
    aot::bash::{
        make_id_from_command_map, make_string_constant, write_generic_subword_fn, write_match_fn,
        write_subword_fn, MATCH_FN_NAME,
    },
    dfa::DFA,
    grammar::Specialization,
    regex::Input,
};

use super::{
    get_subword_transitions, get_transitions, make_external_command_fn_name, make_subword_fn_name,
};

pub fn write_bash_completion_shell_code<W: Write>(
    completed_command: &str,
    dfa: &DFA,
    words_before_cursor: &[&str],
    entered_prefix: &str,
    superfluous_prefix: &str,
    output: &mut W,
    test_mode: bool,
) -> anyhow::Result<()> {
    let prefix_constant = make_string_constant(entered_prefix);

    let mut transitions = get_transitions(&dfa, &words_before_cursor);
    transitions.sort_unstable_by_key(|input| input.get_fallback_level());

    write_match_fn(output)?;

    let id_from_cmd = make_id_from_command_map(dfa);

    for (cmd, id) in &id_from_cmd {
        write!(
            output,
            r#"{} () {{
    {cmd}
}}
"#,
            make_external_command_fn_name(completed_command, *id)
        )?;
    }

    let id_from_dfa = get_subword_transitions(&transitions);
    if !id_from_dfa.is_empty() {
        write_generic_subword_fn(output, completed_command)?;
    }
    for (dfa, id) in &id_from_dfa {
        write_subword_fn(output, completed_command, *id, dfa.as_ref(), &id_from_cmd)?;
        writeln!(output)?;
    }

    // Generate shell code for fallback levels in order, optionally calling shell functions defined
    // above.
    writeln!(
        output,
        r#"__complgen_jit () {{
    local -a completions=()
    local -a matches=()"#
    )?;

    if !entered_prefix.is_empty() {
        writeln!(
            output,
            r#"    local ignore_case=$(bind -v | grep completion-ignore-case | cut -d' ' -f3)"#
        )?;
    }

    let groups: Vec<&[Input]> = transitions
        .linear_group_by_key(|t| t.get_fallback_level())
        .collect();
    for group in groups {
        let literals: Vec<&Ustr> = group
            .iter()
            .filter_map(|t| match t {
                Input::Literal(literal, ..) => Some(literal),
                _ => None,
            })
            .collect();
        if !literals.is_empty() {
            writeln!(
                output,
                r#"    completions+=({})"#,
                itertools::join(literals.iter().map(|s| make_string_constant(s)), " ")
            )?;
        }

        // An external command -- execute it and collect stdout lines as completions
        for cmd in group.iter().filter_map(|t| match t {
            Input::Command(cmd, ..) => Some(*cmd),
            Input::Nonterminal(
                _,
                Some(Specialization {
                    bash: None,
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
                r#"    readarray -t -O ${{#completions[@]}} completions < <({fn_name} {prefix_constant})"#
            )?
        }

        for subdfa in group.iter().filter_map(|t| match t {
            Input::Subword(subdfa, ..) => Some(subdfa),
            _ => None,
        }) {
            let subdfa_id = id_from_dfa.get(subdfa).unwrap();
            let fn_name = make_subword_fn_name(completed_command, *subdfa_id);
            writeln!(
                output,
                r#"    readarray -t -O ${{#completions[@]}} completions < <({fn_name} complete {prefix_constant})"#
            )?
        }
        for cmd in group.iter().filter_map(|t| match t {
            Input::Nonterminal(
                _,
                Some(Specialization {
                    bash: Some(cmd), ..
                }),
                ..,
            ) => Some(*cmd),
            _ => None,
        }) {
            let command_id = id_from_cmd.get(&cmd).unwrap();
            let fn_name = make_external_command_fn_name(completed_command, *command_id);
            writeln!(
                output,
                r#"    readarray -t -O ${{#completions[@]}} completions < <({fn_name} {prefix_constant})"#
            )?
        }

        if entered_prefix.is_empty() {
            writeln!(output, r#"    matches=("${{completions[@]}}")"#)?;
        } else {
            // Filter `completions` by `entered_prefix`, respecting `ignore_case`
            writeln!(
                output,
                r#"    readarray -t matches < <(printf "%s\n" "${{completions[@]}}" | {MATCH_FN_NAME} "$ignore_case" {prefix_constant})"#
            )?;
        }

        // Strip `superfluous_prefix` bashism
        if superfluous_prefix.is_empty() {
            writeln!(
                output,
                r#"    if [[ ${{#matches[@]}} -gt 0 ]]; then
        COMPREPLY=("${{matches[@]}}")
        return
    fi"#
            )?;
        } else {
            writeln!(
                output,
                r#"    if [[ ${{#matches[@]}} -gt 0 ]]; then
        matches=("${{matches[@]#{superfluous_prefix}}}")
        COMPREPLY=("${{matches[@]}}")
        return
    fi"#
            )?;
        }
    }
    writeln!(output, r#"}}"#)?;

    if test_mode {
        return Ok(());
    }

    // Call the generated shell function.
    writeln!(output, r#"__complgen_jit"#)?;
    Ok(())
}

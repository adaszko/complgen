use std::io::Write;

use crate::LiteralId;
use crate::Result;
use crate::dfa::DFA;
use hashbrown::HashMap;
use indexmap::IndexSet;
use itertools::Itertools;
use ustr::Ustr;

// PowerShell array indexes start at 0 (like Bash).
// PowerShell uses dynamic scoping.

pub const ARRAY_START: u32 = 0;

fn make_string_constant(s: &str) -> String {
    // PowerShell string escaping: backtick is the escape character
    // Need to escape: backtick (`), double-quote ("), dollar sign ($), newline, carriage return
    format!(
        r#""{}""#,
        s.replace('`', "``")
            .replace('"', "`\"")
            .replace('$', "`$")
            .replace('\n', "`n")
            .replace('\r', "`r")
    )
}

fn write_matching_tables<W: Write>(
    buffer: &mut W,
    dfa: &DFA,
    id_from_cmd: &IndexSet<Ustr>,
    needs_commands_code: bool,
) -> Result<HashMap<(Ustr, Ustr), u32>> {
    let all_literals = dfa.get_all_literals(ARRAY_START as usize);

    let id_from_literal_description: HashMap<(Ustr, Ustr), LiteralId> = all_literals
        .iter()
        .map(|(id, input, description)| ((*input, *description), *id))
        .collect();

    let literals = all_literals
        .iter()
        .map(|(_, literal, _)| make_string_constant(literal))
        .join(", ");
    writeln!(buffer, r#"    $literals = @({literals})"#)?;

    let descriptions = all_literals
        .iter()
        .filter(|(_, _, desc)| !desc.is_empty())
        .map(|(id, _, desc)| format!("{} = {}", id, make_string_constant(desc)))
        .join("; ");
    writeln!(buffer, r#"    $descriptions = @{{{descriptions}}}"#)?;

    let all_states = dfa.get_all_states();

    writeln!(buffer, r#"    $literal_transitions = @{{}}"#)?;
    for (state, state_transitions) in
        dfa.get_literal_transitions(&all_states, &id_from_literal_description)
    {
        let transitions = state_transitions
            .iter()
            .map(|(literal_id, to)| format!("{} = {}", literal_id, to))
            .join("; ");
        writeln!(
            buffer,
            r#"    $literal_transitions[{state}] = @{{{transitions}}}"#
        )?;
    }

    if needs_commands_code {
        writeln!(buffer, r#"    $command_transitions = @{{}}"#)?;
        for (state, state_transitions) in dfa.get_command_transitions(&all_states, id_from_cmd) {
            let transitions = state_transitions
                .iter()
                .map(|(literal_id, to)| format!("{} = {}", literal_id, to))
                .join("; ");
            writeln!(
                buffer,
                r#"    $command_transitions[{state}] = @{{{transitions}}}"#
            )?;
        }
    }

    let star_transitions = dfa
        .iter_top_level_star_transitions()
        .map(|(from, to)| format!("{from} = {to}"))
        .join("; ");
    writeln!(buffer, r#"    $star_transitions = @{{{star_transitions}}}"#)?;

    Ok(id_from_literal_description)
}

fn write_completion_tables<W: Write>(
    buffer: &mut W,
    dfa: &DFA,
    id_from_cmd: &IndexSet<Ustr>,
    needs_commands_code: bool,
    id_from_literal_description: &HashMap<(Ustr, Ustr), LiteralId>,
    max_fallback_level: usize,
) -> Result<()> {
    for (level, transitions) in dfa
        .get_completion_literals(&id_from_literal_description, max_fallback_level)
        .iter()
        .enumerate()
    {
        let initializer = transitions
            .iter()
            .map(|(from_state, literal_ids)| {
                format!(
                    "{from_state} = @({})",
                    literal_ids.iter().map(|id| id.to_string()).join(", ")
                )
            })
            .join("; ");
        writeln!(
            buffer,
            r#"    $literal_transitions_level_{level} = @{{{initializer}}}"#
        )?;
    }

    if needs_commands_code {
        for (level, transitions) in dfa
            .get_completion_commands(id_from_cmd, max_fallback_level)
            .iter()
            .enumerate()
        {
            let initializer = transitions
                .iter()
                .map(|(from_state, cmd_ids)| {
                    format!(
                        "{from_state} = @({})",
                        cmd_ids.iter().map(|id| id.to_string()).join(", ")
                    )
                })
                .join("; ");
            writeln!(
                buffer,
                r#"    $commands_level_{level} = @{{{initializer}}}"#
            )?;
        }
    }

    Ok(())
}

fn write_subword_fn<W: Write>(
    buffer: &mut W,
    command: &str,
    needs_commands_code: bool,
) -> Result<()> {
    writeln!(
        buffer,
        r#"function _{command}_subword {{
    param([string]$mode, [string]$word)

    $char_index = 0
    $matched = $false
    :outer while ($true) {{
        if ($char_index -ge $word.Length) {{
            $matched = $true
            break
        }}

        $subword = $word.Substring($char_index)

        if ($literal_transitions.ContainsKey($state)) {{
            $state_transitions = $literal_transitions[$state]

            for ($literal_id = 0; $literal_id -lt $literals.Count; $literal_id++) {{
                $literal = $literals[$literal_id]
                if ($subword -eq $literal -And $state_transitions.ContainsKey($literal_id)) {{
                    $state = $state_transitions[$literal_id]
                    $char_index += $literal.Length
                    continue outer
                }}
                if ($literal.StartsWith($subword)) {{
                    break outer
                }}
                if ($subword.StartsWith($literal) -And $state_transitions.ContainsKey($literal_id)) {{
                    $state = $state_transitions[$literal_id]
                    $char_index += $literal.Length
                    continue outer
                }}
            }}
        }}"#
    )?;

    if needs_commands_code {
        write!(
            buffer,
            r#"
        if ($command_transitions.ContainsKey($state)) {{
            $state_transitions = $command_transitions[$state]

            foreach ($cmd_id in $state_transitions.Keys) {{
                $output = & "_{command}_cmd_$cmd_id"
                $decreasing_length = $output | Sort-Object -Property {{ $_.Length }} -Descending
                foreach ($line in $decreasing_length) {{
                    if ([string]::IsNullOrWhiteSpace($line)) {{ continue }}
                    $parts = $line -split "`t", 2
                    $candidate = $parts[0]
                    $desc = if ($parts.Count -gt 1) {{ $parts[1] }} else {{ $candidate }}

                    if ($candidate -eq $subword) {{
                        $char_index += $candidate.Length
                        $state = $state_transitions[$cmd_id]
                        continue outer
                    }}

                    if ($candidate.StartsWith($subword)) {{
                        break outer
                    }}

                    if ($subword.StartsWith($candidate)) {{
                        $char_index += $candidate.Length
                        $state = $state_transitions[$cmd_id]
                        continue outer
                    }}
                }}
            }}
        }}"#
        )?;
    }

    write!(
        buffer,
        r#"
        if ($star_transitions.ContainsKey($state)) {{
            $matched = $true
            break
        }}

        break
    }}

    if ($mode -eq 'matches') {{
        return $matched
    }}

    # Complete mode: return possible completions
    $matched_prefix = $word.Substring(0, $char_index)
    $completed_prefix = $word.Substring($char_index)
    $completions = @()

    for ($fallback_level = 0; $fallback_level -le $max_fallback_level; $fallback_level++) {{
        # Literal completions at this level
        $transitions_var = "literal_transitions_level_$fallback_level"
        $transitions = Get-Variable -Name $transitions_var -ValueOnly -ErrorAction SilentlyContinue
        if ($transitions -and $transitions.ContainsKey($state)) {{
            foreach ($literal_id in $transitions[$state]) {{
                $literal = $literals[$literal_id]
                if ($literal.StartsWith($completed_prefix, [StringComparison]::OrdinalIgnoreCase)) {{
                    $completion = $matched_prefix + $literal
                    $desc = if ($descriptions.ContainsKey($literal_id)) {{ $descriptions[$literal_id] }} else {{ $completion }}
                    $completions += [PSCustomObject]@{{ Text = $completion; Description = $desc }}
                }}
            }}
        }}"#
    )?;

    if needs_commands_code {
        write!(
            buffer,
            r#"

        $commands_var = "commands_level_$fallback_level"
        $commands = Get-Variable -Name $commands_var -ValueOnly -ErrorAction SilentlyContinue
        if ($commands -and $commands.ContainsKey($state)) {{
            foreach ($cmd_id in $commands[$state]) {{
                $output = & "_{command}_cmd_$cmd_id"
                foreach ($line in $output) {{
                    if ([string]::IsNullOrWhiteSpace($line)) {{ continue }}
                    $parts = $line -split "`t", 2
                    $text = $matched_prefix + $parts[0]
                    $desc = if ($parts.Count -gt 1) {{ $parts[1] }} else {{ $text }}
                    if ($text.StartsWith($word, [StringComparison]::OrdinalIgnoreCase)) {{
                        $completions += [PSCustomObject]@{{ Text = $text; Description = $desc }}
                    }}
                }}
            }}
        }}"#
        )?;
    }

    write!(
        buffer,
        r#"

        if ($completions.Count -gt 0) {{
            break
        }}
    }}

    return $completions
}}

"#
    )?;
    Ok(())
}

fn write_subword_wrapper_fn<W: Write>(
    buffer: &mut W,
    command: &str,
    id: usize,
    dfa: &DFA,
    id_from_cmd: &IndexSet<Ustr>,
    needs_commands_code: bool,
) -> Result<()> {
    writeln!(
        buffer,
        r#"function _{command}_subword_{id} {{
    param([string]$mode, [string]$word)"#
    )?;

    let id_from_literal_description =
        write_matching_tables(buffer, dfa, id_from_cmd, needs_commands_code)?;

    let max_fallback_level = dfa.get_max_fallback_level().unwrap_or(ARRAY_START as usize);

    write_completion_tables(
        buffer,
        dfa,
        id_from_cmd,
        needs_commands_code,
        &id_from_literal_description,
        max_fallback_level,
    )?;

    writeln!(buffer, r#"    $max_fallback_level = {max_fallback_level}"#)?;

    writeln!(
        buffer,
        r#"
    $state = {starting_state}
    _{command}_subword $mode $word
}}"#,
        starting_state = dfa.starting_state
    )?;

    Ok(())
}

pub fn write_completion_script<W: Write>(buffer: &mut W, command: &str, dfa: &DFA) -> Result<()> {
    let needs_subwords_code = dfa.needs_subwords_code();
    let needs_top_level_commands_code = dfa.needs_top_level_commands_code();
    let needs_subword_commands_code = dfa.needs_subword_commands_code();

    write!(
        buffer,
        r#"# {command} completion script generated by https://github.com/adaszko/complgen
# Requires PowerShell 7.0 or later

$ErrorActionPreference = "Stop"
"#
    )?;

    let id_from_cmd = dfa.get_commands();
    for cmd in &id_from_cmd {
        let id = id_from_cmd.get_index_of(cmd).unwrap();
        let cmd = cmd.trim();
        let cmd = if cmd.is_empty() {
            "# empty command"
        } else {
            cmd
        };
        writeln!(
            buffer,
            r#"function _{command}_cmd_{id} {{
    {cmd}
}}
"#
        )?;
    }

    let id_from_dfa = dfa.get_subwords(ARRAY_START as usize);
    if needs_subwords_code {
        write_subword_fn(buffer, command, needs_subword_commands_code)?;
        for (dfaid, id) in &id_from_dfa {
            let dfa = dfa.subdfas.lookup(*dfaid);
            write_subword_wrapper_fn(
                buffer,
                command,
                *id,
                dfa,
                &id_from_cmd,
                needs_subword_commands_code,
            )?;
            writeln!(buffer)?;
        }
    }

    writeln!(
        buffer,
        r#"Register-ArgumentCompleter -Native -CommandName '{command}' -ScriptBlock {{"#
    )?;

    writeln!(
        buffer,
        r#"    param($wordToComplete, $commandAst, $cursorPosition)

    # Parse command line into words
    $words = @($commandAst.CommandElements | ForEach-Object {{ $_.Extent.Text }})
    $lastElement = $commandAst.CommandElements[-1]
    # Determine current word index: if cursor is past the last element (trailing space),
    # we're completing a new word; otherwise we're completing the current word
    $cword = if ($words.Count -eq 0 -or $cursorPosition -gt $lastElement.Extent.EndOffset) {{ $words.Count }} else {{ $words.Count - 1 }}
"#
    )?;

    let id_from_literal_description =
        write_matching_tables(buffer, dfa, &id_from_cmd, needs_top_level_commands_code)?;

    if needs_subwords_code {
        writeln!(buffer, r#"    $subword_transitions = @{{}}"#)?;
        for state in dfa.get_all_states() {
            let subword_transitions = dfa.get_subword_transitions_from(state);
            if !subword_transitions.is_empty() {
                let state_transitions = subword_transitions
                    .into_iter()
                    .map(|(dfa, to)| format!("{} = {to}", id_from_dfa.get(&dfa).unwrap()))
                    .join("; ");
                writeln!(
                    buffer,
                    r#"    $subword_transitions[{state}] = @{{{state_transitions}}}"#
                )?;
            }
        }
    }

    write!(
        buffer,
        r#"
    $state = {starting_state}
    $word_index = 1
    :outer while ($word_index -lt $cword) {{
        $word = $words[$word_index]

        if ($literal_transitions.ContainsKey($state)) {{
            $state_transitions = $literal_transitions[$state]

            for ($literal_id = 0; $literal_id -lt $literals.Count; $literal_id++) {{
                if ($literals[$literal_id] -ceq $word) {{
                    if ($state_transitions.ContainsKey($literal_id)) {{
                        $state = $state_transitions[$literal_id]
                        $word_index++
                        continue outer
                    }}
                }}
            }}
        }}"#,
        starting_state = dfa.starting_state
    )?;

    if needs_subwords_code {
        write!(
            buffer,
            r#"

        if ($subword_transitions.ContainsKey($state)) {{
            $subword_state_transitions = $subword_transitions[$state]

            foreach ($subword_id in $subword_state_transitions.Keys) {{
                if (& "_{command}_subword_$subword_id" 'matches' $word) {{
                    $state = $subword_state_transitions[$subword_id]
                    $word_index++
                    continue outer
                }}
            }}
        }}"#
        )?;
    }

    if needs_top_level_commands_code {
        write!(
            buffer,
            r#"

        if ($command_transitions.ContainsKey($state)) {{
            $state_transitions = $command_transitions[$state]

            foreach ($cmd_id in $state_transitions.Keys) {{
                $output = & "_{command}_cmd_$cmd_id"
                foreach ($line in $output) {{
                    if ([string]::IsNullOrWhiteSpace($line)) {{ continue }}
                    $parts = $line -split "`t", 2
                    $text = $parts[0]
                    $desc = if ($parts.Count -gt 1) {{ $parts[1] }} else {{ $text }}

                    if ($text -eq $word) {{
                        $state = $state_transitions[$cmd_id]
                        $word_index++
                        continue outer
                    }}
                }}
            }}
        }}"#
        )?;
    }

    write!(
        buffer,
        r#"

        if ($star_transitions.ContainsKey($state)) {{
            $state = $star_transitions[$state]
            $word_index++
            continue
        }}

        # No valid transition found
        return
    }}
"#
    )?;

    let max_fallback_level = dfa.get_max_fallback_level().unwrap_or(ARRAY_START as usize);

    write_completion_tables(
        buffer,
        dfa,
        &id_from_cmd,
        needs_top_level_commands_code,
        &id_from_literal_description,
        max_fallback_level,
    )?;

    if needs_subwords_code {
        for (level, transitions) in dfa
            .get_completion_subwords(id_from_dfa, max_fallback_level)
            .iter()
            .enumerate()
        {
            let initializer = transitions
                .iter()
                .map(|(from_state, subword_ids)| {
                    format!(
                        r#"{from_state} = @({})"#,
                        subword_ids.iter().map(|id| id.to_string()).join(", ")
                    )
                })
                .join("; ");
            writeln!(
                buffer,
                r#"    $subword_transitions_level_{level} = @{{{initializer}}}"#
            )?;
        }
    }

    write!(
        buffer,
        r#"
    $max_fallback_level = {max_fallback_level}
    $prefix = if ($cword -lt $words.Count) {{ $words[$cword] }} else {{ "" }}
    $results = @()

    for ($fallback_level = 0; $fallback_level -le $max_fallback_level; $fallback_level++) {{
        $level_results = @()

        # Literal completions
        $transitions_var = "literal_transitions_level_$fallback_level"
        $transitions = Get-Variable -Name $transitions_var -ValueOnly -ErrorAction SilentlyContinue
        if ($transitions -and $transitions.ContainsKey($state)) {{
            foreach ($literal_id in $transitions[$state]) {{
                $literal = $literals[$literal_id]
                if ($literal.StartsWith($prefix, [StringComparison]::OrdinalIgnoreCase)) {{
                    $desc = if ($descriptions.ContainsKey($literal_id)) {{ $descriptions[$literal_id] }} else {{ $literal }}
                    $level_results += [System.Management.Automation.CompletionResult]::new(
                        $literal,
                        $literal,
                        'ParameterValue',
                        $desc
                    )
                }}
            }}
        }}"#
    )?;

    if needs_subwords_code {
        write!(
            buffer,
            r#"

        # Subword completions
        $transitions_var = "subword_transitions_level_$fallback_level"
        $transitions = Get-Variable -Name $transitions_var -ValueOnly -ErrorAction SilentlyContinue
        if ($transitions -and $transitions.ContainsKey($state)) {{
            foreach ($subword_id in $transitions[$state]) {{
                $subword_completions = & "_{command}_subword_$subword_id" 'complete' $prefix
                foreach ($comp in $subword_completions) {{
                    if ($comp.Text.StartsWith($prefix, [StringComparison]::OrdinalIgnoreCase)) {{
                        $level_results += [System.Management.Automation.CompletionResult]::new(
                            $comp.Text,
                            $comp.Text,
                            'ParameterValue',
                            $comp.Description
                        )
                    }}
                }}
            }}
        }}"#
        )?;
    }

    if needs_top_level_commands_code {
        write!(
            buffer,
            r#"

        # Command completions
        $commands_var = "commands_level_$fallback_level"
        $commands = Get-Variable -Name $commands_var -ValueOnly -ErrorAction SilentlyContinue
        if ($commands -and $commands.ContainsKey($state)) {{
            foreach ($cmd_id in $commands[$state]) {{
                $output = & "_{command}_cmd_$cmd_id"
                foreach ($line in $output) {{
                    if ([string]::IsNullOrWhiteSpace($line)) {{ continue }}
                    $parts = $line -split "`t", 2
                    $text = $parts[0]
                    $desc = if ($parts.Count -gt 1) {{ $parts[1] }} else {{ $text }}
                    if ($text.StartsWith($prefix, [StringComparison]::OrdinalIgnoreCase)) {{
                        $level_results += [System.Management.Automation.CompletionResult]::new(
                            $text,
                            $text,
                            'ParameterValue',
                            $desc
                        )
                    }}
                }}
            }}
        }}"#
        )?;
    }

    write!(
        buffer,
        r#"

        if ($level_results.Count -gt 0) {{
            $results = $level_results
            break
        }}
    }}

    return $results
}}
"#
    )?;

    Ok(())
}

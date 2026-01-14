use std::io::Write;

use crate::Result;
use crate::StateId;
use crate::dfa::DFA;
use crate::dfa::Inp;
use hashbrown::HashMap;
use indexmap::IndexSet;
use ustr::{Ustr, ustr};

// PowerShell array indexes start at 0 (like Bash).
// PowerShell uses dynamic scoping. We use $script:state to share state
// between the main completion function and subword helper functions.

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


fn write_lookup_tables<W: Write>(
    buffer: &mut W,
    dfa: &DFA,
    id_from_regex: &IndexSet<Ustr>,
    needs_nontails_code: bool,
) -> Result<HashMap<(Ustr, Ustr), usize>> {
    let all_literals: Vec<(usize, Ustr, Ustr)> = dfa
        .get_all_literals()
        .into_iter()
        .enumerate()
        .map(|(id, (literal, description))| (id, literal, description.unwrap_or(ustr(""))))
        .collect();

    let literal_id_from_input_description: HashMap<(Ustr, Ustr), usize> = all_literals
        .iter()
        .map(|(id, input, description)| ((*input, *description), *id))
        .collect();

    // Write literals array
    let literals: String = itertools::join(
        all_literals
            .iter()
            .map(|(_, literal, _)| make_string_constant(literal)),
        ", ",
    );
    writeln!(buffer, r#"    $literals = @({literals})"#)?;

    // Write descriptions hashtable (sparse - only for literals that have descriptions)
    let descriptions: Vec<String> = all_literals
        .iter()
        .filter(|(_, _, desc)| !desc.is_empty())
        .map(|(id, _, desc)| format!("{} = {}", id, make_string_constant(desc)))
        .collect();
    if descriptions.is_empty() {
        writeln!(buffer, r#"    $descriptions = @{{}}"#)?;
    } else {
        let desc_str = itertools::join(descriptions, "; ");
        writeln!(buffer, r#"    $descriptions = @{{ {desc_str} }}"#)?;
    }

    writeln!(buffer, r#"    $literal_transitions = @{{}}"#)?;

    if needs_nontails_code {
        let regexes: String = itertools::join(
            id_from_regex
                .iter()
                .map(|regex| make_string_constant(regex)),
            ", ",
        );
        writeln!(buffer, r#"    $regexes = @({regexes})"#)?;
        writeln!(buffer, r#"    $nontail_transitions = @{{}}"#)?;
    }

    for state in dfa.get_all_states() {
        let literal_transitions =
            dfa.get_literal_transitions_from(StateId::try_from(state).unwrap());
        if !literal_transitions.is_empty() {
            let literal_transitions: Vec<(usize, StateId)> = literal_transitions
                .into_iter()
                .map(|(literal, description, to)| {
                    (
                        *literal_id_from_input_description
                            .get(&(literal, description))
                            .unwrap(),
                        to,
                    )
                })
                .collect();
            let state_literal_transitions: String = itertools::join(
                literal_transitions
                    .into_iter()
                    .map(|(literal_id, to)| format!("{} = {}", literal_id, to)),
                "; ",
            );
            writeln!(
                buffer,
                r#"    $literal_transitions[{state}] = @{{ {state_literal_transitions} }}"#
            )?;
        }

        if needs_nontails_code {
            let nontail_transitions = dfa.get_nontail_transitions_from(state as StateId);
            if !nontail_transitions.is_empty() {
                let nontail_command_transitions: Vec<(usize, StateId)> = nontail_transitions
                    .into_iter()
                    .map(|(regex, to)| (id_from_regex.get_index_of(&regex).unwrap(), to))
                    .collect();
                let state_nontail_transitions: String = itertools::join(
                    nontail_command_transitions
                        .into_iter()
                        .map(|(regex_id, to)| format!("{regex_id} = {to}")),
                    "; ",
                );
                writeln!(
                    buffer,
                    r#"    $nontail_transitions[{state}] = @{{ {state_nontail_transitions} }}"#
                )?;
            }
        }
    }

    let star_transitions: Vec<String> = dfa
        .iter_top_level_star_transitions()
        .map(|(from, to)| format!("{from} = {to}"))
        .collect();
    if star_transitions.is_empty() {
        writeln!(buffer, r#"    $star_transitions = @{{}}"#)?;
    } else {
        let star_str = itertools::join(star_transitions, "; ");
        writeln!(buffer, r#"    $star_transitions = @{{ {star_str} }}"#)?;
    }

    Ok(literal_id_from_input_description)
}

fn write_generic_subword_fn<W: Write>(
    buffer: &mut W,
    command: &str,
    needs_nontails_code: bool,
    needs_commands_code: bool,
) -> Result<()> {
    writeln!(
        buffer,
        r#"function _{command}_subword {{
    param([string]$mode, [string]$word)

    $char_index = 0
    $matched = $false
    while ($true) {{
        if ($char_index -ge $word.Length) {{
            $matched = $true
            break
        }}

        $subword = $word.Substring($char_index)

        if ($literal_transitions.ContainsKey($script:state)) {{
            $state_transitions = $literal_transitions[$script:state]

            $literal_matched = $false
            for ($literal_id = 0; $literal_id -lt $literals.Count; $literal_id++) {{
                $literal = $literals[$literal_id]
                $literal_len = $literal.Length
                if ($subword.StartsWith($literal)) {{
                    if ($state_transitions.ContainsKey($literal_id)) {{
                        $script:state = $state_transitions[$literal_id]
                        $char_index += $literal_len
                        $literal_matched = $true
                        break
                    }}
                }}
            }}
            if ($literal_matched) {{
                continue
            }}
        }}"#
    )?;

    if needs_nontails_code {
        write!(
            buffer,
            r#"
        if ($nontail_transitions.ContainsKey($script:state)) {{
            $nontail_state_transitions = $nontail_transitions[$script:state]

            $nontail_matched = $false
            foreach ($regex_id in $nontail_state_transitions.Keys) {{
                $regex = $regexes[$regex_id]
                if ($subword -match "^($regex)") {{
                    $match_len = $Matches[1].Length
                    if ($match_len -gt 0) {{
                        $script:state = $nontail_state_transitions[$regex_id]
                        $char_index += $match_len
                        $nontail_matched = $true
                        break
                    }}
                }}
            }}
            if ($nontail_matched) {{
                continue
            }}
        }}"#
        )?;
    }


    write!(
        buffer,
        r#"
        if ($star_transitions.ContainsKey($script:state)) {{
            if ($mode -eq 'matches') {{
                return $true
            }}
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
        if ($transitions -and $transitions.ContainsKey($script:state)) {{
            foreach ($literal_id in $transitions[$script:state]) {{
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

        # Command completions at this level
        $commands_var = "commands_level_$fallback_level"
        $commands = Get-Variable -Name $commands_var -ValueOnly -ErrorAction SilentlyContinue
        if ($commands -and $commands.ContainsKey($script:state)) {{
            foreach ($cmd_id in $commands[$script:state]) {{
                $output = & "_{command}_cmd_$cmd_id" $completed_prefix $matched_prefix
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

fn write_subword_fn<W: Write>(
    buffer: &mut W,
    command: &str,
    id: usize,
    dfa: &DFA,
    id_from_cmd: &IndexSet<Ustr>,
    id_from_regex: &IndexSet<Ustr>,
    needs_nontails_code: bool,
    needs_commands_code: bool,
) -> Result<()> {
    let all_literals: Vec<(usize, Ustr, Ustr)> = dfa
        .get_all_literals()
        .into_iter()
        .enumerate()
        .map(|(id, (literal, description))| (id, literal, description.unwrap_or(ustr(""))))
        .collect();

    let literal_id_from_input_description: HashMap<(Ustr, Ustr), usize> = all_literals
        .iter()
        .map(|(id, input, description)| ((*input, *description), *id))
        .collect();

    writeln!(
        buffer,
        r#"function _{command}_subword_{id} {{
    param([string]$mode, [string]$word)"#
    )?;

    // Write literals array
    let literals: String = itertools::join(
        all_literals
            .iter()
            .map(|(_, literal, _)| make_string_constant(literal)),
        ", ",
    );
    writeln!(buffer, r#"    $literals = @({literals})"#)?;

    // Write descriptions hashtable
    let descriptions: Vec<String> = all_literals
        .iter()
        .filter(|(_, _, desc)| !desc.is_empty())
        .map(|(id, _, desc)| format!("{} = {}", id, make_string_constant(desc)))
        .collect();
    if descriptions.is_empty() {
        writeln!(buffer, r#"    $descriptions = @{{}}"#)?;
    } else {
        let desc_str = itertools::join(descriptions, "; ");
        writeln!(buffer, r#"    $descriptions = @{{ {desc_str} }}"#)?;
    }

    writeln!(buffer, r#"    $literal_transitions = @{{}}"#)?;

    if needs_nontails_code {
        let regexes: String = itertools::join(
            id_from_regex
                .iter()
                .map(|regex| make_string_constant(regex)),
            ", ",
        );
        writeln!(buffer, r#"    $regexes = @({regexes})"#)?;
        writeln!(buffer, r#"    $nontail_transitions = @{{}}"#)?;
    }

    for state in dfa.get_all_states() {
        let literal_transitions =
            dfa.get_literal_transitions_from(StateId::try_from(state).unwrap());
        if !literal_transitions.is_empty() {
            let literal_transitions: Vec<(usize, StateId)> = literal_transitions
                .into_iter()
                .map(|(literal, description, to)| {
                    (
                        *literal_id_from_input_description
                            .get(&(literal, description))
                            .unwrap(),
                        to,
                    )
                })
                .collect();
            let state_literal_transitions: String = itertools::join(
                literal_transitions
                    .into_iter()
                    .map(|(literal_id, to)| format!("{} = {}", literal_id, to)),
                "; ",
            );
            writeln!(
                buffer,
                r#"    $literal_transitions[{state}] = @{{ {state_literal_transitions} }}"#
            )?;
        }

        if needs_nontails_code {
            let nontail_transitions = dfa.get_nontail_transitions_from(state as StateId);
            if !nontail_transitions.is_empty() {
                let nontail_command_transitions: Vec<(usize, StateId)> = nontail_transitions
                    .into_iter()
                    .map(|(regex, to)| (id_from_regex.get_index_of(&regex).unwrap(), to))
                    .collect();
                let state_nontail_transitions: String = itertools::join(
                    nontail_command_transitions
                        .into_iter()
                        .map(|(regex_id, to)| format!("{regex_id} = {to}")),
                    "; ",
                );
                writeln!(
                    buffer,
                    r#"    $nontail_transitions[{state}] = @{{ {state_nontail_transitions} }}"#
                )?;
            }
        }

    }

    let star_transitions: Vec<String> = dfa
        .iter_top_level_star_transitions()
        .map(|(from, to)| format!("{from} = {to}"))
        .collect();
    if star_transitions.is_empty() {
        writeln!(buffer, r#"    $star_transitions = @{{}}"#)?;
    } else {
        let star_str = itertools::join(star_transitions, "; ");
        writeln!(buffer, r#"    $star_transitions = @{{ {star_str} }}"#)?;
    }

    // Collect command transitions for completion (similar to bash implementation)
    let max_fallback_level = dfa.get_max_fallback_level().unwrap_or(ARRAY_START as usize);

    // Collect literal transitions by fallback level
    let mut completion_literals: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    completion_literals.resize_with(max_fallback_level + 1, Default::default);

    // Collect command transitions by fallback level
    let mut completion_commands: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    completion_commands.resize_with(max_fallback_level + 1, Default::default);

    for (from, input_id, _) in dfa.iter_transitions() {
        match dfa.get_input(input_id) {
            Inp::Literal {
                literal,
                description,
                fallback_level,
            } => {
                let literal_id = *literal_id_from_input_description
                    .get(&(*literal, description.unwrap_or(ustr(""))))
                    .unwrap();
                completion_literals[*fallback_level]
                    .entry(from)
                    .or_default()
                    .push(literal_id);
            }
            Inp::Command {
                cmd,
                regex: None,
                fallback_level,
                zsh_compadd: false,
            } => {
                let command_id = id_from_cmd.get_index_of(cmd).unwrap();
                completion_commands[*fallback_level]
                    .entry(from)
                    .or_default()
                    .push(command_id);
            }
            _ => {}
        }
    }

    // Write literal_transitions_level_X
    for (level, transitions) in completion_literals.iter().enumerate() {
        if transitions.is_empty() {
            writeln!(buffer, r#"    $literal_transitions_level_{level} = @{{}}"#)?;
        } else {
            let init_str: String = itertools::join(
                transitions.iter().map(|(from_state, literal_ids)| {
                    let ids_str = itertools::join(literal_ids.iter().map(|id| id.to_string()), ", ");
                    format!("{from_state} = @({ids_str})")
                }),
                "; ",
            );
            writeln!(
                buffer,
                r#"    $literal_transitions_level_{level} = @{{ {init_str} }}"#
            )?;
        }
    }

    // Write commands_level_X
    if needs_commands_code {
        for (level, transitions) in completion_commands.iter().enumerate() {
            if transitions.is_empty() {
                writeln!(buffer, r#"    $commands_level_{level} = @{{}}"#)?;
            } else {
                let init_str: String = itertools::join(
                    transitions.iter().map(|(from_state, cmd_ids)| {
                        let ids_str = itertools::join(cmd_ids.iter().map(|id| id.to_string()), ", ");
                        format!("{from_state} = @({ids_str})")
                    }),
                    "; ",
                );
                writeln!(
                    buffer,
                    r#"    $commands_level_{level} = @{{ {init_str} }}"#
                )?;
            }
        }
    }

    writeln!(buffer, r#"    $max_fallback_level = {max_fallback_level}"#)?;

    writeln!(
        buffer,
        r#"
    $script:state = {starting_state}
    _{command}_subword $mode $word
}}"#,
        starting_state = dfa.starting_state
    )?;

    Ok(())
}

fn make_id_from_command_map(dfa: &DFA) -> (IndexSet<Ustr>, IndexSet<Ustr>) {
    let mut id_from_cmd: IndexSet<Ustr> = Default::default();
    let mut id_from_regex: IndexSet<Ustr> = Default::default();

    for input in dfa.iter_inputs() {
        match input {
            Inp::Command {
                zsh_compadd: true, ..
            } => {}
            Inp::Command {
                cmd,
                regex,
                zsh_compadd: false,
                fallback_level: _,
            } => {
                id_from_cmd.insert(*cmd);
                if let Some(rx) = regex {
                    id_from_regex.insert(*rx);
                }
            }
            Inp::Literal { .. } | Inp::Star => {}
            Inp::Subword {
                subdfa: subdfaid, ..
            } => {
                let subdfa = dfa.subdfas.lookup(*subdfaid);
                for input in subdfa.iter_inputs() {
                    match input {
                        Inp::Command {
                            zsh_compadd: true, ..
                        } => unreachable!(),
                        Inp::Command {
                            cmd,
                            regex,
                            zsh_compadd: false,
                            fallback_level: _,
                        } => {
                            id_from_cmd.insert(*cmd);
                            if let Some(rx) = regex {
                                id_from_regex.insert(*rx);
                            }
                        }
                        Inp::Literal { .. } | Inp::Subword { .. } | Inp::Star => {}
                    }
                }
            }
        }
    }

    (id_from_cmd, id_from_regex)
}

pub fn write_completion_script<W: Write>(buffer: &mut W, command: &str, dfa: &DFA) -> Result<()> {
    let needs_subwords_code = dfa.needs_subwords_code();
    let needs_nontails_code = dfa.needs_nontails_code();
    let needs_subword_nontails_code = dfa.needs_subword_nontails_code();
    let needs_commands_code = dfa.needs_commands_code();
    let needs_subword_commands_code = dfa.needs_subword_commands_code();

    write!(
        buffer,
        r#"# {command} completion script generated by complgen
# Requires PowerShell 7.0 or later

"#
    )?;

    let (id_from_cmd, id_from_regex) = make_id_from_command_map(dfa);
    for cmd in &id_from_cmd {
        let id = id_from_cmd.get_index_of(cmd).unwrap();
        let cmd = cmd.trim();
        // Replace $1 and $2 placeholders with PowerShell parameter names
        let cmd = cmd.replace("$1", "$prefix").replace("$2", "$matchedPrefix");
        let cmd = if cmd.is_empty() {
            "# empty command".to_string()
        } else {
            cmd
        };
        writeln!(
            buffer,
            r#"function _{command}_cmd_{id} {{
    param([string]$prefix, [string]$matchedPrefix)
    {cmd}
}}
"#
        )?;
    }

    let id_from_dfa = dfa.get_subwords(ARRAY_START as usize);
    if needs_subwords_code {
        write_generic_subword_fn(
            buffer,
            command,
            needs_nontails_code,
            needs_subword_commands_code,
        )?;
        for (dfaid, id) in &id_from_dfa {
            let dfa = dfa.subdfas.lookup(*dfaid);
            write_subword_fn(
                buffer,
                command,
                *id,
                dfa,
                &id_from_cmd,
                &id_from_regex,
                needs_subword_nontails_code,
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

    let literal_id_from_input_description =
        write_lookup_tables(buffer, dfa, &id_from_regex, needs_nontails_code)?;

    if needs_subwords_code {
        writeln!(buffer, r#"    $subword_transitions = @{{}}"#)?;
        for state in dfa.get_all_states() {
            let subword_transitions = dfa.get_subword_transitions_from(state);
            if subword_transitions.is_empty() {
                continue;
            }
            let state_transitions: String = itertools::join(
                subword_transitions
                    .into_iter()
                    .map(|(dfa, to)| format!("{} = {}", id_from_dfa.get(&dfa).unwrap(), to)),
                "; ",
            );
            writeln!(
                buffer,
                r#"    $subword_transitions[{state}] = @{{ {state_transitions} }}"#
            )?;
        }
    }

    write!(
        buffer,
        r#"
    $state = {starting_state}
    $word_index = 1
    while ($word_index -lt $cword) {{
        $word = $words[$word_index]

        if ($literal_transitions.ContainsKey($state)) {{
            $state_transitions = $literal_transitions[$state]

            $word_matched = $false
            for ($literal_id = 0; $literal_id -lt $literals.Count; $literal_id++) {{
                if ($literals[$literal_id] -ceq $word) {{
                    if ($state_transitions.ContainsKey($literal_id)) {{
                        $state = $state_transitions[$literal_id]
                        $word_index++
                        $word_matched = $true
                        break
                    }}
                }}
            }}
            if ($word_matched) {{
                continue
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

            $subword_matched = $false
            foreach ($subword_id in $subword_state_transitions.Keys) {{
                if (& "_{command}_subword_$subword_id" 'matches' $word) {{
                    $state = $subword_state_transitions[$subword_id]
                    $word_index++
                    $subword_matched = $true
                    break
                }}
            }}
            if ($subword_matched) {{
                continue
            }}
        }}"#
        )?;
    }

    if needs_nontails_code {
        write!(
            buffer,
            r#"

        if ($nontail_transitions.ContainsKey($state)) {{
            $nontail_state_transitions = $nontail_transitions[$state]

            $nontail_matched = $false
            foreach ($regex_id in $nontail_state_transitions.Keys) {{
                $regex = $regexes[$regex_id]
                if ($word -match "^($regex)$") {{
                    $state = $nontail_state_transitions[$regex_id]
                    $word_index++
                    $nontail_matched = $true
                    break
                }}
            }}
            if ($nontail_matched) {{
                continue
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

    // Build completion data structures by fallback level
    let max_fallback_level = dfa.get_max_fallback_level().unwrap_or(ARRAY_START as usize);

    let mut completion_literals: Vec<HashMap<StateId, Vec<usize>>> =
        vec![HashMap::default(); max_fallback_level + 1];
    let mut completion_subwords: Vec<HashMap<StateId, Vec<usize>>> =
        vec![HashMap::default(); max_fallback_level + 1];
    let mut completion_commands: Vec<HashMap<StateId, Vec<usize>>> =
        vec![HashMap::default(); max_fallback_level + 1];
    let mut completion_nontails: Vec<HashMap<StateId, Vec<(usize, usize)>>> =
        vec![HashMap::default(); max_fallback_level + 1];

    for (from, inpid, _to) in dfa.iter_transitions() {
        let input = dfa.get_input(inpid);
        match input {
            Inp::Literal {
                literal,
                description,
                fallback_level,
            } => {
                let descr = description.unwrap_or(ustr(""));
                let literal_id = literal_id_from_input_description
                    .get(&(*literal, descr))
                    .unwrap();
                completion_literals[*fallback_level]
                    .entry(from)
                    .or_default()
                    .push(*literal_id);
            }
            Inp::Subword {
                subdfa,
                fallback_level,
            } => {
                let subword_id = id_from_dfa.get(subdfa).unwrap();
                completion_subwords[*fallback_level]
                    .entry(from)
                    .or_default()
                    .push(*subword_id);
            }
            Inp::Command {
                cmd,
                regex: None,
                zsh_compadd: false,
                fallback_level,
            } => {
                let cmd_id = id_from_cmd.get_index_of(cmd).unwrap();
                completion_commands[*fallback_level]
                    .entry(from)
                    .or_default()
                    .push(cmd_id);
            }
            Inp::Command {
                cmd,
                regex: Some(regex),
                zsh_compadd: false,
                fallback_level,
            } => {
                let cmd_id = id_from_cmd.get_index_of(cmd).unwrap();
                let regex_id = id_from_regex.get_index_of(regex).unwrap();
                completion_nontails[*fallback_level]
                    .entry(from)
                    .or_default()
                    .push((cmd_id, regex_id));
            }
            Inp::Command {
                zsh_compadd: true, ..
            }
            | Inp::Star => {}
        }
    }

    // Write completion tables by fallback level
    for (level, transitions) in completion_literals.iter().enumerate() {
        let initializer: Vec<String> = transitions
            .iter()
            .map(|(from_state, literal_ids)| {
                let joined_literal_ids = itertools::join(literal_ids.iter().map(|id| id.to_string()), ", ");
                format!(r#"{from_state} = @({joined_literal_ids})"#)
            })
            .collect();
        if initializer.is_empty() {
            writeln!(buffer, r#"    $literal_transitions_level_{level} = @{{}}"#)?;
        } else {
            let init_str = itertools::join(initializer, "; ");
            writeln!(
                buffer,
                r#"    $literal_transitions_level_{level} = @{{ {init_str} }}"#
            )?;
        }
    }

    if needs_subwords_code {
        for (level, transitions) in completion_subwords.iter().enumerate() {
            let initializer: Vec<String> = transitions
                .iter()
                .map(|(from_state, subword_ids)| {
                    let joined_subword_ids = itertools::join(subword_ids.iter().map(|id| id.to_string()), ", ");
                    format!(r#"{from_state} = @({joined_subword_ids})"#)
                })
                .collect();
            if initializer.is_empty() {
                writeln!(buffer, r#"    $subword_transitions_level_{level} = @{{}}"#)?;
            } else {
                let init_str = itertools::join(initializer, "; ");
                writeln!(
                    buffer,
                    r#"    $subword_transitions_level_{level} = @{{ {init_str} }}"#
                )?;
            }
        }
    }

    if needs_commands_code {
        for (level, transitions) in completion_commands.iter().enumerate() {
            let initializer: Vec<String> = transitions
                .iter()
                .map(|(from_state, command_ids)| {
                    let joined_command_ids = itertools::join(command_ids.iter().map(|id| id.to_string()), ", ");
                    format!(r#"{from_state} = @({joined_command_ids})"#)
                })
                .collect();
            if initializer.is_empty() {
                writeln!(buffer, r#"    $commands_level_{level} = @{{}}"#)?;
            } else {
                let init_str = itertools::join(initializer, "; ");
                writeln!(
                    buffer,
                    r#"    $commands_level_{level} = @{{ {init_str} }}"#
                )?;
            }
        }
    }

    if needs_nontails_code {
        for (level, transitions) in completion_nontails.iter().enumerate() {
            let commands_initializer: Vec<String> = transitions
                .iter()
                .map(|(from_state, ids)| {
                    let joined_ids = itertools::join(ids.iter().map(|(cmd_id, _)| cmd_id.to_string()), ", ");
                    format!(r#"{from_state} = @({joined_ids})"#)
                })
                .collect();
            if commands_initializer.is_empty() {
                writeln!(buffer, r#"    $nontail_commands_level_{level} = @{{}}"#)?;
            } else {
                let init_str = itertools::join(commands_initializer, "; ");
                writeln!(
                    buffer,
                    r#"    $nontail_commands_level_{level} = @{{ {init_str} }}"#
                )?;
            }

            let regexes_initializer: Vec<String> = transitions
                .iter()
                .map(|(from_state, ids)| {
                    let joined_ids = itertools::join(ids.iter().map(|(_, regex_id)| regex_id.to_string()), ", ");
                    format!(r#"{from_state} = @({joined_ids})"#)
                })
                .collect();
            if regexes_initializer.is_empty() {
                writeln!(buffer, r#"    $nontail_regexes_level_{level} = @{{}}"#)?;
            } else {
                let init_str = itertools::join(regexes_initializer, "; ");
                writeln!(
                    buffer,
                    r#"    $nontail_regexes_level_{level} = @{{ {init_str} }}"#
                )?;
            }
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

    if needs_commands_code {
        write!(
            buffer,
            r#"

        # Command completions
        $commands_var = "commands_level_$fallback_level"
        $commands = Get-Variable -Name $commands_var -ValueOnly -ErrorAction SilentlyContinue
        if ($commands -and $commands.ContainsKey($state)) {{
            foreach ($cmd_id in $commands[$state]) {{
                $output = & "_{command}_cmd_$cmd_id" $prefix ""
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

    if needs_nontails_code {
        write!(
            buffer,
            r#"

        # Nontail completions (commands with regex constraints)
        $commands_var = "nontail_commands_level_$fallback_level"
        $nontail_commands = Get-Variable -Name $commands_var -ValueOnly -ErrorAction SilentlyContinue
        $regexes_var = "nontail_regexes_level_$fallback_level"
        $nontail_regexes = Get-Variable -Name $regexes_var -ValueOnly -ErrorAction SilentlyContinue
        if ($nontail_commands -and $nontail_commands.ContainsKey($state)) {{
            $cmd_ids = $nontail_commands[$state]
            $regex_ids = $nontail_regexes[$state]
            for ($i = 0; $i -lt $cmd_ids.Count; $i++) {{
                $cmd_id = $cmd_ids[$i]
                $regex_id = $regex_ids[$i]
                $regex = "^(" + $regexes[$regex_id] + ")"
                $output = & "_{command}_cmd_$cmd_id" $prefix ""
                foreach ($line in $output) {{
                    if ([string]::IsNullOrWhiteSpace($line)) {{ continue }}
                    $parts = $line -split "`t", 2
                    $text = $parts[0]
                    if ($text -match $regex -and $Matches[1]) {{
                        $match_text = $Matches[1]
                        if ($match_text.StartsWith($prefix, [StringComparison]::OrdinalIgnoreCase)) {{
                            $desc = if ($parts.Count -gt 1) {{ $parts[1] }} else {{ $match_text }}
                            $level_results += [System.Management.Automation.CompletionResult]::new(
                                $match_text,
                                $match_text,
                                'ParameterValue',
                                $desc
                            )
                        }}
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

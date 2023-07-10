use std::ffi::OsStr;
use std::{io::Write, process::Output};
use std::process::Command;

use complgen::StateId;
use hashbrown::HashMap;

use ustr::ustr;
use anyhow::{anyhow, Context};

use crate::grammar::Specialization;
use crate::{dfa::DFA, regex::{Input, MatchAnythingInput}};



#[derive(Debug, Clone, Copy)]
pub enum Shell {
    Bash,
    Fish,
    Zsh,
}


fn shell_out_bash(command: &str) -> anyhow::Result<Output> {
    Command::new("bash").arg("-c").arg(command).output().map_err(Into::into)
}


fn shell_out_fish(command: &str) -> anyhow::Result<Output> {
    Command::new("fish").arg("-c").arg(command).output().map_err(Into::into)
}


fn shell_out_zsh(command: &str) -> anyhow::Result<Output> {
    Command::new("zsh").arg("-c").arg(command).output().map_err(Into::into)
}


fn stdout_from_output(output: Output) -> anyhow::Result<String> {
    if !output.status.success() {
        let stdout: String = String::from_utf8_lossy(&output.stdout).to_string();
        let stderr: String = String::from_utf8_lossy(&output.stderr).to_string();
        let result = anyhow::Result::Err(anyhow!("Command invokation failed"))
            .context(stdout)
            .context(stderr);
        return result;
    }

    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    Ok(stdout)
}


fn get_bash_command_stdout(command: &str) -> anyhow::Result<String> {
    let output = shell_out_bash(command).with_context(|| command.to_string())?;
    stdout_from_output(output)
}

fn get_fish_command_stdout(command: &str) -> anyhow::Result<String> {
    let output = shell_out_fish(command).with_context(|| command.to_string())?;
    stdout_from_output(output)
}

fn get_zsh_command_stdout(command: &str) -> anyhow::Result<String> {
    let output = shell_out_zsh(command).with_context(|| command.to_string())?;
    stdout_from_output(output)
}

fn get_zsh_script_stdout<P: AsRef<OsStr>>(script_path: P, arg: &str) -> anyhow::Result<String> {
    let output = Command::new("zsh").arg(script_path).arg(arg).output()?;
    stdout_from_output(output)
}


fn capture_zsh_completions(completion_code: &str, command: &str, user_input: &str) -> anyhow::Result<String> {
    let preamble = include_str!("../capture_preamble.zsh");
    let postamble = include_str!("../capture_postamble.zsh");

    let mut capture_script = tempfile::NamedTempFile::new()?;
    write!(capture_script, "{}", preamble)?;

    writeln!(capture_script, r#"_{command} () {{"#)?;
    writeln!(capture_script, "{}", completion_code.replace("'", "''"))?;
    writeln!(capture_script, r#"}}"#)?;
    writeln!(capture_script, r#""#)?;
    writeln!(capture_script, "compdef _{command} {command}")?;

    write!(capture_script, "{}", postamble)?;

    capture_script.as_file().flush()?;

    get_zsh_script_stdout(capture_script.path(), user_input)
        .with_context(|| completion_code.to_string())
        .with_context(|| command.to_string())
        .with_context(|| user_input.to_string())
}


impl Shell {
    fn shell_out(&self, command: &str) -> anyhow::Result<String> {
        let output = match self {
            Shell::Bash => shell_out_bash(command)?,
            Shell::Fish => shell_out_fish(command)?,
            Shell::Zsh => shell_out_zsh(command)?,
        };

        stdout_from_output(output)
    }
}


pub fn get_match_final_state(dfa: &DFA, inputs: &[&str], completed_word_index: usize) -> Option<StateId> {
    let mut backtracking_stack = Vec::from_iter([(0, dfa.starting_state)]);
    while let Some((input_index, current_state)) = backtracking_stack.pop() {
        if input_index >= inputs.len() {
            return Some(current_state);
        }

        if input_index >= completed_word_index {
            return Some(current_state);
        }

        for (transition_input, to) in dfa.transitions.get(&current_state).unwrap_or(&HashMap::default()) {
            if transition_input.matches_anything() {
                backtracking_stack.push((input_index + 1, *to));
            }
        }

        for (transition_input, to) in dfa.transitions.get(&current_state).unwrap_or(&HashMap::default()) {
            if let Input::Literal(s, _) = transition_input {
                if s.as_str() == inputs[input_index] {
                    backtracking_stack.push((input_index + 1, *to));
                }
            }
        }
    }
    None
}


fn capture_specialized_completions(shell: Shell, specialization: &Specialization, prefix: &str) -> anyhow::Result<Vec<(String, String)>> {
    let stdout = match shell {
        Shell::Bash => {
            let Some(command) = specialization.bash.or(specialization.generic) else {
                return Ok(vec![]);
            };
            get_bash_command_stdout(&command)?
        },
        Shell::Fish => {
            let Some(command) = specialization.fish.or(specialization.generic) else {
                return Ok(vec![]);
            };
            get_fish_command_stdout(&command)?
        },
        Shell::Zsh => {
            if let Some(command) = specialization.zsh {
                capture_zsh_completions(&command, "dummy", &format!("dummy "))?
            }
            else if let Some(command) = specialization.generic {
                get_zsh_command_stdout(&command)?
            }
            else {
                return Ok(vec![]);
            }
        },
    };

    let result: Vec<(String, String)> = stdout
        .lines()
        .filter(|line| line.starts_with(prefix)).map(|line| match line.split_once("\t") {
            Some((completion, description)) => (completion.to_owned(), description.to_owned()),
            None => (line.to_string(), "".to_string()),
        }).collect();

    Ok(result)
}


fn do_get_completions_for_input(input: &Input, prefix: &str, shell: Shell) -> anyhow::Result<Vec<(String, String)>> {
    let completions = match input {
        Input::Literal(literal, description) => {
            if literal.starts_with(prefix) {
                vec![(literal.as_str().to_string(), description.unwrap_or(ustr("")).as_str().to_string())]
            }
            else {
                vec![]
            }
        },

        Input::Any(MatchAnythingInput::Command(command)) => {
            let stdout = shell.shell_out(command.as_str())?;

            let result: Vec<(String, String)> = stdout.lines().filter(|line| line.starts_with(prefix)).map(|line| match line.split_once("\t") {
                Some((completion, description)) => (completion.to_owned(), description.to_owned()),
                None => (line.to_string(), "".to_string()),
            }).collect();

            result
        },

        Input::Any(MatchAnythingInput::Nonterminal(nonterm, None)) if nonterm.as_str() == "PATH" => {
            let specialization = &Specialization {
                bash: Some(ustr(&format!("compgen -A file {prefix}"))),
                fish: Some(ustr(&format!("__fish_complete_path {prefix}"))),
                zsh: Some(ustr("_path_files")),
                generic: None,
            };
            capture_specialized_completions(shell, specialization, prefix)?
        },

        Input::Any(MatchAnythingInput::Nonterminal(nonterm, None)) if nonterm.as_str() == "DIRECTORY" => {
            let specialization = &Specialization {
                bash: Some(ustr(&format!("compgen -A directory {prefix}"))),
                fish: Some(ustr(&format!("__fish_complete_directories {prefix}"))),
                zsh: Some(ustr("_path_files -/")),
                generic: None,
            };
            capture_specialized_completions(shell, specialization, prefix)?
        },

        Input::Any(MatchAnythingInput::Nonterminal(_, None)) => vec![],

        Input::Any(MatchAnythingInput::Nonterminal(_, Some(specialization))) => capture_specialized_completions(shell, specialization, prefix)?
    };
    Ok(completions)
}


fn get_completions_for_input(input: &Input, prefix: &str, shell: Shell) -> Vec<(String, String)> {
    match do_get_completions_for_input(input, prefix, shell) {
        Ok(completions) => completions,
        Err(e) => {
            eprintln!("{:?}", e);
            return vec![];
        },
    }
}


pub fn get_completions<'a, 'b>(dfa: &DFA, words_before_cursor: &'b [&'a str], completed_word_index: usize, shell: Shell) -> Vec<(String, String)> {
    let prefix = if completed_word_index < words_before_cursor.len() {
        words_before_cursor[completed_word_index]
    }
    else {
        ""
    };

    let state_id = match get_match_final_state(dfa, words_before_cursor, completed_word_index) {
        Some(state_id) => state_id,
        None => return vec![],
    };

    let mut completions: Vec<(String, String)> = dfa.transitions.get(&state_id).unwrap_or(&HashMap::default()).iter().map(|(input, _)| get_completions_for_input(input, prefix, shell)).flatten().collect();
    completions.sort_unstable();
    completions
}


#[cfg(test)]
mod tests {
    use bumpalo::Bump;
    use hashbrown::HashSet;

    use crate::{grammar::{Grammar, ValidGrammar}, regex::AugmentedRegex, dfa::DFA};

    use super::*;

    fn get_grammar_completions<'a, 'b>(grammar: &str, words_before_cursor: &'b [&'a str], completed_word_index: usize) -> Vec<(String, String)> {
        let g = Grammar::parse(grammar).unwrap();
        let validated = ValidGrammar::from_grammar(g).unwrap();
        let arena = Bump::new();
        let regex = AugmentedRegex::from_expr(&validated.expr, &validated.specializations, &arena);
        let dfa = DFA::from_regex(&regex);
        let dfa = dfa.minimize();
        get_completions(&dfa, words_before_cursor, completed_word_index, Shell::Bash)
    }

    #[test]
    fn completes_darcs_add() {
        const GRAMMAR: &str = r#"darcs add ( --boring | ( --case-ok | --reserved-ok ) | ( ( -r | --recursive ) | --not-recursive ) | ( --date-trick | --no-date-trick ) | --repodir <DIRECTORY> | --dry-run | --umask <UMASK> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... ( <FILE> | <DIRECTORY> )...;"#;
        assert_eq!(get_grammar_completions(GRAMMAR, &[], 0), vec![("add".to_string(), "".to_string())]);

        let input = vec!["add"];
        let generated: HashSet<_> = HashSet::from_iter(get_grammar_completions(GRAMMAR, &input, 1).into_iter().map(|(completion, _)| completion));
        let expected = HashSet::from_iter(["--boring", "--debug", "--dry-run", "--no-prehook", "--prehook", "--quiet", "--reserved-ok", "--standard-verbosity", "--verbose", "-v", "--case-ok", "--debug-http", "--no-date-trick", "--not-recursive", "--prompt-posthook", "--recursive", "--run-posthook", "--timings", "-q", "--date-trick", "--debug-verbose", "--no-posthook", "--posthook", "--prompt-prehook", "--repodir", "--run-prehook", "--umask", "-r"].map(|s| s.to_string()));
        assert_eq!(generated, expected);
    }

    #[test]
    fn does_not_hang_on_many1_of_optional() {
        const GRAMMAR: &str = r#"grep [--help]...;"#;
        let input = vec!["--version"];
        let generated: HashSet<_> = HashSet::from_iter(get_grammar_completions(GRAMMAR, &input, 1));
        assert!(generated.is_empty());
    }

    #[test]
    fn falls_through_optionals() {
        const GRAMMAR: &str = r#"
grep [<OPTION>]...;
<OPTION> ::= (--color [<WHEN>]) | --extended-regexp;
<WHEN> ::= always | never | auto;
"#;
        let input = vec!["--color"];
        let generated: HashSet<_> = HashSet::from_iter(get_grammar_completions(GRAMMAR, &input, 1).into_iter().map(|(completion, _)| completion));
        let expected = HashSet::from_iter(["always", "auto", "never", "--extended-regexp", "--color"].map(|s| s.to_string()));
        assert_eq!(generated, expected);
    }

    #[test]
    fn completes_after_command() {
        const GRAMMAR: &str = r#"
cargo [<toolchain>] (--version | --help);
<toolchain> ::= { rustup toolchain list | cut -d' ' -f1 | sed 's/^/+/' };
"#;
        let input = vec!["foo"];
        let generated: HashSet<_> = HashSet::from_iter(get_grammar_completions(GRAMMAR, &input, 1).into_iter().map(|(completion, _)| completion));
        let expected = HashSet::from_iter(["--version", "--help"].map(|s| s.to_string()));
        assert_eq!(generated, expected);
    }

    #[test]
    fn completes_after_variable() {
        const GRAMMAR: &str = r#"
grep (--context "print NUM lines of output context" <NUM> | --version | --help)...;
"#;
        let input = vec!["--context", "123"];
        let generated: HashSet<_> = HashSet::from_iter(get_grammar_completions(GRAMMAR, &input, 2).into_iter().map(|(completion, _)| completion));
        let expected = HashSet::from_iter(["--version", "--help", "--context"].map(|s| s.to_string()));
        assert_eq!(generated, expected);
    }

    #[test]
    fn completes_word_prefix() {
        const GRAMMAR: &str = r#"
grep (--help | --version);
"#;
        let input = vec!["--h"];
        let generated: HashSet<_> = HashSet::from_iter(get_grammar_completions(GRAMMAR, &input, 0).into_iter().map(|(completion, _)| completion));
        let expected = HashSet::from_iter(["--help"].map(|s| s.to_string()));
        assert_eq!(generated, expected);
    }
}

use std::ffi::OsStr;
use std::{io::Write, process::Output};
use std::process::Command;

use crate::StateId;

use ustr::ustr;
use anyhow::{anyhow, Context, bail};

use crate::grammar::Specialization;
use crate::{dfa::DFA, regex::Input};



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
        let result = anyhow::Result::Err(anyhow!("Command invocation failed"))
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
    writeln!(capture_script, "{}", completion_code.replace('\'', "''"))?;
    writeln!(capture_script, r#"}}"#)?;
    writeln!(capture_script)?;
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


fn capture_specialized_completions(shell: Shell, specialization: &Specialization, prefix: &str, output: &mut Vec<(String, String)>) -> anyhow::Result<()> {
    let stdout = match shell {
        Shell::Bash => {
            let Some(command) = specialization.bash.or(specialization.generic) else {
                return Ok(());
            };
            get_bash_command_stdout(&command)?
        },
        Shell::Fish => {
            let Some(command) = specialization.fish.or(specialization.generic) else {
                return Ok(());
            };
            get_fish_command_stdout(&command)?
        },
        Shell::Zsh => {
            if let Some(command) = specialization.zsh {
                capture_zsh_completions(&command, "dummy", "dummy ")?
            }
            else if let Some(command) = specialization.generic {
                get_zsh_command_stdout(&command)?
            }
            else {
                return Ok(());
            }
        },
    };

    for line in stdout.lines() {
        if !line.starts_with(prefix) {
            continue;
        }
        let (completion, description) = match line.split_once('\t') {
            Some((completion, description)) => (completion.to_owned(), description.to_owned()),
            None => (line.to_string(), "".to_string()),
        };
        output.push((completion, description));
    }
    Ok(())
}


pub fn get_subword_match_final_state<'a>(dfa: &DFA, mut remaining_input: &'a str) -> Option<(StateId, &'a str)> {
    let mut current_state = dfa.starting_state;
    'outer: while !remaining_input.is_empty() {
        for (transition_input, to) in dfa.iter_transitions_from(current_state) {
            if let Input::Literal(s, _) = transition_input {
                if remaining_input.starts_with(s.as_str()) {
                    remaining_input = &remaining_input[s.len()..];
                    current_state = to;
                    continue 'outer;
                }
            }
        }

        for (transition_input, _) in dfa.iter_transitions_from(current_state) {
            if transition_input.matches_anything() {
                remaining_input = "";
                continue 'outer;
            }
        }

        break;
    }
    Some((current_state, remaining_input))
}



fn get_completions_for_input(input: &Input, entered_prefix: &str, shell: Shell, output: &mut Vec<(String, String)>) -> anyhow::Result<()> {
    match input {
        Input::Subword(subword_dfa) => {
            let (state, remaining_input) = match get_subword_match_final_state(subword_dfa.as_ref(), entered_prefix) {
                Some(state) => state,
                None => return Ok(()),
            };
            for (input, _) in subword_dfa.as_ref().iter_transitions_from(state) {
                get_completions_for_input(&input, remaining_input, shell, output)?;
            }
        },

        Input::Literal(literal, description) if literal.starts_with(entered_prefix) => output.push((literal.as_str().to_string(), description.unwrap_or(ustr("")).as_str().to_string())),
        Input::Literal(_, _) => {},

        Input::Command(command) => {
            let stdout = shell.shell_out(command.as_str())?;
            for line in stdout.lines() {
                if !line.starts_with(entered_prefix) {
                    continue;
                }
                let (completion, description) = match line.split_once('\t') {
                    Some((completion, description)) => (completion.to_owned(), description.to_owned()),
                    None => (line.to_owned(), "".to_owned()),
                };
                output.push((completion, description));
            }
        },

        Input::Nonterminal(_, None) => {},

        Input::Nonterminal(_, Some(specialization)) => capture_specialized_completions(shell, specialization, entered_prefix, output)?
    }
    Ok(())
}


pub fn get_completions<'a, 'b>(dfa: &DFA, words: &'b [&'a str], completed_word_index: usize, shell: Shell) -> anyhow::Result<Vec<(String, String)>> {
    let prefix = if completed_word_index < words.len() {
        words[completed_word_index]
    }
    else if completed_word_index == words.len() {
        ""
    } else {
        bail!("Trying to complete a word too far beyond the last one");
    };

    // Match words up to `completed_word_index`
    let mut word_index = 0;
    let mut state = dfa.starting_state;
    assert!(completed_word_index <= words.len()); // an invariant
    'outer: while word_index < words.len() && word_index < completed_word_index {
        for (transition_input, to) in dfa.iter_transitions_from(state) {
            if let Input::Literal(s, _) = transition_input {
                if words[word_index] == s.as_str() {
                    word_index += 1;
                    state = to;
                    continue 'outer;
                }
            }
        }

        for (transition_input, to) in dfa.iter_transitions_from(state) {
            if let Input::Subword(dfa) = transition_input {
                if dfa.as_ref().accepts_str(words[word_index]) {
                    word_index += 1;
                    state = to;
                    continue 'outer;
                }
            }
        }

        for (transition_input, to) in dfa.iter_transitions_from(state) {
            if transition_input.matches_anything() {
                word_index += 1;
                state = to;
                continue 'outer;
            }
        }

        return Ok(vec![]);
    }

    // Complete `prefix` based on `state`.
    let mut output: Vec<(String, String)> = Default::default();
    for (input, _) in dfa.iter_transitions_from(state) {
        get_completions_for_input(&input, prefix, shell, &mut output)?;
    }
    output.sort_unstable();
    Ok(output)
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
        get_completions(&dfa, words_before_cursor, completed_word_index, Shell::Bash).unwrap()
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

    #[test]
    fn completes_nested_prefix() {
        const GRAMMAR: &str = r#"
dummy --prefix=<SUFFIX>;
<SUFFIX> ::= another-prefix=<ANOTHER-SUFFIX>;
<ANOTHER-SUFFIX> ::= foo | bar;
"#;
        let input = vec!["--prefix="];
        let generated: HashSet<_> = HashSet::from_iter(get_grammar_completions(GRAMMAR, &input, 0).into_iter().map(|(completion, _)| completion));
        let expected = HashSet::from_iter(["another-prefix="].map(|s| s.to_string()));
        assert_eq!(generated, expected);
    }

    #[test]
    fn matches_prefix() {
        const GRAMMAR: &str = r#"
    cargo +<toolchain> foo;
    cargo test --test testname;
    <toolchain> ::= stable-aarch64-apple-darwin | stable-x86_64-apple-darwin;
"#;
        let input = vec!["+stable-aarch64-apple-darwin"];
        let generated: HashSet<_> = HashSet::from_iter(get_grammar_completions(GRAMMAR, &input, 1).into_iter().map(|(completion, _)| completion));
        let expected = HashSet::from_iter(["foo"].map(|s| s.to_string()));
        assert_eq!(generated, expected);
    }

    #[test]
    fn completes_subword_prefix() {
        const GRAMMAR: &str = r#"
strace -e <EXPR>;
<EXPR> ::= [<qualifier>=][!]<value>[,<value>]...;
<qualifier> ::= trace | read | write | fault;
<value> ::= %file | file | all;
"#;
        let input = vec!["-e", "tr"];
        let generated: HashSet<_> = HashSet::from_iter(get_grammar_completions(GRAMMAR, &input, 1).into_iter().map(|(completion, _)| completion));
        let expected = HashSet::from_iter(["trace"].map(|s| s.to_string()));
        assert_eq!(generated, expected);
    }
}

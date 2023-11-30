use std::ffi::OsStr;
use std::{io::Write, process::Output};
use std::process::Command;

use crate::StateId;

use ustr::ustr;
use anyhow::{anyhow, Context};

use crate::grammar::Specialization;
use crate::{dfa::DFA, regex::Input};



#[derive(Debug, Clone, Copy)]
pub enum Shell {
    Bash,
    Fish,
    Zsh,
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Completion {
    pub matched_subword_prefix: String,
    pub completed_subword_suffix: String,
    pub description: String,
    pub is_shell_word_ending: bool,
}


impl Completion {
    pub fn get_completion(&self) -> String {
        format!("{}{}", self.matched_subword_prefix, self.completed_subword_suffix)
    }

    pub fn get_completion_with_trailing_space(&self) -> String {
        let space = if self.is_shell_word_ending { " " } else { "" };
        format!("{}{}{space}", self.matched_subword_prefix, self.completed_subword_suffix)
    }

    // Subword completions may not have a description coming from a grammar but we still pass *a*
    // description to compadd that indicates the suffix completed.  So "description" should be
    // understood broadly here.
    pub fn has_zsh_description(&self) -> bool {
        !self.matched_subword_prefix.is_empty() || !self.description.is_empty()
    }
}


fn shell_out_bash(command: &str, prefix: &str) -> anyhow::Result<Output> {
    let mut script_file = tempfile::NamedTempFile::new()?;
    writeln!(script_file, r#"_complgen_complete_function () {{"#)?;
    writeln!(script_file, "{}", command)?;
    writeln!(script_file, r#"}}"#)?;
    writeln!(script_file, r#"_complgen_complete_function "{}""#, prefix)?;
    script_file.flush()?;
    Command::new("bash").arg("--noprofile").arg(script_file.path()).output().map_err(Into::into)
}


fn shell_out_fish(command: &str, prefix: &str) -> anyhow::Result<Output> {
    let mut script_file = tempfile::NamedTempFile::new()?;
    writeln!(script_file, r#"function _complgen_complete_function"#)?;
    writeln!(script_file, r#"set 1 $argv[1]"#)?;
    writeln!(script_file, "{}", command)?;
    writeln!(script_file, r#"end"#)?;
    writeln!(script_file, r#"_complgen_complete_function "{}""#, prefix)?;
    script_file.flush()?;
    Command::new("fish").arg("--private").arg("--no-config").arg(script_file.path()).output().map_err(Into::into)
}


fn shell_out_zsh(command: &str, prefix: &str) -> anyhow::Result<Output> {
    let mut script_file = tempfile::NamedTempFile::new()?;
    writeln!(script_file, r#"_complgen_complete_function () {{"#)?;
    writeln!(script_file, "{}", command)?;
    writeln!(script_file, r#"}}"#)?;
    writeln!(script_file, r#"_complgen_complete_function "{}""#, prefix)?;
    script_file.flush()?;
    Command::new("zsh").arg(script_file.path()).output().map_err(Into::into)
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


fn get_bash_command_stdout(command: &str, prefix: &str) -> anyhow::Result<String> {
    let output = shell_out_bash(command, prefix).with_context(|| command.to_string())?;
    stdout_from_output(output)
}

fn get_fish_command_stdout(command: &str, prefix: &str) -> anyhow::Result<String> {
    let output = shell_out_fish(command, prefix).with_context(|| command.to_string())?;
    stdout_from_output(output)
}

fn get_zsh_command_stdout(command: &str, prefix: &str) -> anyhow::Result<String> {
    let output = shell_out_zsh(command, prefix).with_context(|| command.to_string())?;
    stdout_from_output(output)
}

fn get_zsh_script_stdout<P: AsRef<OsStr>>(script_path: P, arg: &str) -> anyhow::Result<String> {
    let output = Command::new("zsh").arg(script_path).arg(arg).output()?;
    stdout_from_output(output)
}


fn capture_zsh_completions(completion_code: &str, command: &str, prefix: &str) -> anyhow::Result<String> {
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

    get_zsh_script_stdout(capture_script.path(), prefix)
        .with_context(|| completion_code.to_string())
        .with_context(|| command.to_string())
        .with_context(|| prefix.to_string())
}


impl Shell {
    fn shell_out(&self, command: &str, prefix: &str) -> anyhow::Result<String> {
        let output = match self {
            Shell::Bash => shell_out_bash(command, prefix)?,
            Shell::Fish => shell_out_fish(command, prefix)?,
            Shell::Zsh => shell_out_zsh(command, prefix)?,
        };

        stdout_from_output(output)
    }

    fn completion_matches(&self, completion: &str, prefix: &str) -> bool {
        match self {
            Shell::Bash | Shell::Fish => completion.starts_with(prefix),
            Shell::Zsh => true,
        }
    }
}


fn capture_specialized_completions(shell: Shell, specialization: &Specialization, prefix: &str, output: &mut Vec<Completion>) -> anyhow::Result<()> {
    let stdout = match shell {
        Shell::Bash => {
            let Some(command) = specialization.bash.or(specialization.generic) else {
                return Ok(());
            };
            get_bash_command_stdout(&command, prefix)?
        },
        Shell::Fish => {
            let Some(command) = specialization.fish.or(specialization.generic) else {
                return Ok(());
            };
            get_fish_command_stdout(&command, prefix)?
        },
        Shell::Zsh => {
            if let Some(command) = specialization.zsh {
                capture_zsh_completions(&command, "dummy", &format!("dummy {}", prefix))?
            }
            else if let Some(command) = specialization.generic {
                get_zsh_command_stdout(&command, prefix)?
            }
            else {
                return Ok(());
            }
        },
    };

    for line in stdout.lines() {
        if !shell.completion_matches(line, prefix) {
            continue;
        }
        let (completion, description) = match line.split_once('\t') {
            Some((completion, description)) => (completion.to_owned(), description.to_owned()),
            None => (line.to_string(), "".to_string()),
        };
        output.push(Completion {
            matched_subword_prefix: "".to_string(),
            completed_subword_suffix: completion,
            description,
            is_shell_word_ending: false,
        });
    }
    Ok(())
}


pub fn get_subword_match_final_state<'a>(dfa: &DFA, input: &'a str) -> Option<(StateId, &'a str, &'a str)> {
    let mut matched_input = "";
    let mut remaining_input = input;
    let mut current_state = dfa.starting_state;
    'outer: while !remaining_input.is_empty() {
        for (transition_input, to) in dfa.iter_transitions_from(current_state) {
            if let Input::Literal(s, _) = transition_input {
                if remaining_input.starts_with(s.as_str()) {
                    matched_input = &input[..matched_input.len() + s.len()];
                    remaining_input = &remaining_input[s.len()..];
                    current_state = to;
                    continue 'outer;
                }
            }
        }

        for (transition_input, _) in dfa.iter_transitions_from(current_state) {
            if transition_input.matches_anything() {
                break;
            }
        }

        break;
    }
    Some((current_state, matched_input, remaining_input))
}


fn do_gen_completions_for_input(input: &Input, prefix: &str, shell: Shell, output: &mut Vec<Completion>) -> anyhow::Result<()> {
    match input {
        Input::Literal(literal, description) if shell.completion_matches(literal, prefix) => {
            let description = description.unwrap_or(ustr("")).as_str().to_string();
            output.push(Completion {
                matched_subword_prefix: "".to_string(),
                completed_subword_suffix: literal.to_string(),
                description,
                is_shell_word_ending: true,
            })
        },

        Input::Literal(_, _) => {},

        Input::Command(command) => {
            let stdout = shell.shell_out(command.as_str(), prefix)?;
            for line in stdout.lines() {
                if !shell.completion_matches(line, prefix) {
                    continue;
                }
                let (completion, description) = match line.split_once('\t') {
                    Some((completion, description)) => (completion.to_owned(), description.to_owned()),
                    None => (line.to_owned(), "".to_owned()),
                };
                output.push(Completion {
                    matched_subword_prefix: "".to_string(),
                    completed_subword_suffix: completion,
                    description,
                    is_shell_word_ending: true,
                });
            }
        },

        Input::Nonterminal(_, None) => {},

        Input::Nonterminal(_, Some(specialization)) => {
            capture_specialized_completions(shell, specialization, prefix, output)?
        },

        Input::Subword(_) => unreachable!(),
    }
    Ok(())
}


fn do_gen_subword_completions_for_input(input: &Input, matched_subword_prefix: &str, prefix_to_match: &str, shell: Shell, is_final_subword_transition: bool, output: &mut Vec<Completion>) -> anyhow::Result<()> {
    match input {
        Input::Literal(literal, description) if literal.starts_with(prefix_to_match) => {
            let description = description.unwrap_or(ustr("")).as_str().to_string();
            output.push(Completion {
                matched_subword_prefix: matched_subword_prefix.to_string(),
                completed_subword_suffix: literal.to_string(),
                description,
                is_shell_word_ending: is_final_subword_transition,
            })
        },

        Input::Literal(_, _) => {},

        Input::Command(command) => {
            let stdout = shell.shell_out(command.as_str(), "")?;
            for line in stdout.lines() {
                if !line.starts_with(prefix_to_match) {
                    continue;
                }
                let (completion, description) = match line.split_once('\t') {
                    Some((completion, description)) => (completion.to_owned(), description.to_owned()),
                    None => (line.to_owned(), "".to_owned()),
                };
                output.push(Completion {
                    matched_subword_prefix: matched_subword_prefix.to_string(),
                    completed_subword_suffix: completion,
                    description,
                    is_shell_word_ending: is_final_subword_transition,
                });
            }
        },

        Input::Nonterminal(_, None) => {},

        Input::Nonterminal(_, Some(specialization)) => {
            capture_specialized_completions(shell, specialization, "", output)?
        },

        Input::Subword(_) => unreachable!(),
    }
    Ok(())
}


fn get_completions_for_input(input: &Input, prefix: &str, shell: Shell, output: &mut Vec<Completion>) -> anyhow::Result<()> {
    match input {
        Input::Subword(subword_dfa) => {
            let (state, matched_input, remaining_input) = match get_subword_match_final_state(subword_dfa.as_ref(), prefix) {
                Some(state) => state,
                None => return Ok(()),
            };
            let subdfa = subword_dfa.as_ref();
            for (input, to) in subdfa.iter_transitions_from(state) {
                let is_final_subword_transition = subdfa.iter_transitions_from(to).next().is_none();
                do_gen_subword_completions_for_input(&input, matched_input, remaining_input, shell, is_final_subword_transition, output)?;
            }
        },

        _ => do_gen_completions_for_input(input, prefix, shell, output)?,
    }
    Ok(())
}


pub fn get_completions(dfa: &DFA, words: &[&str], prefix: &str, shell: Shell) -> anyhow::Result<Vec<Completion>> {
    // Match words up to `completed_word_index`
    let mut word_index = 0;
    let mut state = dfa.starting_state;
    'outer: while word_index < words.len() {
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
    let mut output: Vec<Completion> = Default::default();
    for (input, _) in dfa.iter_transitions_from(state) {
        get_completions_for_input(&input, prefix, shell, &mut output)?;
    }
    output.sort_unstable_by(|left, right| left.get_completion().cmp(&right.get_completion()));
    Ok(output)
}


#[cfg(test)]
mod tests {
    use bumpalo::Bump;
    use hashbrown::HashSet;

    use crate::{grammar::{Grammar, ValidGrammar}, regex::AugmentedRegex, dfa::DFA};

    use super::*;

    fn get_bash_grammar_completions<'a, 'b>(grammar: &str, words_before_cursor: &'b [&'a str], prefix: &str) -> Vec<Completion> {
        let g = Grammar::parse(grammar).map_err(|e| e.to_string()).unwrap();
        let validated = ValidGrammar::from_grammar(g).unwrap();
        let arena = Bump::new();
        let regex = AugmentedRegex::from_expr(&validated.expr, &validated.specializations, &arena);
        let dfa = DFA::from_regex(&regex);
        let dfa = dfa.minimize();
        get_completions(&dfa, words_before_cursor, prefix, Shell::Bash).unwrap()
    }

    #[test]
    fn completes_darcs_add() {
        const GRAMMAR: &str = r#"darcs add ( --boring | ( --case-ok | --reserved-ok ) | ( ( -r | --recursive ) | --not-recursive ) | ( --date-trick | --no-date-trick ) | --repodir <DIRECTORY> | --dry-run | --umask <UMASK> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... ( <FILE> | <DIRECTORY> )...;"#;
        assert_eq!(get_bash_grammar_completions(GRAMMAR, &[], ""), vec![Completion {matched_subword_prefix: "".to_string(), completed_subword_suffix: "add".to_string(), description: "".to_string(), is_shell_word_ending: true}]);

        let input = vec!["add"];
        let generated: HashSet<_> = HashSet::from_iter(get_bash_grammar_completions(GRAMMAR, &input, "").into_iter().map(|completion| completion.get_completion()));
        let expected = HashSet::from_iter(["--boring", "--debug", "--dry-run", "--no-prehook", "--prehook", "--quiet", "--reserved-ok", "--standard-verbosity", "--verbose", "-v", "--case-ok", "--debug-http", "--no-date-trick", "--not-recursive", "--prompt-posthook", "--recursive", "--run-posthook", "--timings", "-q", "--date-trick", "--debug-verbose", "--no-posthook", "--posthook", "--prompt-prehook", "--repodir", "--run-prehook", "--umask", "-r"].map(|s| s.to_string()));
        assert_eq!(generated, expected);
    }

    #[test]
    fn does_not_hang_on_many1_of_optional() {
        const GRAMMAR: &str = r#"grep [--help]...;"#;
        let input = vec!["--version"];
        let generated: HashSet<_> = HashSet::from_iter(get_bash_grammar_completions(GRAMMAR, &input, "").into_iter().map(|completion| completion.get_completion()));
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
        let generated: HashSet<_> = HashSet::from_iter(get_bash_grammar_completions(GRAMMAR, &input, "").into_iter().map(|completion| completion.get_completion()));
        let expected = HashSet::from_iter(["always", "auto", "never", "--extended-regexp", "--color"].map(|s| s.to_string()));
        assert_eq!(generated, expected);
    }

    #[test]
    fn completes_after_command() {
        const GRAMMAR: &str = r#"
cargo [<toolchain>] (--version | --help);
<toolchain> ::= {{{ rustup toolchain list | cut -d' ' -f1 | sed 's/^/+/' }}};
"#;
        let input = vec!["foo"];
        let generated: HashSet<_> = HashSet::from_iter(get_bash_grammar_completions(GRAMMAR, &input, "").into_iter().map(|completion| completion.get_completion()));
        let expected = HashSet::from_iter(["--version", "--help"].map(|s| s.to_string()));
        assert_eq!(generated, expected);
    }


    #[test]
    fn does_not_complete_subword_after_command() {
        const GRAMMAR: &str = r#"cargo +{{{ echo -e "bar\nbaz\nquux" }}};"#;
        let input = vec![];
        let generated: HashSet<_> = HashSet::from_iter(get_bash_grammar_completions(GRAMMAR, &input, "+foo").into_iter().map(|completion| completion.get_completion()));
        let expected = HashSet::from_iter([].map(|s: String| s.to_string()));
        assert_eq!(generated, expected);
    }

    #[test]
    fn completes_after_variable() {
        const GRAMMAR: &str = r#"
grep (--context "print NUM lines of output context" <NUM> | --version | --help)...;
"#;
        let input = vec!["--context", "123"];
        let generated: HashSet<_> = HashSet::from_iter(get_bash_grammar_completions(GRAMMAR, &input, "").into_iter().map(|completion| completion.get_completion()));
        let expected = HashSet::from_iter(["--version", "--help", "--context"].map(|s| s.to_string()));
        assert_eq!(generated, expected);
    }

    #[test]
    fn completes_word_prefix() {
        const GRAMMAR: &str = r#"
grep (--help | --version);
"#;
        let input = vec![];
        let generated: HashSet<_> = HashSet::from_iter(get_bash_grammar_completions(GRAMMAR, &input, "--h").into_iter().map(|completion| completion.get_completion()));
        let expected = HashSet::from_iter(["--help"].map(|s| s.to_string()));
        assert_eq!(generated, expected);
    }

    #[test]
    fn completes_in_word() {
        const GRAMMAR: &str = r#"
cmd prefix-infix-suffix;
"#;
        let input = vec![];
        let generated: HashSet<_> = HashSet::from_iter(get_bash_grammar_completions(GRAMMAR, &input, "prefix-").into_iter().map(|completion| completion.get_completion()));
        let expected = HashSet::from_iter(["prefix-infix-suffix"].map(|s| s.to_string()));
        assert_eq!(generated, expected);
    }


    #[test]
    fn completes_nested_prefix() {
        const GRAMMAR: &str = r#"
dummy --prefix=<SUFFIX>;
<SUFFIX> ::= another-prefix=<ANOTHER-SUFFIX>;
<ANOTHER-SUFFIX> ::= foo | bar;
"#;
        let input = vec![];
        let generated: HashSet<_> = HashSet::from_iter(get_bash_grammar_completions(GRAMMAR, &input, "--prefix=").into_iter().map(|completion| completion.get_completion()));
        let expected = HashSet::from_iter(["--prefix=another-prefix="].map(|s| s.to_string()));
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
        let generated: HashSet<_> = HashSet::from_iter(get_bash_grammar_completions(GRAMMAR, &input, "").into_iter().map(|completion| completion.get_completion()));
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
        let input = vec!["-e"];
        let generated: HashSet<_> = HashSet::from_iter(get_bash_grammar_completions(GRAMMAR, &input, "tr").into_iter().map(|completion| completion.get_completion()));
        let expected = HashSet::from_iter(["trace"].map(|s| s.to_string()));
        assert_eq!(generated, expected);
    }
}

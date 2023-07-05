use std::process::{Command, Output};

use complgen::StateId;
use hashbrown::HashMap;

use ustr::ustr;

use crate::{dfa::DFA, regex::{Input, MatchAnythingInput}};



#[derive(Debug, Clone, Copy)]
pub enum Shell {
    Bash,
    Fish,
    Zsh,
}


impl Shell {
    fn shell_out(&self, command: &str) -> std::io::Result<Output> {
        match self {
            Shell::Bash => Command::new("bash").arg("-c").arg(command).output(),
            Shell::Fish => Command::new("fish").arg("-c").arg(command).output(),
            Shell::Zsh => Command::new("zsh").arg("-c").arg(command).output(),
        }
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


fn get_completions_for_input<'a, 'b>(input: &Input, words_before_cursor: &'b [&'a str], completed_word_index: usize, shell: Shell) -> Vec<(String, String)> {
    match input {
        Input::Literal(literal, description) => vec![(literal.as_str().to_string(), description.unwrap_or(ustr("")).as_str().to_string())],

        Input::Any(MatchAnythingInput::Command(command)) => {
            let output = match shell.shell_out(command.as_str()) {
                Ok(output) => output,
                Err(e) => {
                    eprintln!("{:?}", e);
                    return vec![];
                },
            };
            if !output.status.success() {
                eprint!("{}", String::from_utf8_lossy(&output.stdout));
                eprint!("{}", String::from_utf8_lossy(&output.stderr));
                return vec![];
            }

            let stdout = match String::from_utf8(output.stdout) {
                Ok(stdout) => stdout,
                Err(e) => {
                    eprintln!("{:?}", e);
                    return vec![];
                },
            };
            let mut result: Vec<(String, String)> = stdout.lines().map(|line| match line.split_once("\t") {
                Some((completion, description)) => (completion.to_owned(), description.to_owned()),
                None => (line.to_string(), "".to_string()),
            }).collect();

            if completed_word_index < words_before_cursor.len() {
                result.retain(|(completion, _)| completion.starts_with(words_before_cursor[completed_word_index]));
            }

            result
        },

        Input::Any(MatchAnythingInput::Nonterminal(nonterm)) if nonterm.as_str() == "FILE" || nonterm.as_str() == "PATH" => {
            let prefix = if completed_word_index < words_before_cursor.len() {
                words_before_cursor[completed_word_index]
            }
            else {
                ""
            };
            let cmd = format!(r#"printf "%s\n" {}*"#, prefix);
            let output = std::process::Command::new("sh").arg("-c").arg(&cmd).output().unwrap();
            let result: Vec<(String, String)> = String::from_utf8(output.stdout).unwrap().lines().filter(|line| !line.is_empty()).map(|line| (line.to_string(), "".to_string())).collect();
            result
        },

        Input::Any(MatchAnythingInput::Nonterminal(_)) => vec![],
    }
}


pub fn get_completions<'a, 'b>(dfa: &DFA, words_before_cursor: &'b [&'a str], completed_word_index: usize, shell: Shell) -> Vec<(String, String)> {
    let state_id = match get_match_final_state(dfa, words_before_cursor, completed_word_index) {
        Some(state_id) => state_id,
        None => return vec![],
    };

    let mut completions: Vec<(String, String)> = dfa.transitions.get(&state_id).unwrap_or(&HashMap::default()).iter().map(|(input, _)| get_completions_for_input(input, words_before_cursor, completed_word_index, shell)).flatten().collect();
    if completed_word_index < words_before_cursor.len() {
        completions.retain(|(completion, _)| completion.starts_with(words_before_cursor[completed_word_index]));
    }
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
        let regex = AugmentedRegex::from_expr(&validated.expr, &arena);
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

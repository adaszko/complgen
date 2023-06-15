use complgen::StateId;
use hashbrown::HashMap;

use crate::{dfa::DFA, regex::Input};


pub fn get_match_final_state(dfa: &DFA, inputs: &[&str]) -> Option<StateId> {
    let mut backtracking_stack = Vec::from_iter([(0, dfa.starting_state)]);
    while let Some((input_index, current_state)) = backtracking_stack.pop() {
        if input_index >= inputs.len() {
            return Some(current_state);
        }

        for (transition_input, to) in dfa.transitions.get(&current_state).unwrap_or(&HashMap::default()) {
            if let Input::Any = transition_input {
                backtracking_stack.push((input_index + 1, *to));
            }
        }

        for (transition_input, to) in dfa.transitions.get(&current_state).unwrap_or(&HashMap::default()) {
            if let Input::Literal(s) = transition_input {
                if s.as_str() == inputs[input_index] {
                    backtracking_stack.push((input_index + 1, *to));
                }
            }
        }
    }
    None
}


pub fn get_completions<'a, 'b>(dfa: &DFA, words_before_cursor: &'b [&'a str]) -> Vec<&'a str> {
    if let Some(state_id) = get_match_final_state(dfa, words_before_cursor) {
        let mut inputs: Vec<&str> = dfa.transitions.get(&state_id).unwrap_or(&HashMap::default()).iter().filter_map(|(input, _)| match input {
            Input::Literal(s) => Some(s.as_str()),
            Input::Any => None,
        }).collect();
        inputs.sort_unstable();
        inputs
    }
    else {
        vec![]
    }
}


#[cfg(test)]
mod tests {
    use bumpalo::Bump;
    use hashbrown::HashSet;

    use crate::{grammar::parse, regex::AugmentedRegex, dfa::DFA};

    use super::*;

    #[test]
    fn completes_darcs_add() {
        const GRAMMAR: &str = r#"darcs add ( --boring | ( --case-ok | --reserved-ok ) | ( ( -r | --recursive ) | --not-recursive ) | ( --date-trick | --no-date-trick ) | --repodir <DIRECTORY> | --dry-run | --umask <UMASK> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... ( <FILE> | <DIRECTORY> )...;"#;
        let g = parse(GRAMMAR).unwrap();
        let validated = g.validate().unwrap();
        let arena = Bump::new();
        let regex = AugmentedRegex::from_expr(&validated.expr, &arena);
        let dfa = DFA::from_regex(&regex);
        let dfa = dfa.minimize();
        assert_eq!(get_completions(&dfa, &vec![]), vec!["add"]);

        let input = vec!["add"];
        let generated: HashSet<&str> = HashSet::from_iter(get_completions(&dfa, &input));
        let expected = HashSet::from_iter(["--boring", "--debug", "--dry-run", "--no-prehook", "--prehook", "--quiet", "--reserved-ok", "--standard-verbosity", "--verbose", "-v", "--case-ok", "--debug-http", "--no-date-trick", "--not-recursive", "--prompt-posthook", "--recursive", "--run-posthook", "--timings", "-q", "--date-trick", "--debug-verbose", "--no-posthook", "--posthook", "--prompt-prehook", "--repodir", "--run-prehook", "--umask", "-r"]);
        assert_eq!(generated, expected);
    }

    #[test]
    fn does_not_hang_on_many1_of_optional() {
        const GRAMMAR: &str = r#"grep [--help]...;"#;
        let g = parse(GRAMMAR).unwrap();
        let validated = g.validate().unwrap();
        let arena = Bump::new();
        let regex = AugmentedRegex::from_expr(&validated.expr, &arena);
        let dfa = DFA::from_regex(&regex);
        let dfa = dfa.minimize();
        let input = vec!["--version"];
        let generated: HashSet<&str> = HashSet::from_iter(get_completions(&dfa, &input));
        assert!(generated.is_empty());
    }

    #[test]
    fn falls_through_optionals() {
        const GRAMMAR: &str = r#"
grep [<OPTION>]...;
<OPTION> ::= (--color [<WHEN>]) | --extended-regexp;
<WHEN> ::= always | never | auto;
"#;
        let g = parse(GRAMMAR).unwrap();
        let validated = g.validate().unwrap();
        let arena = Bump::new();
        let regex = AugmentedRegex::from_expr(&validated.expr, &arena);
        let dfa = DFA::from_regex(&regex);
        let dfa = dfa.minimize();
        let input = vec!["--color"];
        let generated: HashSet<&str> = HashSet::from_iter(get_completions(&dfa, &input));
        let expected = HashSet::from_iter(["always", "auto", "never", "--extended-regexp", "--color"]);
        assert_eq!(generated, expected);
    }
}

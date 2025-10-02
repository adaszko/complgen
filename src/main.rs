use std::ffi::OsStr;
use std::io::{BufWriter, Read, Write};
use std::path::Path;
use std::process::exit;

use anyhow::{Context, bail};
use clap::Parser;

use complgen::grammar::{Grammar, HumanSpan, Shell, ValidGrammar};

use complgen::Error;
use complgen::dfa::DFA;
use complgen::regex::{Regex, diagnostic_display_input};

#[derive(clap::Parser)]
struct Cli {
    #[clap(
        long,
        help = "Read another program's --help output and generate a grammar *skeleton*"
    )]
    scrape: bool,

    #[clap(long, help = "Show version and exit")]
    version: bool,

    usage_file_path: Option<String>,

    #[clap(long, help = "Write bash completion script", name = "BASH_SCRIPT_PATH")]
    bash: Option<String>,

    #[clap(long, help = "Write fish completion script", name = "FISH_SCRIPT_PATH")]
    fish: Option<String>,

    #[clap(long, help = "Write zsh completion script", name = "ZSH_SCRIPT_PATH")]
    zsh: Option<String>,

    #[clap(
        long,
        help = "Write regex in GraphViz .dot format",
        name = "REGEX_DOT_PATH"
    )]
    regex: Option<String>,

    #[clap(
        long,
        help = "Write DFA in GraphViz .dot format",
        name = "DFA_DOT_PATH"
    )]
    dfa: Option<String>,
}

fn get_file_or_stdin(path: &str) -> anyhow::Result<Box<dyn Read>> {
    let result: Box<dyn Read> = if path == "-" {
        Box::new(std::io::stdin())
    } else {
        Box::new(std::fs::File::open(path).context(path.to_owned())?)
    };
    Ok(result)
}

fn handle_parse_error(input: &str) -> anyhow::Result<Grammar> {
    match Grammar::parse(input) {
        Ok(g) => Ok(g),
        Err(e) => {
            eprintln!("{}", e.to_string());
            exit(1);
        }
    }
}

fn handle_validation_error(e: Error, input: &str, command: &str) -> anyhow::Result<()> {
    match e {
        Error::VaryingCommandNames(cmds) => {
            let joined = itertools::join(cmds.into_iter().map(|c| format!("{c}")), ", ");
            eprintln!("Multiple commands specified: {joined}");
        }
        Error::SubwordSpaces(left, right, trace) => {
            let HumanSpan {
                line_start: left_line_start,
                start: left_start,
                end: left_end,
            } = left.unwrap();
            let HumanSpan {
                line_start: right_line_start,
                end: right_end,
                start: right_start,
            } = right.unwrap();
            let error = chic::Error::new(
                "Adjacent literals in expression used in a subword context.  First one:",
            )
            .error(
                left_line_start,
                left_start,
                left_end,
                input.lines().nth(left_line_start).unwrap(),
                "",
            );
            eprintln!("{}:{}:{}", left_line_start, left_start, error.to_string());

            let right_error = chic::Error::new("Second one:").error(
                right_line_start,
                right_start,
                right_end,
                input.lines().nth(right_line_start).unwrap(),
                "",
            ).help("Join the adjacent literals into one as spaces are invalid in a subword context");
            eprintln!(
                "{}:{}:{}",
                right_line_start,
                right_start,
                right_error.to_string()
            );

            for t in trace {
                let HumanSpan {
                    line_start,
                    start,
                    end,
                } = t;
                let e = chic::Error::new("Referenced in a subword context at").error(
                    line_start,
                    start,
                    end,
                    input.lines().nth(line_start).unwrap(),
                    "",
                );
                eprintln!("{}:{}:{}", line_start, start, e.to_string());
            }
        }
        Error::AmbiguousMatchable(first, second) => {
            let Some(HumanSpan {
                line_start: left_line_start,
                start: left_start,
                end: left_end,
            }) = first.get_span()
            else {
                unreachable!()
            };
            let Some(HumanSpan {
                line_start: right_line_start,
                start: right_start,
                end: right_end,
            }) = second.get_span()
            else {
                unreachable!()
            };
            let error = chic::Error::new("Ambiguous grammar.  Matching can't differentiate:")
                .error(
                    left_line_start,
                    left_start,
                    left_end,
                    input.lines().nth(left_line_start).unwrap(),
                    "",
                );
            eprintln!("{}:{}:{}", left_line_start, left_start, error.to_string());
            let error = chic::Error::new("and:").error(
                right_line_start,
                right_start,
                right_end,
                input.lines().nth(right_line_start).unwrap(),
                "",
            );
            eprintln!("{}:{}:{}", right_line_start, right_start, error.to_string());
        }
        Error::AmbiguousDFA(path, ambiguous_inputs) => {
            let joined_path = {
                let mut buf = String::new();
                for (i, inp) in path.iter().enumerate() {
                    diagnostic_display_input(&mut buf, inp)?;
                    if i < path.len() - 1 {
                        buf.push(' ');
                    }
                }
                buf
            };
            eprintln!("Ambiguity:");
            for input in ambiguous_inputs {
                let mut buffer = String::new();
                diagnostic_display_input(&mut buffer, &input)?;
                eprintln!("  {command} {joined_path} {buffer}");
            }
        }
        Error::ClashingVariants(first, second) => {
            let Some(HumanSpan {
                line_start: left_line_start,
                start: left_start,
                end: left_end,
            }) = first
            else {
                unreachable!()
            };
            let Some(HumanSpan {
                line_start: right_line_start,
                start: right_start,
                end: right_end,
            }) = second
            else {
                unreachable!()
            };
            let error = chic::Error::new("Clashing variants.  Completion can't differentiate:")
                .error(
                    left_line_start,
                    left_start,
                    left_end,
                    input.lines().nth(left_line_start).unwrap(),
                    "",
                );
            eprintln!("{}:{}:{}", left_line_start, left_start, error.to_string());
            let error = chic::Error::new("and:").error(
                right_line_start,
                right_start,
                right_end,
                input.lines().nth(right_line_start).unwrap(),
                "",
            );
            eprintln!("{}:{}:{}", right_line_start, right_start, error.to_string());
        }
        e => {
            eprintln!("{}", e);
        }
    }
    exit(1);
}

fn get_file_or_stdout(path: &str) -> anyhow::Result<Box<dyn Write>> {
    let result: Box<dyn Write> = if path == "-" {
        Box::new(std::io::stdout())
    } else {
        Box::new(std::fs::File::create(path).context(path.to_owned())?)
    };
    Ok(result)
}

fn aot(args: &Cli) -> anyhow::Result<()> {
    let Some(ref usage_file_path) = args.usage_file_path else {
        bail!("Missing usage file path argument")
    };

    let input = {
        let mut usage_file = get_file_or_stdin(usage_file_path)?;
        let mut input = String::default();
        usage_file
            .read_to_string(&mut input)
            .context(usage_file_path.to_owned())?;
        input
    };

    let grammar = handle_parse_error(&input)?;

    let (shell, path) = match (&args.bash, &args.fish, &args.zsh) {
        (Some(path), None, None) => (Shell::Bash, path),
        (None, Some(path), None) => (Shell::Fish, path),
        (None, None, Some(path)) => (Shell::Zsh, path),

        _ => {
            eprintln!("Please specify at least one of: --bash, --fish, --zsh");
            exit(1);
        }
    };

    let validated = match ValidGrammar::from_grammar(grammar, shell) {
        Ok(validated) => validated,
        Err(e) => return handle_validation_error(e, &input, "dummy"),
    };

    if !validated.undefined_nonterminals.is_empty() {
        let joined = itertools::join(&validated.undefined_nonterminals, " ");
        eprintln!("warning: undefined nonterminal(s): {}", joined);
    }

    if !validated.unused_nonterminals.is_empty() {
        let joined = itertools::join(&validated.unused_nonterminals, " ");
        eprintln!("warning: unused nonterminal(s): {}", joined);
    }

    let regex = match Regex::from_valid_grammar(&validated, shell) {
        Ok(regex) => regex,
        Err(e) => return handle_validation_error(e, &input, &validated.command),
    };

    if let Some(regex_dot_file_path) = &args.regex {
        let mut dot_file = get_file_or_stdout(regex_dot_file_path)?;
        regex
            .to_dot(&mut dot_file)
            .context(regex_dot_file_path.clone())?;
    }

    let dfa = match DFA::from_regex_strict(regex, validated.subdfas) {
        Ok(dfa) => dfa,
        Err(e) => return handle_validation_error(e, &input, &validated.command),
    };

    let dfa = dfa.minimize();

    if let Some(dot_file_path) = &args.dfa {
        let mut dot_file = get_file_or_stdout(dot_file_path)?;
        dfa.to_dot(&mut dot_file).context(dot_file_path.clone())?;
    }

    let script_file = get_file_or_stdout(path)?;
    let mut writer = BufWriter::new(script_file);

    match shell {
        Shell::Bash => {
            complgen::bash::write_completion_script(&mut writer, &validated.command, &dfa)?
        }
        Shell::Fish => {
            complgen::fish::write_completion_script(&mut writer, &validated.command, &dfa)?
        }
        Shell::Zsh => {
            let expected_name = format!("_{}", validated.command);
            if path != "-"
                && Path::new(path).file_name().unwrap_or_default() != OsStr::new(&expected_name)
            {
                eprintln!(
                    "warning: ZSH requires the output script to be named {expected_name:?} for autoloading to work"
                );
            }
            complgen::zsh::write_completion_script(&mut writer, &validated.command, &dfa)?;
        }
    }

    Ok(())
}

fn scrape() -> anyhow::Result<()> {
    let input: String = {
        let mut input = String::default();
        std::io::stdin().read_to_string(&mut input)?;
        input
    };

    let mut writer = BufWriter::new(std::io::stdout());
    complgen::scrape::scrape(&input, &mut writer)?;
    Ok(())
}

fn main() -> anyhow::Result<()> {
    let args = Cli::parse();

    if args.version {
        println!("{}", env!("COMPLGEN_VERSION"));
        return Ok(());
    }

    if args.scrape {
        return scrape();
    }

    aot(&args)
}

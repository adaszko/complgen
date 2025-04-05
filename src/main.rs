use std::ffi::OsStr;
use std::io::{BufWriter, Read, Write};
use std::path::Path;
use std::process::exit;

use anyhow::{Context, bail};
use bumpalo::Bump;
use clap::Parser;

use complgen::grammar::{ChicSpan, Grammar, ValidGrammar, to_railroad_diagram_file};

use complgen::Error;
use complgen::dfa::DFA;
use complgen::regex::AugmentedRegex;

#[derive(clap::Parser)]
struct Cli {
    #[clap(
        long,
        help = "Read another program's --help output and generate a grammar SKELETON"
    )]
    scrape: bool,

    #[clap(long, help = "Show version and exit")]
    version: bool,

    usage_file_path: Option<String>,

    #[clap(long, help = "Write bash completion script")]
    bash: Option<String>,

    #[clap(long, help = "Write fish completion script")]
    fish: Option<String>,

    #[clap(long, help = "Write zsh completion script")]
    zsh: Option<String>,

    #[clap(long, help = "Dump resulting DFA as a GraphViz .dot format")]
    dfa: Option<String>,

    #[clap(long, help = "Dump the parsed grammar as SVG railroad diagram")]
    railroad: Option<String>,
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
    match Grammar::parse(&input) {
        Ok(g) => Ok(g),
        Err(e) => {
            eprintln!("{}", e.to_string());
            exit(1);
        }
    }
}

fn handle_validation_error(g: Grammar, input: &str) -> anyhow::Result<ValidGrammar> {
    match ValidGrammar::from_grammar(g) {
        Ok(g) => Ok(g),
        Err(Error::NontailCommand(
            _,
            complgen::grammar::ChicSpan::Significant {
                line_start,
                start,
                end,
            },
        )) => {
            let error = chic::Error::new("External commands are only allowed at tail position")
                .error(
                    line_start,
                    start,
                    end,
                    input.lines().nth(line_start).unwrap(),
                    "",
                )
                .help("Either try moving the command into tail position (i.e. last branch of | or ||; end of a subword)")
                .help("or output the suffix from the external command")
                .help(r#"or specify regex the output needs to match via @SHELL"..." construct"#);
            eprintln!("{}:{}:{}", line_start, start, error.to_string());
            exit(1);
        }
        Err(Error::NontailUndefNonterm(
            _,
            complgen::grammar::ChicSpan::Significant {
                line_start,
                start,
                end,
            },
        )) => {
            let error = chic::Error::new("Undefined nonterminals are only allowed at tail position")
                .error(
                    line_start,
                    start,
                    end,
                    input.lines().nth(line_start).unwrap(),
                    "",
                )
                .help("Either try moving the command into tail position (i.e. last branch of | or ||; end of a subword)")
                .help("or supply its definition via ::=");
            eprintln!("{}:{}:{}", line_start, start, error.to_string());
            exit(1);
        }
        Err(Error::NontailCommand(_, complgen::grammar::ChicSpan::Dummy)) => {
            unreachable!()
        }
        Err(Error::SubwordSpaces(
            ChicSpan::Significant {
                line_start: left_line_start,
                start: left_start,
                end: left_end,
            },
            ChicSpan::Significant {
                line_start: right_line_start,
                end: right_end,
                start: right_start,
            },
            trace,
        )) => {
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
                let ChicSpan::Significant {
                    line_start,
                    start,
                    end,
                } = t
                else {
                    unreachable!()
                };
                let e = chic::Error::new("Referenced in a subword context at").error(
                    line_start,
                    start,
                    end,
                    input.lines().nth(line_start).unwrap(),
                    "",
                );
                eprintln!("{}:{}:{}", line_start, start, e.to_string());
            }
            exit(1);
        }
        Err(e) => {
            eprintln!("{}", e.to_string());
            exit(1);
        }
    }
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
        let mut usage_file = get_file_or_stdin(&usage_file_path)?;
        let mut input = String::default();
        usage_file
            .read_to_string(&mut input)
            .context(usage_file_path.to_owned())?;
        input
    };

    let grammar = handle_parse_error(&input)?;

    if let Some(railroad_svg_path) = &args.railroad {
        to_railroad_diagram_file(&grammar, railroad_svg_path).context(railroad_svg_path.clone())?;
    }

    let validated = handle_validation_error(grammar, &input)?;

    if !validated.undefined_nonterminals.is_empty() {
        let joined = itertools::join(validated.undefined_nonterminals, " ");
        eprintln!("Warning: Undefined nonterminal(s): {}", joined);
    }

    if !validated.unused_nonterminals.is_empty() {
        let joined = itertools::join(validated.unused_nonterminals, " ");
        eprintln!("Warning: Unused nonterminal(s): {}", joined);
    }

    let arena = Bump::new();

    log::debug!("Grammar -> Regex");
    let regex = AugmentedRegex::from_expr(&validated.expr, &validated.specializations, &arena);

    log::debug!("Regex -> DFA");
    let dfa = DFA::from_regex(&regex);

    log::debug!("Minimizing DFA");
    let dfa = dfa.minimize();

    if let Some(dot_file_path) = &args.dfa {
        let mut dot_file = get_file_or_stdout(dot_file_path)?;
        dfa.to_dot(&mut dot_file).context(dot_file_path.clone())?;
    }

    if dfa.is_ambiguous(validated.command) {
        exit(1);
    };

    if let Some(path) = &args.bash {
        log::debug!("Writing Bash completion script");
        let script_file = get_file_or_stdout(path)?;
        let mut writer = BufWriter::new(script_file);
        complgen::bash::write_completion_script(&mut writer, &validated.command, &dfa)?;
    }

    if let Some(path) = &args.fish {
        log::debug!("Writing Fish completion script");
        let script_file = get_file_or_stdout(path)?;
        let mut writer = BufWriter::new(script_file);
        complgen::fish::write_completion_script(&mut writer, &validated.command, &dfa)?;
    }

    if let Some(path) = &args.zsh {
        log::debug!("Writing Zsh completion script");
        let expected_name = format!("_{}", validated.command);
        if path != "-"
            && Path::new(path).file_name().unwrap_or_default() != OsStr::new(&expected_name)
        {
            eprintln!(
                "Warning: ZSH requires the output script to be named {expected_name:?} for autoloading to work"
            );
        }
        let script_file = get_file_or_stdout(path)?;
        let mut writer = BufWriter::new(script_file);
        complgen::zsh::write_completion_script(&mut writer, &validated.command, &dfa)?;
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
    env_logger::init();
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

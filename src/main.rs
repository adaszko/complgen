use std::ffi::OsStr;
use std::io::{BufWriter, Read, Write};
use std::path::Path;
use std::process::exit;

use anyhow::Context;
use bumpalo::Bump;
use clap::Parser;

use complgen::grammar::{Grammar, ValidGrammar, to_railroad_diagram_file};

use complgen::dfa::DFA;
use complgen::regex::AugmentedRegex;
use complgen::{Error, check_dfa_ambiguity};

#[derive(clap::Parser)]
struct Cli {
    #[clap(subcommand)]
    mode: Mode,
}

#[derive(clap::Subcommand)]
enum Mode {
    #[command(about = "Do not complete -- only check a grammar file for errors")]
    Check(CheckArgs),

    #[command(about = "Write autocompletions shell script file")]
    Aot(AotArgs),

    #[command(about = "Read `cmd --help` output of another command and emit a grammar")]
    Scrape,

    #[command(about = "Show version and exit")]
    Version,
}

#[derive(clap::Args)]
struct CheckArgs {
    usage_file_path: String,
}

#[derive(clap::Args)]
struct AotArgs {
    usage_file_path: String,

    #[clap(long)]
    bash_script: Option<String>,

    #[clap(long)]
    fish_script: Option<String>,

    #[clap(long)]
    zsh_script: Option<String>,

    #[clap(long)]
    dfa_dot: Option<String>,

    #[clap(long)]
    railroad_svg: Option<String>,
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
        Err(Error::NontailCommand(_, complgen::grammar::ChicSpan::Dummy)) => {
            unreachable!()
        }
        Err(e) => {
            eprintln!("{}", e.to_string());
            exit(1);
        }
    }
}

fn check(args: &CheckArgs) -> anyhow::Result<()> {
    let mut input_file =
        get_file_or_stdin(&args.usage_file_path).context(args.usage_file_path.to_owned())?;
    let mut input: String = Default::default();
    input_file.read_to_string(&mut input)?;
    let grammar = handle_parse_error(&input)?;
    let validated = handle_validation_error(grammar, &input)?;
    let arena = Bump::new();
    let regex = AugmentedRegex::from_expr(&validated.expr, &validated.specializations, &arena);
    let dfa = DFA::from_regex(&regex);
    check_dfa_ambiguity(&dfa);
    Ok(())
}

fn get_file_or_stdout(path: &str) -> anyhow::Result<Box<dyn Write>> {
    let result: Box<dyn Write> = if path == "-" {
        Box::new(std::io::stdout())
    } else {
        Box::new(std::fs::File::create(path).context(path.to_owned())?)
    };
    Ok(result)
}

fn aot(args: &AotArgs) -> anyhow::Result<()> {
    if let (None, None, None, None, None) = (
        &args.railroad_svg,
        &args.dfa_dot,
        &args.bash_script,
        &args.fish_script,
        &args.zsh_script,
    ) {
        eprintln!(
            "Please specify at least one of --railroad-svg, --dfa-dot, --bash-script, --fish-script, --zsh-script options"
        );
        std::process::exit(1);
    }

    let input = {
        let mut usage_file = get_file_or_stdin(&args.usage_file_path)?;
        let mut input = String::default();
        usage_file
            .read_to_string(&mut input)
            .context(args.usage_file_path.to_owned())?;
        input
    };

    let grammar = handle_parse_error(&input)?;

    if let Some(railroad_svg_path) = &args.railroad_svg {
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

    if let Some(dot_file_path) = &args.dfa_dot {
        let mut dot_file = get_file_or_stdout(dot_file_path)?;
        dfa.to_dot(&mut dot_file).context(dot_file_path.clone())?;
    }

    check_dfa_ambiguity(&dfa);

    if let Some(path) = &args.bash_script {
        log::debug!("Writing Bash completion script");
        let script_file = get_file_or_stdout(path)?;
        let mut writer = BufWriter::new(script_file);
        complgen::bash::write_completion_script(&mut writer, &validated.command, &dfa)?;
    }

    if let Some(path) = &args.fish_script {
        log::debug!("Writing Fish completion script");
        let script_file = get_file_or_stdout(path)?;
        let mut writer = BufWriter::new(script_file);
        complgen::fish::write_completion_script(&mut writer, &validated.command, &dfa)?;
    }

    if let Some(path) = &args.zsh_script {
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
    match args.mode {
        Mode::Check(args) => check(&args)?,
        Mode::Aot(args) => aot(&args)?,
        Mode::Scrape => scrape()?,
        Mode::Version => {
            println!("{}", env!("COMPLGEN_VERSION"));
        }
    };
    Ok(())
}

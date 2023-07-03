use std::io::{BufWriter, Write, Read};
use std::rc::Rc;

use anyhow::Context;
use bumpalo::Bump;
use clap::Parser;

use grammar::{ValidGrammar, Grammar};

use crate::dfa::DFA;
use crate::regex::AugmentedRegex;

mod grammar;
mod dfa;
mod bash;
mod zsh;
mod fish;
mod complete;
mod regex;


#[derive(clap::Parser)]
struct Cli {
    #[clap(subcommand)]
    mode: Mode,
}


#[derive(clap::Subcommand)]
enum Mode {
    Complete(CompleteArgs),
    Compile(CompileArgs),
}

#[derive(clap::Args)]
struct CompleteArgs {
    usage_file_path: String,
    args: Vec<String>,

    #[clap(long)]
    railroad_svg: Option<String>,
}


#[derive(clap::Args)]
struct CompileArgs {
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


fn complete(args: &CompleteArgs) -> anyhow::Result<()> {
    let input = std::fs::read_to_string(&args.usage_file_path).context(args.usage_file_path.to_owned())?;
    let grammar = Grammar::parse(&input)?;
    let validated = ValidGrammar::from_grammar(grammar)?;

    if let Some(railroad_svg_path) = &args.railroad_svg {
        grammar::to_railroad_diagram_file(Rc::clone(&validated.expr), railroad_svg_path)?;
    }

    let arena = Bump::new();
    let regex = AugmentedRegex::from_expr(&validated.expr, &arena);
    let dfa = DFA::from_regex(&regex);

    let words_before_cursor: Vec<&str> = args.args.iter().map(|s| s.as_ref()).collect();
    for completion in complete::get_completions(&dfa, &words_before_cursor) {
        println!("{}", completion);
    }
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


fn compile(args: &CompileArgs) -> anyhow::Result<()> {
    if args.railroad_svg.is_none() && args.dfa_dot.is_none() && args.bash_script.is_none() && args.fish_script.is_none() && args.zsh_script.is_none() {
        eprintln!("Please specify at least one of --railroad-svg, --dfa-dot, --bash-script, --fish-script, --zsh-script options");
        std::process::exit(1);
    }

    let input: String = if args.usage_file_path == "-" {
        let mut input: String = Default::default();
        std::io::stdin().read_to_string(&mut input)?;
        input
    } else {
        std::fs::read_to_string(&args.usage_file_path).context(args.usage_file_path.to_owned())?
    };
    let grammar = Grammar::parse(&input)?;
    let validated = ValidGrammar::from_grammar(grammar)?;

    if !validated.undefined_nonterminals.is_empty() {
        let joined = itertools::join(validated.undefined_nonterminals, " ");
        eprintln!("Warning: Undefined nonterminal(s): {}", joined);
    }

    let arena = Bump::new();

    if let Some(railroad_svg_path) = &args.railroad_svg {
        grammar::to_railroad_diagram_file(Rc::clone(&validated.expr), railroad_svg_path).context(railroad_svg_path.clone())?;
    }

    log::debug!("Grammar -> Regex");
    let regex = AugmentedRegex::from_expr(&validated.expr, &arena);

    log::debug!("Regex -> DFA");
    let dfa = DFA::from_regex(&regex);

    log::debug!("Minimizing DFA");
    let dfa = dfa.minimize();

    if let Some(dot_file_path) = &args.dfa_dot {
        let mut dot_file = get_file_or_stdout(&dot_file_path)?;
        dfa.to_dot(&mut dot_file).context(dot_file_path.clone())?;
    }

    if let Some(path) = &args.bash_script {
        log::debug!("Writing Bash completion script");
        let script_file = get_file_or_stdout(path)?;
        let mut writer = BufWriter::new(script_file);
        bash::write_completion_script(&mut writer, &validated.command, &dfa)?;
    }

    if let Some(path) = &args.fish_script {
        log::debug!("Writing Fish completion script");
        let script_file = get_file_or_stdout(path)?;
        let mut writer = BufWriter::new(script_file);
        fish::write_completion_script(&mut writer, &validated.command, &dfa)?;
    }

    if let Some(path) = &args.zsh_script {
        log::debug!("Writing Zsh completion script");
        let script_file = get_file_or_stdout(path)?;
        let mut writer = BufWriter::new(script_file);
        zsh::write_completion_script(&mut writer, &validated.command, &dfa)?;
    }

    Ok(())
}


fn main() -> anyhow::Result<()> {
    env_logger::init();
    let args = Cli::parse();
    match args.mode {
        Mode::Complete(args) => complete(&args)?,
        Mode::Compile(args) => compile(&args)?,
    };
    Ok(())
}

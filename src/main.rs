use std::rc::Rc;

use bumpalo::Bump;
use clap::Parser;

use complgen::Result;
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


fn complete(args: &CompleteArgs) -> Result<()> {
    let input = std::fs::read_to_string(&args.usage_file_path).unwrap();
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


fn compile(args: &CompileArgs) -> Result<()> {
    if args.railroad_svg.is_none() && args.dfa_dot.is_none() && args.bash_script.is_none() && args.fish_script.is_none() && args.zsh_script.is_none() {
        eprintln!("Please specify at least one of --railroad-svg, --dfa-dot, --bash-script, --fish-script, --zsh-script options");
        std::process::exit(1);
    }

    let input = std::fs::read_to_string(&args.usage_file_path).unwrap();
    let grammar = Grammar::parse(&input)?;
    let validated = ValidGrammar::from_grammar(grammar)?;
    let arena = Bump::new();

    if let Some(railroad_svg_path) = &args.railroad_svg {
        grammar::to_railroad_diagram_file(Rc::clone(&validated.expr), railroad_svg_path)?;
    }

    log::debug!("Grammar -> Regex");
    let regex = AugmentedRegex::from_expr(&validated.expr, &arena);

    log::debug!("Regex -> DFA");
    let dfa = DFA::from_regex(&regex);

    log::debug!("Minimizing DFA");
    let dfa = dfa.minimize();

    if let Some(dot_file_path) = &args.dfa_dot {
        dfa.to_dot_file(dot_file_path)?;
    }

    if let Some(path) = &args.bash_script {
        log::debug!("Writing Bash completion script");
        let mut bash_completion_script = std::fs::File::create(path).unwrap();
        bash::write_completion_script(&mut bash_completion_script, &validated.command, &dfa).unwrap();
    }

    if let Some(path) = &args.fish_script {
        log::debug!("Writing Fish completion script");
        let mut fish_completion_script = std::fs::File::create(path).unwrap();
        fish::write_completion_script(&mut fish_completion_script, &validated.command, &dfa).unwrap();
    }

    if let Some(path) = &args.zsh_script {
        log::debug!("Writing Zsh completion script");
        let mut zsh_completion_script = std::fs::File::create(path).unwrap();
        zsh::write_completion_script(&mut zsh_completion_script, &validated.command, &dfa).unwrap();
    }

    Ok(())
}


fn main() -> Result<()> {
    env_logger::init();
    let args = Cli::parse();
    match args.mode {
        Mode::Complete(args) => complete(&args)?,
        Mode::Compile(args) => compile(&args)?,
    };
    Ok(())
}

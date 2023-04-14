use std::io::Write;

use clap::Parser;

use complgen::{Result};

use crate::dfa::DFA;
use crate::nfa::NFA;
use crate::grammar::parse;
use crate::epsilon_nfa::EpsilonNFA;

mod grammar;
mod epsilon_nfa;
mod nfa;
mod dfa;
mod bash;
mod fish;
mod complete;


#[derive(clap::Parser)]
struct Cli {
    #[clap(subcommand)]
    mode: Mode,
}


#[derive(clap::Subcommand)]
enum Mode {
    Complete(CompleteArgs),
    Compile,
}


#[derive(clap::Args)]
struct CompleteArgs {
    usage_file_path: String,
    args: Vec<String>,
}


fn complete(args: &[&str], usage_file_path: &str) -> Result<()> {
    let input = std::fs::read_to_string(usage_file_path).unwrap();
    let grammar = parse(&input)?;
    let (_, expr) = grammar.into_command_expr();
    for completion in complete::get_completions(&expr, args) {
        println!("{}", completion);
    }
    Ok(())
}


fn compile() -> Result<()> {
    let input = std::io::read_to_string(std::io::stdin()).unwrap();
    let grammar = parse(&input)?;
    let (command, expr) = grammar.into_command_expr();

    println!("Grammar -> EpsilonNFA");
    let epsilon_nfa = EpsilonNFA::from_expr(&expr);

    println!("EpsilonNFA -> NFA");
    let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);

    println!("NFA -> DFA");
    let dfa = DFA::from_nfa(&nfa);

    let mut output = String::default();

    {
        println!("Writing Bash completion script");
        bash::write_completion_script(&mut output, &command, &dfa).unwrap();
        let mut bash_completion_script = std::fs::File::create(format!("{command}.bash")).unwrap();
        bash_completion_script.write_all(output.as_bytes()).unwrap();
    }

    output.clear();

    {
        println!("Writing Fish completion script");
        fish::write_completion_script(&mut output, &command, &dfa).unwrap();
        let mut fish_completion_script = std::fs::File::create(format!("{command}.fish")).unwrap();
        fish_completion_script.write_all(output.as_bytes()).unwrap();
    }

    Ok(())
}


fn main() -> Result<()> {
    let args = Cli::parse();
    match args.mode {
        Mode::Complete(args) => {
            let v: Vec<&str> = args.args.iter().map(|s| s.as_ref()).collect();
            complete(&v, &args.usage_file_path)?
        },
        Mode::Compile => compile()?,
    };
    Ok(())
}

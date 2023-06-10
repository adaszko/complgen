use std::io::Write;

use bumpalo::Bump;
use clap::Parser;

use complgen::Result;

use crate::dfa::DFA;
use crate::grammar::parse;
use crate::regex::AugmentedRegex;

mod grammar;
mod dfa;
mod bash;
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
}


#[derive(clap::Args)]
struct CompileArgs {
    usage_file_path: String,

    #[clap(long)]
    bash_script_path: Option<String>,

    #[clap(long)]
    fish_script_path: Option<String>,
}


fn complete(args: &[&str], usage_file_path: &str) -> Result<()> {
    let input = std::fs::read_to_string(usage_file_path).unwrap();
    let grammar = parse(&input)?;
    let validated = grammar.validate()?;
    for completion in complete::get_completions(&validated.expr, args) {
        println!("{}", completion);
    }
    Ok(())
}


fn compile(args: &CompileArgs) -> Result<()> {
    let input = std::fs::read_to_string(&args.usage_file_path).unwrap();
    let grammar = parse(&input)?;
    let validated = grammar.validate()?;
    let arena = Bump::new();

    println!("Grammar -> Regex");
    let regex = AugmentedRegex::from_expr(&validated.expr, &arena);

    println!("Regex -> DFA");
    let dfa = DFA::from_regex(&regex);

    println!("Minimizing DFA");
    let dfa = dfa.minimize();

    let mut output = String::default();

    if let Some(path) = &args.bash_script_path {
        println!("Writing Bash completion script");
        bash::write_completion_script(&mut output, &validated.command, &dfa).unwrap();
        let mut bash_completion_script = std::fs::File::create(path).unwrap();
        bash_completion_script.write_all(output.as_bytes()).unwrap();
    }

    output.clear();

    if let Some(path) = &args.fish_script_path {
        println!("Writing Fish completion script");
        fish::write_completion_script(&mut output, &validated.command, &dfa).unwrap();
        let mut fish_completion_script = std::fs::File::create(path).unwrap();
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
        Mode::Compile(args) => compile(&args)?,
    };
    Ok(())
}

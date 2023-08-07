use std::io::{BufWriter, Write, Read};

use anyhow::Context;
use bumpalo::Bump;
use clap::Parser;

use complgen::complete::get_completions;
use complgen::grammar::{ValidGrammar, Grammar, to_railroad_diagram, to_railroad_diagram_file};

use complgen::dfa::DFA;
use complgen::regex::AugmentedRegex;
use complgen::zsh::make_string_constant;

#[derive(clap::Parser)]
struct Cli {
    #[clap(subcommand)]
    mode: Mode,
}


#[derive(clap::Subcommand)]
enum Mode {
    Complete(CompleteArgs),
    Compile(CompileArgs),
    Scrape,
}

#[derive(clap::Args)]
struct CompleteShellArgs {
    completed_word_index: usize,
    words: Vec<String>,
}

#[derive(clap::Subcommand)]
enum Shell {
    Bash(CompleteShellArgs),
    Fish(CompleteShellArgs),
    Zsh(CompleteShellArgs),
}

#[derive(clap::Args)]
struct CompleteArgs {
    #[clap(long)]
    railroad_svg: Option<String>,

    usage_file_path: String,

    #[clap(subcommand)]
    shell: Shell,
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


fn get_file_or_stdin(path: &str) -> anyhow::Result<Box<dyn Read>> {
    let result: Box<dyn Read> = if path == "-" {
        Box::new(std::io::stdin())
    } else {
        Box::new(std::fs::File::open(path).context(path.to_owned())?)
    };
    Ok(result)
}


fn complete(args: &CompleteArgs) -> anyhow::Result<()> {
    let mut input_file = get_file_or_stdin(&args.usage_file_path).context(args.usage_file_path.to_owned())?;
    let mut input: String = Default::default();
    input_file.read_to_string(&mut input)?;
    let grammar = Grammar::parse(&input)?;

    if let Some(railroad_svg_path) = &args.railroad_svg {
        let mut railroad_svg = get_file_or_stdout(&railroad_svg_path)?;
        to_railroad_diagram(&grammar, &mut railroad_svg)?;
    }

    let validated = ValidGrammar::from_grammar(grammar)?;

    let arena = Bump::new();
    let regex = AugmentedRegex::from_expr(&validated.expr, &validated.specializations, &arena);
    let dfa = DFA::from_regex(&regex);

    let (shell, completed_word_index, words) = match &args.shell {
        Shell::Bash(a) => (complgen::complete::Shell::Bash, a.completed_word_index, &a.words),
        Shell::Fish(a) => (complgen::complete::Shell::Fish, a.completed_word_index, &a.words),
        Shell::Zsh(a) => (complgen::complete::Shell::Zsh, a.completed_word_index, &a.words),
    };

    let words_before_cursor: Vec<&str> = words.iter().map(|s| s.as_ref()).collect();

    let completions = get_completions(&dfa, &words_before_cursor, completed_word_index, shell)?;

    match args.shell {
        Shell::Bash(_) => {
            for (completion, _) in completions {
                println!("{}", completion);
            }
        },
        Shell::Fish(_) => {
            for (completion, description) in completions {
                println!("{}\t{}", completion, description);
            }
        },
        Shell::Zsh(_) => {
            let completions_array_initializer = itertools::join(completions.iter().map(|(completion, _)| make_string_constant(completion)), " ");
            println!(r#"local -a completions=({completions_array_initializer})"#);

            let descriptions_array_initializer = itertools::join(completions.iter().map(|(completion, description)| {
                if !description.is_empty() {
                    make_string_constant(&format!("{} ({})", completion, description))
                }
                else {
                    make_string_constant(completion)
                }
            }), " ");
            println!(r#"local -a descriptions=({descriptions_array_initializer})"#);

            println!(r#"compadd -Q -S '' -d descriptions -a completions"#);
        },
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
    match (&args.railroad_svg, &args.dfa_dot, &args.bash_script, &args.fish_script, &args.zsh_script) {
        (None, None, None, None, None) => {
            eprintln!("Please specify at least one of --railroad-svg, --dfa-dot, --bash-script, --fish-script, --zsh-script options");
            std::process::exit(1);
        },
        _ => {},
    }

    let input = {
        let mut usage_file = get_file_or_stdin(&args.usage_file_path)?;
        let mut input = String::default();
        usage_file.read_to_string(&mut input).context(args.usage_file_path.to_owned())?;
        input
    };

    let grammar = Grammar::parse(&input)?;

    if let Some(railroad_svg_path) = &args.railroad_svg {
        to_railroad_diagram_file(&grammar, railroad_svg_path).context(railroad_svg_path.clone())?;
    }

    let validated = ValidGrammar::from_grammar(grammar)?;

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
        let mut dot_file = get_file_or_stdout(&dot_file_path)?;
        dfa.to_dot(&mut dot_file).context(dot_file_path.clone())?;
    }

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

    let exprs = complgen::scrape::scrape(&input)?;
    complgen::scrape::pretty_print(&exprs);
    Ok(())
}


fn main() -> anyhow::Result<()> {
    env_logger::init();
    let args = Cli::parse();
    match args.mode {
        Mode::Complete(args) => complete(&args)?,
        Mode::Compile(args) => compile(&args)?,
        Mode::Scrape => scrape()?,
    };
    Ok(())
}

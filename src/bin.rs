use std::io::{BufWriter, Write, Read};
use std::process::exit;

use anyhow::Context;
use bumpalo::Bump;
use clap::Parser;

use complgen::complete::{get_completions, Completion};
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
    #[command(about = "Do not complete -- only check a grammar file for errors")]
    Check(CheckArgs),

    #[command(about = "Emit completions on stdout")]
    Complete(CompleteArgs),

    #[command(about = "Write autocompletions shell script file")]
    Compile(CompileArgs),

    #[command(about = "Read `cmd --help` output of another command and emit a grammar")]
    Scrape,
}

#[derive(clap::Args)]
struct CheckArgs {
    usage_file_path: String,
}

#[derive(clap::Args)]
struct CompleteShellArgs {
    #[arg(long)]
    prefix: Option<String>,

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


fn handle_parse_error(input: &str) -> anyhow::Result<Grammar> {
    match Grammar::parse(&input) {
        Ok(g) => Ok(g),
        Err(e) => {
            eprintln!("{}", e.to_string());
            exit(1);
        },
    }
}


fn check(args: &CheckArgs) -> anyhow::Result<()> {
    let mut input_file = get_file_or_stdin(&args.usage_file_path).context(args.usage_file_path.to_owned())?;
    let mut input: String = Default::default();
    input_file.read_to_string(&mut input)?;
    let grammar = handle_parse_error(&input)?;
    let validated = ValidGrammar::from_grammar(grammar)?;
    let arena = Bump::new();
    let regex = AugmentedRegex::from_expr(&validated.expr, &validated.specializations, &arena);
    let _ = DFA::from_regex(&regex);
    Ok(())
}


fn zsh_compadd(completions: &[&Completion], trailing_space: bool) {
    if completions.is_empty() {
        return;
    }

    let completions_array_initializer = itertools::join(completions.iter().map(|completion| make_string_constant(&completion.get_completion())), " ");
    println!(r#"local -a completions=({completions_array_initializer})"#);
    if trailing_space {
        println!(r#"compadd -Q -a completions"#);
    }
    else {
        println!(r#"compadd -Q -S '' -a completions"#);
    }
}


fn zsh_compadd_with_description(completions: &[&Completion], trailing_space: bool) {
    if completions.is_empty() {
        return;
    }

    let completions_array_initializer = itertools::join(completions.iter().map(|completion| make_string_constant(&completion.get_completion())), " ");
    println!(r#"local -a completions=({completions_array_initializer})"#);

    let descriptions_array_initializer = itertools::join(completions.iter().map(|completion| make_string_constant(&completion.get_zsh_compadd_description())), " ");
    println!(r#"local -a descriptions=({descriptions_array_initializer})"#);

    if trailing_space {
        println!(r#"compadd -Q -a -d descriptions completions"#);
    }
    else {
        println!(r#"compadd -Q -S '' -a -d descriptions completions"#);
    }
}


fn zsh_describe(completions: &[&Completion], trailing_space: bool) {
    if completions.is_empty() {
        return;
    }

    let completions_array_initializer = itertools::join(completions.iter().map(|completion| make_string_constant(&completion.get_completion())), " ");
    println!(r#"local -a completions=({completions_array_initializer})"#);

    let descriptions: Vec<String> = completions.iter().map(|compl| {
        format!("{compl}:{descr}", compl = compl.completed_subword_suffix.replace(':', "\\:"), descr = compl.description)
    }).collect();

    let descriptions_array_initializer = itertools::join(descriptions.into_iter().map(|s| make_string_constant(&s)), " ");
    println!(r#"local -a descriptions=({descriptions_array_initializer})"#);
    if trailing_space {
        println!(r#"_describe '' descriptions completions -Q"#);
    }
    else {
        println!(r#"_describe '' descriptions completions -Q -S ''"#);
    }
}


fn complete(args: &CompleteArgs) -> anyhow::Result<()> {
    let mut input_file = get_file_or_stdin(&args.usage_file_path).context(args.usage_file_path.to_owned())?;
    let mut input: String = Default::default();
    input_file.read_to_string(&mut input)?;
    let grammar = handle_parse_error(&input)?;

    if let Some(railroad_svg_path) = &args.railroad_svg {
        let mut railroad_svg = get_file_or_stdout(railroad_svg_path)?;
        to_railroad_diagram(&grammar, &mut railroad_svg)?;
    }

    let validated = ValidGrammar::from_grammar(grammar)?;

    let arena = Bump::new();
    let regex = AugmentedRegex::from_expr(&validated.expr, &validated.specializations, &arena);
    let dfa = DFA::from_regex(&regex);

    let (shell, prefix, words) = match &args.shell {
        Shell::Bash(a) => (complgen::complete::Shell::Bash, &a.prefix, &a.words),
        Shell::Fish(a) => (complgen::complete::Shell::Fish, &a.prefix, &a.words),
        Shell::Zsh(a) => (complgen::complete::Shell::Zsh, &a.prefix, &a.words),
    };

    let words_before_cursor: Vec<&str> = words.iter().map(|s| s.as_ref()).collect();

    let completions = get_completions(&dfa, &words_before_cursor, prefix.as_deref().unwrap_or(""), shell)?;

    match args.shell {
        Shell::Bash(_) => {
            for completion in completions {
                println!("{}", completion.get_completion());
            }
        },
        Shell::Fish(_) => {
            for completion in completions {
                println!("{}\t{}", completion.get_completion(), completion.description);
            }
        },
        Shell::Zsh(_) => {
            let (with_description, without_description): (Vec<&Completion>, Vec<&Completion>) = completions.iter().partition(|completion| completion.has_zsh_description());

            if !without_description.is_empty() {
                let (trailing_space, no_trailing_space): (Vec<&Completion>, Vec<&Completion>) = without_description.iter().partition(|completion| completion.has_zsh_trailing_space());
                zsh_compadd(&trailing_space, true);
                zsh_compadd(&no_trailing_space, false);
            }

            if !with_description.is_empty() {
                let (compadd, describe): (Vec<&Completion>, Vec<&Completion>) = with_description.iter().partition(|completion| completion.is_zsh_compadd());

                if !compadd.is_empty() {
                    let (trailing_space, no_trailing_space): (Vec<&Completion>, Vec<&Completion>) = compadd.iter().partition(|completion| completion.has_zsh_trailing_space());
                    zsh_compadd_with_description(&trailing_space, true);
                    zsh_compadd_with_description(&no_trailing_space, false);
                }

                if !describe.is_empty() {
                    // We pessimistically assume here no trailing space (i.e. -S option) because
                    // _describe when called twice leads to garbled output (see comment in the
                    // _describe source code).
                    zsh_describe(&describe, false);
                }
            }
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
    if let (None, None, None, None, None) = (&args.railroad_svg, &args.dfa_dot, &args.bash_script, &args.fish_script, &args.zsh_script) {
        eprintln!("Please specify at least one of --railroad-svg, --dfa-dot, --bash-script, --fish-script, --zsh-script options");
        std::process::exit(1);
    }

    let input = {
        let mut usage_file = get_file_or_stdin(&args.usage_file_path)?;
        let mut input = String::default();
        usage_file.read_to_string(&mut input).context(args.usage_file_path.to_owned())?;
        input
    };

    let grammar = handle_parse_error(&input)?;

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

    if let Some(inputs) = dfa.get_any_ambiguous_state() {
        eprintln!("Warning: Final DFA contains ambiguous transitions; inputs: {:?}", inputs);
    }

    if let Some(dot_file_path) = &args.dfa_dot {
        let mut dot_file = get_file_or_stdout(dot_file_path)?;
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

    let mut writer = BufWriter::new(std::io::stdout());
    complgen::scrape::scrape(&input, &mut writer)?;
    Ok(())
}


fn main() -> anyhow::Result<()> {
    env_logger::init();
    let args = Cli::parse();
    match args.mode {
        Mode::Check(args) => check(&args)?,
        Mode::Complete(args) => complete(&args)?,
        Mode::Compile(args) => compile(&args)?,
        Mode::Scrape => scrape()?,
    };
    Ok(())
}

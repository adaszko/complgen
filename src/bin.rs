use std::ffi::OsStr;
use std::io::{BufWriter, Write, Read};
use std::path::Path;
use std::process::exit;

use anyhow::Context;
use bumpalo::Bump;
use clap::Parser;

use complgen::jit::{get_completions, Completion};
use complgen::grammar::{ValidGrammar, Grammar, to_railroad_diagram, to_railroad_diagram_file};

use complgen::Error;
use complgen::dfa::DFA;
use complgen::regex::AugmentedRegex;
use complgen::aot::zsh::make_string_constant;
use slice_group_by::GroupBy;

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
    Jit(JitArgs),

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
struct CompleteShellArgs {
    #[arg(long)]
    prefix: Option<String>,

    words: Vec<String>,
}


#[derive(clap::Args)]
struct CompleteBashArgs {
    #[arg(long)]
    comp_wordbreaks: Option<String>,

    #[arg(long)]
    prefix: Option<String>,

    words: Vec<String>,
}

#[derive(clap::Subcommand)]
enum Shell {
    Bash(CompleteBashArgs),
    Fish(CompleteShellArgs),
    Zsh(CompleteShellArgs),
}

#[derive(clap::Args)]
struct JitArgs {
    #[clap(long)]
    railroad_svg: Option<String>,

    usage_file_path: String,

    #[clap(subcommand)]
    shell: Shell,
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
        },
    }
}


fn handle_validation_error(g: Grammar, input: &str) -> anyhow::Result<ValidGrammar> {
    match ValidGrammar::from_grammar(g) {
        Ok(g) => Ok(g),
        Err(Error::CommandAtNonTailPosition(_, span)) => {
            let error = chic::Error::new("Commands are only allowed at a tail position to avoid ambiguities in matching")
                .error(span.line_start, span.start, span.end, input.lines().nth(span.line_start).unwrap(), "")
                .help("try moving the command to the tail position");
            eprintln!("{}:{}:{}", span.line_start, span.start, error.to_string());
            exit(1);
        },
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
    let validated = handle_validation_error(grammar, &input)?;
    let arena = Bump::new();
    let regex = AugmentedRegex::from_expr(&validated.expr, &validated.specializations, &arena);
    let dfa = DFA::from_regex(&regex);
    if let Some(inputs) = dfa.get_any_ambiguous_state() {
        eprintln!("Error: Final DFA contains ambiguous transitions.");
        eprintln!("A sequence of shell words leading to the ambiguity: {:?}", inputs);
        eprintln!("They arise when there's a need to match a shell word against the output of more than one external shell command.");
        eprintln!("The grammar needs to be modified to match against at most one external command output to avoid ambiguities.");
        exit(1);
    }
    Ok(())
}


fn print_zsh_completion_level(completions: &[Completion]) {
    let mut without_description_trailing_space: Vec<String> = Default::default();
    let mut without_description_no_trailing_space: Vec<String> = Default::default();

    let mut with_description_trailing_space: Vec<(String, String, &str)> = Default::default();
    let mut with_description_no_trailing_space: Vec<(String, &str, &str)> = Default::default();

    let mut without_description_subword_trailing_space: Vec<(String, &str)> = Default::default();
    let mut without_description_subword_no_trailing_space: Vec<(String, &str)> = Default::default();

    for completion in completions {
        if completion.has_zsh_description() {
            if completion.description.is_empty() {
                // * Subword completion without description => Use completed suffix as compadd description
                if completion.is_shell_word_ending {
                    without_description_subword_trailing_space.push((completion.get_completion(), &completion.completed_subword_suffix));
                }
                else {
                    without_description_subword_no_trailing_space.push((completion.get_completion(), &completion.completed_subword_suffix));
                }
            }
            else {
                // * Subword completion with non-empty description => Use that description
                if completion.is_shell_word_ending {
                    with_description_trailing_space.push((completion.get_completion(), completion.completed_subword_suffix.to_string(), &completion.description));
                }
                else {
                    with_description_no_trailing_space.push((completion.get_completion(), &completion.completed_subword_suffix, &completion.description));
                }
            }
        }
        else {
            if completion.is_shell_word_ending {
                without_description_trailing_space.push(completion.get_completion());
            }
            else {
                without_description_no_trailing_space.push(completion.get_completion());
            }
        }
    }

    println!(r#"    local -a matches=()"#);

    if !without_description_trailing_space.is_empty() {
        let initializer = itertools::join(without_description_trailing_space.iter().map(|completion| make_string_constant(&completion)), " ");
        println!(r#"    local -a completions=({initializer})"#);
        println!(r#"    compadd -Q -a completions"#);
        println!(r#"    compadd -O matches -a completions"#);
    }

    if !without_description_no_trailing_space.is_empty() {
        let initializer = itertools::join(without_description_no_trailing_space.iter().map(|completion| make_string_constant(&completion)), " ");
        println!(r#"    local -a completions=({initializer})"#);
        println!(r#"    compadd -Q -S '' -a completions"#);
        println!(r#"    compadd -O matches -a completions"#);
    }

    if !without_description_subword_trailing_space.is_empty() {
        let completions_initializer = itertools::join(without_description_subword_trailing_space.iter().map(|(completion, _)| make_string_constant(&completion)), " ");
        println!(r#"    local -a completions=({completions_initializer})"#);

        let descriptions_initializer = itertools::join(without_description_subword_trailing_space.iter().map(|(_, suffix)| make_string_constant(suffix)), " ");
        println!(r#"    local -a descriptions=({descriptions_initializer})"#);

        println!(r#"    compadd -Q -a -d descriptions completions"#);
        println!(r#"    compadd -O matches -a completions"#);
    }

    if !without_description_subword_no_trailing_space.is_empty() {
        let completions_initializer = itertools::join(without_description_subword_no_trailing_space.iter().map(|(completion, _)| make_string_constant(&completion)), " ");
        println!(r#"    local -a completions=({completions_initializer})"#);

        let descriptions_initializer = itertools::join(without_description_subword_no_trailing_space.iter().map(|(_, suffix)| make_string_constant(suffix)), " ");
        println!(r#"    local -a descriptions=({descriptions_initializer})"#);

        println!(r#"    compadd -Q -S '' -a -d descriptions completions"#);
        println!(r#"    compadd -O matches -a completions"#);
    }

    let maxlen = {
        let trailing_space_maxlen = with_description_trailing_space.iter().map(|(_, suffix, _)| suffix.len()).max().unwrap_or(0);
        let no_trailing_space_maxlen = with_description_no_trailing_space.iter().map(|(_, suffix, _)| suffix.len()).max().unwrap_or(0);
        std::cmp::max(trailing_space_maxlen, no_trailing_space_maxlen)
    };

    if !with_description_trailing_space.is_empty() {
        let completions_initializer = itertools::join(with_description_trailing_space.iter().map(|(completion, _, _)| make_string_constant(&completion)), " ");
        println!(r#"    local -a completions=({completions_initializer})"#);

        let descriptions_initializer = itertools::join(with_description_trailing_space.iter().map(|(_, suffix, description)| make_string_constant(&format!("{suffix:maxlen$} -- {description}"))), " ");
        println!(r#"    local -a descriptions=({descriptions_initializer})"#);

        println!(r#"    compadd -l -Q -a -d descriptions completions"#);
        println!(r#"    compadd -O matches -a completions"#);
    }

    if !with_description_no_trailing_space.is_empty() {
        let completions_initializer = itertools::join(with_description_no_trailing_space.iter().map(|(completion, _, _)| make_string_constant(&completion)), " ");
        println!(r#"    local -a completions=({completions_initializer})"#);

        let descriptions_initializer = itertools::join(with_description_no_trailing_space.iter().map(|(_, suffix, description)| make_string_constant(&format!("{suffix:maxlen$} -- {description}"))), " ");
        println!(r#"    local -a descriptions=({descriptions_initializer})"#);

        println!(r#"    compadd -l -Q -S '' -a -d descriptions completions"#);
        println!(r#"    compadd -O matches -a completions"#);
    }

    println!(r#"    [[ ${{#matches}} -gt 0 ]] && return"#);
}


fn print_zsh_completions(completions: &[Completion]) {
    println!(r#"__complgen_jit () {{"#);
    for group in completions.linear_group_by(|left, right| left.fallback_level == right.fallback_level) {
        print_zsh_completion_level(group)
    }
    println!(r#"}}"#);
    println!(r#"__complgen_jit"#);
}


fn complete(args: &JitArgs) -> anyhow::Result<()> {
    let mut input_file = get_file_or_stdin(&args.usage_file_path).context(args.usage_file_path.to_owned())?;
    let mut input: String = Default::default();
    input_file.read_to_string(&mut input)?;
    let grammar = handle_parse_error(&input)?;

    if let Some(railroad_svg_path) = &args.railroad_svg {
        let mut railroad_svg = get_file_or_stdout(railroad_svg_path)?;
        to_railroad_diagram(&grammar, &mut railroad_svg)?;
    }

    let validated = handle_validation_error(grammar, &input)?;

    let arena = Bump::new();
    let regex = AugmentedRegex::from_expr(&validated.expr, &validated.specializations, &arena);
    let dfa = DFA::from_regex(&regex);

    let (shell, prefix, words) = match &args.shell {
        Shell::Bash(a) => (complgen::jit::Shell::Bash, &a.prefix, &a.words),
        Shell::Fish(a) => (complgen::jit::Shell::Fish, &a.prefix, &a.words),
        Shell::Zsh(a) => (complgen::jit::Shell::Zsh, &a.prefix, &a.words),
    };

    let words_before_cursor: Vec<&str> = words.iter().map(|s| s.as_ref()).collect();

    let word = prefix.as_deref().unwrap_or("");
    let mut completions = get_completions(&dfa, &words_before_cursor, word, shell)?;
    completions.sort_by_key(|c| c.fallback_level);

    match args.shell {
        Shell::Bash(ref args) => {
            // Bash behaves weirdly depending on wheter a completion contains a special character
            // from $COMP_WORDBREAKS or not.  If it does, it is necessary to strip from a
            // completion string a longest prefix up to and including the last such special
            // character.

            let comp_wordbreaks: Vec<char> = args.comp_wordbreaks.as_deref().unwrap_or("").chars().collect();
            let superfluous_prefix = match word.rfind(comp_wordbreaks.as_slice()) {
                Some(pos) => &word[..=pos],
                None => "",
            };

            let matches = {
                let mut matches: Vec<String> = Default::default();
                for group in completions.linear_group_by(|left, right| left.fallback_level == right.fallback_level) {
                    for completion in group {
                        let comp = completion.get_completion_with_trailing_space();
                        if comp.starts_with(word) {
                            matches.push(comp.strip_prefix(superfluous_prefix).unwrap_or(&comp).to_owned());
                        }
                    }
                    if !matches.is_empty() {
                        break;
                    }
                }
                matches
            };

            for m in matches {
                println!("{}", m);
            }
        },
        Shell::Fish(_) => {
            let matches = {
                let mut matches: Vec<String> = Default::default();
                for group in completions.linear_group_by(|left, right| left.fallback_level == right.fallback_level) {
                    for completion in group {
                        let comp = completion.get_completion();
                        if comp.starts_with(word) {
                            matches.push(format!("{}\t{}", comp, completion.description));
                        }
                    }
                    if !matches.is_empty() {
                        break;
                    }
                }
                matches
            };
            for m in matches {
                println!("{}", m);
            }
        },
        Shell::Zsh(_) => print_zsh_completions(&completions),
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


fn compile(args: &AotArgs) -> anyhow::Result<()> {
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

    if let Some(inputs) = dfa.get_any_ambiguous_state() {
        eprintln!("Error: Final DFA contains ambiguous transitions.");
        eprintln!("A sequence of shell words leading to the ambiguity: {:?}", inputs);
        eprintln!("They arise when there's a need to match a shell word against the output of more than one external shell command.");
        eprintln!("The grammar needs to be modified to match against at most one external command output to avoid ambiguities.");
        exit(1);
    }

    if let Some(dot_file_path) = &args.dfa_dot {
        let mut dot_file = get_file_or_stdout(dot_file_path)?;
        dfa.to_dot(&mut dot_file).context(dot_file_path.clone())?;
    }

    if let Some(path) = &args.bash_script {
        log::debug!("Writing Bash completion script");
        let script_file = get_file_or_stdout(path)?;
        let mut writer = BufWriter::new(script_file);
        complgen::aot::bash::write_completion_script(&mut writer, &validated.command, &dfa)?;
    }

    if let Some(path) = &args.fish_script {
        log::debug!("Writing Fish completion script");
        let script_file = get_file_or_stdout(path)?;
        let mut writer = BufWriter::new(script_file);
        complgen::aot::fish::write_completion_script(&mut writer, &validated.command, &dfa)?;
    }

    if let Some(path) = &args.zsh_script {
        log::debug!("Writing Zsh completion script");
        let expected_name = format!("_{}", validated.command);
        if path != "-" && Path::new(path).file_name().unwrap_or_default() != OsStr::new(&expected_name) {
            eprintln!("Warning: ZSH requires the output script to be named {expected_name:?} for autoloading to work");
        }
        let script_file = get_file_or_stdout(path)?;
        let mut writer = BufWriter::new(script_file);
        complgen::aot::zsh::write_completion_script(&mut writer, &validated.command, &dfa)?;
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
        Mode::Jit(args) => complete(&args)?,
        Mode::Aot(args) => compile(&args)?,
        Mode::Scrape => scrape()?,
        Mode::Version => {
            println!("{}", env!("COMPLGEN_VERSION"));
        },
    };
    Ok(())
}

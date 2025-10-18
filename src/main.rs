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

struct ErrMsg {
    err: chic::Error,
}

impl ErrMsg {
    fn new(label: &str) -> Self {
        Self {
            err: chic::Error::new(label),
        }
    }

    fn error(self, span: &HumanSpan, source: &str, what: &str) -> Self {
        Self {
            err: self.err.error(
                span.line,
                span.column_start_machine(),
                span.column_end_machine(),
                source.lines().nth(span.line_machine()).unwrap(),
                what,
            ),
        }
    }

    fn help(self, label: &str) -> Self {
        Self {
            err: self.err.help(label),
        }
    }

    fn into_string(self, path: &str, span: &HumanSpan) -> String {
        format!(
            "{}:{}:{}:{}",
            path,
            span.line,
            span.column_start,
            self.err.to_string()
        )
    }
}

struct WarnMsg {
    warning: chic::Warning,
}

impl WarnMsg {
    fn new(label: &str) -> Self {
        Self {
            warning: chic::Warning::new(label),
        }
    }

    fn warning(self, span: &HumanSpan, source: &str, what: &str) -> Self {
        Self {
            warning: self.warning.warning(
                span.line,
                span.column_start_machine(),
                span.column_end_machine(),
                source.lines().nth(span.line_machine()).unwrap(),
                what,
            ),
        }
    }

    fn into_string(self, path: &str, span: &HumanSpan) -> String {
        format!(
            "{}:{}:{}:{}",
            path,
            span.line,
            span.column_start,
            self.warning.to_string()
        )
    }
}

fn handle_error(e: Error, path: &str, source: &str, command: &str) -> anyhow::Result<()> {
    match e {
        Error::ParseError(span) => {
            let err = ErrMsg::new("Parse error").error(&span, source, "");
            eprintln!("{}", err.into_string(path, &span));
        }
        Error::InvalidCommandName(span) => {
            let err = ErrMsg::new("Invalid command name").error(&span, source, "");
            eprintln!("{}", err.into_string(path, &span));
        }
        Error::VaryingCommandNames(spans) => {
            for span in spans {
                let err = ErrMsg::new("Varying command names:").error(&span, source, "");
                eprintln!("{}", err.into_string(path, &span));
            }
        }
        Error::NonterminalDefinitionsCycle(spans) => {
            for span in spans {
                let err = ErrMsg::new("Nonterminal definitions cycle").error(&span, source, "");
                eprintln!("{}", err.into_string(path, &span));
            }
        }
        Error::UnknownShell(span) => {
            let err = ErrMsg::new("Unknown shell").error(&span, source, "");
            eprintln!("{}", err.into_string(path, &span));
        }
        Error::NonCommandSpecialization(span) => {
            let err = ErrMsg::new("Can only specialize external commands").error(&span, source, "");
            eprintln!("{}", err.into_string(path, &span));
        }
        Error::SubwordSpaces(left, right, trace) => {
            let err = ErrMsg::new("Adjacent literals in expression used in a subword context")
                .error(&left, source, "First one");
            eprintln!("{}", err.into_string(path, &left));

            let err = ErrMsg::new("").error(&right, source, "Second one").help(
                "Join the adjacent literals into one as spaces are invalid in a subword context",
            );
            eprintln!("{}", err.into_string(path, &right));

            for t in trace {
                let err = ErrMsg::new("Referenced in a subword context at").error(&t, source, "");
                eprintln!("{}", err.into_string(path, &t));
            }
        }
        Error::AmbiguousMatchable(left, right) => {
            let err = ErrMsg::new("Ambiguous grammar.  Matching can't differentiate:")
                .error(&left, source, "");
            eprintln!("{}", err.into_string(path, &left));

            let err = ErrMsg::new("and:").error(&right, source, "");
            eprintln!("{}", err.into_string(path, &right));
        }
        Error::UnboundedMatchable(left, right) => {
            let err = ErrMsg::new(
                "Ambiguous grammar.  Matching can't ascertain where below element ends:",
            )
            .error(&left, source, "");
            eprintln!("{}", err.into_string(path, &left));

            let err = ErrMsg::new("...and where below element begins:").error(&right, source, "");
            eprintln!("{}", err.into_string(path, &right));
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
        Error::ClashingVariants(left, right) => {
            let err = ErrMsg::new("Clashing variants.  Completion can't differentiate:")
                .error(&left, source, "");
            eprintln!("{}", err.into_string(path, &left));

            let err = ErrMsg::new("and:").error(&right, source, "");
            eprintln!("{}", err.into_string(path, &right));
        }
        Error::ClashingSubwordLeaders(left, right) => {
            let err = ErrMsg::new("Clashing subword leaders.  Completion can't differentiate:")
                .error(&left, source, "");
            eprintln!("{}", err.into_string(path, &left));

            let err = ErrMsg::new("and:").error(&right, source, "");
            eprintln!("{}", err.into_string(path, &right));
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

    let grammar = match Grammar::parse(&input) {
        Ok(grammar) => grammar,
        Err(e) => return handle_error(e, usage_file_path, &input, "dummy"),
    };

    let (shell, path) = match (&args.bash, &args.fish, &args.zsh) {
        (Some(path), None, None) => (Shell::Bash, path),
        (None, Some(path), None) => (Shell::Fish, path),
        (None, None, Some(path)) => (Shell::Zsh, path),

        _ => {
            eprintln!("Please specify exactly one of: --bash, --fish, --zsh");
            exit(1);
        }
    };

    let validated = match ValidGrammar::from_grammar(grammar, shell) {
        Ok(validated) => validated,
        Err(e) => return handle_error(e, usage_file_path, &input, "dummy"),
    };

    if !validated.undefined_nonterminals.is_empty() {
        for (_, span) in &validated.undefined_nonterminals {
            let err = WarnMsg::new("Undefined nonterminal").warning(&span, &input, "");
            eprintln!("{}", err.into_string(usage_file_path, &span));
        }
    }

    if !validated.unused_nonterminals.is_empty() {
        for (_, span) in &validated.unused_nonterminals {
            let err = WarnMsg::new("Unused nonterminal").warning(&span, &input, "");
            eprintln!("{}", err.into_string(usage_file_path, &span));
        }
    }

    let regex = match Regex::from_valid_grammar(&validated, shell) {
        Ok(regex) => regex,
        Err(e) => return handle_error(e, usage_file_path, &input, &validated.command),
    };

    if let Some(regex_dot_file_path) = &args.regex {
        let mut dot_file = get_file_or_stdout(regex_dot_file_path)?;
        regex
            .to_dot(&mut dot_file)
            .context(regex_dot_file_path.clone())?;
    }

    let dfa = match DFA::from_regex(regex, validated.subdfas) {
        Ok(dfa) => dfa,
        Err(e) => return handle_error(e, usage_file_path, &input, &validated.command),
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

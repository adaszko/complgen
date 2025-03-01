use itertools::PutBack;
use regex::Regex;
use std::io::Write;

fn take_lines_while_descr<'a>(
    iter: &mut PutBack<impl Iterator<Item = &'a str>>,
    line: &str,
    descr_first_line: &'a str,
    continued_descr_rx: &Regex,
) -> String {
    let mut descr_lines: Vec<&'a str> = vec![descr_first_line];
    let non_descr_len = line.len() - descr_first_line.len();
    while let Some(l) = iter.next() {
        if let Some(c) = continued_descr_rx.captures(l) {
            if c.get(1).unwrap().len() == non_descr_len {
                descr_lines.push(c.get(2).unwrap().as_str());
            } else {
                iter.put_back(l);
                break;
            }
        } else {
            iter.put_back(l);
            break;
        }
    }
    itertools::join(descr_lines, " ")
}

pub fn scrape<W: Write>(input: &str, output: &mut W) -> crate::Result<()> {
    let short_long_opt_triangle_arg_descr =
        Regex::new(r#"^\s*(-\S),\s*(--\S+) \[<([^>]+)>\]\s+(.+)$"#).unwrap();
    let short_long_triangle_arg_descr =
        Regex::new(r#"^\s*(-\S),\s*(--\S+) <([^>]+)>\s+(.+)$"#).unwrap();
    let short_long_arg_descr =
        Regex::new(r#"^\s*(-\S),\s*(--\S+)=(\p{Uppercase}+)\s+(.+)$"#).unwrap();
    let short_long_descr = Regex::new(r#"^\s*(-\S),\s*(--\S+)\s+(.+)$"#).unwrap();
    let long_arg_descr = Regex::new(r#"^\s*(--\S+)=(\p{Uppercase}+)\s+(.+)$"#).unwrap();
    let long_triangle_arg_descr = Regex::new(r#"^\s*(--\S+) <([^>]+)>\s+(.+)$"#).unwrap();
    let long_opt_triangle_arg_descr = Regex::new(r#"^\s*(--\S+) \[<([^>]+)>\]\s+(.+)$"#).unwrap();
    let long_opt_arg_descr = Regex::new(r#"^\s*(--\S+)\[=(\p{Uppercase}+)\]\s+(.+)$"#).unwrap();
    let long_descr = Regex::new(r#"^\s*(--\S+)\s+(.+)$"#).unwrap();
    let short_triangle_arg_descr = Regex::new(r#"^\s*(-\S)\s*<([^>]+)>\s+(.+)$"#).unwrap();
    let short_descr = Regex::new(r#"^\s*(-\S)\s+(.+)$"#).unwrap();
    let continued_descr_rx = Regex::new(r#"^(\s*)(.+)$"#).unwrap();

    let mut iter = itertools::put_back(input.lines());
    while let Some(line) = iter.next() {
        if let Some(caps) = short_long_opt_triangle_arg_descr.captures(line) {
            let short = caps.get(1).unwrap().as_str();
            let long = caps.get(2).unwrap().as_str();
            let arg = format!("<{}>", caps.get(3).unwrap().as_str());
            let descr = caps.get(4).unwrap().as_str();
            writeln!(output, r#" | ({short} [{arg}] | {long} [{arg}]) "{descr}""#)?;
            continue;
        }

        if let Some(caps) = short_long_triangle_arg_descr.captures(line) {
            let short = caps.get(1).unwrap().as_str();
            let long = caps.get(2).unwrap().as_str();
            let arg = format!("<{}>", caps.get(3).unwrap().as_str());
            let descr = caps.get(4).unwrap().as_str();
            writeln!(output, r#" | ({short} {arg} | {long} {arg}) "{descr}""#)?;
            continue;
        }

        if let Some(caps) = short_long_arg_descr.captures(line) {
            let short = caps.get(1).unwrap().as_str();
            let long = caps.get(2).unwrap().as_str();
            let arg = format!("<{}>", caps.get(3).unwrap().as_str());
            let descr = caps.get(4).unwrap().as_str();
            writeln!(output, r#" | ({short} {arg} | {long}={arg}) "{descr}""#)?;
            continue;
        }

        if let Some(caps) = short_long_descr.captures(line) {
            let short = caps.get(1).unwrap().as_str();
            let long = caps.get(2).unwrap().as_str();
            let descr = caps.get(3).unwrap().as_str();
            writeln!(output, r#" | ({short} | {long}) "{descr}""#)?;
            continue;
        }

        if let Some(caps) = long_arg_descr.captures(line) {
            let long = caps.get(1).unwrap().as_str();
            let arg = format!("<{}>", caps.get(2).unwrap().as_str());
            let descr = caps.get(3).unwrap().as_str();
            let description = take_lines_while_descr(&mut iter, line, descr, &continued_descr_rx);
            writeln!(output, r#" | {long}={arg} "{description}""#,)?;
            continue;
        }

        if let Some(caps) = long_opt_triangle_arg_descr.captures(line) {
            let long = caps.get(1).unwrap().as_str();
            let arg = format!("<{}>", caps.get(2).unwrap().as_str());
            let descr = caps.get(3).unwrap().as_str();
            writeln!(output, r#" | ({long} [{arg}]) "{descr}""#)?;
            continue;
        }

        if let Some(caps) = long_triangle_arg_descr.captures(line) {
            let long = caps.get(1).unwrap().as_str();
            let arg = format!("<{}>", caps.get(2).unwrap().as_str());
            let descr = caps.get(3).unwrap().as_str();
            writeln!(output, r#" | ({long} {arg}) "{descr}""#)?;
            continue;
        }

        if let Some(caps) = long_opt_arg_descr.captures(line) {
            let long = caps.get(1).unwrap().as_str();
            let arg = format!("<{}>", caps.get(2).unwrap().as_str());
            let descr = caps.get(3).unwrap().as_str();
            writeln!(output, r#" | {long}[={arg}] "{descr}""#)?;
            continue;
        }

        if let Some(caps) = long_descr.captures(line) {
            let long = caps.get(1).unwrap().as_str();
            let descr = caps.get(2).unwrap().as_str();
            let description = take_lines_while_descr(&mut iter, line, descr, &continued_descr_rx);
            writeln!(output, r#" | {long} "{description}""#)?;
            continue;
        }

        if let Some(caps) = short_triangle_arg_descr.captures(line) {
            let short = caps.get(1).unwrap().as_str();
            let arg = format!("<{}>", caps.get(2).unwrap().as_str());
            let descr = caps.get(3).unwrap().as_str();
            writeln!(output, r#" | ({short} {arg}) "{descr}""#)?;
            continue;
        }

        if let Some(caps) = short_descr.captures(line) {
            let short = caps.get(1).unwrap().as_str();
            let descr = caps.get(2).unwrap().as_str();
            writeln!(output, r#" | {short} "{descr}""#)?;
            continue;
        }
    }
    output.flush()?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn do_scrape(input: &str) -> String {
        let mut buffer: Vec<u8> = Default::default();
        scrape(input, &mut buffer).unwrap();
        String::from_utf8(buffer).unwrap()
    }

    #[test]
    fn short_long_descr() {
        const INPUT: &str =
            r#"-E, --extended-regexp     PATTERNS are extended regular expressions"#;
        let output = do_scrape(INPUT);
        assert_eq!(
            output,
            r#" | (-E | --extended-regexp) "PATTERNS are extended regular expressions"
"#
        );
    }

    #[test]
    fn short_long_arg_descr() {
        const INPUT: &str = r#"-e, --regexp=PATTERNS     use PATTERNS for matching"#;
        let output = do_scrape(INPUT);
        assert_eq!(
            output,
            r#" | (-e <PATTERNS> | --regexp=<PATTERNS>) "use PATTERNS for matching"
"#
        );
    }

    #[test]
    fn long_descr() {
        const INPUT: &str = "--list                List installed commands";
        let output = do_scrape(INPUT);
        assert_eq!(
            output,
            r#" | --list "List installed commands"
"#
        );
    }

    #[test]
    fn short_triangle_arg_descr() {
        const INPUT: &str =
            "  -C <DIRECTORY>            Change to DIRECTORY before doing anything (nightly-only)";
        let output = do_scrape(INPUT);
        assert_eq!(
            output,
            r#" | (-C <DIRECTORY>) "Change to DIRECTORY before doing anything (nightly-only)"
"#
        );
    }

    #[test]
    fn long_triangle_arg_descr() {
        const INPUT: &str = "      --explain <CODE>      Run `rustc --explain CODE`";
        let output = do_scrape(INPUT);
        assert_eq!(
            output,
            r#" | (--explain <CODE>) "Run `rustc --explain CODE`"
"#
        );
    }

    #[test]
    fn long_opt_triangle_arg_descr() {
        const INPUT: &str = "      --bin [<NAME>]            Build only the specified binary";
        let output = do_scrape(INPUT);
        assert_eq!(
            output,
            r#" | (--bin [<NAME>]) "Build only the specified binary"
"#
        );
    }

    #[test]
    fn short_long_opt_triangle_arg_descr() {
        const INPUT: &str =
            "  -p, --package [<SPEC>]        Package to build (see `cargo help pkgid`)";
        let output = do_scrape(INPUT);
        assert_eq!(
            output,
            r#" | (-p [<SPEC>] | --package [<SPEC>]) "Package to build (see `cargo help pkgid`)"
"#
        );
    }

    #[test]
    fn short_long_triangle_arg_descr() {
        const INPUT: &str =
            "  -j, --jobs <N>                Number of parallel jobs, defaults to # of CPUs.";
        let output = do_scrape(INPUT);
        assert_eq!(
            output,
            r#" | (-j <N> | --jobs <N>) "Number of parallel jobs, defaults to # of CPUs."
"#
        );
    }

    #[test]
    fn long_arg_multiline_descr() {
        const INPUT: &str = "
      --quoting-style=WORD   use quoting style WORD for entry names:
                             literal, locale, shell, shell-always,
                             shell-escape, shell-escape-always, c, escape
                             (overrides QUOTING_STYLE environment variable)
";
        let output = do_scrape(INPUT);
        assert_eq!(
            output,
            r#" | --quoting-style=<WORD> "use quoting style WORD for entry names: literal, locale, shell, shell-always, shell-escape, shell-escape-always, c, escape (overrides QUOTING_STYLE environment variable)"
"#
        );
    }

    #[test]
    fn long_multiline_descr() {
        const INPUT: &str = "
          --allow-untrusted     Install packages with untrusted signature or no
                                signature
";
        let output = do_scrape(INPUT);
        assert_eq!(
            output,
            r#" | --allow-untrusted "Install packages with untrusted signature or no signature"
"#
        );
    }
}

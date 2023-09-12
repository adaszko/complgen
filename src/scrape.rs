use std::io::Write;
use regex::Regex;


pub fn scrape<W: Write>(input: &str, output: &mut W) -> crate::Result<()> {
    let short_long_opt_triangle_arg_descr = Regex::new(r#"^\s*(-\S),\s*(--\S+) \[<([^>]+)>\]\s+(.+)$"#).unwrap();
    let short_long_triangle_arg_descr = Regex::new(r#"^\s*(-\S),\s*(--\S+) <([^>]+)>\s+(.+)$"#).unwrap();
    let short_long_arg_descr = Regex::new(r#"^\s*(-\S),\s*(--\S+)=(\p{Uppercase}+)\s+(.+)$"#).unwrap();
    let short_long_descr = Regex::new(r#"^\s*(-\S),\s*(--\S+)\s+(.+)$"#).unwrap();
    let long_arg_descr = Regex::new(r#"^\s*(--\S+)=(\p{Uppercase}+)\s+(.+)$"#).unwrap();
    let long_triangle_arg_descr = Regex::new(r#"^\s*(--\S+) <([^>]+)>\s+(.+)$"#).unwrap();
    let long_opt_triangle_arg_descr = Regex::new(r#"^\s*(--\S+) \[<([^>]+)>\]\s+(.+)$"#).unwrap();
    let long_opt_arg_descr = Regex::new(r#"^\s*(--\S+)\[=(\p{Uppercase}+)\]\s+(.+)$"#).unwrap();
    let long_descr = Regex::new(r#"^\s*(--\S+)\s+(.+)$"#).unwrap();
    let short_triangle_arg_descr = Regex::new(r#"^\s*(-\S)\s*<([^>]+)>\s+(.+)$"#).unwrap();
    let short_descr = Regex::new(r#"^\s*(-\S)\s+(.+)$"#).unwrap();

    for line in input.lines() {
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
            writeln!(output, r#" | {long}={arg} "{descr}""#)?;
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
            writeln!(output, r#" | {long} "{descr}""#)?;
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
        const INPUT: &str = r#"-E, --extended-regexp     PATTERNS are extended regular expressions"#;
        let output = do_scrape(INPUT);
        assert_eq!(output, r#" | (-E | --extended-regexp) "PATTERNS are extended regular expressions"
"#);
    }

    #[test]
    fn short_long_arg_descr() {
        const INPUT: &str = r#"-e, --regexp=PATTERNS     use PATTERNS for matching"#;
        let output = do_scrape(INPUT);
        assert_eq!(output, r#" | (-e <PATTERNS> | --regexp=<PATTERNS>) "use PATTERNS for matching"
"#);
    }

    #[test]
    fn long_descr() {
        const INPUT: &str = "--list                List installed commands";
        let output = do_scrape(INPUT);
        assert_eq!(output, r#" | --list "List installed commands"
"#);
    }

    #[test]
    fn short_triangle_arg_descr() {
        const INPUT: &str = "  -C <DIRECTORY>            Change to DIRECTORY before doing anything (nightly-only)";
        let output = do_scrape(INPUT);
        assert_eq!(output, r#" | (-C <DIRECTORY>) "Change to DIRECTORY before doing anything (nightly-only)"
"#);
    }

    #[test]
    fn long_triangle_arg_descr() {
        const INPUT: &str = "      --explain <CODE>      Run `rustc --explain CODE`";
        let output = do_scrape(INPUT);
        assert_eq!(output, r#" | (--explain <CODE>) "Run `rustc --explain CODE`"
"#);
    }

    #[test]
    fn long_opt_triangle_arg_descr() {
        const INPUT: &str = "      --bin [<NAME>]            Build only the specified binary";
        let output = do_scrape(INPUT);
        assert_eq!(output, r#" | (--bin [<NAME>]) "Build only the specified binary"
"#);
    }

    #[test]
    fn short_long_opt_triangle_arg_descr() {
        const INPUT: &str = "  -p, --package [<SPEC>]        Package to build (see `cargo help pkgid`)";
        let output = do_scrape(INPUT);
        assert_eq!(output, r#" | (-p [<SPEC>] | --package [<SPEC>]) "Package to build (see `cargo help pkgid`)"
"#);
    }

    #[test]
    fn short_long_triangle_arg_descr() {
        const INPUT: &str = "  -j, --jobs <N>                Number of parallel jobs, defaults to # of CPUs.";
        let output = do_scrape(INPUT);
        assert_eq!(output, r#" | (-j <N> | --jobs <N>) "Number of parallel jobs, defaults to # of CPUs."
"#);
    }
}

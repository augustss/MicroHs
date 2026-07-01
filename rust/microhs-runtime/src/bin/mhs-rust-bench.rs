use std::env;
use std::fs;
use std::hint::black_box;
use std::process::Command;
use std::process::ExitCode;
use std::time::{Duration, Instant};

use microhs_runtime::parse_program;

const DEFAULT_ITERS: usize = 1_000;
const DEFAULT_SCENARIO: &str = "identity-chain:1000";

struct Config {
    input: Vec<u8>,
    name: String,
    iters: usize,
    c_mhseval: Option<String>,
}

fn usage() {
    eprintln!(
        "usage: mhs-rust-bench [--iters N] [--input FILE | --scenario identity-chain:N|arith-chain:N|int64-chain:N|float64-chain:N|float32-chain:N|bytes-chain:N|unpack-chain:N|fromutf8-chain:N|array-chain:N|io-chain:N|io-array-chain:N|io-bytes-chain:N|zoo-chain:N|data-chain:N]\n\
                                  [--c-mhseval PATH]\n\
         default: --scenario {DEFAULT_SCENARIO} --iters {DEFAULT_ITERS}"
    );
}

fn main() -> ExitCode {
    let config = match parse_args(env::args().skip(1)) {
        Ok(config) => config,
        Err(message) => {
            eprintln!("{message}");
            usage();
            return ExitCode::from(2);
        }
    };

    let parse = bench_parse(&config.input, config.iters);
    let reduce = bench_reduce(&config.input, config.iters);
    let bytes = config.input.len();

    println!("input: {}", config.name);
    println!("bytes: {bytes}");
    println!("iters: {}", config.iters);
    println!("parse_total_ms: {:.3}", millis(parse.elapsed));
    println!(
        "parse_ns_per_iter: {:.1}",
        nanos_per_iter(parse.elapsed, config.iters)
    );
    println!(
        "parse_mib_per_s: {:.1}",
        mib_per_s(bytes, config.iters, parse.elapsed)
    );
    println!("reduce_total_ms: {:.3}", millis(reduce.elapsed));
    println!(
        "reduce_ns_per_iter: {:.1}",
        nanos_per_iter(reduce.elapsed, config.iters)
    );
    println!(
        "reduce_steps_per_iter: {:.1}",
        reduce.steps as f64 / config.iters as f64
    );
    println!(
        "reduce_steps_per_s: {:.1}",
        reduce.steps as f64 / reduce.elapsed.as_secs_f64()
    );

    if let Some(c_mhseval) = &config.c_mhseval {
        match bench_c_mhseval(&config.input, c_mhseval, config.iters) {
            Ok(c) => {
                println!("c_mhseval: {c_mhseval}");
                println!("c_parse_serialize_total_ms: {:.3}", millis(c.elapsed));
                println!(
                    "c_parse_serialize_ns_per_iter: {:.1}",
                    nanos_per_iter(c.elapsed, config.iters)
                );
            }
            Err(err) => {
                eprintln!("C mhseval benchmark failed: {err}");
                return ExitCode::from(1);
            }
        }
    }

    ExitCode::SUCCESS
}

fn parse_args(args: impl Iterator<Item = String>) -> Result<Config, String> {
    let mut iters = DEFAULT_ITERS;
    let mut input = None;
    let mut name = None;
    let mut c_mhseval = None;
    let mut args = args.peekable();

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--iters" => {
                let value = args.next().ok_or("--iters requires a value")?;
                iters = value
                    .parse()
                    .map_err(|_| format!("invalid --iters value: {value}"))?;
                if iters == 0 {
                    return Err("--iters must be greater than zero".to_owned());
                }
            }
            "--input" => {
                let file = args.next().ok_or("--input requires a file")?;
                let bytes = fs::read(&file).map_err(|err| format!("{file}: {err}"))?;
                input = Some(bytes);
                name = Some(file);
            }
            "--scenario" => {
                let scenario = args.next().ok_or("--scenario requires a value")?;
                input = Some(make_scenario(&scenario)?);
                name = Some(scenario);
            }
            "--c-mhseval" => {
                c_mhseval = Some(args.next().ok_or("--c-mhseval requires a path")?);
            }
            "-h" | "--help" => return Err(String::new()),
            _ => return Err(format!("unknown argument: {arg}")),
        }
    }

    let (input, name) = match (input, name) {
        (Some(input), Some(name)) => (input, name),
        (None, None) => (
            make_scenario(DEFAULT_SCENARIO)?,
            DEFAULT_SCENARIO.to_owned(),
        ),
        _ => unreachable!(),
    };

    Ok(Config {
        input,
        name,
        iters,
        c_mhseval,
    })
}

fn make_scenario(scenario: &str) -> Result<Vec<u8>, String> {
    if let Some(size) = scenario.strip_prefix("identity-chain:") {
        let size = parse_scenario_size("identity-chain", size)?;
        let mut out = String::from("v8.4\n0\nI");
        for _ in 1..size {
            out.push_str(" I @");
        }
        out.push_str(" #1 @ }\n");
        return Ok(out.into_bytes());
    }
    if let Some(size) = scenario.strip_prefix("arith-chain:") {
        let size = parse_scenario_size("arith-chain", size)?;
        let mut expr = String::from("#0");
        for _ in 0..size {
            expr = format!("+ {expr} @ #1 @");
        }
        return Ok(format!("v8.4\n0\n{expr} }}\n").into_bytes());
    }
    if let Some(size) = scenario.strip_prefix("int64-chain:") {
        let size = parse_scenario_size("int64-chain", size)?;
        let mut expr = String::from("##0");
        for _ in 0..size {
            expr = format!("I+ {expr} @ ##1 @");
        }
        return Ok(format!("v8.4\n0\n{expr} }}\n").into_bytes());
    }
    if let Some(size) = scenario.strip_prefix("float64-chain:") {
        let size = parse_scenario_size("float64-chain", size)?;
        let mut expr = String::from("&0");
        for _ in 0..size {
            expr = format!("d+ {expr} @ &1.25 @");
        }
        return Ok(format!("v8.4\n0\n{expr} }}\n").into_bytes());
    }
    if let Some(size) = scenario.strip_prefix("float32-chain:") {
        let size = parse_scenario_size("float32-chain", size)?;
        let mut expr = String::from("&&0");
        for _ in 0..size {
            expr = format!("f+ {expr} @ &&1.25 @");
        }
        return Ok(format!("v8.4\n0\n{expr} }}\n").into_bytes());
    }
    if let Some(size) = scenario.strip_prefix("bytes-chain:") {
        let size = parse_scenario_size("bytes-chain", size)?;
        let mut expr = String::from("\"\"");
        for idx in 0..size {
            let byte = (b'a' + (idx % 26) as u8) as char;
            expr = format!("bs++ {expr} @ \"{byte}\" @");
        }
        return Ok(format!("v8.4\n0\n{expr} }}\n").into_bytes());
    }
    if let Some(size) = scenario.strip_prefix("unpack-chain:") {
        let bytes = ascii_payload("unpack-chain", size)?;
        return Ok(format!("v8.4\n0\nbsunpack \"{bytes}\" @ #0 @ K @ }}\n").into_bytes());
    }
    if let Some(size) = scenario.strip_prefix("fromutf8-chain:") {
        let bytes = ascii_payload("fromutf8-chain", size)?;
        return Ok(format!("v8.4\n0\nfromUTF8 \"{bytes}\" @ #0 @ K @ }}\n").into_bytes());
    }
    if let Some(size) = scenario.strip_prefix("array-chain:") {
        let size = parse_scenario_size("array-chain", size)?;
        let last = size - 1;
        let mut items = String::new();
        for _ in 0..size {
            items.push_str("#0 ");
        }
        return Ok(format!(
            "v8.4\n1\nseq A.write {items}[{size}] :0 @ #{last} @ #42 @ @ A.read _0 @ #{last} @ @ }}\n"
        )
        .into_bytes());
    }
    if let Some(size) = scenario.strip_prefix("io-chain:") {
        let size = parse_scenario_size("io-chain", size)?;
        let mut expr = String::from("IO.return #0 @");
        for idx in 1..size {
            expr = format!("IO.>> {expr} @ IO.return #{idx} @ @");
        }
        return Ok(format!("v8.4\n0\nIO.performIO {expr} @ }}\n").into_bytes());
    }
    if let Some(size) = scenario.strip_prefix("io-array-chain:") {
        let size = parse_scenario_size("io-array-chain", size)?;
        let last = size - 1;
        let mut items = String::new();
        for _ in 0..size {
            items.push_str("#0 ");
        }
        return Ok(format!(
            "v8.4\n1\nIO.performIO IO.>> A.write {items}[{size}] :0 @ #{last} @ #42 @ @ A.read _0 @ #{last} @ @ @ }}\n"
        )
        .into_bytes());
    }
    if let Some(size) = scenario.strip_prefix("io-bytes-chain:") {
        let bytes = ascii_payload("io-bytes-chain", size)?;
        let last = bytes.len() - 1;
        return Ok(format!(
            "v8.4\n1\nIO.performIO IO.>> bswrite \"{bytes}\" :0 @ #{last} @ #42 @ @ bsread _0 @ #{last} @ @ @ }}\n"
        )
        .into_bytes());
    }
    if let Some(size) = scenario.strip_prefix("zoo-chain:") {
        let size = parse_scenario_size("zoo-chain", size)?;
        let mut expr = String::from("#1");
        for idx in 0..size {
            expr = match idx % 11 {
                0 => format!("S' K @ K @ K @ {expr} @ #0 @"),
                1 => format!("B' K @ {expr} @ K @ #0 @"),
                2 => format!("Z K @ {expr} @ #0 @ #1 @"),
                3 => format!("J {expr} @ #0 @ I @"),
                4 => format!("L {expr} @ I @ #0 @"),
                5 => format!("KK #0 @ {expr} @ #1 @"),
                6 => format!("KA #0 @ #1 @ {expr} @"),
                7 => format!("C' A @ K @ {expr} @ #0 @"),
                8 => format!("R #0 @ K @ {expr} @"),
                9 => format!("O {expr} @ #0 @ #1 @ K @"),
                _ => format!("C'B K @ K @ {expr} @ #0 @"),
            };
        }
        return Ok(format!("v8.4\n0\n{expr} }}\n").into_bytes());
    }
    if let Some(size) = scenario.strip_prefix("data-chain:") {
        let size = parse_scenario_size("data-chain", size)?;
        let mut expr = String::from("#1");
        for idx in 0..size {
            expr = if idx % 3 == 0 {
                format!("TAG{} {expr} @ A @", idx % 33)
            } else if idx % 3 == 1 {
                format!("T3 {expr} @ #0 @ #1 @ K3 @ #0 @")
            } else {
                format!("T4 {expr} @ #0 @ #1 @ #2 @ K4 @ #0 @")
            };
        }
        return Ok(format!("v8.4\n0\n{expr} }}\n").into_bytes());
    }
    Err(format!("unsupported scenario: {scenario}"))
}

fn parse_scenario_size(name: &str, size: &str) -> Result<usize, String> {
    let size: usize = size
        .parse()
        .map_err(|_| format!("invalid {name} size: {size}"))?;
    if size == 0 {
        return Err(format!("{name} size must be greater than zero"));
    }
    Ok(size)
}

fn ascii_payload(name: &str, size: &str) -> Result<String, String> {
    let size = parse_scenario_size(name, size)?;
    let mut bytes = String::with_capacity(size);
    for idx in 0..size {
        bytes.push((b'a' + (idx % 26) as u8) as char);
    }
    Ok(bytes)
}

struct ParseBench {
    elapsed: Duration,
}

fn bench_parse(input: &[u8], iters: usize) -> ParseBench {
    let started = Instant::now();
    for _ in 0..iters {
        let program = parse_program(black_box(input)).expect("parse benchmark input");
        black_box(program.nodes().len());
    }
    ParseBench {
        elapsed: started.elapsed(),
    }
}

struct ReduceBench {
    elapsed: Duration,
    steps: usize,
}

fn bench_reduce(input: &[u8], iters: usize) -> ReduceBench {
    let started = Instant::now();
    let mut steps = 0;
    for _ in 0..iters {
        let mut program = parse_program(black_box(input)).expect("reduce benchmark input");
        let (root, n) = program
            .reduce_whnf(usize::MAX)
            .expect("reduce benchmark input");
        black_box(program.render(root));
        steps += n;
    }
    ReduceBench {
        elapsed: started.elapsed(),
        steps,
    }
}

struct CBench {
    elapsed: Duration,
}

fn bench_c_mhseval(input: &[u8], c_mhseval: &str, iters: usize) -> Result<CBench, String> {
    let file = env::temp_dir().join(format!("mhs-rust-bench-{}.comb", std::process::id()));
    fs::write(&file, input).map_err(|err| format!("{}: {err}", file.display()))?;
    let file_arg = format!("-r{}", file.display());

    let started = Instant::now();
    for _ in 0..iters {
        let status = Command::new(c_mhseval)
            .args(["+RTS", "-H1M", &file_arg, "-o/dev/null", "-RTS"])
            .status()
            .map_err(|err| format!("{c_mhseval}: {err}"))?;
        if !status.success() {
            let _ = fs::remove_file(&file);
            return Err(format!("{c_mhseval} exited with {status}"));
        }
    }
    let elapsed = started.elapsed();
    let _ = fs::remove_file(&file);
    Ok(CBench { elapsed })
}

fn millis(duration: Duration) -> f64 {
    duration.as_secs_f64() * 1_000.0
}

fn nanos_per_iter(duration: Duration, iters: usize) -> f64 {
    duration.as_secs_f64() * 1_000_000_000.0 / iters as f64
}

fn mib_per_s(bytes: usize, iters: usize, duration: Duration) -> f64 {
    let mib = (bytes as f64 * iters as f64) / (1024.0 * 1024.0);
    mib / duration.as_secs_f64()
}

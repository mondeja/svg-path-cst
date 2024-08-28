use svg_path_cst::svg_path_cst;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.contains(&String::from("--help"))
        || args.contains(&String::from("-h"))
        || args.len() < 2
    {
        eprintln!("Usage: svg-path-cst \"<path>\"");
        std::process::exit(1);
    }

    let svg_path = args[args.len() - 1].as_bytes();
    match svg_path_cst(svg_path) {
        Ok(result) => {
            #[allow(clippy::print_stdout)]
            {
                println!("{result:?}");
            }
        }
        Err(e) => {
            eprintln!("Error: {e}");
            std::process::exit(1);
        }
    }
}

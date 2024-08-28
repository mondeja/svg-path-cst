use svg_path_cst::svg_path_cst;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let data = args[args.len() - 1].as_bytes();
    match svg_path_cst(data) {
        Ok(result) => {
            #[allow(clippy::print_stdout)]
            {
                println!("{result:?}");
            }
        }
        Err(e) => {
            eprintln!("{e:?}");
            std::process::exit(1);
        }
    }
}

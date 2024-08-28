fn init_tracing() -> tracing_appender::non_blocking::WorkerGuard {
    let (non_blocking, guard) = tracing_appender::non_blocking(std::io::stdout());
    let filter = tracing_subscriber::EnvFilter::builder()
        .with_default_directive(tracing::metadata::LevelFilter::TRACE.into())
        .from_env()
        .unwrap();

    tracing_subscriber::fmt()
        .with_writer(non_blocking)
        .with_env_filter(filter)
        .with_target(false)
        .with_file(false)
        .with_thread_ids(true)
        .with_span_events(tracing_subscriber::fmt::format::FmtSpan::ACTIVE)
        .init();
    guard
}

fn main() {
    let _tracing_guard = init_tracing();
    let path = include_bytes!("../../../fuzz/corpus/simpleicons.txt");
    _ = svg_path_cst::svg_path_cst(path).unwrap();
}

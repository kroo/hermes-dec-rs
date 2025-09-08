use hermes_dec_rs::{
    cfg::{analysis::LoopType, Cfg},
    decompiler::Decompiler,
    hbc::HbcFile,
};
use std::fs;
use std::path::Path;

fn analyze_loops(hbc_path: &Path, func_index: u32) -> Option<Vec<LoopType>> {
    let data = fs::read(hbc_path).ok()?;
    let hbc = HbcFile::parse(&data).ok()?;
    let mut cfg = Cfg::new(&hbc, func_index);
    cfg.build();
    let loop_analysis = cfg.analyze_loops();
    if loop_analysis.loops.is_empty() {
        return None;
    }
    Some(loop_analysis.loops.iter().map(|l| l.loop_type.clone()).collect())
}

fn decompile(hbc_path: &Path, func_index: u32) -> Option<String> {
    let data = fs::read(hbc_path).ok()?;
    let hbc = HbcFile::parse(&data).ok()?;
    let mut decompiler = Decompiler::new().ok()?;
    decompiler.decompile_function(&hbc, func_index).ok()
}

#[test]
fn test_while_loop_round_trip() -> Result<(), Box<dyn std::error::Error>> {
    let hbc_path = Path::new("data/while_loop.hbc");
    if !hbc_path.exists() {
        eprintln!("missing fixture {:?}; skipping test", hbc_path);
        return Ok(());
    }
    let loops = analyze_loops(hbc_path, 1).unwrap();
    assert_eq!(loops, vec![LoopType::While]);
    let output = decompile(hbc_path, 1).unwrap();
    assert!(output.contains("while"));
    assert!(output.contains("<"));
    Ok(())
}

#[test]
fn test_loop_type_detection() -> Result<(), Box<dyn std::error::Error>> {
    let hbc_path = Path::new("data/loop_types.hbc");
    if !hbc_path.exists() {
        eprintln!("missing fixture {:?}; skipping test", hbc_path);
        return Ok(());
    }
    let loops = analyze_loops(hbc_path, 1).unwrap();
    assert!(loops.contains(&LoopType::While));
    assert!(loops.contains(&LoopType::DoWhile));
    assert!(loops.contains(&LoopType::For));
    assert!(loops.contains(&LoopType::ForIn));
    assert!(loops.contains(&LoopType::ForOf));
    Ok(())
}

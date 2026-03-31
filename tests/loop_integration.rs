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
    Some(
        loop_analysis
            .loops
            .iter()
            .map(|l| l.loop_type.clone())
            .collect(),
    )
}

fn decompile(hbc_path: &Path, func_index: u32) -> Option<String> {
    let data = fs::read(hbc_path).ok()?;
    let hbc = HbcFile::parse(&data).ok()?;
    let mut decompiler = Decompiler::new().ok()?;
    decompiler.decompile_function(&hbc, func_index).ok()
}

fn count_occurrences(haystack: &str, needle: &str) -> usize {
    haystack.matches(needle).count()
}

#[test]
fn test_while_loop_round_trip() -> Result<(), Box<dyn std::error::Error>> {
    let hbc_path = Path::new("data/while_loop.hbc");
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
    let loops = analyze_loops(hbc_path, 1).unwrap();
    assert_eq!(
        loops,
        vec![
            LoopType::While,
            LoopType::While,
            LoopType::While,
            LoopType::ForIn,
            LoopType::ForOf,
        ]
    );

    Ok(())
}

#[test]
fn test_forin_fallback_bails_out_explicitly() -> Result<(), Box<dyn std::error::Error>> {
    let hbc_path = Path::new("data/loop_types.hbc");
    let output = decompile(hbc_path, 1).unwrap();
    assert!(output.contains("throw new Error(\"Unsupported for-in loop fallback\")"));
    assert!(!output.contains("[Symbol.iterator]()"));

    Ok(())
}

#[test]
fn test_try_in_loop_preserves_loop_shape() -> Result<(), Box<dyn std::error::Error>> {
    let hbc_path = Path::new("data/try_catch_test.hbc");
    let output = decompile(hbc_path, 4).unwrap();
    assert!(output.contains("do {"));
    assert!(output.contains("catch ("));
    assert!(output.contains("var6 = 0;"));
    assert!(!output.contains("switch (0)"));
    assert!(!output.contains("try {}"));
    assert!(!output.contains("while (var0 === var1)"));
    assert_eq!(count_occurrences(&output, "let var6;"), 1);
    assert_eq!(
        count_occurrences(&output, "const var10 = \"Loop error\";"),
        1
    );

    Ok(())
}

#[test]
fn test_forof_fallback_uses_iterator_next_exit() -> Result<(), Box<dyn std::error::Error>> {
    let hbc_path = Path::new("data/ast-04-tests/test_rest_params.hbc");
    let output = decompile(hbc_path, 1).unwrap();
    assert!(output.contains("const var2 = var3[Symbol.iterator]();"));
    assert!(output.contains("const var5 = var2.next();"));
    assert!(output.contains("if (var5 === undefined)"));
    assert!(output.contains("finally {"));
    assert!(output.contains("var2.return && var2.return();"));
    assert_eq!(count_occurrences(&output, "var0 = var4_a + var5;"), 1);

    Ok(())
}

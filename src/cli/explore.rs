//! Bounded, evidence-addressable exploration without whole-project CFG analysis.
use crate::bundle::{export_function_fragments, operands};
use crate::hbc::serialized_literal_parser::{unpack_slp_array, SLPValue};
use crate::{DecompilerError, DecompilerResult, HbcFile};
use regex::RegexBuilder;
use serde_json::{json, Value};
use std::collections::{BTreeSet, HashSet};
use std::path::Path;

fn error(message: impl Into<String>) -> DecompilerError {
    DecompilerError::Internal {
        message: message.into(),
    }
}

fn parse(data: &[u8]) -> DecompilerResult<HbcFile<'_>> {
    HbcFile::parse_for_bundle(data).map_err(error)
}

fn preview(value: &str) -> String {
    let mut chars = value.chars();
    let mut result: String = chars.by_ref().take(180).collect();
    if chars.next().is_some() {
        result.push_str("...");
    }
    result
}

fn pc_excerpt(code: &str, id: u32, start: usize, end: usize) -> String {
    let prefix = format!("// HBC function {id}, PC ");
    let mut index = None;
    let mut excerpt = String::new();
    for line in code.split_inclusive('\n') {
        if line
            .strip_prefix(&prefix)
            .is_some_and(|pc| pc.trim().parse::<u32>().is_ok())
        {
            index = Some(index.map_or(0, |n| n + 1));
        }
        if index.is_some_and(|n| n >= end) {
            break;
        }
        if index.is_some_and(|n| n >= start) {
            excerpt.push_str(line);
        }
    }
    excerpt
}

fn strings_in_instruction(
    hbc: &HbcFile<'_>,
    i: &crate::hbc::tables::function_table::HbcFunctionInstruction,
) -> DecompilerResult<Vec<u32>> {
    let name = i.instruction.name();
    let position = match name {
        "LoadConstString" | "LoadConstStringLongIndex" => Some(1),
        "GetById" | "GetByIdShort" | "GetByIdLong" | "TryGetById" | "TryGetByIdLong"
        | "PutById" | "PutByIdLong" | "TryPutById" | "TryPutByIdLong" => Some(3),
        "PutNewOwnById"
        | "PutNewOwnByIdShort"
        | "PutNewOwnByIdLong"
        | "PutNewOwnNEById"
        | "PutNewOwnNEByIdLong"
        | "DelById"
        | "DelByIdLong" => Some(2),
        "DeclareGlobalVar" | "ThrowIfHasRestrictedGlobalProperty" => Some(0),
        _ => None,
    };
    if let Some(position) = position {
        return Ok(vec![operands(&i.instruction)?[position] as u32]);
    }
    if name == "CreateRegExp" {
        let o = operands(&i.instruction)?;
        return Ok(vec![o[1] as u32, o[2] as u32]);
    }
    let buffers = match name {
        "NewArrayWithBuffer" | "NewArrayWithBufferLong" => {
            let o = operands(&i.instruction)?;
            vec![(
                hbc.serialized_literals.arrays_data,
                o[3] as usize,
                o[2] as usize,
            )]
        }
        "NewObjectWithBuffer" | "NewObjectWithBufferLong" => {
            let o = operands(&i.instruction)?;
            vec![
                (
                    hbc.serialized_literals.object_keys_data,
                    o[3] as usize,
                    o[2] as usize,
                ),
                (
                    hbc.serialized_literals.object_values_data,
                    o[4] as usize,
                    o[2] as usize,
                ),
            ]
        }
        _ => return Ok(Vec::new()),
    };
    let mut ids = Vec::new();
    for (data, offset, count) in buffers {
        let slice = data
            .get(offset..)
            .ok_or_else(|| error("Invalid literal offset"))?;
        for value in unpack_slp_array(slice, Some(count)).map_err(error)?.items {
            match value {
                SLPValue::LongString(id) => ids.push(id),
                SLPValue::ShortString(id) => ids.push(u32::from(id)),
                SLPValue::ByteString(id) => ids.push(u32::from(id)),
                _ => {}
            }
        }
    }
    Ok(ids)
}

fn edges(hbc: &HbcFile<'_>) -> DecompilerResult<Vec<Value>> {
    let mut result = Vec::new();
    for id in 0..hbc.functions.count() {
        for i in hbc.functions.get_instructions_ref(id)? {
            let name = i.instruction.name();
            let kind = match name {
                "CreateClosure"
                | "CreateClosureLongIndex"
                | "CreateGeneratorClosure"
                | "CreateGeneratorClosureLongIndex"
                | "CreateAsyncClosure"
                | "CreateAsyncClosureLongIndex"
                | "CreateGenerator"
                | "CreateGeneratorLongIndex" => "closure",
                "CallDirect" | "CallDirectLongIndex" => "direct_call",
                _ => continue,
            };
            result.push(json!({"from":id,"to":operands(&i.instruction)?[2],"pc":i.offset.value(),"kind":kind}));
        }
    }
    Ok(result)
}

fn emit(value: &Value, compact: bool, output: Option<&Path>) -> DecompilerResult<()> {
    let code = if compact {
        serde_json::to_string(value)
    } else {
        serde_json::to_string_pretty(value)
    }
    .map_err(|e| error(e.to_string()))?;
    match output {
        Some(path) => std::fs::write(path, format!("{code}\n"))?,
        None => println!("{code}"),
    }
    Ok(())
}

pub struct SearchOptions {
    pub regex: bool,
    pub case_sensitive: bool,
    pub word: bool,
    pub all: bool,
    pub limit: usize,
    pub offset: usize,
    pub json: bool,
}

pub fn search(input: &Path, queries: &[String], options: &SearchOptions) -> DecompilerResult<()> {
    if options.limit == 0 || options.limit > 1000 {
        return Err(error("--limit must be 1..1000"));
    }
    let patterns = queries
        .iter()
        .map(|q| {
            let pattern = if options.regex {
                q.clone()
            } else {
                regex::escape(q)
            };
            let pattern = if options.word {
                format!("(?:^|[^[:alnum:]])(?:{pattern})(?:$|[^[:alnum:]])")
            } else {
                pattern
            };
            RegexBuilder::new(&pattern)
                .case_insensitive(!options.case_sensitive)
                .build()
                .map_err(|e| error(e.to_string()))
        })
        .collect::<DecompilerResult<Vec<_>>>()?;
    let matches = |s: &str| patterns.iter().any(|p| p.is_match(s));
    let data = std::fs::read(input)?;
    let hbc = parse(&data)?;
    let query_indices = |s: &str| {
        patterns
            .iter()
            .enumerate()
            .filter_map(|(index, pattern)| pattern.is_match(s).then_some(index))
            .collect::<BTreeSet<_>>()
    };
    let matched_strings: std::collections::BTreeMap<u32, BTreeSet<usize>> =
        (0..hbc.strings.string_count)
            .filter_map(|id| {
                let indices = query_indices(&hbc.strings.get(id).ok()?);
                (!indices.is_empty()).then_some((id, indices))
            })
            .collect();
    let mut results = Vec::new();
    let mut bindings = std::collections::BTreeMap::<u32, Vec<Value>>::new();
    let mut binding_queries = std::collections::BTreeMap::<u32, BTreeSet<usize>>::new();
    for binding in super::bindings::collect(&hbc)? {
        if matches(&binding.name) || (!binding.target.is_empty() && matches(&binding.target)) {
            let indices = binding_queries.entry(binding.function_id).or_default();
            indices.extend(query_indices(&binding.name));
            if !binding.target.is_empty() {
                indices.extend(query_indices(&binding.target));
            }
            bindings.entry(binding.function_id).or_default().push(json!({"name":preview(&binding.name),"target":preview(&binding.target),"source_function_id":binding.source_function_id,"pc":binding.pc,"kind":"static_property_assignment"}));
        }
    }
    for id in 0..hbc.functions.count() {
        let name = hbc
            .functions
            .get_function_name(id, &hbc.strings)
            .unwrap_or_default();
        let name_match = matches(&name);
        let mut matched_queries = query_indices(&name);
        matched_queries.extend(binding_queries.remove(&id).unwrap_or_default());
        let mut evidence = Vec::new();
        let mut hits = BTreeSet::new();
        let mut reference_count = 0usize;
        for i in hbc.functions.get_instructions_ref(id)? {
            for string_id in strings_in_instruction(&hbc, i)? {
                if let Some(indices) = matched_strings.get(&string_id) {
                    matched_queries.extend(indices.iter().copied());
                    reference_count += 1;
                    hits.insert(string_id);
                    if evidence.len() < 12 {
                        evidence.push(json!({"string_id":string_id,"pc":i.offset.value(),"text":preview(&hbc.strings.get(string_id).map_err(error)?)}));
                    }
                }
            }
        }
        let assignments = bindings.remove(&id).unwrap_or_default();
        let assignment_count = assignments.len();
        if (name_match || !hits.is_empty() || assignment_count > 0)
            && (!options.all || matched_queries.len() == patterns.len())
        {
            results.push(((assignment_count > 0, hits.len() + usize::from(name_match) + assignment_count), json!({"function_id":id,"name":preview(&name),"name_match":name_match,"matching_strings":hits.len(),"matching_references":reference_count,"evidence":evidence,"evidence_truncated":reference_count>12,"matching_assignments":assignment_count,"assignments":assignments.into_iter().take(12).collect::<Vec<_>>(),"assignments_truncated":assignment_count>12})));
        }
    }
    results.sort_by(|a, b| {
        b.0.cmp(&a.0).then_with(|| {
            a.1["function_id"]
                .as_u64()
                .cmp(&b.1["function_id"].as_u64())
        })
    });
    let total = results.len();
    let next = options.offset.saturating_add(options.limit).min(total);
    emit(
        &json!({"schema_version":1,"hbc_version":hbc.header.version(),"query_mode":if options.all {"all_in_function"} else {"any_in_function"},"total":total,"offset":options.offset,"next_offset":if next<total {Some(next)} else {None},"functions":results.into_iter().skip(options.offset).take(options.limit).map(|(_,v)|v).collect::<Vec<_>>(),"notes":"Substring search (or explicit --regex/--word); OR by default, --all requires each query to match somewhere in the function's name, literal/property references, or assigned aliases, not necessarily the same instruction. String previews are bounded and UTF-16 display is lossy. Assignments are direct local closure-to-property evidence, not runtime exports, slot values, or a dynamic call graph. Assignment matches rank first. Use show for complete JS; refs/slots for static closure/environment candidates."}),
        options.json,
        None,
    )
}

pub fn show(
    input: &Path,
    ids: &[u32],
    output: Option<&Path>,
    json_output: bool,
    max_bytes: usize,
    around_pc: Option<u32>,
    context: usize,
) -> DecompilerResult<()> {
    if around_pc.is_some() && (ids.len() != 1 || context > 1000) {
        return Err(error(
            "--around-pc requires one function and --context <= 1000 (use --max-bytes for a larger excerpt budget)",
        ));
    }
    let data = std::fs::read(input)?;
    let hbc = parse(&data)?;
    if let Some(pc) = around_pc {
        let id = ids[0];
        let instructions = hbc.functions.get_instructions_ref(id)?;
        let index = instructions
            .iter()
            .position(|i| i.offset.value() == pc)
            .ok_or_else(|| {
                error(format!(
                    "PC {pc} is not an instruction boundary in function {id}"
                ))
            })?;
        let start = index.saturating_sub(context);
        let end = index
            .saturating_add(context)
            .saturating_add(1)
            .min(instructions.len());
        let code = export_function_fragments(&hbc, &[id])?.remove(0).1;
        let excerpt = pc_excerpt(&code, id, start, end);
        let value = json!({"schema_version":1,"function_id":id,"requested_pc":pc,"first_pc":instructions[start].offset.value(),"last_pc":instructions[end-1].offset.value(),"next_pc":instructions.get(end).map(|i|i.offset.value()),"javascript_excerpt":excerpt,"complete_function":false,"covers_all_instructions":start==0 && end==instructions.len(),"notes":"Excerpt of correctness-first JS, not a standalone program. Surrounding dispatch braces may be omitted. Use show without --around-pc for the complete function; export-bundle for runtime helper definitions."});
        let serialized = serde_json::to_string(&value).map_err(|e| error(e.to_string()))?;
        if serialized.len() > max_bytes {
            return Err(error(format!(
                "JS excerpt requires {} bytes; raise --max-bytes",
                serialized.len()
            )));
        }
        return emit(&value, true, output);
    }
    let mut functions = Vec::new();
    let mut code = String::new();
    let selected: HashSet<u32> = ids.iter().copied().collect();
    let mut assignments = std::collections::BTreeMap::<u32, Vec<Value>>::new();
    for binding in super::bindings::collect(&hbc)? {
        if selected.contains(&binding.function_id) {
            assignments
                .entry(binding.function_id)
                .or_default()
                .push(json!({
                    "name": preview(&binding.name), "target": preview(&binding.target),
                    "source_function_id": binding.source_function_id, "pc": binding.pc,
                    "kind": "static_property_assignment"
                }));
        }
    }
    for (id, js) in export_function_fragments(&hbc, ids)? {
        let sites = assignments.get(&id).map(Vec::as_slice).unwrap_or(&[]);
        let metadata = json!({"function_id":id,"name":hbc.functions.get_function_name(id,&hbc.strings),"bytecode_bytes":hbc.functions.get_parsed_header(id).unwrap().body.len(),"static_assignments":sites.iter().take(12).collect::<Vec<_>>(),"static_assignments_truncated":sites.len()>12,"javascript":js});
        if json_output {
            functions.push(metadata);
        } else {
            if !sites.is_empty() {
                // JSON escaping keeps names containing newlines safe in a JS comment.
                code.push_str(&format!(
                    "// Static assignment candidates (not runtime exports): {}\n",
                    serde_json::to_string(&sites.iter().take(12).collect::<Vec<_>>())
                        .map_err(|e| error(e.to_string()))?
                        .replace('\u{2028}', "\\u2028")
                        .replace('\u{2029}', "\\u2029")
                ));
                if sites.len() > 12 {
                    code.push_str("// [additional assignment candidates omitted]\n");
                }
            }
            code.push_str(&js);
        }
    }
    if !json_output {
        code.insert_str(0, "// Decompiled JS fragments; not standalone. F = bodies; M = metadata; r = registers.\n// env = captured environment; self = this; args = arguments.\n// Helpers and referenced functions are defined by export-bundle.\n");
    }
    if json_output {
        code = serde_json::to_string(
            &json!({"schema_version":1,"functions":functions,"standalone":false}),
        )
        .map_err(|e| error(e.to_string()))?;
    }
    if code.len() > max_bytes {
        return Err(error(format!("Complete JS requires {} bytes (budget {max_bytes}); select fewer functions, raise --max-bytes, or use -o with --max-bytes. No truncated JS was emitted.",code.len())));
    }
    match output {
        Some(path) => std::fs::write(path, code)?,
        None => println!("{code}"),
    }
    Ok(())
}

pub fn refs(
    input: &Path,
    ids: &[u32],
    direction: &str,
    depth: usize,
    limit: usize,
) -> DecompilerResult<()> {
    if depth == 0 || depth > 8 || limit == 0 || limit > 10000 {
        return Err(error("--depth must be 1..8 and --limit 1..10000"));
    }
    let data = std::fs::read(input)?;
    let hbc = parse(&data)?;
    for &id in ids {
        if id >= hbc.functions.count() {
            return Err(error(format!("Unknown function {id}")));
        }
    }
    let edges = edges(&hbc)?;
    let mut reached: HashSet<u32> = ids.iter().copied().collect();
    let mut selected = BTreeSet::new();
    for _ in 0..depth {
        let mut next = reached.clone();
        for (index, e) in edges.iter().enumerate() {
            let from = e["from"].as_u64().unwrap() as u32;
            let to = e["to"].as_u64().unwrap() as u32;
            if (direction != "in" && reached.contains(&from))
                || (direction != "out" && reached.contains(&to))
            {
                selected.insert(index);
                next.insert(from);
                next.insert(to);
            }
        }
        reached = next;
    }
    emit(
        &json!({"schema_version":1,"total":selected.len(),"truncated":selected.len()>limit,"edges":selected.into_iter().take(limit).map(|i|&edges[i]).collect::<Vec<_>>(),"notes":"Static closure creation and direct calls only; not a complete dynamic call graph. 'in' finds creators/callers; 'out' finds created/called functions."}),
        true,
        None,
    )
}

#[cfg(test)]
mod tests {
    use super::pc_excerpt;

    #[test]
    fn marker_text_in_a_literal_is_not_a_pc_boundary() {
        let code = "// HBC function 0, PC 0\nr[0] = \"// HBC function 0, PC 999\";\n// HBC function 0, PC 4\nreturn r[0];\n";
        assert_eq!(
            pc_excerpt(code, 0, 1, 2),
            "// HBC function 0, PC 4\nreturn r[0];\n"
        );
        assert!(pc_excerpt(code, 0, 0, 1).contains("PC 999"));
        assert!(pc_excerpt(code, 1, 0, 1).is_empty());
    }
}

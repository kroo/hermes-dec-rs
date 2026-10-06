//! Independent, non-evaluating projection of origins-v1 reports.
use crate::{DecompilerError, DecompilerResult};
use serde::Deserialize;
use serde_json::{json, Value};
use std::collections::BTreeMap;

fn error(message: impl Into<String>) -> DecompilerError {
    DecompilerError::internal(format!("origins text: {}", message.into()))
}

#[derive(Deserialize)]
struct Report {
    schema_version: u32,
    schema: String,
    semantics: String,
    function: u32,
    pc: u32,
    unknown: Vec<String>,
    truncated: bool,
    unresolved: bool,
    blocks: usize,
    normal_edges: Vec<(u32, u32)>,
    normal_edges_total: usize,
    normal_edges_returned: usize,
    normal_edges_truncated: bool,
    control_index_work: usize,
    limits: BTreeMap<String, u64>,
    definitions: Vec<Definition>,
    demands: Vec<Demand>,
    #[serde(flatten)]
    extra: BTreeMap<String, Value>,
}
#[derive(Deserialize)]
struct Definition {
    id: u64,
    pc: u32,
    block_pc: u32,
    register: u32,
    source: Excerpt,
    #[serde(flatten)]
    extra: BTreeMap<String, Value>,
}
#[derive(Deserialize)]
struct Demand {
    // Value deliberately requires the field even when its value is null.
    owner_definition: Value,
    pc: u32,
    register: u32,
    read: Excerpt,
    candidates: Vec<u64>,
    unresolved: bool,
    same_pc_ambiguity: bool,
    cycle: bool,
    truncated: bool,
    #[serde(flatten)]
    extra: BTreeMap<String, Value>,
}
#[derive(Deserialize, serde::Serialize)]
struct Excerpt {
    start: u32,
    end: u32,
    javascript: String,
    snippet_truncated: bool,
}
impl Excerpt {
    fn validate(&self) -> DecompilerResult<()> {
        let length = self
            .end
            .checked_sub(self.start)
            .ok_or_else(|| error("reversed span"))? as usize;
        if self.javascript.len() > length
            || self.snippet_truncated != (self.javascript.len() < length)
        {
            return Err(error("inconsistent UTF-8 excerpt length"));
        }
        Ok(())
    }
}

fn summary(view: &Value) -> DecompilerResult<Value> {
    let object = view
        .as_object()
        .ok_or_else(|| error("expression must be an object"))?;
    let nodes = object
        .get("nodes")
        .and_then(Value::as_array)
        .ok_or_else(|| error("expression nodes missing"))?;
    if object.get("projected_nodes").and_then(Value::as_u64) != Some(nodes.len() as u64)
        || object.get("schema_version") != Some(&json!(1))
        || object.get("semantics") != Some(&json!("syntax_only"))
        || !object.get("omissions").is_some_and(Value::is_array)
        || !object.get("truncated").is_some_and(Value::is_boolean)
        || !object.get("limits").is_some_and(Value::is_object)
        || !object.contains_key("root")
        || !object.contains_key("source")
    {
        return Err(error("inconsistent expression schema"));
    }
    let mut result = object.clone();
    result.remove("nodes");
    let mut counts = BTreeMap::<String, usize>::new();
    for node in nodes {
        for key in node
            .as_object()
            .ok_or_else(|| error("invalid expression node"))?
            .keys()
        {
            *counts.entry(key.clone()).or_default() += 1;
        }
    }
    result.insert("renderer_omitted_nodes".into(), json!(nodes.len()));
    result.insert("node_field_counts".into(), json!(counts));
    Ok(Value::Object(result))
}

/// Render atomically; a budget failure never returns a partial report.
pub fn render(report: &Value, max_bytes: usize) -> DecompilerResult<Vec<u8>> {
    if max_bytes == 0 || max_bytes > 16 * 1024 * 1024 {
        return Err(error("byte budget must be 1..16777216"));
    }
    let r: Report = serde_json::from_value(report.clone()).map_err(|e| error(e.to_string()))?;
    if r.schema != "origins-v1"
        || r.schema_version != 1
        || r.limits.is_empty()
        || r.normal_edges_returned != r.normal_edges.len()
        || r.normal_edges_total < r.normal_edges_returned
        || r.normal_edges_truncated != (r.normal_edges_total > r.normal_edges_returned)
    {
        return Err(error("inconsistent report schema or edge counts"));
    }
    let expression_fields = [
        "instruction_expressions",
        "instruction_expressions_total",
        "instruction_expressions_omitted",
        "expressions_truncated",
        "expression_source",
    ];
    if expression_fields
        .iter()
        .any(|key| r.extra.contains_key(*key))
    {
        if expression_fields
            .iter()
            .any(|key| !r.extra.contains_key(*key))
        {
            return Err(error("incomplete expression report"));
        }
        let returned = r.extra["instruction_expressions"]
            .as_array()
            .ok_or_else(|| error("invalid expression list"))?
            .len() as u64;
        let total = r.extra["instruction_expressions_total"]
            .as_u64()
            .ok_or_else(|| error("invalid expression total"))?;
        let omitted = r.extra["instruction_expressions_omitted"]
            .as_u64()
            .ok_or_else(|| error("invalid expression omission count"))?;
        if returned.checked_add(omitted) != Some(total)
            || !r.extra["expressions_truncated"].is_boolean()
            || !r.extra["expression_source"].is_object()
        {
            return Err(error("inconsistent expression counts or source"));
        }
    }
    let mut ids = BTreeMap::new();
    for d in &r.definitions {
        d.source.validate()?;
        if ids.insert(d.id, d.register).is_some() {
            return Err(error("duplicate definition ID"));
        }
    }
    for d in &r.demands {
        d.read.validate()?;
        if !d.owner_definition.is_null()
            && !d
                .owner_definition
                .as_u64()
                .is_some_and(|id| ids.contains_key(&id))
        {
            return Err(error("invalid demand owner"));
        }
        if d.candidates
            .iter()
            .any(|id| ids.get(id) != Some(&d.register))
        {
            return Err(error(
                "missing or register-inconsistent candidate definition",
            ));
        }
    }
    let mut out = Vec::new();
    let mut line = |label: &str, value: Value| -> DecompilerResult<()> {
        let encoded = serde_json::to_vec(&value).map_err(|e| error(e.to_string()))?;
        if out
            .len()
            .saturating_add(label.len())
            .saturating_add(encoded.len())
            .saturating_add(2)
            > max_bytes
        {
            return Err(error("output byte budget exceeded"));
        }
        out.extend_from_slice(label.as_bytes());
        out.push(b' ');
        out.extend(encoded);
        out.push(b'\n');
        Ok(())
    };
    line(
        "origins",
        json!({"renderer_schema_version":1,"source_schema":r.schema,"function":r.function,"pc":r.pc,"semantics":r.semantics,"candidates":"normal-flow candidates, not runtime bindings","offset_unit":"utf8_bytes","offset_origin":"complete_raw_exporter_fragment_without_inspection_header","unknown":r.unknown,"truncated":r.truncated,"unresolved":r.unresolved,"byte_budget_policy":"Text is bounded by the requested --max-bytes; limits.output_bytes describes the intermediate JSON report budget, not the text budget.","expression_policy":"Typed graph nodes are omitted in this renderer; summaries retain source/count/omission metadata only. Local graph node IDs cannot be resolved in text output. Use JSON for syntax graphs."}),
    )?;
    line("limits", json!(r.limits))?;
    line(
        "flow",
        json!({"blocks":r.blocks,"normal_edges":r.normal_edges,"total":r.normal_edges_total,"returned":r.normal_edges_returned,"truncated":r.normal_edges_truncated,"control_index_work":r.control_index_work}),
    )?;
    for d in r.definitions {
        line(
            "definition",
            json!({"id":d.id,"register":d.register,"pc":d.pc,"block_pc":d.block_pc,"source":d.source}),
        )?;
        for (key, value) in d.extra {
            let value = if key == "expression" {
                summary(&value)?
            } else {
                value
            };
            line(
                "definition-extra",
                json!({"id":d.id,"field":key,"value":value}),
            )?;
        }
    }
    for d in r.demands {
        line(
            "demand",
            json!({"owner_definition":d.owner_definition,"register":d.register,"pc":d.pc,"read":d.read,"candidates":d.candidates,"unresolved":d.unresolved,"same_pc_ambiguity":d.same_pc_ambiguity,"cycle":d.cycle,"truncated":d.truncated,"extra":d.extra}),
        )?;
    }
    for (key, value) in r.extra {
        let value = if key == "instruction_expressions" {
            Value::Array(
                value
                    .as_array()
                    .ok_or_else(|| error("invalid instruction expressions"))?
                    .iter()
                    .map(summary)
                    .collect::<DecompilerResult<Vec<_>>>()?,
            )
        } else {
            value
        };
        line("report-extra", json!({"field":key,"value":value}))?;
    }
    Ok(out)
}

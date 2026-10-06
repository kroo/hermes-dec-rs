// Structural evidence checks only. Never evaluates recovered JavaScript.
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import readline from 'node:readline';
import {fileURLToPath} from 'node:url';

function span(raw, start, end) {
  assert(Number.isSafeInteger(start) && Number.isSafeInteger(end));
  assert(start >= 0 && end >= start && end <= raw.length, 'source span bounds');
  for (const offset of [start, end]) {
    assert(offset === raw.length || (raw[offset] & 0xc0) !== 0x80, 'UTF-8 span boundary');
  }
  return raw.subarray(start, end);
}

function sourceId(raw, id, fn) {
  const match = /^(?:f(\d+):source:|(\d+):)(\d+):(\d+)$/.exec(id);
  assert(match, 'source ID syntax');
  assert.equal(Number(match[1] ?? match[2]), fn, 'source ID function');
  return span(raw, Number(match[3]), Number(match[4]));
}

export function validateEvidenceNode(report, raw, fn) {
  assert(Buffer.isBuffer(raw));
  let spans = 0, ids = 0, nodes = 0;
  const queue = [[report, 'report']];
  while (queue.length) {
    const [value, key] = queue.pop();
    assert(++nodes <= 2_000_000, 'evidence traversal work cap');
    if (value === null || typeof value !== 'object') {
      if ((key === 'source_id' || key === 'definition_id') && value !== null) {
        sourceId(raw, value, fn);
        ids++;
      }
      continue;
    }
    if (key === 'source_span') {
      assert(Array.isArray(value) && value.length === 2, 'source span pair');
      span(raw, value[0], value[1]);
      spans++;
    }
    if (!Array.isArray(value) && Number.isInteger(value.start) && Number.isInteger(value.end)) {
      span(raw, value.start, value.end);
      spans++;
    }
    if (typeof value.javascript === 'string') {
      const [start, end] = value.source_span;
      const original = span(raw, start, end);
      const preview = Buffer.from(value.javascript);
      assert(original.subarray(0, preview.length).equals(preview), 'exact source preview');
      assert.equal(value.original_bytes, original.length);
      assert.equal(value.bytes_omitted, original.length - preview.length);
      assert.equal(value.truncated, original.length !== preview.length);
    }
    if (key === 'definitions') {
      for (const id of Object.keys(value)) {
        sourceId(raw, id, fn);
        ids++;
      }
    }
    if (key === 'definition_ids_source_order') {
      assert(Array.isArray(value));
      for (const id of value) {
        sourceId(raw, id, fn);
        ids++;
      }
    }
    for (const [childKey, child] of Object.entries(value)) queue.push([child, childKey]);
  }
  return {spans, ids, nodes};
}

export async function validateWorkspace(directory, baseline) {
  const manifest = JSON.parse(fs.readFileSync(path.join(directory, 'manifest.json')));
  const entries = manifest.functions;
  assert.equal(entries.length, manifest.function_count);
  const original = baseline && JSON.parse(fs.readFileSync(path.join(baseline, 'manifest.json')));
  if (original) assert.equal(original.function_count, entries.length);
  let rawBytes = 0, viewBytes = 0;
  const rawHash = crypto.createHash('sha256');
  for (const [id, entry] of entries.entries()) {
    assert.equal(entry.id, id);
    assert.equal(entry.path, `f${id}.js`);
    const raw = fs.readFileSync(path.join(directory, entry.path));
    assert.equal(raw.length, entry.js_bytes);
    const fragment = raw.subarray(entry.fragment_prefix_bytes);
    rawHash.update(`${id}:`).update(fragment);
    rawBytes += raw.length;
    if (original) assert(raw.equals(fs.readFileSync(path.join(baseline, entry.path))), 'raw fragment unchanged');
    if (entry.view_path) {
      assert.equal(entry.view_path, `view/f${id}.txt`);
      const view = fs.readFileSync(path.join(directory, entry.view_path));
      assert.equal(view.length, entry.view_bytes);
      viewBytes += view.length;
      if (original?.functions[id].view_path) {
        assert(view.equals(fs.readFileSync(path.join(baseline, entry.view_path))), 'compact view unchanged');
      }
    }
  }
  const runtime = fs.readFileSync(path.join(directory, 'runtime.js'));
  assert.equal(runtime.length, manifest.runtime_bytes);
  assert.equal(rawBytes + runtime.length, manifest.js_bytes);
  assert.equal(viewBytes, manifest.views_bytes);
  if (original) assert(runtime.equals(fs.readFileSync(path.join(baseline, 'runtime.js'))));
  const reports = [];
  for (const summary of manifest.source_evidence ?? []) {
    assert(['links.jsonl', 'initializers.jsonl'].includes(summary.path));
    const file = path.join(directory, summary.path);
    assert.equal(fs.statSync(file).size, summary.bytes);
    const lines = readline.createInterface({input: fs.createReadStream(file), crlfDelay: Infinity});
    let functions = 0, unavailable = 0, incomplete = 0, spans = 0, ids = 0;
    for await (const line of lines) {
      assert(line.length > 0, 'no blank evidence rows');
      const row = JSON.parse(line);
      assert.equal(row.function, functions);
      const entry = entries[functions];
      assert(entry, 'unexpected evidence function');
      assert.equal(row.raw_path, entry.path);
      assert.equal(row.fragment_prefix_bytes, entry.fragment_prefix_bytes);
      const raw = fs.readFileSync(path.join(directory, entry.path)).subarray(entry.fragment_prefix_bytes);
      const checked = validateEvidenceNode(row.report, raw, functions);
      spans += checked.spans;
      ids += checked.ids;
      unavailable += Number(row.report.status === 'unavailable');
      incomplete += Number(row.report.table_complete === false || row.report.scan_complete === false);
      functions++;
    }
    assert.equal(functions, entries.length);
    assert.equal(functions, summary.functions);
    assert.equal(unavailable, summary.unavailable_functions);
    reports.push({kind: summary.kind, bytes: summary.bytes, functions, unavailable, incomplete, spans, ids});
  }
  return {functions: entries.length, raw_bytes: rawBytes, view_bytes: viewBytes,
    ordered_fragment_sha256: rawHash.digest('hex'), baseline_byte_comparison: Boolean(original),
    reports, scope: 'Structural source-span/byte checks only; no semantic or runtime verification; no JS execution.'};
}

if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const [directory, baseline] = process.argv.slice(2);
  if (!directory) throw new Error('Usage: node scripts/validate_workspace_evidence.mjs WORKSPACE [BASELINE]');
  console.log(JSON.stringify(await validateWorkspace(directory, baseline), null, 2));
}

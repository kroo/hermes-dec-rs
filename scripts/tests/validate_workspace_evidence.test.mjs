import {test} from 'node:test';
import assert from 'node:assert/strict';
import {validateEvidenceNode} from '../validate_workspace_evidence.mjs';

test('joins UTF-8 spans, previews and both source-ID forms without evaluating source', () => {
  const raw = Buffer.from("r[0] = 'caf\u00e9';");
  const result = validateEvidenceNode({
    source_span: [0, raw.length],
    source_id: `f7:source:0:${raw.length}`,
    javascript: raw.toString(), original_bytes: raw.length, bytes_omitted: 0, truncated: false,
    definitions: {[`7:0:${raw.length}`]: {source_span: [0, raw.length]}},
    definition_ids_source_order: [`7:0:${raw.length}`],
  }, raw, 7);
  assert.equal(result.ids, 3);
  assert(result.spans >= 2);
});

test('rejects a span inside a multi-byte scalar', () => {
  assert.throws(() => validateEvidenceNode({source_span: [1, 2]}, Buffer.from('\u00e9'), 0), /UTF-8/);
});

test('rejects a mismatched source function and out-of-range IDs', () => {
  for (const id of ['f1:source:0:1', '0:0:99', 'bad']) {
    assert.throws(() => validateEvidenceNode({definition_id: id}, Buffer.from('x'), 0));
  }
});

test('does not mistake raw JavaScript strings for source IDs', () => {
  assert.equal(validateEvidenceNode({literal: 'f99:source:0:999'}, Buffer.from('x'), 0).ids, 0);
});

test('rejects changed, oversized or incorrectly charged previews', () => {
  for (const fields of [
    {javascript: 'z', original_bytes: 3, bytes_omitted: 2, truncated: true},
    {javascript: 'abcd', original_bytes: 3, bytes_omitted: -1, truncated: true},
    {javascript: 'a', original_bytes: 3, bytes_omitted: 1, truncated: true},
    {javascript: 'a', original_bytes: 3, bytes_omitted: 2, truncated: false},
  ]) {
    assert.throws(() => validateEvidenceNode({source_span: [0, 3], ...fields}, Buffer.from('abc'), 0));
  }
});

test('accepts explicit unavailable analysis without supplying semantic evidence', () => {
  const result = validateEvidenceNode({status: 'unavailable', reason: 'unsupported'}, Buffer.from('x'), 0);
  assert.equal(result.spans, 0);
  assert.equal(result.ids, 0);
});

import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { mkdtemp, readFile, rm, writeFile, link, readdir } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import test from 'node:test';
import { parseArgs, summarizeSession } from '../measure_cli_agents.mjs';

const script = fileURLToPath(new URL('../measure_cli_agents.mjs', import.meta.url));
const secret = 'SECRET_DO_NOT_EMIT';
const at = (second) => `2026-10-05T12:00:${String(second).padStart(2, '0')}.000Z`;
const event = (type, second, fields = {}) => ({ type: 'event_msg', timestamp: at(second), payload: { type, ...fields } });
const response = (type, second, fields = {}) => ({ type: 'response_item', timestamp: at(second), payload: { type, ...fields } });
const usage = (total) => ({ input_tokens: total - 10, cached_input_tokens: 4, output_tokens: 10, reasoning_output_tokens: 3, total_tokens: total });
const token = (second, total, last = usage(20)) => event('token_info', second, { info: { total_token_usage: usage(total), last_token_usage: last } });
const usageRecord = (second, thread, turn, last) => ({
  type: 'token_usage_record', timestamp: at(second), payload: {
    thread_token_usage: usage(thread), turn_token_usage: usage(turn), usage: usage(last),
    thread_id: secret, turn_id: secret, response_id: secret,
  },
});

async function fixture(t, records) {
  const dir = await mkdtemp(join(tmpdir(), 'measure-cli-agents-'));
  t.after(() => rm(dir, { recursive: true, force: true }));
  const path = join(dir, 'synthetic.jsonl');
  const text = typeof records === 'string' ? records : `${records.map((record) => JSON.stringify(record)).join('\n')}\n`;
  await writeFile(path, text);
  return { dir, path, text };
}

const cli = (...args) => spawnSync(process.execPath, [script, ...args], { encoding: 'utf8' });

test('actual snapshots, task/session wall, UTF-8 outputs, and explicit completion', async (t) => {
  const { path, text } = await fixture(t, [
    { type: 'session_meta', timestamp: at(0), payload: { timestamp: at(0), id: secret, cwd: secret } },
    event('task_started', 2, { turn_id: secret }),
    token(3, 100), token(4, 100), token(5, 150),
    response('function_call', 6, { name: secret, arguments: secret }),
    response('custom_tool_call', 7, { name: secret, input: secret }),
    response('function_call_output', 8, { output: 'é' }),
    response('custom_tool_call_output', 9, { output: secret }),
    response('message', 10, { role: 'assistant', phase: 'final_answer', content: [{ text: secret }] }),
    event('task_complete', 12, { turn_id: secret, last_agent_message: secret }),
  ]);
  const result = await summarizeSession(path);
  assert.equal(result.source.file_bytes, Buffer.byteLength(text));
  assert.equal(result.tokens.cumulative.total_tokens.value, 150);
  assert.equal(result.tokens.cumulative.cached_input_tokens.value, 4);
  assert.equal(result.tokens.cumulative.reasoning_output_tokens.value, 3);
  assert.equal(result.tokens.last.total_tokens.value, 20);
  assert.equal(result.tokens.event_count, 3);
  assert.equal(result.tokens.cumulative.event_line, 5);
  assert.equal(result.wall.session_elapsed_ms.value, 12000);
  assert.equal(result.wall.observed_span_ms.value, 12000);
  assert.equal(result.wall.tasks[0].elapsed_ms.value, 10000);
  assert.equal(result.tools.call_count, 2);
  assert.equal(result.tools.output_bytes.value, 2 + Buffer.byteLength(secret));
  assert.equal(result.markers.task_completions, 1);
  assert.equal(result.markers.assistant_final_responses, 1);
  assert.equal(result.markers.final_completion_observed, true);
  assert.ok(!JSON.stringify(result).includes(secret));
});

test('missing tokens remain null, including missing total despite a breakdown', async (t) => {
  const { path } = await fixture(t, [event('token_info', 0, { info: { total_token_usage: { input_tokens: 10, output_tokens: 5 } } })]);
  const result = await summarizeSession(path);
  assert.equal(result.tokens.cumulative.total_tokens.value, null);
  assert.equal(result.tokens.cumulative.total_tokens.reason, 'field_missing');
  assert.equal(result.tokens.last.input_tokens.reason, 'usage_not_available');
  assert.equal(result.wall.session_elapsed_ms.value, null);
  assert.equal(result.tools.output_bytes.value, null);
});

test('null token events supersede stale counters and decreases are not summed', async (t) => {
  const { path } = await fixture(t, [token(1, 100), token(2, 50), event('token_info', 3, { info: null })]);
  const result = await summarizeSession(path);
  assert.equal(result.tokens.cumulative_decreases, 1);
  assert.equal(result.tokens.cumulative.total_tokens.value, null);
  assert.equal(result.tokens.cumulative.event_line, 3);
  assert.equal(result.tokens.last.total_tokens.value, null);
});

test('invalid token values are unavailable without coercion', async (t) => {
  const { path } = await fixture(t, [event('token_info', 0, { info: { total_token_usage: {
    input_tokens: '12', cached_input_tokens: -1, output_tokens: 1.5,
    reasoning_output_tokens: Number.MAX_SAFE_INTEGER + 1, total_tokens: null,
  } } })]);
  const result = await summarizeSession(path);
  for (const field of ['input_tokens', 'cached_input_tokens', 'output_tokens', 'reasoning_output_tokens']) {
    assert.equal(result.tokens.cumulative[field].value, null);
    assert.equal(result.tokens.cumulative[field].reason, 'invalid_nonnegative_integer');
  }
});

test('token_count supports the info shape and cache-write counters with provenance', async (t) => {
  const counters = { ...usage(100), cache_write_input_tokens: 7 };
  const { path } = await fixture(t, [event('token_count', 1, {
    info: { total_token_usage: counters, last_token_usage: counters },
  })]);
  const result = await summarizeSession(path);
  assert.equal(result.tokens.cumulative.total_tokens.value, 100);
  assert.equal(result.tokens.last.total_tokens.value, 100);
  assert.equal(result.tokens.cumulative.cache_write_input_tokens.value, 7);
  assert.equal(result.tokens.cumulative.event_type, 'token_count');
  assert.equal(result.tokens.cumulative.schema_path, 'payload.info.total_token_usage');
  assert.equal(result.tokens.last.schema_path, 'payload.info.last_token_usage');
  assert.deepEqual(result.tokens.event_counts, { token_info: 0, token_count: 1, token_usage_record: 0 });
});

test('thread counters win across interrupted turns and mirrored snapshots are not summed', async (t) => {
  const { path } = await fixture(t, [
    event('task_started', 0),
    usageRecord(1, 100, 100, 20),
    event('turn_aborted', 2),
    event('task_started', 3),
    token(4, 30),
    usageRecord(5, 150, 50, 25),
    usageRecord(6, 150, 50, 25),
    event('token_count', 7, { info: { total_token_usage: usage(50), last_token_usage: usage(50) } }),
    event('task_complete', 8),
  ]);
  const result = await summarizeSession(path);
  assert.equal(result.tokens.cumulative.total_tokens.value, 150);
  assert.equal(result.tokens.cumulative.schema_path, 'payload.thread_token_usage');
  assert.equal(result.tokens.cumulative.event_type, 'token_usage_record');
  assert.equal(result.tokens.cumulative.event_line, 7);
  assert.equal(result.tokens.last.total_tokens.value, 25);
  assert.equal(result.tokens.last.schema_path, 'payload.usage');
  assert.equal(result.tokens.last.event_line, 7);
  assert.equal(result.tokens.turn.total_tokens.value, 50);
  assert.equal(result.tokens.turn.schema_path, 'payload.turn_token_usage');
  assert.equal(result.tokens.cumulative_decreases, 0);
  assert.equal(result.tokens.event_count, 5);
  assert.equal(result.markers.interruptions, 1);
  assert.ok(!JSON.stringify(result).includes(secret));
});

test('usage records alone work, decreasing thread counters are reported, and missing fields stay null', async (t) => {
  const last = usageRecord(2, 50, 20, 20);
  delete last.payload.thread_token_usage.total_tokens;
  last.payload.usage.cache_write_input_tokens = -1;
  const { path } = await fixture(t, [usageRecord(0, 100, 100, 20), usageRecord(1, 60, 20, 20), last]);
  const result = await summarizeSession(path);
  assert.equal(result.tokens.cumulative_decreases, 1);
  assert.equal(result.tokens.cumulative.total_tokens.value, null);
  assert.equal(result.tokens.cumulative.total_tokens.reason, 'field_missing');
  assert.equal(result.tokens.cumulative.input_tokens.value, 40);
  assert.equal(result.tokens.last.cache_write_input_tokens.value, null);
  assert.equal(result.tokens.last.cache_write_input_tokens.reason, 'invalid_nonnegative_integer');
});

test('absent record keys permit info fallbacks; turn counters never replace thread/response usage', async (t) => {
  const { path } = await fixture(t, [token(0, 100), {
    type: 'token_usage_record', payload: { turn_token_usage: usage(40) },
  }]);
  const result = await summarizeSession(path);
  assert.equal(result.tokens.cumulative.total_tokens.value, 100);
  assert.equal(result.tokens.last.total_tokens.value, 20);
  assert.equal(result.tokens.turn.total_tokens.value, 40);
  const onlyTurn = await fixture(t, [{ type: 'token_usage_record', payload: { turn_token_usage: usage(40) } }]);
  const missing = await summarizeSession(onlyTurn.path);
  assert.equal(missing.tokens.cumulative.total_tokens.value, null);
  assert.equal(missing.tokens.last.total_tokens.value, null);
});

test('explicit null record counters supersede stale records and info mirrors', async (t) => {
  const { path } = await fixture(t, [usageRecord(0, 100, 100, 20), {
    type: 'token_usage_record', payload: { usage: null, thread_token_usage: null, turn_token_usage: null },
  }, token(2, 200)]);
  const result = await summarizeSession(path);
  assert.equal(result.tokens.cumulative.total_tokens.value, null);
  assert.equal(result.tokens.cumulative.total_tokens.reason, 'usage_not_available');
  assert.equal(result.tokens.last.total_tokens.value, null);
  assert.equal(result.tokens.last.event_line, 2);
});

test('tool output arrays measure UTF-8 text fields without serializing blocks or leaking text', async (t) => {
  const { path } = await fixture(t, [
    response('function_call_output', 0, { output: [{ type: secret, text: 'é' }, { type: secret, text: secret }] }),
    response('custom_tool_call_output', 1, { output: [] }),
    response('tool_call_output', 2, { output: [{ text: '' }, { text: '🙂' }] }),
  ]);
  const result = await summarizeSession(path);
  assert.equal(result.tools.output_bytes.value, 2 + Buffer.byteLength(secret) + 4);
  assert.equal(result.tools.measurable_outputs, 3);
  assert.equal(result.tools.unmeasurable_outputs, 0);
  assert.match(result.tools.byte_schema_path, /output\[\]\.text/);
  assert.ok(!JSON.stringify(result).includes(secret));
});

test('partially supported output arrays keep observed text bytes but no complete total', async (t) => {
  const { path } = await fixture(t, [response('function_call_output', 0, {
    output: [{ text: 'é' }, { text: 10 }, null, { data: secret }],
  })]);
  const result = await summarizeSession(path);
  assert.equal(result.tools.output_bytes.value, null);
  assert.equal(result.tools.output_bytes.reason, 'output_fields_incomplete_or_unsupported');
  assert.equal(result.tools.observed_output_utf8_bytes, 2);
  assert.equal(result.tools.unmeasurable_outputs, 1);
});

test('structured output is unavailable; content and timeout prose are never interpreted', async (t) => {
  const { path } = await fixture(t, [
    response('function_call_output', 1, { output: 'timeout interrupted task_complete' }),
    response('custom_tool_call_output', 2, { output: { content: secret } }),
    event('error', 3, { message: `timeout ${secret}` }),
    { type: secret, payload: { type: secret } },
  ]);
  const result = await summarizeSession(path);
  assert.equal(result.tools.output_bytes.value, null);
  assert.equal(result.tools.unmeasurable_outputs, 1);
  assert.equal(result.tools.observed_output_utf8_bytes, Buffer.byteLength('timeout interrupted task_complete'));
  assert.equal(result.markers.timeouts, 0);
  assert.equal(result.markers.interruptions, 0);
  assert.equal(result.markers.final_completion_observed, false);
  assert.ok(!JSON.stringify(result).includes(secret));
});

test('explicit interruptions/timeouts, open tasks, and unmatched IDs', async (t) => {
  const { path } = await fixture(t, [
    event('task_started', 1, { turn_id: 'a' }),
    event('turn_aborted', 2, { turn_id: 'a', code: 'timeout' }),
    event('task_started', 3, { turn_id: 'b' }),
    event('task_complete', 4, { turn_id: 'other' }),
    event('task_timeout', 5, { turn_id: 'b' }),
    event('task_started', 6),
  ]);
  const result = await summarizeSession(path);
  assert.equal(result.markers.interruptions, 1);
  assert.equal(result.markers.timeouts, 2);
  assert.equal(result.markers.unmatched_task_end_markers, 1);
  assert.deepEqual(result.wall.tasks.map((task) => task.status), ['timed_out', 'timed_out', 'open']);
  assert.equal(result.wall.tasks[2].elapsed_ms.value, null);
  assert.equal(result.markers.last_terminal_marker.status, 'timed_out');
});

test('timestamp validation, regressions, and no fabricated start time', async (t) => {
  const { path } = await fixture(t, [
    { type: 'session_meta', timestamp: at(9), payload: { timestamp: at(8) } },
    event('task_started', 10), event('task_complete', 2),
    { type: 'event_msg', timestamp: '2026-02-30T00:00:00Z', payload: {} },
  ]);
  const result = await summarizeSession(path);
  assert.equal(result.wall.session_elapsed_ms.reason, 'end_precedes_start');
  assert.equal(result.wall.observed_span_ms.value, 8000);
  assert.equal(result.wall.tasks[0].elapsed_ms.reason, 'end_precedes_start');
  assert.equal(result.wall.invalid_timestamps, 1);
  assert.equal(result.wall.timestamp_regressions, 1);
});

test('overlapping anonymous or duplicate-ID tasks never get guessed durations', async (t) => {
  const { path } = await fixture(t, [
    event('task_started', 1, { turn_id: 'a' }),
    event('task_started', 2, { turn_id: 'b' }),
    event('task_complete', 3),
    event('task_complete', 4, { turn_id: 'a' }),
    event('task_complete', 5),
    event('task_started', 6, { turn_id: 'duplicate' }),
    event('task_started', 7, { turn_id: 'duplicate' }),
    event('task_complete', 8, { turn_id: 'duplicate' }),
  ]);
  const result = await summarizeSession(path);
  assert.equal(result.markers.unmatched_task_end_markers, 2);
  assert.deepEqual(result.wall.tasks.map((task) => task.elapsed_ms.value), [3000, 3000, null, null]);
});

test('explicit final event markers do not fabricate task completion or durations', async (t) => {
  const { path } = await fixture(t, [
    event('task_started', 0),
    event('agent_message', 1, { phase: 'final_answer', message: secret }),
    response('message', 2, { role: 'user', phase: 'final_answer', content: secret }),
  ]);
  const result = await summarizeSession(path);
  assert.equal(result.markers.assistant_final_events, 1);
  assert.equal(result.markers.assistant_final_responses, 0);
  assert.equal(result.markers.final_completion_observed, true);
  assert.equal(result.markers.task_completions, 0);
  assert.equal(result.wall.tasks[0].status, 'open');
  assert.equal(result.wall.tasks[0].elapsed_ms.value, null);
  assert.ok(!JSON.stringify(result).includes(secret));
});

test('BOM, CRLF, blanks, and token_info never observed', async (t) => {
  const { path } = await fixture(t, '\uFEFF{"type":"session_meta"}\r\n\r\n');
  const result = await summarizeSession(path);
  assert.equal(result.source.records, 1);
  assert.equal(result.source.blank_lines, 1);
  assert.equal(result.tokens.cumulative.input_tokens.reason, 'token_info_not_observed');
});

test('CLI help, JSON and human modes have no default writes or transcript text', async (t) => {
  const { dir, path } = await fixture(t, [token(1, 100)]);
  const before = await readdir(dir);
  assert.equal(cli('--help').status, 0);
  assert.match(cli('--help').stdout, /never estimated/);
  const json = cli('--json', path);
  assert.equal(json.status, 0, json.stderr);
  assert.equal(JSON.parse(json.stdout).schema_version, 1);
  const human = cli(path);
  assert.equal(human.status, 0);
  assert.match(human.stdout, /input_tokens=90/);
  assert.deepEqual(await readdir(dir), before);
  const output = join(dir, 'report.json');
  const written = cli('--json', '-o', output, path);
  assert.equal(written.status, 0, written.stderr);
  assert.equal(await readFile(output, 'utf8'), written.stdout);
  assert.equal(cli('-o', output, path).status, 1);
  assert.equal(await readFile(output, 'utf8'), written.stdout);
  assert.equal(cli('-o', path, path).status, 1);
});

test('input validation, duplicate aliases, and no output after a parse failure', async (t) => {
  const { dir, path } = await fixture(t, [token(1, 100)]);
  const alias = join(dir, 'alias.jsonl');
  await link(path, alias);
  assert.equal(cli('--json', path, alias).status, 1);
  assert.equal(cli(dir).status, 1);
  assert.equal(cli(join(dir, secret)).status, 1);
  assert.ok(!cli(join(dir, secret)).stderr.includes(secret));
  const bad = await fixture(t, `{"type":"event_msg"}\n${secret}\n`);
  const output = join(dir, 'must-not-exist.json');
  const failure = cli('--json', '-o', output, path, bad.path);
  assert.equal(failure.status, 1);
  assert.match(failure.stderr, /input 2, line 2: invalid JSON/);
  assert.ok(!failure.stderr.includes(secret));
  assert.ok(!(await readdir(dir)).includes('must-not-exist.json'));
  for (const content of ['', '\n', '[]\n', '{"type":4}\n']) {
    const invalid = await fixture(t, content);
    await assert.rejects(summarizeSession(invalid.path));
  }
});

test('argument validation and explicit input ordering', async (t) => {
  assert.throws(() => parseArgs([]));
  assert.throws(() => parseArgs(['--unknown']));
  assert.throws(() => parseArgs(['-o']));
  assert.throws(() => parseArgs(['-o', 'a', '-o', 'b', 'c']));
  assert.deepEqual(parseArgs(['--json', '--', '-session.jsonl']).inputs, ['-session.jsonl']);
  const first = await fixture(t, [token(1, 100)]);
  const second = await fixture(t, [token(1, 50)]);
  const result = cli('--json', second.path, first.path);
  assert.equal(result.status, 0, result.stderr);
  const sessions = JSON.parse(result.stdout).sessions;
  assert.deepEqual(sessions.map((session) => session.source.index), [1, 2]);
  assert.deepEqual(sessions.map((session) => session.tokens.cumulative.total_tokens.value), [50, 100]);
});

test('explicitly opted-in session CLI has available thread and response totals', {
  skip: !process.env.MEASURE_CLI_AGENTS_TEST_SESSION,
}, () => {
  // This opt-in path is supplied by the caller; never discover session files.
  const result = cli('--json', process.env.MEASURE_CLI_AGENTS_TEST_SESSION);
  assert.equal(result.status, 0, 'session CLI must succeed');
  const session = JSON.parse(result.stdout).sessions[0];
  assert.equal(session.tokens.cumulative.total_tokens.availability, 'available');
  assert.equal(session.tokens.last.total_tokens.availability, 'available');
  assert.equal(session.tokens.cumulative.schema_path, 'payload.thread_token_usage');
  assert.equal(session.tokens.last.schema_path, 'payload.usage');
  assert.equal(session.tools.output_bytes.availability, 'available');
});

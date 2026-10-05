#!/usr/bin/env node
import { createReadStream } from 'node:fs';
import { stat, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { createInterface } from 'node:readline';
import { pathToFileURL } from 'node:url';

const TOKEN_FIELDS = [
  'input_tokens',
  'cached_input_tokens',
  'cache_write_input_tokens',
  'output_tokens',
  'reasoning_output_tokens',
  'total_tokens',
];
const CALL_TYPES = ['function_call', 'custom_tool_call', 'tool_call'];
const OUTPUT_TYPES = ['function_call_output', 'custom_tool_call_output', 'tool_call_output'];
const COMPLETE = new Set(['task_complete', 'task_completed', 'turn_complete']);
const INTERRUPTED = new Set(['turn_aborted', 'task_aborted', 'task_interrupted', 'interrupted']);
const TIMEOUT = new Set(['task_timeout', 'turn_timeout', 'timeout']);
const HELP = `Usage: node scripts/measure_cli_agents.mjs [--json] [-o FILE] [--] SESSION.jsonl ...

Read only the explicitly supplied regular JSONL files; no discovery or network.
--json       Print the stable schema_version=1 JSON report (default: numeric summary).
-o FILE      Also create a JSON report file; existing files are never overwritten.
--help, -h   Show this help.

Tokens accept token_info/token_count info and token_usage_record counters.
The final recorded thread_token_usage is preferred for cumulative usage; usage
is preferred for per-response usage. Info total/last counters are fallbacks.
Turn counters are reported separately, never used as thread/response totals.
Snapshots are never summed, missing tokens are never estimated, and decreases
are reported. Cached/reasoning counters are not added to input/output counters.
Wall time uses session_meta start through the last observed record timestamp;
observed span and explicit task start/end durations are reported separately.
Tool output bytes measure strings or array text fields in UTF-8, not network
bytes or model tokens. Unsupported/missing blocks make the byte total null.
Only explicit interruption/timeout/completion markers are counted; prose is
never interpreted. Unknown shapes are ignored. No transcript text, identifiers,
arguments, tool names, or paths are emitted. Sources are indexed in argument order.
Absent markers do not prove absence of interruptions or successful completion.
`;

const object = (value) => value !== null && typeof value === 'object' && !Array.isArray(value);
const metric = (value, reason = null) => ({
  value,
  availability: value === null ? 'unavailable' : 'available',
  reason: value === null ? reason : null,
});

function timestamp(value) {
  if (typeof value !== 'string') return null;
  const match = /^(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2})(?:\.\d{1,9})?(?:Z|([+-])(\d{2}):(\d{2}))$/.exec(value);
  if (!match) return null;
  const [, year, month, day, hour, minute, second, , offsetHour, offsetMinute] = match;
  const y = Number(year);
  const m = Number(month);
  const leap = y % 4 === 0 && (y % 100 !== 0 || y % 400 === 0);
  const days = [31, leap ? 29 : 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
  if (m < 1 || m > 12 || Number(day) < 1 || Number(day) > days[m - 1]
      || Number(hour) > 23 || Number(minute) > 59 || Number(second) > 59
      || Number(offsetHour ?? 0) > 23 || Number(offsetMinute ?? 0) > 59) return null;
  const parsed = Date.parse(value);
  return Number.isFinite(parsed) ? parsed : null;
}

function duration(start, end) {
  if (start === null) return metric(null, 'start_timestamp_unavailable');
  if (end === null) return metric(null, 'end_timestamp_unavailable');
  if (end < start) return metric(null, 'end_precedes_start');
  return metric(end - start);
}

function tokenSnapshot(usage, line, schemaPath, missingReason, eventType = null) {
  return {
    event_line: line,
    event_type: eventType,
    schema_path: schemaPath,
    ...Object.fromEntries(TOKEN_FIELDS.map((field) => {
      let reason = missingReason;
      let value = null;
      if (object(usage)) {
        if (!(field in usage) || usage[field] === null) reason = 'field_missing';
        else if (!Number.isSafeInteger(usage[field]) || usage[field] < 0) reason = 'invalid_nonnegative_integer';
        else value = usage[field];
      }
      return [field, metric(value, reason)];
    })),
  };
}

function outputBytes(output) {
  if (typeof output === 'string') return { bytes: Buffer.byteLength(output, 'utf8'), complete: true };
  if (!Array.isArray(output)) return { bytes: 0, complete: false };
  let bytes = 0;
  let complete = true;
  for (const block of output) {
    if (object(block) && typeof block.text === 'string') bytes += Buffer.byteLength(block.text, 'utf8');
    else complete = false;
  }
  return { bytes, complete };
}

function counterHistory() {
  return { snapshot: null, previousTotal: null, decreases: 0 };
}

function recordCounter(history, snapshot) {
  history.snapshot = snapshot;
  const total = snapshot.total_tokens.value;
  if (total !== null) {
    if (history.previousTotal !== null && total < history.previousTotal) history.decreases += 1;
    history.previousTotal = total;
  }
}

export async function summarizeSession(path, index = 1) {
  let metadata;
  try {
    metadata = await stat(path);
  } catch {
    throw new Error(`input ${index}: cannot read file`);
  }
  if (!metadata.isFile()) throw new Error(`input ${index}: expected a regular file`);
  const report = {
    source: { index, file_bytes: metadata.size, records: 0, blank_lines: 0 },
    tokens: {
      event_count: 0,
      event_counts: { token_info: 0, token_count: 0, token_usage_record: 0 },
      cumulative: tokenSnapshot(null, null, 'payload.info.total_token_usage', 'token_info_not_observed'),
      last: tokenSnapshot(null, null, 'payload.info.last_token_usage', 'token_info_not_observed'),
      turn: tokenSnapshot(null, null, 'payload.turn_token_usage', 'turn_token_usage_not_observed'),
      cumulative_decreases: 0,
    },
    wall: {
      session_elapsed_ms: null,
      session_start_schema_path: null,
      end_schema_path: 'timestamp (last timestamped record in file order)',
      observed_span_ms: null,
      timestamped_records: 0,
      invalid_timestamps: 0,
      timestamp_regressions: 0,
      tasks: [],
    },
    tools: {
      calls: Object.fromEntries(CALL_TYPES.map((type) => [type, 0])),
      call_count: 0,
      outputs: Object.fromEntries(OUTPUT_TYPES.map((type) => [type, 0])),
      output_count: 0,
      output_bytes: null,
      observed_output_utf8_bytes: 0,
      measurable_outputs: 0,
      unmeasurable_outputs: 0,
      byte_schema_path: 'payload.output (string) or payload.output[].text (UTF-8)',
    },
    markers: {
      interruptions: 0,
      timeouts: 0,
      task_completions: 0,
      assistant_final_responses: 0,
      assistant_final_events: 0,
      final_completion_observed: false,
      last_terminal_marker: null,
      unmatched_task_end_markers: 0,
    },
  };
  let line = 0;
  let sessionStart = null;
  let lastTime = null;
  let minTime = null;
  let maxTime = null;
  const infoCumulative = counterHistory();
  const threadCumulative = counterHistory();
  let responseUsage = null;
  const taskStarts = new Map();
  const openTasks = new Set();
  const stream = createReadStream(path, { encoding: 'utf8' });
  const lines = createInterface({ input: stream, crlfDelay: Infinity });
  try {
    for await (const raw of lines) {
      line += 1;
      if (!raw.trim()) {
        report.source.blank_lines += 1;
        continue;
      }
      let record;
      try {
        record = JSON.parse(line === 1 ? raw.replace(/^\uFEFF/, '') : raw);
      } catch {
        throw new Error(`input ${index}, line ${line}: invalid JSON`);
      }
      if (!object(record) || typeof record.type !== 'string') {
        throw new Error(`input ${index}, line ${line}: expected an object with a string type`);
      }
      report.source.records += 1;
      const time = timestamp(record.timestamp);
      if (time !== null) {
        report.wall.timestamped_records += 1;
        if (lastTime !== null && time < lastTime) report.wall.timestamp_regressions += 1;
        lastTime = time;
        minTime = minTime === null ? time : Math.min(minTime, time);
        maxTime = maxTime === null ? time : Math.max(maxTime, time);
      } else if (record.timestamp !== undefined) report.wall.invalid_timestamps += 1;
      const payload = object(record.payload) ? record.payload : {};
      if (record.type === 'session_meta' && sessionStart === null) {
        const metaTime = timestamp(payload.timestamp);
        if (payload.timestamp !== undefined && metaTime === null) report.wall.invalid_timestamps += 1;
        sessionStart = metaTime ?? time;
        if (sessionStart !== null) {
          report.wall.session_start_schema_path = metaTime !== null ? 'payload.timestamp (session_meta)' : 'timestamp (session_meta)';
        }
      }
      if (record.type === 'event_msg' && ['token_info', 'token_count'].includes(payload.type)) {
        report.tokens.event_count += 1;
        report.tokens.event_counts[payload.type] += 1;
        // Use the latest event even when unavailable: stale counters must not look current.
        const info = object(payload.info) ? payload.info : {};
        recordCounter(infoCumulative, tokenSnapshot(info.total_token_usage, line, 'payload.info.total_token_usage', 'usage_not_available', payload.type));
        report.tokens.last = tokenSnapshot(info.last_token_usage, line, 'payload.info.last_token_usage', 'usage_not_available', payload.type);
      }
      if (record.type === 'token_usage_record') {
        report.tokens.event_count += 1;
        report.tokens.event_counts.token_usage_record += 1;
        // Prefer explicit thread/response counters over mirrored info events. A null
        // counter supersedes stale values, but an absent key permits the info fallback.
        if ('thread_token_usage' in payload) {
          recordCounter(threadCumulative, tokenSnapshot(payload.thread_token_usage, line, 'payload.thread_token_usage', 'usage_not_available', 'token_usage_record'));
        }
        if ('usage' in payload) {
          responseUsage = tokenSnapshot(payload.usage, line, 'payload.usage', 'usage_not_available', 'token_usage_record');
        }
        report.tokens.turn = tokenSnapshot(payload.turn_token_usage, line, 'payload.turn_token_usage', 'usage_not_available', 'token_usage_record');
      }
      if (record.type === 'response_item') {
        if (CALL_TYPES.includes(payload.type)) {
          report.tools.calls[payload.type] += 1;
          report.tools.call_count += 1;
        }
        if (OUTPUT_TYPES.includes(payload.type)) {
          report.tools.outputs[payload.type] += 1;
          report.tools.output_count += 1;
          const measured = outputBytes(payload.output);
          report.tools.observed_output_utf8_bytes += measured.bytes;
          if (measured.complete) report.tools.measurable_outputs += 1;
          else report.tools.unmeasurable_outputs += 1;
        }
        if (payload.type === 'message' && payload.role === 'assistant' && payload.phase === 'final_answer') {
          report.markers.assistant_final_responses += 1;
        }
      }
      if (record.type !== 'event_msg') continue;
      if (payload.type === 'agent_message' && payload.phase === 'final_answer') report.markers.assistant_final_events += 1;
      if (payload.type === 'task_started') {
        const task = { index: report.wall.tasks.length + 1, start_line: line, end_line: null, status: 'open', elapsed_ms: metric(null, 'end_marker_not_observed') };
        report.wall.tasks.push(task);
        const start = { task, time, id: typeof payload.turn_id === 'string' ? payload.turn_id : null };
        openTasks.add(start);
        if (start.id !== null) {
          const matches = taskStarts.get(start.id) ?? new Set();
          matches.add(start);
          taskStarts.set(start.id, matches);
        }
      }
      const completed = COMPLETE.has(payload.type);
      const timedOut = TIMEOUT.has(payload.type)
        || ((payload.type === 'error' || INTERRUPTED.has(payload.type)) && TIMEOUT.has(payload.code));
      const interrupted = INTERRUPTED.has(payload.type);
      if (interrupted) report.markers.interruptions += 1;
      if (timedOut) report.markers.timeouts += 1;
      if (completed) report.markers.task_completions += 1;
      if (!completed && !timedOut && !interrupted) continue;
      const status = timedOut ? 'timed_out' : interrupted ? 'interrupted' : 'completed';
      report.markers.last_terminal_marker = { line, status };
      // IDs stay private. Never guess which overlapping task an anonymous end closes.
      const hasId = typeof payload.turn_id === 'string';
      const matches = hasId ? taskStarts.get(payload.turn_id) : openTasks;
      const start = matches?.size === 1 ? matches.values().next().value : null;
      if (start) {
        start.task.end_line = line;
        start.task.status = status;
        start.task.elapsed_ms = duration(start.time, time);
        openTasks.delete(start);
        if (start.id !== null) taskStarts.delete(start.id);
      } else report.markers.unmatched_task_end_markers += 1;
    }
  } catch (error) {
    if (error instanceof Error && /^input \d+(?:, line \d+)?:/.test(error.message)) throw error;
    throw new Error(`input ${index}: cannot read file`);
  } finally {
    lines.close();
    stream.destroy();
  }
  if (report.source.records === 0) throw new Error(`input ${index}: no JSONL records`);
  const cumulative = threadCumulative.snapshot !== null ? threadCumulative : infoCumulative;
  if (cumulative.snapshot !== null) report.tokens.cumulative = cumulative.snapshot;
  report.tokens.cumulative_decreases = cumulative.decreases;
  if (responseUsage !== null) report.tokens.last = responseUsage;
  report.wall.session_elapsed_ms = duration(sessionStart, lastTime);
  report.wall.observed_span_ms = duration(minTime, maxTime);
  report.tools.output_bytes = report.tools.output_count === 0
    ? metric(null, 'tool_outputs_not_observed')
    : report.tools.unmeasurable_outputs > 0
      ? metric(null, 'output_fields_incomplete_or_unsupported')
      : metric(report.tools.observed_output_utf8_bytes);
  report.markers.final_completion_observed = report.markers.task_completions > 0
    || report.markers.assistant_final_responses > 0 || report.markers.assistant_final_events > 0;
  return report;
}

export function parseArgs(args) {
  const options = { json: false, help: false, output: null, inputs: [] };
  let positional = false;
  for (let i = 0; i < args.length; i += 1) {
    const arg = args[i];
    if (!positional && arg === '--') positional = true;
    else if (!positional && (arg === '--help' || arg === '-h')) options.help = true;
    else if (!positional && arg === '--json') options.json = true;
    else if (!positional && arg === '-o') {
      if (options.output !== null || !args[i + 1] || args[i + 1].startsWith('-')) throw new Error('expected one output path after -o');
      options.output = args[++i];
    } else if (!positional && arg.startsWith('-')) throw new Error('unknown option; see --help');
    else if (!arg) throw new Error('empty input path');
    else options.inputs.push(arg);
  }
  if (!options.help && options.inputs.length === 0) throw new Error('provide explicit session JSONL paths; see --help');
  return options;
}

function humanReport(report) {
  const show = (item) => item.value === null ? `null (${item.reason})` : String(item.value);
  return report.sessions.map((session) => [
    `Session ${session.source.index}: ${session.source.records} records, ${session.source.file_bytes} file bytes`,
    `  Cumulative tokens: ${TOKEN_FIELDS.map((field) => `${field}=${show(session.tokens.cumulative[field])}`).join(', ')}`,
    `  Last tokens: ${TOKEN_FIELDS.map((field) => `${field}=${show(session.tokens.last[field])}`).join(', ')}`,
    `  Wall ms: ${show(session.wall.session_elapsed_ms)}; observed span ms: ${show(session.wall.observed_span_ms)}`,
    `  Tool calls: ${session.tools.call_count}; output bytes: ${show(session.tools.output_bytes)}`,
    `  Interruptions: ${session.markers.interruptions}; timeouts: ${session.markers.timeouts}; task completions: ${session.markers.task_completions}; assistant final markers: ${session.markers.assistant_final_responses + session.markers.assistant_final_events}`,
  ].join('\n')).join('\n');
}

export async function main(args) {
  const options = parseArgs(args);
  if (options.help) {
    process.stdout.write(HELP);
    return;
  }
  const identities = new Set();
  const sessions = [];
  for (const [i, path] of options.inputs.entries()) {
    let metadata;
    try { metadata = await stat(path); } catch { throw new Error(`input ${i + 1}: cannot read file`); }
    const identity = `${metadata.dev}:${metadata.ino}`;
    if (identities.has(identity)) throw new Error(`input ${i + 1}: duplicate input file`);
    identities.add(identity);
    sessions.push(await summarizeSession(path, i + 1));
  }
  const report = { schema_version: 1, sessions };
  const json = `${JSON.stringify(report, null, 2)}\n`;
  if (options.output !== null) {
    try { await writeFile(options.output, json, { flag: 'wx', mode: 0o600 }); }
    catch { throw new Error('cannot create output file; it must be a new writable path'); }
  }
  process.stdout.write(options.json ? json : `${humanReport(report)}\n`);
}

if (process.argv[1] && import.meta.url === pathToFileURL(resolve(process.argv[1])).href) {
  main(process.argv.slice(2)).catch((error) => {
    process.stderr.write(`measure_cli_agents: ${error.message}\n`);
    process.exitCode = 1;
  });
}

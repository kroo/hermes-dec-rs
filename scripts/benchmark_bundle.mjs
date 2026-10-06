// Release CLI benchmark; correctness checks are retained in every measured run.
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import crypto from 'node:crypto';
import {spawn, execFileSync} from 'node:child_process';

const [binary, input, directory = '/tmp/hermes-bundle-benchmark', count = '10', budget = '1000'] = process.argv.slice(2);
if (!input) throw new Error('Usage: node scripts/benchmark_bundle.mjs BINARY INPUT [OUTDIR] [RUNS] [BUDGET_MS]');
const runs = Number(count), budgetMs = Number(budget);
if (!Number.isInteger(runs) || runs < 2 || !(budgetMs > 0)) throw new Error('Invalid runs or budget');
fs.mkdirSync(directory, {recursive:true});
const startedAt = new Date().toISOString(), loadAverageBefore = os.loadavg();
const output = path.resolve(directory, 'bundle.js');
const sha = bytes => crypto.createHash('sha256').update(bytes).digest('hex');
const duration = (log, key) => {
  const match = log.match(new RegExp(`${key}=([0-9.]+)(ns|µs|ms|s)`));
  return match ? Number(match[1]) * ({ns:1e-6, 'µs':1e-3, ms:1, s:1000}[match[2]]) : null;
};
async function run(sampleMemory = false) {
  const start = performance.now();
  const child = spawn(path.resolve(binary), ['export-bundle', path.resolve(input), '-o', output],
    {env:{...process.env, RUST_LOG:'info'}, stdio:['ignore','ignore','pipe']});
  let stderr = '', sampledPeakRssKiB = 0, memoryAvailable = false;
  child.stderr.on('data', data => { stderr += data; });
  const sample = sampleMemory ? setInterval(() => {
    try {
      const rss = Number(execFileSync('ps', ['-o','rss=','-p',String(child.pid)], {encoding:'utf8'}).trim());
      if (rss > 0) { memoryAvailable = true; sampledPeakRssKiB = Math.max(sampledPeakRssKiB, rss); }
    }
    catch {} // The process may have exited between the timer and ps.
  }, 25) : null;
  await new Promise((resolve,reject) => {
    child.once('error',reject);
    child.once('close', code => code === 0 ? resolve() : reject(new Error(`Export failed (${code}): ${stderr}`)));
  }).finally(() => { if (sample) clearInterval(sample); });
  return {wallMs:performance.now()-start, readMs:duration(stderr,'read'), parseMs:duration(stderr,'parse'),
    exportMs:duration(stderr,'export'), writeMs:duration(stderr,'write'), cliTotalMs:duration(stderr,'total'),
    functions:Number(stderr.match(/functions=(\d+)/)?.[1]), bytes:fs.statSync(output).size,
    sha256:sha(fs.readFileSync(output)), ...(sampleMemory ? {sampledPeakRssKiB:memoryAvailable ? sampledPeakRssKiB : null} : {})};
}
const firstRun = await run(); // Not an OS-cache-flushed cold run.
const samples = [];
for (let i = 0; i < runs; i++) {
  const sample = await run();
  if (sample.sha256 !== firstRun.sha256) throw new Error('Nondeterministic export');
  samples.push(sample);
  console.log(JSON.stringify({run:i+1, ...sample}));
}
const memoryRun = await run(true); // Excluded from latency stats due to ps overhead.
execFileSync('node', ['--check', output], {timeout:120000});
const sorted = samples.map(sample=>sample.wallMs).sort((a,b)=>a-b);
const report = {startedAt, completedAt:new Date().toISOString(), loadAverageBefore, loadAverageAfter:os.loadavg(),
  hardware:{platform:os.platform(), arch:os.arch(), cpu:os.cpus()[0]?.model, logicalCpus:os.cpus().length},
  input:path.resolve(input), inputSha256:sha(fs.readFileSync(input)), binarySha256:sha(fs.readFileSync(binary)),
  rayonThreads:process.env.RAYON_NUM_THREADS || 'default', budgetMs, firstRun, samples, memoryRun,
  medianMs:sorted[Math.floor(sorted.length/2)], p95Ms:sorted[Math.ceil(sorted.length*0.95)-1],
  worstMs:sorted.at(-1), targetMet:sorted.every(value=>value<budgetMs)};
fs.writeFileSync(path.join(directory,'report.json'), JSON.stringify(report,null,2)+'\n');
console.log(JSON.stringify({medianMs:report.medianMs,p95Ms:report.p95Ms,worstMs:report.worstMs,targetMet:report.targetMet}));
if (!report.targetMet) process.exitCode = 1;

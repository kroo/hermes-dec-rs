// Offline native-Hermes differential checks, including the large literal stress case.
import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import vm from 'node:vm';
import {execFileSync} from 'node:child_process';

const [hermesc, hermes, exporter, outDir = '/tmp/hermes-fixture-validation'] = process.argv.slice(2);
if (!exporter) throw new Error('Usage: node scripts/validate_bundle_fixtures.mjs HERMESC HERMES EXPORTER [OUTDIR]');
const root = new URL('../data/', import.meta.url);
const host = fs.readFileSync(new URL('./console_host.js', import.meta.url), 'utf8');
const sha = data => crypto.createHash('sha256').update(data).digest('hex');
const fixtures = [
  {name:'bundle_semantics', probe:'bundleResults', sourceEquivalent:true},
  {name:'bundle_eval', probe:'evalResults'},
  {name:'bundle_hermes_semantics', selfPrinting:true},
  {name:'bundle_intrinsics', probe:'intrinsicResults', sourceEquivalent:true},
  {name:'massive_literals', probe:'hermesHostLogs', sourceEquivalent:true}
];
fs.mkdirSync(outDir, {recursive:true});
const report = {compiler:execFileSync(hermesc,['-version'],{encoding:'utf8'}),
  compilerSha256:sha(fs.readFileSync(hermesc)),runtimeSha256:sha(fs.readFileSync(hermes)),
  harnessSha256:sha(fs.readFileSync(new URL(import.meta.url))),
  exporterSha256:sha(fs.readFileSync(exporter)), fixtures:[]};
for (const fixture of fixtures) {
  const source = fs.readFileSync(new URL(`${fixture.name}.js`,root),'utf8');
  const code = `${host}\n${source}\n${fixture.selfPrinting ? '' : `print(JSON.stringify(${fixture.probe}));`}\n`;
  const input = path.resolve(outDir, `${fixture.name}.js`);
  const bytecode = path.resolve(outDir, `${fixture.name}.hbc`);
  const output = path.resolve(outDir, `${fixture.name}.bundle.js`);
  try {
    fs.writeFileSync(input, code);
    execFileSync(hermesc,['-O','-emit-binary','-out',bytecode,input],{timeout:120000,maxBuffer:8e6});
    const expected = execFileSync(hermes,[bytecode],{encoding:'utf8',timeout:120000,maxBuffer:8e6});
    if (fixture.sourceEquivalent) {
      const observed = [];
      vm.runInNewContext(code,{print:value=>observed.push(value)},{timeout:120000});
      if (observed.join('\n')+'\n' !== expected) throw new Error('Hermes differs from source baseline');
    }
    const start = performance.now();
    execFileSync(exporter,['export-bundle',bytecode,'-o',output],{timeout:120000,maxBuffer:8e6});
    const exportMs = performance.now()-start;
    const exported = fs.readFileSync(output,'utf8');
    const actual = [];
    vm.runInNewContext(exported,{print:value=>actual.push(value)},{timeout:120000});
    if (actual.join('\n')+'\n' !== expected) throw new Error('Export differs from original Hermes');
    report.fixtures.push({name:fixture.name,passed:true,sourceSha256:sha(source),
      inputSha256:sha(code),
      bytecodeSha256:sha(fs.readFileSync(bytecode)),outputSha256:sha(expected),
      hbcBytes:fs.statSync(bytecode).size,jsBytes:Buffer.byteLength(exported),
      functions:(exported.match(/ = function function_/g)||[]).length,exportMs});
  } catch (error) {
    report.fixtures.push({name:fixture.name,passed:false,error:String(error),stderr:error.stderr?.toString()});
  }
  fs.writeFileSync(path.join(outDir,'report.json'),JSON.stringify(report,null,2)+'\n');
  console.log(JSON.stringify(report.fixtures.at(-1)));
}
if (report.fixtures.some(fixture=>!fixture.passed)) process.exitCode = 1;

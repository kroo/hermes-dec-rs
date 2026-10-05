// Optional, offline differential runs over pinned public npm archives.
// Fetch archives separately with `npm pack --ignore-scripts --pack-destination DIR`.
import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import vm from 'node:vm';
import {execFileSync} from 'node:child_process';

const [archives, hermesc, hermes, exporter, outDir = '/tmp/hermes-bundle-validation', mode] = process.argv.slice(2);
if (!exporter || (mode && mode !== '--reuse-inputs'))
  throw new Error('Usage: node scripts/validate_bundle_projects.mjs ARCHIVES HERMESC HERMES EXPORTER [OUTDIR] [--reuse-inputs]');
fs.mkdirSync(outDir, {recursive: true});
const projects = [
  {
    name: 'lodash', version: '4.17.21', source: 'lodash.js',
    archiveSha256: '6a087ac9e5702a0c9d60fbcd48696012646ec8df1491dea472b150e79fcaf804',
    probe: `(() => {
      const results = [];
      for (let seed = 1; seed <= 40; seed++) {
        const xs = _.range(0, 100).map(x => (x * seed + 17) % 31);
        results.push(_.chain(xs).filter(x => x % 2).map(x => x * 3).uniq().sortBy().value());
        results.push(_.groupBy(xs, x => x % 7), _.countBy(xs), _.chunk(xs, seed % 9 + 1));
        const object = {a: [{b: seed}], x: {y: [seed, seed + 1]}};
        const cloned = _.cloneDeep(object);
        _.set(cloned, 'a[0].b', seed + 4);
        results.push([_.get(cloned, 'a[0].b'), _.isEqual(object, cloned), _.merge({}, object, {x:{z:seed}})]);
        const partial = _.partial((a,b,c) => a+b+c, seed);
        const curried = _.curry((a,b,c) => a*b+c);
        results.push([partial(3,4), curried(seed)(2)(5), _.deburr('déjà vu'), _.camelCase('Foo BAR')]);
        results.push(_.padStart(String(seed), 5, '0'), _.escape('<tag> & text'));
      }
      return results;
    })()`
  },
  {
    name: 'immutable', version: '4.3.7', source: 'dist/immutable.js',
    archiveSha256: 'def89fdd1c1cfdf037ef4ae87f30bb332ef7df8cd668195d91fecdecd836aa61',
    probe: `(() => {
      const I = Immutable, results = [];
      for (let seed = 1; seed <= 40; seed++) {
        const list = I.List(I.Range(0, 100)).map(x => x * seed).filter(x => x % 3);
        const map = I.Map({a: seed, b: seed+1}).setIn(['nested','items'], list);
        results.push(map.toJS(), list.slice(2,12).reverse().toArray());
        results.push(I.Set(list).union([2,4,6]).sort().toArray());
        const R = I.Record({count:0, label:'test'}), record = new R({count:seed});
        results.push(record.set('count',seed+1).toJS(), record.count);
        results.push(I.fromJS({a:[{b:seed}]}).updateIn(['a',0,'b'], x => x+1).toJS());
        results.push(I.OrderedMap([['z',seed],['a',2]]).entrySeq().toArray());
        results.push(I.Stack([1,2]).push(seed).pop().toArray());
      }
      return results;
    })()`
  },
  {
    name: 'moment', version: '2.30.1', source: 'moment.js',
    archiveSha256: '52219a9fee5e1faade4c72536c173c54cedd5e2619272dd0c251a30aeafcde8c',
    probe: `(() => {
      const results = [];
      for (let seed = 1; seed <= 120; seed++) {
        const date = moment.utc('2020-01-01').add(seed,'days').add(seed%24,'hours');
        results.push(date.format('YYYY-MM-DD HH:mm:ss'), date.clone().startOf('month').format());
        results.push(date.diff(moment.utc('2019-01-01'),'days'), date.isoWeek(), date.dayOfYear());
        results.push(moment.duration({days:seed,hours:4}).asMinutes());
        results.push(moment.utc('2024-02-29','YYYY-MM-DD',true).isValid());
        results.push(moment.utc('2023-02-29','YYYY-MM-DD',true).isValid());
        results.push(moment.parseZone('2020-01-01T12:00:00+05:30').utc().format());
      }
      return results;
    })()`
  },
  {
    name:'typescript', version:'5.9.3', source:'lib/typescript.js', license:'LICENSE.txt', downlevel:true,
    archiveSha256:'10e108c9cf7d5f2879053dff18515fb405abf2ccef63eaaf017d9c571687a1d3',
    probe:`(() => {
      const sources = [
        'interface Point { x:number; y:number }; const p:Point={x:2,y:3}; export const value=p.x+p.y;',
        'class Counter { private value=0; increment(step=1) { return this.value+=step; } }; export default Counter;',
        'export async function work(xs:number[]) { const values=await Promise.all(xs.map(async x=>x*2)); return values; }',
        'export function* values() { try { yield* [1,2,3]; } finally { console.log("done"); } }',
        'export const element = <section title="test">{[1,2].map(x=><span key={x}>{x}</span>)}</section>;',
        'type Pair<T> = readonly [T,T]; const f = <T,>(x:T):Pair<T> => [x,x]; export const result=f(4);'
      ];
      return sources.map((source,index) => {
        const file = ts.createSourceFile('fixture.tsx',source,ts.ScriptTarget.Latest,true,ts.ScriptKind.TSX);
        const result = ts.transpileModule(source,{fileName:'fixture.tsx',reportDiagnostics:true,compilerOptions:{
          target:ts.ScriptTarget.ES2018,module:ts.ModuleKind.CommonJS,jsx:ts.JsxEmit.React,sourceMap:true}});
        const printer = ts.createPrinter({newLine:ts.NewLineKind.LineFeed});
        return [result.outputText,result.sourceMapText,(result.diagnostics||[]).map(d=>d.code),
          file.statements.map(statement=>ts.SyntaxKind[statement.kind]),printer.printFile(file)];
      });
    })()`
  },
  {
    name:'babel-standalone', version:'7.28.4', source:'babel.js', downlevel:true, hostConsole:true,
    archiveSha256:'abe1d3dfe38b902afc7a4da8d6b9a0ea23e5a795e39d94bf09a23b0197c42a17',
    probe:`(() => {
      const sources = [
        'const xs = [1,2,3].map(x=>x*2); const answer = xs.reduce((a,b)=>a+b,0);',
        'class Counter { constructor(value=0) { this.value=value; } increment(step=1) { return this.value+=step; } }',
        'async function work(xs) { return await Promise.all(xs.map(async x=>x*2)); }',
        'function* values() { try { yield* [1,2,3]; } finally { console.log("done"); } }',
        'const object={a:1,b:2}; const {a,...rest}=object; const result={...rest,c:a};',
        'const result = value?.nested?.count ?? 4; const text = "value:" + result;'
      ];
      return sources.map(source => Babel.transform(source,{presets:[['env',{targets:{ie:'11'},modules:'commonjs'}]],
        sourceMaps:true,filename:'fixture.js',comments:false}).code);
    })()`
  }
];
const sha = data => crypto.createHash('sha256').update(data).digest('hex');
let preprocessor;
function downlevel(source) {
  if (!preprocessor) {
    const archive = path.resolve(archives,'typescript-5.9.3.tgz');
    if (sha(fs.readFileSync(archive)) !== projects.find(project=>project.name==='typescript').archiveSha256)
      throw new Error('TypeScript preprocessor integrity mismatch');
    const directory = path.resolve(outDir,'preprocessor');
    fs.mkdirSync(directory,{recursive:true});
    execFileSync('tar',['-xzf',archive,'-C',directory,'package/lib/typescript.js','package/LICENSE.txt']);
    const context = vm.createContext({console});
    vm.runInContext(fs.readFileSync(path.join(directory,'package/lib/typescript.js'),'utf8'),context,{timeout:120000});
    preprocessor = context.ts;
  }
  return preprocessor.transpileModule(source,{fileName:'input.js',compilerOptions:{
    target:preprocessor.ScriptTarget.ES5,module:preprocessor.ModuleKind.None,
    allowJs:true,downlevelIteration:true,removeComments:true}}).outputText;
}
const report = {compiler: execFileSync(hermesc, ['-version'], {encoding:'utf8'}),
  compilerSha256:sha(fs.readFileSync(hermesc)), runtimeSha256:sha(fs.readFileSync(hermes)),
  harnessSha256:sha(fs.readFileSync(new URL(import.meta.url))),
  exporterSha256:sha(fs.readFileSync(exporter)), reusedInputs:mode === '--reuse-inputs', projects: []};
const previous = report.reusedInputs ? JSON.parse(fs.readFileSync(path.join(outDir,'report.json'),'utf8')) : null;
if (previous && previous.compiler !== report.compiler) throw new Error('Cached compiler version mismatch');
if (previous?.compilerSha256 && previous.compilerSha256 !== report.compilerSha256)
  throw new Error('Cached compiler binary mismatch');
for (const project of projects) {
  const archive = path.resolve(archives, `${project.name}-${project.version}.tgz`);
  if (sha(fs.readFileSync(archive)) !== project.archiveSha256) throw new Error(`${project.name}: archive integrity mismatch`);
  const directory = path.resolve(outDir, `${project.name}-${project.version}`);
  fs.mkdirSync(directory, {recursive:true});
  execFileSync('tar', ['-xzf', archive, '-C', directory, `package/${project.source}`, `package/${project.license || 'LICENSE'}`], {stdio:'pipe'});
  const source = fs.readFileSync(path.join(directory, 'package', project.source), 'utf8');
  const input = path.join(directory, 'input.js'), bytecode = path.join(directory, 'input.hbc');
  const bundle = path.join(directory, 'bundle.js');
  const host = project.hostConsole ? fs.readFileSync(new URL('./console_host.js',import.meta.url),'utf8') : '';
  const resultExpression = project.hostConsole ? `{result:${project.probe},logs:hermesHostLogs}` : project.probe;
  const code = `${host}\n${source}\nprint(JSON.stringify(${resultExpression}));\n`;
  const observed = [];
  vm.runInNewContext(code, {print: value => observed.push(value), console}, {timeout:60000});
  const expected = observed.join('\n') + '\n';
  // Hermes 90/96 require classes to be downleveled, as in Metro's normal flow.
  // Compare preprocessing against the original source before testing bytecode.
  const suffix = `\nprint(JSON.stringify(${resultExpression}));\n`;
  let compiledSource;
  if (previous) {
    const prior = previous.projects.find(item=>item.name === project.name && item.version === project.version && item.passed);
    const cached = fs.readFileSync(input,'utf8');
    const prefix = `${host}\n`;
    if (!prior || prior.archiveSha256 !== project.archiveSha256 || prior.sourceSha256 !== sha(source)
        || prior.bytecodeSha256 !== sha(fs.readFileSync(bytecode))
        || !cached.startsWith(prefix) || !cached.endsWith(suffix))
      throw new Error(`${project.name}: cached input integrity mismatch`);
    compiledSource = cached.slice(prefix.length, -suffix.length);
    if (sha(compiledSource) !== prior.compiledSourceSha256)
      throw new Error(`${project.name}: cached transformed source mismatch`);
  } else compiledSource = project.downlevel ? downlevel(source) : source;
  const compiledCode = `${host}\n${compiledSource}\nprint(JSON.stringify(${resultExpression}));\n`;
  if (project.downlevel) {
    const transformed = [];
    vm.runInNewContext(compiledCode,{print:value=>transformed.push(value),console},{timeout:120000});
    if (transformed.join('\n')+'\n' !== expected) throw new Error(`${project.name}: preprocessing changed workload behavior`);
  }
  if (!previous) {
    fs.writeFileSync(input,compiledCode);
    execFileSync(hermesc, ['-O', '-emit-binary', '-out', bytecode, input], {timeout:120000, maxBuffer:8e6});
  }
  const nativeOutput = execFileSync(hermes, [bytecode], {encoding:'utf8',timeout:120000,maxBuffer:16e6});
  if (nativeOutput !== expected) throw new Error(`${project.name}: original Hermes differs from source baseline`);
  const start = performance.now();
  try {
    execFileSync(exporter, ['export-bundle', bytecode, '-o', bundle], {timeout:120000,maxBuffer:8e6});
    const exportMs = performance.now()-start;
    const exported = fs.readFileSync(bundle, 'utf8');
    const actual = [];
    vm.runInNewContext(exported, {print:value=>actual.push(value), console}, {timeout:120000});
    if (actual.join('\n')+'\n' !== expected) throw new Error('Export differs from Hermes execution');
    report.projects.push({name:project.name, version:project.version, passed:true,
      archiveSha256:sha(fs.readFileSync(archive)), sourceSha256:sha(source), bytecodeSha256:sha(fs.readFileSync(bytecode)),
      compiledSourceSha256:sha(compiledSource), preprocessing:project.downlevel ? 'typescript-es5' : 'none',
      inputSha256:sha(compiledCode),
      hostSha256:host ? sha(host) : null,
      hbcBytes:fs.statSync(bytecode).size, jsBytes:exported.length,
      functions:(exported.match(/ = function function_/g)||[]).length,
      outputSha256:sha(expected), exportMs:Math.round(exportMs), elapsedMs:Math.round(performance.now()-start)});
  } catch(error) {
    report.projects.push({name:project.name,version:project.version,passed:false,error:String(error),stderr:error.stderr?.toString()});
  }
  fs.writeFileSync(path.join(outDir,'report.json'),JSON.stringify(report,null,2));
  console.log(JSON.stringify(report.projects.at(-1)));
}
if (report.projects.some(project=>!project.passed)) process.exitCode = 1;

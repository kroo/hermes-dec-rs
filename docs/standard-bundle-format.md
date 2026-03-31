# Metro Standard Bundle Format - Detailed Specification

## Overview

This document provides detailed technical specifications for parsing Metro's standard (non-RAM) bundle format, focusing on the implementation requirements for bundle parser and module resolver components.

## Bundle Structure

A Metro standard bundle consists of three main sections:

```
[PRE-CODE] + [MODULES] + [POST-CODE]
```

### Pre-Code Section
- Runtime polyfills and initialization code
- Metro's require implementation
- Script-type modules (type: `js/script`)
- Usually includes React Native polyfills in RN apps

### Modules Section  
- Application modules wrapped in `__d()` calls
- Each module has type: `js/module`
- Sorted by module ID (numeric or string)

### Post-Code Section
- Entry point execution statements (`require()` calls)
- Source map URL comment
- Source URL comment (optional)

## Module Wrapping Format

### Standard Module Pattern
```javascript
__d(function(global, require, _dependencyMap, module, exports) {
  // Original module code here
}, moduleId, dependencyMap, verboseName);
```

### Pattern Variations by Parameters

**Development Build (3 parameters):**
```javascript
__d(function() { /* code */ }, "moduleId", ["dep1", "dep2"], "path/to/module")
```

**Production Build (2 parameters):**
```javascript  
__d(function() { /* code */ }, 123, [456, 789])
```

**With Async Paths:**
```javascript
__d(function() { /* code */ }, 123, {0: 456, 1: 789, paths: {456: "/chunk.bundle?params"}})
```

## Dependency Map Formats

### Array Format (Standard)
```javascript
[moduleId1, moduleId2, moduleId3]
```
- Index corresponds to `_dependencyMap[index]` in module code
- Null entries represent unresolved dependencies

### Object Format (With Async Paths)
```javascript
{
  0: moduleId1,
  1: moduleId2, 
  paths: {
    moduleId2: "/path/to/chunk.bundle?modulesOnly=true&runModule=false"
  }
}
```

## Bundle Parser Implementation

### 1. Bundle Type Detection

```javascript
function detectBundleType(content) {
  // Standard bundles are text-based
  if (typeof content === 'string' || Buffer.isBuffer(content)) {
    const text = content.toString();
    // Look for __d( function calls
    if (text.includes('__d(')) return 'standard';
  }
  
  // RAM bundles start with magic number
  if (Buffer.isBuffer(content) && content.readUInt32LE(0) === 0x464220fb) {
    return 'ram';
  }
  
  throw new Error('Unknown bundle format');
}
```

### 2. Module Extraction

#### Regex-Based Approach
```javascript
const MODULE_REGEX = /__d\(\s*function[^}]*}\s*,\s*([^)]+)\)/g;

function extractModules(bundleCode) {
  const modules = [];
  let match;
  
  while ((match = MODULE_REGEX.exec(bundleCode)) !== null) {
    const fullMatch = match[0];
    const params = match[1];
    
    // Parse parameters: moduleId, dependencyMap, verboseName
    const parsedParams = parseModuleParams(params);
    const moduleCode = extractModuleFunction(fullMatch);
    
    modules.push({
      id: parsedParams.moduleId,
      code: moduleCode,
      dependencyMap: parsedParams.dependencyMap,
      verboseName: parsedParams.verboseName,
      raw: fullMatch
    });
  }
  
  return modules;
}
```

#### AST-Based Approach (Recommended)
```javascript
const {parse} = require('@babel/parser');
const traverse = require('@babel/traverse').default;

function extractModulesAST(bundleCode) {
  const ast = parse(bundleCode, {sourceType: 'script'});
  const modules = [];
  
  traverse(ast, {
    CallExpression(path) {
      if (path.node.callee.name === '__d') {
        const args = path.node.arguments;
        if (args.length >= 2) {
          const moduleFunction = args[0]; // Function expression
          const moduleId = evaluateExpression(args[1]);
          const dependencyMap = args[2] ? evaluateExpression(args[2]) : [];
          const verboseName = args[3] ? evaluateExpression(args[3]) : null;
          
          modules.push({
            id: moduleId,
            code: generate(moduleFunction).code,
            dependencyMap,
            verboseName,
            location: path.node.loc
          });
        }
      }
    }
  });
  
  return modules;
}
```

### 3. Parameter Parsing

```javascript
function parseModuleParams(paramsString) {
  // Handle different parameter formats
  const params = [];
  let currentParam = '';
  let depth = 0;
  let inString = false;
  let stringChar = null;
  
  for (let i = 0; i < paramsString.length; i++) {
    const char = paramsString[i];
    
    if (!inString) {
      if (char === '"' || char === "'") {
        inString = true;
        stringChar = char;
      } else if (char === '[' || char === '{') {
        depth++;
      } else if (char === ']' || char === '}') {
        depth--;
      } else if (char === ',' && depth === 0) {
        params.push(JSON.parse(currentParam.trim()));
        currentParam = '';
        continue;
      }
    } else if (char === stringChar && paramsString[i-1] !== '\\') {
      inString = false;
      stringChar = null;
    }
    
    currentParam += char;
  }
  
  if (currentParam.trim()) {
    params.push(JSON.parse(currentParam.trim()));
  }
  
  return {
    moduleId: params[0],
    dependencyMap: params[1] || [],
    verboseName: params[2] || null
  };
}
```

## Module Resolver Implementation

### 1. Dependency Map Resolution

```javascript
class ModuleResolver {
  constructor(modules, createModuleId) {
    this.modules = new Map();
    this.idToPath = new Map();
    this.pathToId = new Map();
    this.createModuleId = createModuleId;
    
    // Build lookup tables
    modules.forEach(module => {
      this.modules.set(module.id, module);
      if (module.verboseName) {
        this.idToPath.set(module.id, module.verboseName);
        this.pathToId.set(module.verboseName, module.id);
      }
    });
  }
  
  resolveModule(moduleId) {
    return this.modules.get(moduleId);
  }
  
  getModulePath(moduleId) {
    return this.idToPath.get(moduleId);
  }
  
  resolveDependencies(module) {
    const dependencies = [];
    const depMap = module.dependencyMap;
    
    if (Array.isArray(depMap)) {
      depMap.forEach((depId, index) => {
        if (depId !== null) {
          const depModule = this.resolveModule(depId);
          const depPath = this.getModulePath(depId);
          dependencies.push({
            index,
            id: depId,
            module: depModule,
            path: depPath
          });
        }
      });
    } else if (typeof depMap === 'object') {
      // Handle object format with async paths
      Object.entries(depMap).forEach(([key, value]) => {
        if (key !== 'paths' && value !== null) {
          const index = parseInt(key);
          const depModule = this.resolveModule(value);
          const depPath = this.getModulePath(value);
          const asyncPath = depMap.paths?.[value];
          
          dependencies.push({
            index,
            id: value,
            module: depModule,
            path: depPath,
            asyncPath
          });
        }
      });
    }
    
    return dependencies;
  }
}
```

### 2. Path Reconstruction

```javascript
function reconstructModulePaths(modules, projectRoot) {
  const pathMap = new Map();
  
  // Use verboseName for dev builds
  modules.forEach(module => {
    if (module.verboseName) {
      const fullPath = path.resolve(projectRoot, module.verboseName);
      pathMap.set(module.id, fullPath);
    }
  });
  
  // Fallback: analyze dependency patterns
  if (pathMap.size === 0) {
    pathMap = reconstructFromDependencies(modules, projectRoot);
  }
  
  return pathMap;
}

function reconstructFromDependencies(modules, projectRoot) {
  // Build dependency graph
  const graph = new Map();
  const pathMap = new Map();
  
  modules.forEach(module => {
    const deps = [];
    if (Array.isArray(module.dependencyMap)) {
      module.dependencyMap.forEach((depId, index) => {
        if (depId !== null) {
          deps.push(depId);
        }
      });
    }
    graph.set(module.id, deps);
  });
  
  // Identify entry points (modules with no dependencies pointing to them)
  const hasIncomingDeps = new Set();
  graph.forEach(deps => {
    deps.forEach(dep => hasIncomingDeps.add(dep));
  });
  
  const entryPoints = [];
  graph.forEach((_, moduleId) => {
    if (!hasIncomingDeps.has(moduleId)) {
      entryPoints.push(moduleId);
    }
  });
  
  // Assign paths based on structure
  // This is heuristic-based and may need refinement
  let pathCounter = 0;
  const visitedModules = new Set();
  
  function assignPath(moduleId, basePath = '') {
    if (visitedModules.has(moduleId)) return;
    visitedModules.add(moduleId);
    
    const modulePath = basePath || `module_${pathCounter++}`;
    pathMap.set(moduleId, path.join(projectRoot, modulePath));
    
    const deps = graph.get(moduleId) || [];
    deps.forEach((depId, index) => {
      const depPath = `${modulePath}_dep_${index}`;
      assignPath(depId, depPath);
    });
  }
  
  entryPoints.forEach(entryId => {
    assignPath(entryId, 'index');
  });
  
  return pathMap;
}
```

## Source Map Integration

### Extracting Source Map URLs

```javascript
function extractSourceMapInfo(bundleCode) {
  const sourceMapRegex = /\/\/# sourceMappingURL=(.+)$/m;
  const sourceUrlRegex = /\/\/# sourceURL=(.+)$/m;
  
  const sourceMapMatch = bundleCode.match(sourceMapRegex);
  const sourceUrlMatch = bundleCode.match(sourceUrlRegex);
  
  return {
    sourceMapUrl: sourceMapMatch?.[1],
    sourceUrl: sourceUrlMatch?.[1],
    hasInlineSourceMap: sourceMapMatch?.[1]?.startsWith('data:')
  };
}

function parseInlineSourceMap(bundleCode) {
  const sourceMapInfo = extractSourceMapInfo(bundleCode);
  
  if (sourceMapInfo.hasInlineSourceMap) {
    const dataUrl = sourceMapInfo.sourceMapUrl;
    const base64Data = dataUrl.split(',')[1];
    const sourceMapJson = Buffer.from(base64Data, 'base64').toString();
    return JSON.parse(sourceMapJson);
  }
  
  return null;
}
```

## Implementation Notes

### Error Handling
- Handle malformed `__d()` calls gracefully
- Validate parameter counts and types
- Deal with escaped strings in module code
- Handle edge cases in dependency map formats

### Performance Considerations
- Use streaming parsers for large bundles
- Implement lazy module loading where possible
- Cache parsed results for repeated operations
- Consider worker threads for CPU-intensive parsing

### Compatibility
- Support both numeric and string module IDs
- Handle legacy bundle formats
- Account for different Metro versions
- Support platform-specific variations

## Testing Strategy

Create test cases covering:
- Development vs production bundles
- Different module ID strategies
- Various dependency map formats
- Async import scenarios
- Source map variations
- Edge cases and malformed input

## Related Files

Key Metro source files for reference:
- `packages/metro/src/DeltaBundler/Serializers/baseJSBundle.js`
- `packages/metro/src/DeltaBundler/Serializers/helpers/js.js`
- `packages/metro-transform-plugins/src/addParamsToDefineCall.js`
- `packages/metro/src/lib/getAppendScripts.js`
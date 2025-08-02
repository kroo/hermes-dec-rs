# Metro Unbundler Specification

## Overview

This document outlines a comprehensive specification for decompiling Metro bundler output back to original source code. The unbundler leverages Metro's internal structures and follows the same patterns used during bundling to achieve accurate reconstruction.

## Bundle Format Analysis

Metro produces two main bundle types:
- **Standard JS Bundle**: Concatenated modules with dependency maps
- **RAM Bundle**: Binary format with indexed module lookup

## Core Unbundling Process

### Phase 1: Bundle Type Detection

1. **Read bundle header/magic number**
   - Standard bundle: Text-based, starts with runtime code
   - RAM bundle: Binary, starts with magic number (0x464220fb)

2. **Parse bundle metadata**
   - Extract source map URL/inline source map
   - Identify module boundaries and IDs

### Phase 2: Module Extraction

**Standard Bundle:**
1. Parse `__d()` function calls to extract modules
2. Extract module parameters: `[moduleId, dependencyMap, verboseName]`
3. Reverse dependency mapping using createModuleId function

**RAM Bundle:**
1. Read header: `[magic, numModules, startupCodeLength]`
2. Parse table of contents (module offsets/lengths)
3. Extract startup code and individual modules by ID

### Phase 3: Dependency Graph Reconstruction

1. Parse dependency maps from each module
2. Resolve module paths using Metro's module resolution logic
3. Reconstruct original file structure based on:
   - projectRoot relative paths
   - verboseName debugging info (dev bundles)
   - Source map file mappings

### Phase 4: Source Recovery

1. Apply source maps to recover original source code
2. Reverse Metro transformations:
   - Remove `__d()` wrappers
   - Restore original imports/requires
   - Unwrap async import paths
3. Reconstruct file extensions and directory structure

## Key Data Structures

### Module Record
```javascript
{
  id: number,
  path: string,
  code: string,
  dependencies: Map<string, {absolutePath, asyncType}>,
  sourceMap: MixedSourceMap,
  verboseName?: string // dev builds only
}
```

### Bundle Metadata
```javascript
{
  format: 'standard' | 'ram',
  entryPoint: string,
  preModules: Module[],
  postModules: Module[],
  sourceMapUrl?: string,
  platform: string
}
```

## Implementation Architecture

### Core Components

1. **BundleParser**: Detects format and extracts raw module data
2. **ModuleExtractor**: Parses individual modules and dependency maps  
3. **DependencyResolver**: Reconstructs module graph and file paths
4. **SourceRecoverer**: Applies source maps and reverses transformations
5. **FileSystemBuilder**: Recreates original directory structure

### Processing Pipeline
```
Bundle → BundleParser → ModuleExtractor → DependencyResolver → SourceRecoverer → FileSystemBuilder → Original Sources
```

## RAM Bundle Specifics

Uses RamBundleParser structure from `packages/metro/src/lib/RamBundleParser.js`:
- Header: `[MAGIC_NUMBER, numModules, startupCodeLength]`
- Table of contents: `[moduleOffset, moduleLength]` pairs
- Startup code section
- Individual module sections

## Source Map Integration

Leverages Metro's BundleBuilder source map format from `packages/metro-source-map/src/BundleBuilder.js`:
- Index maps with sections for each module
- Offset tracking for line/column mapping
- Original file path recovery from source map sources

## Edge Cases & Considerations

- Handle async/dynamic imports with split bundles
- Preserve Metro's module ID generation strategy
- Support both development and production bundle formats
- Handle platform-specific optimizations (Android assets vs iOS indexed)
- Maintain compatibility with Metro's transformation pipeline

## Related Files

Key Metro source files referenced:
- `packages/metro/src/DeltaBundler/Serializers/baseJSBundle.js`
- `packages/metro/src/DeltaBundler/Serializers/helpers/js.js`
- `packages/metro/src/lib/RamBundleParser.js`
- `packages/metro-source-map/src/BundleBuilder.js`
- `packages/metro/src/shared/output/bundle.flow.js`
# Path Reconstruction Heuristics Guide

## Overview

This guide provides detailed heuristics and algorithms for reconstructing the original file structure from a Metro bundle without debug information (verboseName), particularly when working with compiled bytecode where variable names are lost. These techniques analyze module patterns, dependencies, and code structure to infer the most likely directory organization.

## Core Principles

### 1. Module Classification

Before reconstructing paths, classify each module into categories based on structural patterns:

**Classification Categories:**
- **Entry Points**: Modules with no incoming dependencies
- **Package Indexes**: Modules that primarily re-export from other modules
- **Components**: Modules containing UI/rendering patterns
- **Utilities**: Helper modules with pure logic functions
- **Configuration**: Modules with static data structures
- **Vendor/Third-party**: External library modules
- **Tests**: Testing-related modules

**Classification Signals (without variable names):**
- Dependency count and direction (incoming vs outgoing)
- Module size (bytecode length)
- Presence of specific bytecode patterns (loops, conditionals, function calls)
- Import/export ratios
- Position in the bundle (early modules often framework/vendor)
- Clustering patterns with other modules

## Dependency Analysis Strategies

### 2. Dependency Clustering

Group modules that frequently import each other to identify packages and related components:

**Clustering Algorithm:**
1. Build a bidirectional dependency graph
2. Calculate connection strength between module pairs
3. Group strongly connected modules (threshold: 0.7)
4. Identify module clusters that likely belong in the same directory

**Dependency Strength Calculation:**
- **Bidirectional dependencies** (A imports B, B imports A): High strength (0.6)
- **Unidirectional dependencies** (A imports B only): Medium strength (0.3)
- **Shared dependencies** (A and B import same modules): Variable strength (0.1-0.4)
- **Transitive dependencies** (A → B → C): Low strength (0.1)

**Cluster Patterns:**
- **Tight clusters** (>5 modules, high interconnection): Likely a single package/feature
- **Star pattern** (one module imported by many): Likely a utility or shared component
- **Chain pattern** (A → B → C → D): Likely a processing pipeline or middleware
- **Island clusters** (isolated groups): Likely separate features or packages

### 3. Import Pattern Analysis

Analyze module reference patterns to infer directory relationships:

**Dependency Distance Metrics:**
- **Direct dependency**: Distance = 1 (likely same or adjacent directory)
- **Two-hop dependency**: Distance = 2 (likely parent/child or sibling directories)
- **Three+ hop dependency**: Distance > 2 (likely different packages or distant directories)

**Module Reference Patterns:**
- **High mutual imports**: Modules in the same directory
- **One-way imports**: Parent → child directory relationship
- **Circular dependencies**: Modules at the same directory level
- **No direct connection**: Different feature areas or packages

**Statistical Indicators:**
- Modules with similar dependency sets often share a parent directory
- Modules imported by many others are typically in `utils/`, `common/`, or `shared/`
- Modules importing many others are typically entry points or orchestrators
- Isolated module chains suggest feature-specific subdirectories

## Path Reconstruction Algorithm

### 4. Hierarchical Path Building

Build directory structure from entry points outward using breadth-first traversal:

**Algorithm Steps:**

1. **Place Entry Points**
   - Modules with no incoming dependencies → root level (`index.js`, `App.js`)
   - Secondary entry points → `src/` directory
   - Platform-specific entries → `index.ios.js`, `index.android.js`

2. **Identify Vendor Modules**
   - Modules with IDs 0-100 (typically polyfills and framework)
   - Modules with high incoming dependency count
   - Modules with no outgoing application dependencies
   - Place in `node_modules/` structure

3. **Build Application Structure**
   - Start from entry points
   - Use breadth-first search to assign paths
   - Maintain depth from entry point
   - Group clustered modules together

4. **Path Assignment Rules**
   - **Depth 0**: Entry points → root or `src/`
   - **Depth 1**: Direct dependencies → `src/[category]/`
   - **Depth 2-3**: Feature modules → `src/[feature]/[subfeature]/`
   - **Depth 4+**: Deep dependencies → `src/[feature]/lib/` or `utils/`

**Directory Naming Heuristics:**
- Modules imported by many → `common/`, `shared/`, `utils/`
- Modules in tight clusters → `features/[feature-name]/`
- Modules with UI patterns → `components/`, `screens/`, `views/`
- Modules with data patterns → `models/`, `stores/`, `services/`
- Configuration patterns → `config/`, `constants/`
- Test patterns → `__tests__/`, `tests/`

## Pattern Recognition Techniques

### 5. Bytecode Structure Patterns

Identify file types through bytecode patterns without relying on variable names:

**Component/UI Patterns:**
- High number of function calls (rendering methods)
- Object property access patterns (props, state)
- Array operations (mapping over lists)
- Conditional branches (conditional rendering)
- Suggested paths: `components/`, `screens/`, `views/`

**State Management Patterns:**
- Switch-case structures (reducers)
- Object merging operations (state updates)
- Function returns with object literals
- Suggested paths: `store/`, `reducers/`, `state/`

**Service/API Patterns:**
- Async function patterns
- Promise chains
- String concatenation (URL building)
- Error handling blocks (try-catch)
- Suggested paths: `services/`, `api/`, `network/`

**Utility Patterns:**
- Pure functions (no external calls)
- Mathematical operations
- String/array manipulations
- No UI-related patterns
- Suggested paths: `utils/`, `helpers/`, `lib/`

**Configuration Patterns:**
- Object literals with no functions
- Constant definitions
- No control flow
- Static data structures
- Suggested paths: `config/`, `constants/`

**Test Patterns:**
- Assertion patterns
- Mock/stub patterns
- Describe/it block structures
- Suggested paths: `__tests__/`, `tests/`, `spec/`

### 6. Module Naming Without Variable Names

Generate meaningful file names based on module characteristics:

**Naming Strategy Hierarchy:**

1. **Based on Module ID Patterns:**
   - Entry points (ID 0 or last required): `index.js`, `main.js`
   - Low IDs (0-50): `polyfill_[id].js`, `runtime_[id].js`
   - Sequential clusters: `feature_[cluster_id]_[sequence].js`

2. **Based on Structural Patterns:**
   - Single export modules: `[module_id]_export.js`
   - Multi-export modules: `[module_id]_lib.js`
   - Re-export modules: `[module_id]_index.js`
   - Heavy logic modules: `[module_id]_core.js`

3. **Based on Dependency Patterns:**
   - No dependencies: `[module_id]_constants.js`
   - Many dependencies: `[module_id]_aggregator.js`
   - Imported by many: `[module_id]_common.js`
   - Circular dependencies: `[module_id]_coupled.js`

4. **Based on Bytecode Characteristics:**
   - Many conditionals: `[module_id]_logic.js`
   - Array operations: `[module_id]_list.js`
   - Object operations: `[module_id]_model.js`
   - String operations: `[module_id]_text.js`

5. **Fallback Naming:**
   - Use module ID: `module_[id].js`
   - Use hash of bytecode: `file_[hash].js`
   - Use position in bundle: `bundle_[position].js`

## Advanced Heuristics

### 7. Directory Structure Templates

Match against common project structures based on module patterns:

**React Application Structure:**
- Entry point imports many components
- Component modules cluster together
- Utility modules have high reuse
- Typical structure:
  ```
  src/
    components/
    hooks/
    utils/
    services/
    styles/
  ```

**React Native Application Structure:**
- Platform-specific entry points
- Screen/navigation patterns
- Native module integrations
- Typical structure:
  ```
  src/
    screens/
    components/
    navigation/
    services/
    utils/
  ios/
  android/
  ```

**Node.js Backend Structure:**
- Route/controller patterns
- Middleware chains
- Database models
- Typical structure:
  ```
  src/
    routes/
    controllers/
    models/
    middleware/
    utils/
  ```

**Library Structure:**
- Single entry point
- Pure utility functions
- Minimal external dependencies
- Typical structure:
  ```
  src/
    lib/
    utils/
    helpers/
  dist/
  ```

**Detection Strategy:**
1. Analyze entry point patterns
2. Measure module clustering density
3. Check for platform-specific patterns
4. Count UI vs logic module ratio
5. Match against known templates

### 8. Dependency Distance Calculation

Use graph distance to infer directory proximity:

**Distance-to-Directory Mapping:**
- **Distance 1**: Same directory (siblings)
- **Distance 2**: Parent-child or adjacent directories
- **Distance 3-4**: Same feature area
- **Distance 5+**: Different packages or feature areas

**Graph Algorithms:**

1. **Shortest Path (BFS)**
   - Find minimum hops between modules
   - Lower distance = closer in directory structure

2. **Common Ancestor Detection**
   - Find shared dependencies
   - Modules with same ancestors likely in same subtree

3. **Clustering Coefficient**
   - Measure how connected a module's dependencies are
   - High coefficient = tight feature group

4. **Centrality Analysis**
   - Betweenness centrality: Bridge modules between features
   - Degree centrality: Core utilities and common modules
   - Closeness centrality: Central business logic

**Practical Application:**
- Modules with distance 1-2: Group in same directory
- Modules with distance 3-4: Place in sibling directories
- Modules with distance 5+: Separate feature directories
- Isolated subgraphs: Independent packages

## Validation and Refinement

### 9. Path Validation Rules

Ensure reconstructed paths follow logical patterns:

**Validation Checks:**

1. **Duplicate Path Detection**
   - No two modules should have identical paths
   - Resolution: Append module ID or sequence number

2. **Orphaned Module Detection**
   - Single module in a directory (except root/entry points)
   - Resolution: Move to parent or merge with siblings

3. **Depth Validation**
   - Maximum reasonable depth: 5-6 levels
   - Resolution: Flatten deep hierarchies

4. **Cluster Consistency**
   - Clustered modules should be in same/adjacent directories
   - Resolution: Reorganize to maintain cluster proximity

5. **Import Distance Validation**
   - Direct imports should be nearby in directory tree
   - Resolution: Adjust paths to minimize import distances

**Common Issues and Fixes:**

| Issue | Detection | Resolution |
|-------|-----------|------------|
| Duplicate paths | Same path assigned to multiple modules | Append `_1`, `_2` or module ID |
| Orphaned modules | Single file in directory | Move to parent or `misc/` |
| Over-nesting | Path depth > 6 | Flatten to max 5 levels |
| Split clusters | Cluster members in different trees | Reunite in common directory |
| Circular paths | A imports B, B imports A, different dirs | Place in same directory |
| Missing vendor | High ID module in src/ | Move to node_modules/ |

## Final Recommendations

### 10. Confidence Scoring

Assign confidence scores to reconstructed paths:

**Confidence Factors:**

| Factor | Impact | Rationale |
|--------|--------|-----------|
| Entry point module | +0.2 | Known starting points |
| Part of cluster | +0.15 | Strong relationships identified |
| Vendor module (ID < 100) | +0.2 | Framework/library patterns |
| Validated path | +0.1 | Passed consistency checks |
| Generic name (module_X) | -0.2 | No patterns detected |
| Orphaned module | -0.15 | Uncertain placement |
| Deep nesting (>5 levels) | -0.1 | Likely incorrect |

**Confidence Levels:**
- **High (0.8-1.0)**: Entry points, vendor modules, validated clusters
- **Medium (0.5-0.8)**: Classified modules with clear patterns
- **Low (0.0-0.5)**: Orphaned, deeply nested, or unclassified modules

### Complete Reconstruction Pipeline

1. **Parse Bundle**
   - Extract module list and IDs
   - Build dependency map
   - Identify entry points

2. **Analyze Structure**
   - Find module clusters
   - Calculate dependency distances
   - Detect project type

3. **Classify Modules**
   - Categorize by patterns
   - Identify vendor modules
   - Mark test/config files

4. **Build Paths**
   - Place entry points
   - Assign vendor paths
   - Build application tree
   - Group clusters

5. **Validate**
   - Check for duplicates
   - Find orphaned modules
   - Verify depth limits

6. **Refine**
   - Fix validation issues
   - Optimize import distances
   - Flatten deep nesting

7. **Score**
   - Calculate confidence
   - Mark uncertain paths
   - Generate report

## Key Takeaways

1. **Layer multiple heuristics** - No single approach will be perfect
2. **Start from known points** - Entry points and vendor modules are easier to identify
3. **Use clustering** - Modules that import each other frequently likely belong together
4. **Validate results** - Check for common issues like duplicates and orphans
5. **Assign confidence scores** - Be transparent about reconstruction certainty
6. **Consider project templates** - Most projects follow common structures
7. **Preserve relationships** - Maintain relative positioning of interdependent modules
8. **Handle edge cases** - Have fallbacks for unclassifiable modules

The combination of these techniques should provide reasonable path reconstruction even without debug information or variable names, though results will never be as accurate as with verboseName data.
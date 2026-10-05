# NPM Clustering Experiments (Summary)

This note summarizes quick experiments clustering require/import graphs of a few common packages (axios, lodash, immer, redux, reselect) without using any path information in the algorithm.

Method
- Extract intra-package require/import edges (relative imports only) from .js files.
- Build per-file dependency (deps) and consumer (reverse deps) sets.
- Compute pairwise strength S(a,b):
  - +0.6 bidirectional, +0.35 unidirectional
  - +0.5 IDF-weighted Jaccard(deps) with idf(x) = 1 / ln(2 + indegree(x))
  - +0.3 IDF-weighted Jaccard(consumers)
  - +0.2 cosine(neighbors) over deps ∪ consumers
  - +0.1 transitive hint (A→X→B or B→X→A)
- Union edges S ≥ 0.55 to form clusters; expansion: add nodes with total tie ≥ 1.0 to clusters ≥ 3.

Evaluation (vs. real folder boundaries)
- Purity (homogeneity): percent of files in each cluster that share the majority folder label (micro-avg).
- Completeness: percent of files in each folder captured by the majority predicted cluster (micro-avg).
- F1: harmonic mean of the two.

Highlights
- axios (cohesive): clusteredEdges ≈ 50%, F1 ≈ 0.63; clusters align with lib/ substructure without path inputs.
- lodash (flat utilities): clusteredEdges very low; many tiny clusters; avoids inventing structure (desired).
- Small libs (immer): tiny graphs; small clusters or none; purity high when any cluster exists.

Takeaways
- IDF-weighted shared sets plus mutual-neighbor similarity substantially improve recall on cohesive codebases.
- Flat, utility-style packages remain unclustered, which is correct.
- These learnings guided the Rust clustering improvements for Metro bundles (same path-agnostic signals).

# bayesmokken 0.10.0

The clustered polytomous path now implements the adjusted complete-record
Bayesian-bootstrap interval of the clustered article's current version, in
place of the two-stage hierarchical procedure of 0.9.0. The binary path and
the coefficient kernel are unchanged.

## Breaking changes

- **The default clustered weight law is `"complete_record"`.** With a
  `cluster`, `weight_law = "auto"` now resolves to it in both `mokken_h()`
  and `bb_mokken_h()`. One Dirichlet weight is drawn per complete cluster
  record and nothing is reweighted inside a record: respondent weights are
  `A_g / m_g` (equal-cluster target) and `A_g / sum_h m_h A_h` (respondent
  target). The earlier version's within-cluster stage adds first-order
  variance that no multiplier tending to one can remove, which is why it was
  replaced.

- **The clustered interval is a reflected, expanded error interval.** The
  type-8 quantiles of the draws are reflected about the point estimate,
  expanded by `k_G = sqrt((G + 1)/(G - 1)) * qt(.975, G - 1) / qnorm(.975)`,
  and intersected with `(-Inf, 1]`. `width_multiplier` carries `k_G`, which
  is exactly the width ratio before the intersection; `gamma` is `NA`. The
  reported interval need not contain the point estimate and does not share
  the percentile interval's midpoint. New interval fields record the
  construction: `reflected_lower`/`reflected_upper` (before the
  intersection), `upper_intersection`, `set_empty`, `zero_width`,
  `point_boundary`, `boundary_draws`, `root_multiplier`,
  `multiplier_formula`, `parameter_space`, `archive_method_id`
  (`one_stage_basic_G_adjusted`), `protocol_id` (`C2`), `zero_tolerance`
  and `undefined_draw_rule`. The historical clustered interval gains one
  field, `construction = "midpoint_halfwidth_scaling"`.

- **The 22–80 cluster refusal no longer applies to the default path.** The
  complete-record interval is computable for `G >= 2`, which is a
  computational requirement and not a recommendation. `validation_status`
  reports `CONFIRMED_CLUSTER_COUNT` inside the confirmed range 22–320 and
  `OUTSIDE_CONFIRMED_CLUSTER_COUNT` elsewhere, without refusing or warning;
  `validation_scope` is `POLYTOMOUS_CLUSTERED_COMPLETE_RECORD_BB`.
  `bm_check_data()` now recommends `bb_mokken_h()` for every polytomous
  clustered screen and names the confirmed range in its reason line.

- **A stricter typed-undefined rule for scale draws.** A clustered scale
  draw whose scale denominator or any pair denominator is at most `1e-12` is
  typed undefined (`scale_status = "typed_undefined"` when the package's own
  zero tolerance would have kept it), and the point estimate is treated the
  same way for the interval. Item and pair draws keep the package statuses.
  `fit$draws` gains `scale_denominator` and `minimum_pair_denominator`.

- **New method identifiers.** Law `V5-CRBB-RESP-v1` / `V5-CRBB-CLUSTER-v1`;
  interval `V5-C2-CRBB-ADJUSTED-RESP-v1` / `-CLUSTER-v1`; percentile
  comparator (`interval = "equal_tail"`) `V5-CMP-CRBB-PERCENTILE-RESP-v1` /
  `-CLUSTER-v1`; `bb_type` is `complete_record_cluster_bayesian_bootstrap`.

- **`bm_plot_calibration()` draws `k_G` for `mode = "clustered"`**, above
  one and decreasing toward one, solid over 22–320; the two-stage curve is
  available as `mode = "clustered_two_stage"`. A fit picks its own curve.
  `bm_mc_diagnostics()` and `bm_plot_mc_stability()` rebuild endpoints with
  the construction the interval used; for the complete-record construction
  they describe the endpoints before the intersection with `(-Inf, 1]`.

- **The package title drops "Calibrated".** The binary path is still a
  calibrated correction; the clustered adjustment is a prespecified function
  of the cluster count, and the title now says what both paths share.

## The historical two-stage procedure

`weight_law = "hierarchical_cluster"` runs the 0.9.0 procedure unchanged:
the two-stage law, the shrinkage `1 - 1.5/sqrt(G)` about the percentile
midpoint, the refusal below 22 clusters, the extrapolation warning above 80,
and the `V4-P15-HBB-*-GN150-v1` identifiers. Its numbers are frozen by the
parity gate (fixtures F7 and F8). It is retained so that results of the
earlier version can be reproduced, and every `bb_mokken_h()` call under it
emits a warning of class `bm_historical_method_warning`, independently of
`warn`; `mokken_h()` records the declared law as provenance without warning.
The call printed in the earlier version of the clustered article runs
unchanged and produces that version's identifiers.

## Reproduction of the clustered article

The complete-record draw stream is the article's archive stream
(`V4R-DRAW-MAJOR-EXP-GPLUSN-CMRG-v1`): every draw consumes `G + N` unit
exponentials from one L'Ecuyer-CMRG stream, the first `G` become the record
weights, and the result is invariant to `batch_size` and `workers`. On
`mokken::SWMDK` with `draws = 9999` and the archive's main-stream seeds
(1883301060 for the teacher scale, 1863497163 for the classmate scale) a
plain `bb_mokken_h()` call reproduces the article's four intervals: point
estimates to `1.5e-13`, endpoints to `3.2e-13` (the order in which two
kernels sum the same weighted moments), `k_30 = 1.07888669019652` exactly.
The package's arithmetic applied to the archive's stored draws reproduces
its endpoints to `8e-16`. `tests/testthat/test-swmdk-reproduction.R` and
the published-number gate carry these checks; the gate's Paper B section
now verifies the current and the historical numbers side by side.

## Documentation

- Vignettes a3 and m4 describe the complete-record law, the adjustment, the
  cluster-count metadata and the historical option; m4 carries an
  independent reimplementation of the archive stream as its oracle. m5's
  evidence summary reports the current confirmation.
- The SWMDK caches are now 9,999-draw fits at the archive seeds (scale
  draws only), so the vignette shows the published endpoints.
- `bm_guardrails()` lists the historical lane and its cluster-count rules.
- `inst/CITATION` names the clustered article by its current title.

## Release tooling

- `inst/scripts/verify-published-numbers.R` prints which installed
  bayesmokken it loaded and refuses to run unless that version equals the
  source `DESCRIPTION`; `--lib <path>` selects the library, and
  `run-all-gates.R --published --lib <path>` passes it through. A plain run
  previously certified whichever release the library path held first.

## Unchanged

The binary path, the weighted-coefficient kernel, the deterministic
coefficients, the extraction schemas' existing columns, every argument name
and order, and the frozen 95% level. Parity fixtures F1–F8 pass; F7 and F8
run on the historical lane.

---

# bayesmokken 0.9.0

First release with a user-facing surface. The estimation engine is carried
over from the research version unchanged; everything a user touches is new.

## Breaking changes

These change what previously-working code does. They are collected here rather
than scattered through the notes below, because every one of them can turn a
call that used to return a number into a call that raises. Four came out of the
round-1 external review and the fifth out of the audit of that review, and all
five exist because the previous behaviour let the software make a scientific
choice on the user's behalf.

- **`cluster_target` must be declared whenever `cluster` is supplied.** It
  previously defaulted to `"respondent"`. The respondent-weighted and
  equal-cluster coefficients are *different population quantities* under
  informative cluster size, so defaulting silently selected an estimand. Both
  articles say the software does not do this; now it does not.

- **Ordinal `supports` must be declared.** They were previously inferred from
  the observed values. The maximum covariance in the denominator is computed
  *at the declared support*, so inference silently substituted the sample's
  categories for the instrument's.

- **The calibrated clustered interval refuses fewer than 22 clusters, and
  warns above 80.** The shrinkage `1 - 1.5/sqrt(G)` was confirmed on a
  22–80 cluster grid; at `G = 10` it would have cut the interval width by 47%
  with nothing behind it. The deterministic coefficient and the uncorrected
  `interval = "equal_tail"` comparator remain available at any `G`, so a small
  study is not left without an analysis.

- **Dichotomous items with a `cluster` argument are refused on every route.**
  Declaring `item_type = "binary"` alongside a cluster always raised. The
  default did not: `item_type = "auto"` resolves to `"ordinal"` as soon as a
  cluster is supplied, so the binary rules were never consulted and the
  plainest call a user could write — dichotomous data, a cluster vector,
  nothing else declared — quietly entered the clustered path. Both articles
  state that the package refuses to cross the two paths, so it now does,
  including for the uncorrected `equal_tail` comparator.

- **`n_complete` on a clustered interval now counts respondents.** It
  previously held the cluster count: an analysis of 614 pupils in 30
  classrooms reported `n_complete = 30`. The cluster count is available as
  `cluster_count`, and the calibration input as `calibration_n`.

`bm_item_h(interval =)` and `bm_pair_h(interval =)` are renamed to
`include_interval`; the old name still works and warns. Item and pair
intervals are now opt-in rather than on by default.

The call forms printed in both articles run unchanged.

## New: analysis workflow

- `bm_check_data()` screens a dataset before fitting and ends with a command
  you can run.
- `bm_supports()`, `bm_score_map()`, `bm_missing_codes()` build the arguments
  that previously had to be written by hand. Reverse keying is *reported* by
  the screen and applied only when you ask for it, because published
  instruments frequently arrive already scored.
- `bm_cluster_info()` describes the cluster structure and the weight each
  target gives each cluster.

## New: extracting results

- `bm_scale_h()`, `bm_item_h()`, `bm_pair_h()`, `bm_interval()`,
  `bm_draws()`, `bm_provenance()` return plain data frames whose columns are
  fixed by an installed schema and checked against it by the tests.
- `tidy()` and `glance()` methods for the broom ecosystem.
- Item- and pair-level intervals carry an `interval_calibration` column
  reading `"uncalibrated"`. The width correction was validated for the scale
  coefficient only.

## New: diagnostics and reporting

- `bm_diagnostics()` collects denominator, regularity, transport and Monte
  Carlo diagnostics into one report; `bm_mc_diagnostics()`,
  `bm_knot_diagnostics()` and `bm_regularity()` are available individually.
- `bm_guardrails()` lists every situation in which the package refuses to
  compute, with the reason and what to do instead.
- `bm_report()` builds manuscript tables in plain, Markdown or LaTeX form.
- `bm_benchmark()` reports where an estimate sits relative to the conventional
  descriptive landmarks. It reports a point relation only, and its output
  contains none of the words pass, fail or significant.
- `bm_compare()` refuses to line up fits whose item sets differ, unless you
  say you meant it.

## New: figures

Eight figures, a house theme, and colour scales built on
`grDevices::hcl.colors()`. Every distinction is carried by at least two
channels, so nothing is lost in greyscale or to colour-vision deficiency.
`bm_plot_draws()` shows the bootstrap distribution with the uncorrected and
calibrated endpoints drawn over it; `bm_plot_calibration()` runs with no data
at all, for explaining the correction before an analysis exists.

## New: console output

Print and summary methods for every class, showing the raw and the calibrated
interval side by side so the correction is visible, and closing with a line
naming what to call next. Optional `cli`-decorated formatters are available;
the default printing uses base R only.

## New: data

- `srs`: the 337 × 11 Scientific Reasoning Scale responses used in the binary
  application (CC BY 4.0, Caliciuri & Lanz 2024).
- `classroom_wb`: a fully synthetic clustered polytomous dataset.
- `bm_simulate()`: a teaching generator. It is not the simulation engine of
  either article and cannot be used to evaluate coverage.

## Unchanged: the estimation engine

The numbers are the same. `mokken_h()` and `bb_mokken_h()` retain the frozen
0.0.0.9001 numerical receipts: eight parity fixtures are compared field by
field at `tolerance = 0`. Published evidence is reported in three distinct
classes rather than as one machine-precision claim: archived-draw endpoints
agree within about `5e-16`; deterministic application H values agree within
about `1.6e-13`; and fresh stochastic calls agree within their prespecified
Monte Carlo bands (the reviewed maximum endpoint difference was about
`0.0041`). The frozen calibration constants and the 95% level remain
unreachable from the public interface.

The parity gate carries five documented additive/corrected exceptions:
injective pair-draw identity labels; clustered `n_complete` corrected from
cluster count to respondent count; validation-scope metadata; typed Monte
Carlo diagnostic status; and the declared ordinal `weight_law`. Every legacy
numerical value, including all draw arrays, remains compared exactly.

---

# bayesmokken 0.0.0.9001

* `bm_ordinal_max_covariance_transport()` no longer treats valid positive
  masses at or below `1e-12` as exhausted: pointers now advance only on
  exactly exhausted mass, exhausted support points are zeroed explicitly,
  and total transported mass is asserted to equal one within `mass_tol`.
  The batched survival-sum path used by all Bayesian-bootstrap draws is
  unchanged; only the deterministic per-pair kernel was affected, in
  configurations containing adversarially tiny valid masses.
* `bm_rng_streams()` now rejects fractional seeds instead of silently
  truncating them (`1.1` and `1L` previously produced identical streams);
  the public documentation already promised an integer.
* Mode-specific documentation corrected: `score_map` (ordinal targets map to
  declared supports, not to 0/1), the defined-draw minima
  (`max(100, ceiling(.90B))` binary vs `max(20, ceiling(.99B))` clustered
  ordinal), and the per-mode `interval = "calibrated"` method identifiers.
* `warn` is now validated in both public entry points: exactly one
  nonmissing logical value is required, so `1`, `"TRUE"`, `NA`, or a vector
  fails with a typed input error instead of silently suppressing warnings.

# bayesmokken 0.0.0.9000

## Paper A calibrated-interval integration

- Added `bb_mokken_h()` as the primary public uncertainty API.
- Added the selected `P7-FS-WIDTH-RAW-G025-v1` interval as the default and
  retained `CMP-RAW-BB-EQUAL-TAIL-v1` as a transparent comparator.
- Fixed the calibrated algorithm at 95%, type-8 quantiles, gamma = 0.25,
  no endpoint clipping, and no method cascade.
- Replaced `bm_posterior_h` output semantics with `bm_bb_h` and
  `summary_bm_bb_h`.
- Removed threshold probabilities and posterior-admissibility probabilities
  from the public result.
- Deprecated `bayes_mokken_h()` in favour of `bb_mokken_h()`.
- Added typed interval failures, explicit defined-draw counts, endpoint Monte
  Carlo diagnostics, and a calibrated-interval vignette.

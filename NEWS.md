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

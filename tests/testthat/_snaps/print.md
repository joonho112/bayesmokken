# binary fit prints as designed

    Code
      print(bin_fit())
    Output
      ── Mokken scalability H · Bayesian-bootstrap interval ──────────────────────────
        estimand     fixed 11-item binary scale, complete-scale respondents
        method       P7-FS-WIDTH-RAW-G025-v1  (raw base: CMP-RAW-BB-EQUAL-TAIL-v1)
        sample       n = 337 complete · patterns = 245 · draws = 300
      
        scale H      0.130   95% CI [0.103, 0.159]      status: OK
                     raw     [0.103, 0.159]  ->  width x 1.0136
                     (the correction changes the length by 0.00076, below 3-digit display)
      
        Interpretation: Bayesian-bootstrap-based confidence interval for
        repeated-sampling uncertainty; not a posterior credible interval or posterior
        probability.
      
        See: summary() · bm_item_h() · bm_plot_draws() · bm_diagnostics()

# clustered fit prints as designed

    Code
      print(clu_fit())
    Output
      ── Mokken scalability H · complete-record Bayesian bootstrap ───────────────────
        estimand     fixed 6-item ordinal scale, respondent-weighted target
        target       V4-TARGET-RESPONDENT-WEIGHTED-v1
        method       V5-C2-CRBB-ADJUSTED-RESP-v1  (raw base: V5-CMP-CRBB-PERCENTILE-RESP-v1)
        sample       n = 614 · clusters = 30 (sizes 12-29) · draws = 300
        validation   CONFIRMED_CLUSTER_COUNT
        scope        POLYTOMOUS_CLUSTERED_COMPLETE_RECORD_BB
        range        G = 30; confirmed range 22-320
      
        scale H      0.637   95% CI [0.596, 0.681]      status: OK
                     raw     [0.597, 0.675]  ->  width x 1.0789  (reflected about H)
      
        Interpretation: Bayesian-bootstrap-based confidence interval for
        repeated-sampling uncertainty; not a posterior credible interval or posterior
        probability.
      
        See: summary() · bm_item_h() · bm_plot_draws() · bm_diagnostics()

# historical two-stage fit prints as designed

    Code
      print(clu_two_stage_fit())
    Output
      ── Mokken scalability H · two-stage hierarchical Bayesian bootstrap (historical) 
        estimand     fixed 6-item ordinal scale, respondent-weighted target
        target       V4-TARGET-RESPONDENT-WEIGHTED-v1
        method       V4-P15-HBB-RESP-GN150-v1  (raw base: V4-PRI-TWO-STAGE-HBB-RESP-v1)
        sample       n = 614 · clusters = 30 (sizes 12-29) · draws = 300
        validation   CONFIRMED_CLUSTER_COUNT
        scope        POLYTOMOUS_CLUSTERED_HIERARCHICAL_BB
        range        G = 30; confirmed range 22-80
      
        scale H      0.637   95% CI [0.589, 0.668]      status: OK
                     raw     [0.574, 0.683]  ->  width x 0.7261
      
        Interpretation: Bayesian-bootstrap-based confidence interval for
        repeated-sampling uncertainty; not a posterior credible interval or posterior
        probability.
      
        See: summary() · bm_item_h() · bm_plot_draws() · bm_diagnostics()

---

    Code
      print(clu_two_stage_fit()$interval)
    Output
      ── Interval ────────────────────────────────────────────────────────────────────
        method       V4-P15-HBB-RESP-GN150-v1      status  OK
        level        95%                     draws   300 of 300 defined
        validation   CONFIRMED_CLUSTER_COUNT
        scope        POLYTOMOUS_CLUSTERED_HIERARCHICAL_BB
        range        G = 30; confirmed range 22-80
        point        0.637
        calibrated   [0.589, 0.668]        width x 0.7261  (gamma = -1.50)
        raw          [0.574, 0.683]        quantile type 8, clipping none
      
        Interpretation: Bayesian-bootstrap-based confidence interval for
        repeated-sampling uncertainty; not a posterior credible interval or posterior
        probability.
        See: bm_interval() · ?bayesmokken-intervals

# deterministic fit prints as designed

    Code
      print(mokken_h(srs, warn = FALSE))
    Output
      ── Mokken scalability H · deterministic ────────────────────────────────────────
        estimand     fixed 11-item binary scale, complete-scale respondents
        rows         337 of 337 complete (0 excluded, 0.0%)
        items        11
      
        scale H      0.130                      denominator: positive
        items        lowest  SRS1  0.053      highest  SRS10  0.229
      
        See: summary() · bm_item_h() · bm_plot_item_h() · bb_mokken_h() for an interval

# interval, data check, cluster info and diagnostics print

    Code
      print(bin_fit()$interval)
    Output
      ── Interval ────────────────────────────────────────────────────────────────────
        method       P7-FS-WIDTH-RAW-G025-v1      status  OK
        level        95%                     draws   300 of 300 defined
        point        0.130
        calibrated   [0.103, 0.159]        width x 1.0136  (gamma = 0.25)
        raw          [0.103, 0.159]        quantile type 8, clipping none
      
        Interpretation: Bayesian-bootstrap-based confidence interval for
        repeated-sampling uncertainty; not a posterior credible interval or posterior
        probability.
        See: bm_interval() · ?bayesmokken-intervals

---

    Code
      print(bm_check_data(srs))
    Output
      ── Data check ──────────────────────────────────────────────────────────────────
        rows / items 337 x 11
        item type    binary (detected)   values 0 1
        complete-scale 337 of 337 (0 excluded)
        constant items none
        reverse flags none
      
        Next: fit <- bb_mokken_h(
        data = srs,
        items = c("SRS1", "SRS2", "SRS3", "SRS4", "SRS5", "SRS6", "SRS7", "SRS8", "SRS9", "SRS10", "SRS11"),
        item_type = "binary",
        draws = 2000L,
        seed = 1L
      )

---

    Code
      print(bm_cluster_info(classroom_wb$class_id, data = classroom_wb[, paste0("wb",
        1:6)]))
    Output
      ── Cluster structure ───────────────────────────────────────────────────────────
        clusters     G = 30   N = 614
        sizes        12-29 (median 22, mean 20.5)
        size spread  ##****++++++==----::....  (12-29)
        target weight respondent 0.0195-0.0472   equal-cluster 0.0333 (all clusters)
        size/mean rho 0.89
      
        This Spearman correlation describes cluster size versus mean total score
        only. It does not measure agreement between the two target H values; fit both
        targets and compare their deterministic coefficients directly.
      
        See: bm_plot_clusters() · ?bayesmokken-estimands

---

    Code
      print(bm_diagnostics(bin_fit()))
    Output
      ── Diagnostics ─────────────────────────────────────────────────────────────────
        denominators pair 55 positive · item 11 positive · scale 1 positive
        regularity   smallest max-covariance 0.1001 (SRS10-SRS8)
        transport    not applicable (binary path)
        Monte Carlo  endpoint MCSE 0.0023 / 0.0012   prefix-half shift 0.0025
      
        Overall: Nothing numerically awkward found.
        This describes the arithmetic, not the quality of the scale. No verdict on
        the scale is offered.
      
        See: bm_regularity() · bm_mc_diagnostics() · bm_guardrails()

---

    Code
      print(bm_benchmark(bin_fit()))
    Output
       benchmark H         difference point_relation
       0.3       0.1302059 -0.1697941 below
       0.4       0.1302059 -0.2697941 below
       0.5       0.1302059 -0.3697941 below
       note
       H = 0.13020593; H - landmark 0.3 = -0.16979407 (below).
       H = 0.13020593; H - landmark 0.4 = -0.26979407 (below).
       H = 0.13020593; H - landmark 0.5 = -0.36979407 (below).
      
        Conventional descriptive landmarks from the Mokken tradition, not hypothesis
        tests or decision rules. Each row reports only the point estimate minus the
        landmark and a neutral arithmetic relation.

# field labels have an explicit separator at widths 40 and 80

    Code
      for (w in c(40L, 80L)) {
        withr::local_options(list(width = w))
        .bm_header(sprintf("Field separator, width %d", w))
        .bm_field("rows / items", "337 x 11")
        .bm_field("complete-scale", "614 of 614 complete")
        .bm_field("target weight", "respondent")
      }
    Output
      ── Field separator, width 40 ───────────
        rows / items 337 x 11
        complete-scale 614 of 614 complete
        target weight respondent
      ── Field separator, width 80 ───────────────────────────────────────────────────
        rows / items 337 x 11
        complete-scale 614 of 614 complete
        target weight respondent


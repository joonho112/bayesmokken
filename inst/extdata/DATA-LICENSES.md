# Data licences and attribution

The `bayesmokken` package is released under the MIT licence. The data it ships
carry their own terms, recorded here.

---

## `srs` — Scientific Reasoning Scale, Italian sample

**Licence: CC BY 4.0** (Creative Commons Attribution 4.0 International)
<https://creativecommons.org/licenses/by/4.0/legalcode>

Redistribution is permitted with attribution. Attribution is given here and in
the dataset's help page (`?srs`).

**Original data**

> Caliciuri, R., & Lanz, M. (2024). *Psychometric properties of the Scientific
> Reasoning Scale: Application to the Italian context.* OSF project jk9dp.
> <https://osf.io/jk9dp/>. doi:10.17605/OSF.IO/JK9DP

OSF project `jk9dp`, licence id `563c1cf88c5e4a3877f9e96a`, public, created
2024-02-14. The analysis file used is `3. CFA&IRT_SRS (n=337).sav`
(OSF guid `ynf8g`).

**Original instrument**

> Drummond, C., & Fischhoff, B. (2017). Development and validation of the
> Scientific Reasoning Scale. *Journal of Behavioral Decision Making*, 30(1),
> 26-38. doi:10.1002/bdm.1906

**What was changed**

The long-format table distributed through the Item Response Warehouse was
pivoted to one row per respondent. No item was dropped, no score was reversed,
no respondent was filtered. The 337 rows are the complete-case analysis file as
published. `data-raw/srs.R` performs the rebuild and verifies the source
against SHA-256
`72ca8785e01bcc78f26ff616f7a2249161d6072676cd65e0d0eb96ba84def6fc`.

**Sampling caveats that travel with the data**

- 337 respondents are a complete-case subset of 606 consent-eligible responses.
- Recruitment was by snowball sampling, a nonprobability design.
- No recruiter or network identifier accompanies the analysis file, so any
  clustering induced by snowball recruitment cannot be adjusted for. The
  independent-respondent assumption is a working assumption, not an
  established fact about the design.

---

## `classroom_wb` — simulated classroom well-being ratings

**Licence: MIT**, same as the package.

Fully synthetic. Produced by `data-raw/classroom_wb.R` using `bm_simulate()`
with seed 20260810. No real respondent contributed to it, and it is not a model
of any particular study.

---

## `SWMDK` — not bundled

The clustered polytomous application discussed in the documentation uses the
`SWMDK` dataset, which this package does **not** redistribute.

Two independent reasons:

1. The article's data manifest sets the redistribution policy to
   `no_vendoring_load_from_installed_mokken`.
2. `SWMDK` is distributed in the `mokken` package under GPL (>= 2), which is
   not compatible with redistribution inside an MIT-licensed package.

It is reached through `Suggests: mokken`:

```r
data("SWMDK", package = "mokken")
```

Underlying cohort: doi:10.17026/dans-zfp-egnq.

Where a vignette shows SWMDK results on a machine without `mokken`, it displays
a cached fit from `inst/extdata/precomputed/` and says so; it never presents
cached output as freshly computed.

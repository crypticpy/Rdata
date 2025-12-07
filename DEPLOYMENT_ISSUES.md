# Shinyapps.io Deployment Issues & Solutions

## Summary

Deploying a data-intensive R Shiny dashboard (diabetes risk intelligence) to shinyapps.io revealed critical challenges around memory limits, data compression, and deployment configuration. This document captures all issues encountered and their solutions for presentation purposes.

---

## Issue 1: Quarto Inspect Failure

**Error:**
```
Failed to run quarto inspect... is not a valid Quarto input document
```

**Cause:** rsconnect package detected Quarto files in the project and tried to run `quarto inspect` on the R file.

**Solution:** Create a separate clean `deploy/` directory containing only the Shiny app and its dependencies, excluding Quarto-related files.

---

## Issue 2: renv.lock Parsing Error

**Error:**
```
subscript out of bounds in parseRenvDependencies
```

**Cause:** rsconnect couldn't properly parse the project's renv.lock file.

**Solution:** Deploy from the clean `deploy/` directory without the renv.lock file. Let shinyapps.io auto-detect dependencies from the R code.

---

## Issue 3: Out of Memory (OOM) on Free Tier

**Error (from logs):**
```
Container event from container-12856236: oom (out of memory)
```

**Cause:** Free tier has 1GB RAM limit. The app's data files, when loaded into memory, exceeded this limit.

**Initial Bundle Size:** 720 MB
- `fairness_audit_results.rds` = 439 MB (file) → **6.4 GB in memory!**
- `diabetes_clean.rds` = 253,680 rows
- `random_forest_model.rds` = 108 MB
- `logistic_model.rds` = 38 MB
- Raw data files = 726 MB

**Key Finding:** The `fairness_objects` element within the fairness results contained full model objects that expanded from 439MB compressed to 6.4GB in memory. This was the primary memory culprit.

---

## Issue 4: Starter Plan Still OOM

**Error:** Same OOM error even after upgrading to Starter plan.

**Cause:** Starter plan is also 1GB RAM (same as free tier), not 4GB as initially assumed. The difference is in active hours, not memory.

**Plan Comparison:**
| Plan | RAM | Active Hours/Month | Price |
|------|-----|-------------------|-------|
| Free | 1 GB | 25 | $0 |
| Starter | 1 GB | 100 | $13/mo |
| Basic | 8 GB | 500 | $39/mo |
| Standard | 8 GB | 2000 | $99/mo |

---

## Solutions Implemented

### Solution A: Remove Fairness Model Objects

```r
# Original fairness file structure
fairness <- readRDS('output/fairness_audit_results.rds')
# fairness_objects: 6,432 MB in memory!

# Optimized: Keep only display data, remove model objects
fairness_optimized <- list(
  summary = fairness[['summary']],
  all_disparities = fairness[['all_disparities']],
  flagged_disparities = fairness[['flagged_disparities']],
  intersectional = fairness[['intersectional']],
  calibration = fairness[['calibration']],
  recommendations = fairness[['recommendations']],
  metadata = fairness[['metadata']]
)
# Result: ~0 MB in memory, 3.7 KB file size
```

### Solution B: Stratified Data Sampling

```r
# Sample 30% of data with stratification
set.seed(42)
df_lite <- df |>
  group_by(diabetes_status) |>
  slice_sample(prop = 0.30) |>
  ungroup()
# 253,680 rows → 76,102 rows
```

### Solution C: Remove Raw Data Files

Raw data files (726 MB) were excluded from deployment. The dashboard only needs processed `.rds` files.

### Solution D: Use XZ Compression

```r
saveRDS(object, file, compress = 'xz')
# XZ compression provides better ratios than gzip for R objects
```

---

## Memory Budget for 1GB Limit

| Component | File Size | Memory Size | Notes |
|-----------|-----------|-------------|-------|
| R base overhead | - | ~200 MB | R interpreter + loaded packages |
| Shiny overhead | - | ~100 MB | Shiny server process |
| diabetes_clean.rds (sampled) | 0.6 MB | ~11 MB | 76K rows |
| random_forest_model.rds | 108 MB | ~400 MB | Large tree ensemble |
| logistic_model.rds | 38 MB | ~60 MB | GLM coefficients |
| fairness_audit_results.rds | 3.7 KB | <1 MB | Summary only |
| causal_inference_results.rds | 21 MB | ~50 MB | ATE/E-values |
| Other outputs | ~30 MB | ~50 MB | Various results |
| **TOTAL** | | **~872 MB** | Under 1GB! |

---

## Deployment Command

```r
rsconnect::deployApp(
  appDir = 'deploy',
  appPrimaryDoc = 'diabetes_dashboard.R',
  appName = 'diabetes-dashboard',
  account = 'aphchris',
  forceUpdate = TRUE,
  launch.browser = FALSE
)
```

---

## Key Takeaways for Talk

1. **RDS Compression ≠ Memory Size**: A 439MB compressed file can expand to 6.4GB in RAM
2. **Model Objects Are Memory Hogs**: Random Forest and fairness model objects dominate memory
3. **Stratified Sampling Preserves Insights**: 30% sample maintains statistical validity
4. **Separate Deploy Directory**: Avoid renv/Quarto conflicts with a clean deployment folder
5. **Know Your Plan Limits**: Free and Starter both have 1GB RAM; need Basic ($39/mo) for 8GB
6. **Test Memory Locally First**: Use `object.size()` to audit all loaded objects before deploying

---

## Files Structure After Optimization

```
deploy/
├── diabetes_dashboard.R          # Main app
├── data/
│   └── processed/
│       └── diabetes_clean.rds    # 76K rows (30% sample)
├── output/
│   ├── fairness_audit_results.rds  # 3.7 KB (summary only)
│   ├── causal_inference_results.rds
│   ├── anomaly_discovery_results.rds
│   ├── data_fusion_results.rds
│   └── models/
│       ├── random_forest_model.rds
│       └── logistic_model.rds
└── www/
    └── [static assets]
```

---

## Live Dashboard

**URL:** https://aphchris.shinyapps.io/diabetes-dashboard/

---

*Document created: December 6, 2025*

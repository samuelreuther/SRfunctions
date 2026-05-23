# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

`SRfunctions` is an R utility package providing ~42 functions for data preparation and ML modeling workflows. All functions use the `SR_` prefix (author: Samuel Reuther).

## Common Commands

All development uses `devtools`. Run these in an R session:

```r
# Regenerate documentation (NAMESPACE, man/*.Rd) from roxygen2 comments
devtools::document()

# Run all tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-SR_join_check.R")

# Full package check (CRAN-style)
devtools::check()

# Install locally
devtools::install()

# Spell check (non-blocking)
spelling::spell_check_package()
```

## Architecture

### Function Categories

| Category | Pattern |
|---|---|
| Type checking | `SR_is_date`, `SR_is_number` |
| Feature engineering | `SR_feat_eng_numeric`, `SR_feat_eng_factors`, `SR_feat_eng_date`, `SR_feat_eng_rows` |
| NA imputation | `SR_replace_NA_median`, `SR_replace_NA_modus`, `SR_omit_non_regular_values` |
| Visualization | `SR_univariate_analysis`, `SR_plot_*`, `SR_NA_plot`, `SR_correlation_plot`, `SR_mosaicplot` |
| Utilities | `SR_backup_files`, `SR_join_check`, `SR_view`, `SR_memory_usage`, `SR_open_github`, `SR_clean_sandbox`, `SR_rm_from_env`, `SR_update_gitignore`, `SR_compare_excel_files` |

### Key Design Patterns

**Train/test separation:** The large feature engineering functions (`SR_feat_eng_numeric`, `SR_feat_eng_factors`, `SR_replace_NA_*`) accept a `use_other_df` parameter. When provided, statistics (medians, modes, encodings) are calculated from the other dataframe and applied to the current one — enabling proper leakage-free ML pipelines.

**NA flagging:** Imputation functions automatically create a companion `<colname>_NA` boolean column to flag which rows were imputed.

**Boolean feature flags:** Large functions use many boolean parameters (e.g., `add_log`, `add_sqrt`, `add_interactions`) to toggle optional transformations. Each flag adds derived columns rather than replacing the original.

**Documentation:** All functions use roxygen2 format. Run `devtools::document()` after editing any `@` tags — never edit `man/*.Rd` or `NAMESPACE` directly.

**`SR_clean_sandbox`:** Writes a cleanup timestamp to the hardcoded path `/home/sandbox/sandbox/Functions/.last_cleanup.rds` regardless of the `path` argument. The corresponding test uses `skip_if_not` to skip in environments where that path doesn't exist.

**`SR_update_gitignore`:** Diffs by active patterns only (ignores comment and blank lines), then appends missing entries with a dated `# added by SR_update_gitignore (...)` header. Never overwrites existing content.

**`SR_compare_excel_files`:** Uses `compare::compare(..., allowAll = FALSE)` — no implicit type coercions. Returns `invisible(NULL)` early if sheet structure differs; returns `invisible(TRUE)` only if all sheets are identical. Requires `writexl` in `Suggests` for tests.

### Testing

Tests live in `tests/testthat/test-SR_<function_name>.R`. They use `testthat` with straightforward `expect_equal()` assertions on small inline dataframes. The spell check test at `tests/spelling.R` runs with `error = FALSE` (non-blocking).

Note: `test-SR_replace_NA_modus.R` has a commented-out test with known issues for the cross-dataframe case.

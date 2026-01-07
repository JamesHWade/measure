# Agent Instructions

This project uses **bd** (beads) for issue tracking. Run `bd onboard` to get started.

## Quick Reference

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --status in_progress  # Claim work
bd close <id>         # Complete work
bd sync               # Sync with git (also commits beads changes)
```

## Landing the Plane (Session Completion)

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **PUSH TO REMOTE** - This is MANDATORY:
   ```bash
   git pull --rebase
   bd sync              # Commits any pending beads changes
   git push
   git status           # MUST show "up to date with origin"
   ```
   **Note:** If `git push` fails with "uncommitted changes detected", run `bd sync` again to commit beads database updates, then retry push.
5. **Clean up** - Clear stashes, prune remote branches
6. **Verify** - All changes committed AND pushed
7. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds

# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

`measure` is an R package that extends tidymodels with preprocessing steps for analytical measurement data (spectroscopy, chromatography, mass spectrometry). It provides a recipes-style interface for spectral preprocessing techniques.

## Development Commands

```r
# Generate documentation from roxygen2 comments
devtools::document()

# Run all tests
devtools::test()

# Run a single test file (filter matches test-*.R files)
devtools::test(filter = "smooth")  # runs test-smooth.R

# Full CRAN-like package check
devtools::check()

# Test coverage
covr::package_coverage()

# Rebuild README.md from README.Rmd
devtools::build_readme()

# Build pkgdown site
pkgdown::build_site()

# Bump dev version
usethis::use_version("dev")
```

## Architecture

### Recipe Step Pattern

Every preprocessing step follows the recipes framework with three required methods:

```r
# Constructor function
step_measure_*() → calls add_step()

# Three S3 methods required:
prep.step_measure_*()   # Learn parameters from training data
bake.step_measure_*()   # Apply transformation to new data
tidy.step_measure_*()   # Return step parameters as tibble
```

Step implementations are in `R/` with naming convention: `step_measure_{operation}.R`

### Internal Data Format

The package uses a custom S3 class hierarchy to store measurement data:

- **`measure_tbl`**: Single measurement (tibble with `location` and `value` columns)
- **`measure_list`**: Collection of measurements stored as a list column named `.measures`

Key helper functions in `R/helpers.R`:
- `measure_to_matrix()` / `matrix_to_measure()`: Convert between formats
- `find_measure_cols()`: Detect measure columns by class
- `check_for_measure()` / `check_has_measure()`: Validation utilities

### Tunable Parameters

Parameters for hyperparameter tuning with `tune::tune()` are defined in `R/parameters.R`. Each tunable step parameter has a corresponding `dials` function (e.g., `window_side()`, `baseline_lambda()`).

### Data Flow

1. **Input**: `step_measure_input_long()` or `step_measure_input_wide()` converts data to internal format
2. **Processing**: Chain of `step_measure_*()` functions apply transformations
3. **Output**: `step_measure_output_long()` or `step_measure_output_wide()` converts back

### File Organization

Steps are grouped by functionality:
- `input_*.R` / `output_*.R`: Data format conversion
- `baseline-*.R`: Baseline correction methods (ALS, polynomial, etc.)
- `smooth*.R`: Smoothing/filtering operations
- `align.R`: Spectrum alignment (DTW, PTW, COW)
- `scale.R` / `normalize.R`: Variable-wise scaling / sample-wise normalization
- `peak-operations.R`: Peak detection and integration
- `qc.R`: Quality control steps
- `axis-validation.R`: Data validation utilities
- `data-organization.R`: Column detection and role assignment

### Analytical Validation Subsystem

The package includes a complete analytical method validation framework:

**Calibration & Quantitation** (`calibration-*.R`, `lod-loq.R`):
- `measure_calibration_fit()` creates calibration curves with weighted regression
- `measure_lod()` / `measure_loq()` calculate detection/quantitation limits

**Precision & Accuracy** (`precision.R`, `accuracy.R`):
- `measure_repeatability()`, `measure_intermediate_precision()`, `measure_reproducibility()`
- `measure_accuracy()`, `measure_linearity()`

**Uncertainty** (`uncertainty.R`):
- `measure_uncertainty_budget()` builds ISO GUM-compliant uncertainty budgets
- `uncertainty_component()`, `uncertainty_type_a()`, `uncertainty_type_b_*()`

**Criteria & Assessment** (`criteria.R`):
- `criterion()` and `measure_criteria()` define acceptance criteria
- `measure_assess()` evaluates results against criteria
- Preset criteria: `criteria_ich_q2()`, `criteria_bioanalytical()`, etc.

**Validation Reports** (`validation-report.R`):
- `measure_validation_report()` collects validation results
- `render_validation_report()` generates HTML/PDF/Word using Quarto templates
- Templates in `inst/templates/`: ICH Q2(R2) and USP <1225> formats

## Testing

Tests use testthat edition 3 with snapshots in `tests/testthat/_snaps/`. Helper data is in `tests/testthat/helpers-*.R`.

## Python Integration

Some baseline methods use `pybaselines` via reticulate. The Python dependency is configured in DESCRIPTION under `Config/reticulate`.

## Code Style

- Tidyverse style guide
- Roxygen2 with Markdown syntax for documentation
- All exported functions need `@export` tag
- S3 methods for validation objects should include `print()`, `summary()`, and `tidy()` methods

## Linting & Formatting

The project uses two complementary code quality tools:

```bash
# Linter - catches logic/efficiency issues
jarl check .        # Check for issues
jarl check . --fix  # Auto-fix issues

# Formatter - enforces consistent code style
air format .        # Format all R files
```

**jarl** catches:
- `vector_logic`: Using `|` instead of `||` in `if()` statements
- Other potential bugs and inefficiencies

**air** enforces:
- 2-space indentation in function signatures
- One argument per line for long function calls
- Closing parenthesis on own line for multi-line constructs
- Consistent line length limits

Run both before committing to pass CI checks.

## PR Workflow

Before creating a pull request, run the following checks:

```bash
# 1. Format all R files (including tests)
air format .

# 2. Check for linting issues and auto-fix
jarl check . --fix

# 3. Generate/update documentation
R -e 'devtools::document()'

# 4. Run full package check (should pass with 0 errors, 0 warnings, 0 notes)
R -e 'devtools::check()'

# 5. Build pkgdown site to verify documentation renders correctly
R -e 'pkgdown::build_site()'
```

When adding new exported functions, ensure they are included in `_pkgdown.yml` under the appropriate reference section.

After the PR is merged:

```r
# Clean up: switch to main, pull changes, delete local/remote feature branch
usethis::pr_finish()
```

## Technique Pack Registry

The package includes a registry system for technique packs (external packages that extend measure):

- **`R/registry.R`**: Registration and discovery infrastructure
- **`register_measure_pack()`**: For extension packages to register themselves
- **`register_measure_step()`**: For registering individual steps
- **`measure_packs()`**: List all registered technique packs
- **`measure_steps()`**: List all registered steps (core + extensions)

Steps marked as superseded (e.g., SEC/GPC steps) point users to the appropriate technique pack (e.g., `measure.sec`).

## Dependencies

- Packages in `Suggests` must be available on CRAN or Bioconductor
- Technique packs like `measure.sec` that aren't published yet should NOT be in Suggests
- Use documentation to point users to unpublished companion packages

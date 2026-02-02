# add_response()

**File:** `R/pipeline-add-response.R`
**Exported:** yes
**Added:** 2026-02-01

## Purpose

Joins lab-measured response data to a `horizons_data` object by sample ID.
This is the divergence point in the pipeline — the other branch is
`use_library()`, which declares intent to use OSSL predictions instead of
user lab data. Both paths produce `role = "response"` columns that feed into
`configure()` and `evaluate()`.

The function is deliberately strict: exact matching only, no magic. When
matches are poor, it detects common patterns (case mismatch, whitespace) and
suggests explicit fixes. The philosophy is diagnostics over magic — respect
the user's ability to wrangle tibbles.

## Mechanics

1. **Print header** — `├─ Adding response data...` prints before anything
   else. This means the header appears even if validation fails immediately.
   Intentional for visual consistency across calls.

2. **Input validation (1.1–1.9)** — nine sequential checks, each with a
   tree-nested error via `abort_nested()`. Order matters: class check first
   (1.1), then source type (1.2), then file read if path (1.3), then
   column-level checks (1.4–1.6), then join key parsing and validation
   (1.7), then duplicate detection on both sides (1.8–1.9). The file read
   happens at 1.3 so that subsequent checks (variable exists, is numeric)
   operate on the actual data regardless of whether source was a path or
   tibble.

3. **Prepare join** — selects only the join key + variable columns from
   source. Everything else is dropped before the join. This prevents surprise
   columns appearing in the analysis tibble.

4. **Diagnose match quality** — compares join key values before executing the
   join. Three tiers: 0 matches aborts with pattern detection, <50% warns
   with pattern detection, ≥50% continues normally. The pattern detection
   checks case (`tolower`) and whitespace (`trimws`) independently — both
   can fire on the same input.

5. **Execute join** — `dplyr::left_join()` with a named `by` vector.
   Unmatched horizons samples get NA response values. Unmatched source rows
   are silently dropped (left join semantics).

6. **Update object** — three mutations: append rows to role_map, recompute
   `n_responses`, append a provenance record to the `add_response` list.

7. **Progress output** — source label, match count, variables added. If
   there are unmatched samples (but ≥50% matched), the count is shown
   inline with an arrow to NA.

## Object mutations

| Field | Before | After | Notes |
|-------|--------|-------|-------|
| `data$analysis` | n columns | n + len(variable) columns | New response columns via left join. Unmatched rows get NA. |
| `data$role_map` | m rows | m + len(variable) rows | New rows with `role = "response"` |
| `data$n_responses` | 0 or previous count | Recomputed from role_map | `sum(role_map$role == "response")` |
| `provenance$add_response` | NULL or list | List with appended entry | Each call appends one record: source, variables, by, n_matched, n_unmatched, applied_at |

Fields read but not modified: `data$analysis[[by_horizons]]` (for duplicate
check and match diagnostics), `data$role_map$variable` (for existing-variable
check).

## Error catalog

| Trigger | Message (grep-able) | Class | Fatal? |
|---------|-------------------|-------|--------|
| `x` not horizons_data | "must be a horizons_data object" | `horizons_input_error` | yes |
| source not df or string | "must be a data frame or file path" | `horizons_input_error` | yes |
| path doesn't exist | "Source file does not exist" | `horizons_input_error` | yes |
| variable not in source | "Variables not found in source data" | `horizons_input_error` | yes |
| variable not numeric | "Response variables must be numeric" | `horizons_input_error` | yes |
| variable already in analysis | "already exist in horizons data" | `horizons_input_error` | yes |
| multi-column by | "Multi-column joins not supported" | `horizons_input_error` | yes |
| partially-named by | "partially named" | `horizons_input_error` | yes |
| by column missing (horizons) | "Join column not found in horizons" | `horizons_input_error` | yes |
| by column missing (source) | "Join column not found in source" | `horizons_input_error` | yes |
| duplicate keys in source | "Duplicate values in source join key" | `horizons_input_error` | yes |
| duplicate keys in horizons | "Duplicate sample IDs found" | `horizons_input_error` | yes |
| 0 matches | "No matching samples" | `horizons_input_error` | yes |
| <50% matched | (n_matched)/(n_horizons) "samples matched" | warning (not error) | no |
| ≥50% with unmatched | "samples have no matching response data" | warning (not error) | no |

## Known limitations

1. **No `remove_response()` yet** — error messages reference it but the
   function doesn't exist. Users who need to replace a response variable
   must manually modify the object.

2. **CSV-only path convenience** — the string source path only supports CSV
   via `readr::read_csv()`. Excel, TSV, and other formats require the user
   to read the file and pass the tibble.

3. **Generic "tibble" provenance label** — when source is a tibble,
   provenance records `source = "tibble"` with no further identification.
   Multiple tibble-sourced calls are indistinguishable by source alone
   (though `variables` field helps).

4. **Limited pattern detection** — diagnostics detect case mismatch and
   whitespace but not prefix/suffix patterns, numeric padding, or Unicode
   normalization. These are real issues in soil science datasets.

5. **Progress output is always-on** — no verbose flag. When a cross-cutting
   verbose system is added, every `cat()` call here needs gating. Consider
   wrapping in a `pipeline_msg()` helper when that happens.

6. **Header prints before validation** — the `├─ Adding response data...`
   line prints even if the very first check fails. Acceptable for visual
   consistency, but means the header appears on immediate failures.

7. **50% match threshold is hardcoded** — the warn-vs-continue boundary is
   `match_rate < 0.5`. Not configurable. Chosen as a pragmatic midpoint
   between "obviously wrong join key" and "normal partial coverage."

## Patterns

- **`abort_nested()` helper** — internal function for tree-nested errors
  under the progress header. Uses `cat()` + `cli::col_red()` for display,
  `rlang::abort()` for testability. Future verbs should copy this pattern
  (or extract it to a shared utility).

- **Progress header as Step 0** — every pipeline verb prints a header line
  before doing work. Errors nest under it. This is the visual contract with
  the user.

- **Nested provenance list** — `provenance$add_response` is a list of
  records, one per call. Follows the pattern from `parse_ids()`. Future
  verbs that can be called multiple times should do the same.

- **Role map append, then recompute** — don't set `n_responses` directly.
  Append to role_map, then derive the count. Single source of truth.

- **Validation order** — class check → type dispatch → file read → column
  checks → key parsing → duplicate detection. This order lets later checks
  assume earlier ones passed.

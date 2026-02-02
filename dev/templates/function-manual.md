# Function manual template

Every exported pipeline function gets a developer-facing manual page in
`dev/manuals/`. These are developer-facing reference docs — not user docs
(that's roxygen). The audience is the developer working on this package who
needs to understand or modify the function.

Filename convention: `{function-name}.md` (e.g., `add-response.md`).

---

## Template

Copy everything below the line and fill it in.

---

# {function_name}()

**File:** `R/{filename}.R`
**Exported:** yes/no
**Added:** {date}

## Purpose

One paragraph. What does this function do, where does it sit in the pipeline,
and why does it exist as a separate verb? Be more candid than the roxygen —
expose internals.

## Mechanics

Numbered to match the `## Step N` headers in the source code. Each step gets
a sentence on *why*, not just *what*. Link to line numbers where helpful.

1. **Step name** — why this step exists and what it does
2. **Step name** — ...

## Object mutations

Every field this function reads, creates, or modifies on the `horizons_data`
object. This is the contract the verb makes with the object.

| Field | Before | After | Notes |
|-------|--------|-------|-------|
| `data$analysis` | ... | ... | ... |
| `data$role_map` | ... | ... | ... |

## Error catalog

Every error this function can throw. Lookup table for debugging.

| Trigger | Message (grep-able) | Class | Fatal? |
|---------|-------------------|-------|--------|
| `x` wrong type | "must be a horizons_data object" | `horizons_input_error` | yes |

## Known limitations

Things that don't work, things that are deferred, things that could bite you.
Be honest. Number them for easy reference.

1. **{Short name}** — description of the limitation and why it exists

## Patterns

Conventions this function establishes or follows that other functions should
know about. Reference these when building the next verb.

- **Pattern name** — what it is and where to look


# ---------------------------------------------------------------------------
# Parse R files and build a function audit table
# ---------------------------------------------------------------------------

library(tidyverse)
library(fs)

## -----------------------------------------------------------------------------
## Step 1: Pull all the R files from the package.
## -----------------------------------------------------------------------------

dir_ls(path = c("R"),
       glob = "*.R",
       recurse = TRUE) -> r_files

## -----------------------------------------------------------------------------
## Step 2: Create an empty audit table to fill in.
## -----------------------------------------------------------------------------

tibble(Function_Name  = character(),
       File_Name      = character(),
       Purpose        = character(),
       Depends_On     = list(),
       Contributes_To = list(),
       TODOs          = character()) -> audit_table


## -----------------------------------------------------------------------------
## Helper function: Extract function names from files
## -----------------------------------------------------------------------------

extract_function_info <- function(file_path) {

  lines <- readLines(file_path, warn = FALSE)
  fun_indices <- grep("^[\\s]*[a-zA-Z0-9_.]+[ ]*<- *function\\(", lines)


  map_df(fun_indices,
         function(idx) {

    fun_line <- lines[idx]
    fun_name <- str_extract(fun_line, "^[\\s]*[a-zA-Z0-9_.]+") %>% str_trim()
    comments <- character()
    i        <- idx - 1

    while (i > 0 && grepl("^\\s*#", lines[i])) {

      comments <- c(lines[i], comments)
      i <- i - 1

    }

    comments_clean <- str_remove_all(comments, "^\\s*#\\s*") %>% paste(collapse = " ")

    tibble(Function_Name  = fun_name,
           File_Name      = file_path,
           Purpose        = comments_clean,
           Depends_On     = list(character()),
           Contributes_To = list(character()),
           TODOs          = "")
  })
}

## -----------------------------------------------------------------------------
## Step 3: Apply helper function
## -----------------------------------------------------------------------------

audit_entries <- map_dfr(r_files,
                         extract_function_info)

audit_table   <- bind_rows(audit_table, audit_entries)

## -----------------------------------------------------------------------------
## Step 4: Write the output to a Markdown file
##------------------------------------------------------------------------------

md_lines <- c("# Function Audit Table",
              "",
              "| Function Name | File Name | Purpose | Depends On | Contributes To | TODOs |",
              "|---------------|-----------|---------|------------|----------------|-------|")

for (i in seq_len(nrow(audit_table))) {

  row <- audit_table[i, ]

  md_lines <- c(md_lines, glue::glue(
    "| `{row$Function_Name}` | `{basename(row$File_Name)}` | {row$Purpose} | {paste(row$Depends_On[[1]], collapse = ', ')} | {paste(row$Contributes_To[[1]], collapse = ', ')} | {row$TODOs} |"
  ))

}

# Write to Markdown file
writeLines(md_lines, "dev/function_audit_initial.md")

## ===========================================================================
## build-board.R — generate the pipeline board from the package and content.yml
## ===========================================================================
##
## Reads vignettes/board/content.yml (hand-authored), fills every argument's
## name, default and parameter text from the INSTALLED package's formals() and
## the source man/ pages, and writes:
##
##   vignettes/board/board-data.json   the merged data the template and the test read
##   vignettes/board/board.svg         the static diagram (lanes, nodes, divergences)
##   vignettes/board/board.html        the fragment the vignette includes (SVG + cards)
##   vignettes/board/board-standalone.html   the same, as a full page to open directly
##
## Run from package/, after R CMD INSTALL:
##   Rscript dev/board/build-board.R
##
## Refuses to run if content.yml carries a `default` or `text` field on any
## argument (those are generated, never typed), if a verb's formals are not
## all documented in content.yml, or if the Rd has no text for a formal.
##
## Spec: ../dev/specs/v1-refactor/pipeline-board.md (the project's dev tree, beside package/)
## ===========================================================================

suppressPackageStartupMessages({
  library(yaml)
  library(jsonlite)
})

file_arg <- grep("^--file=", commandArgs(), value = TRUE)
root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(sub("^--file=", "", file_arg[1])), "..", ".."))
} else {
  normalizePath(getwd())
}
board_dir <- file.path(root, "vignettes", "board")
stopifnot(file.exists(file.path(root, "DESCRIPTION")), dir.exists(board_dir))

if (!requireNamespace("horizons", quietly = TRUE)) stop("horizons must be installed; run R CMD INSTALL first")
ns <- asNamespace("horizons")

## ---------------------------------------------------------------------------
## Rd helpers
## ---------------------------------------------------------------------------

## Flatten an Rd fragment to plain text with backticks for code.
rd_text <- function(x) {
  if (is.character(x) && is.null(attr(x, "Rd_tag"))) return(paste(x, collapse = ""))
  tag <- attr(x, "Rd_tag")
  if (is.character(x)) return(paste(as.character(x), collapse = ""))
  inner <- paste(vapply(x, rd_text, character(1)), collapse = "")
  if (is.null(tag)) return(inner)
  switch(tag,
    "\\code"   = paste0("`", inner, "`"),
    "\\dQuote" = paste0("“", inner, "”"),
    "\\sQuote" = paste0("‘", inner, "’"),
    "\\item"   = paste0(" - ", inner),
    "\\dots"   = "...",
    "\\R"      = "R",
    inner)
}

squish <- function(s) trimws(gsub("[[:space:]]+", " ", s))

## name -> text for every \item in the Rd's \arguments block
rd_arguments <- function(rd_name) {
  path <- file.path(root, "man", paste0(rd_name, ".Rd"))
  if (!file.exists(path)) stop("no Rd page for ", rd_name, " at ", path)
  rd   <- tools::parse_Rd(path)
  tags <- vapply(rd, function(e) attr(e, "Rd_tag") %||% "", character(1))
  args <- rd[tags == "\\arguments"]
  if (!length(args)) stop("Rd page ", rd_name, " has no \\arguments block")
  items <- Filter(function(e) identical(attr(e, "Rd_tag"), "\\item"), args[[1]])
  out <- list()
  for (it in items) {
    nm <- squish(rd_text(it[[1]]))
    tx <- squish(rd_text(it[[2]]))
    for (n in strsplit(nm, ",[[:space:]]*")[[1]]) out[[n]] <- tx
  }
  out
}

`%||%` <- function(a, b) if (is.null(a)) b else a

## ---------------------------------------------------------------------------
## Merge content with formals and Rd
## ---------------------------------------------------------------------------

content <- yaml::read_yaml(file.path(board_dir, "content.yml"))

build_verb <- function(v) {
  fn <- get(v$verb, envir = ns)
  if (!is.function(fn)) stop(v$verb, " is not a function in horizons")
  fm <- formals(fn)
  rd <- rd_arguments(v$rd %||% v$verb)

  ## Hand args: typed fields only -------------------------------------------
  hand <- v$args %||% list()
  hand_names <- vapply(hand, function(a) a$name, character(1))
  for (a in hand) {
    if (!is.null(a$default) || !is.null(a$text)) {
      stop("content.yml: argument ", v$verb, "(", a$name, ") carries a `default` or `text` field; those are generated")
    }
  }
  extra <- setdiff(hand_names, names(fm))
  if (length(extra)) stop("content.yml: ", v$verb, " documents arguments that do not exist: ", paste(extra, collapse = ", "))
  missing_touch <- setdiff(setdiff(names(fm), "..."), hand_names)
  if (length(missing_touch)) stop("content.yml: ", v$verb, " has no `touch` for: ", paste(missing_touch, collapse = ", "))

  ## Generated args, in formals order ----------------------------------------
  args <- lapply(names(fm), function(n) {
    h <- hand[[match(n, hand_names)]]
    ## A formal with no default is the empty symbol; binding it to a name and
    ## then touching that name raises "argument is missing", so test it inline.
    no_default <- is.symbol(fm[[n]]) && !nzchar(as.character(fm[[n]]))
    default <- if (no_default) NA_character_ else paste(deparse(fm[[n]]), collapse = " ")
    text <- rd[[n]]
    if (is.null(text) && n != "...") stop("Rd page ", v$rd %||% v$verb, " has no text for argument ", n)
    list(name    = n,
         default = default,
         text    = text %||% "Further arguments.",
         touch   = h$touch %||% NA_character_,
         source  = h$source %||% NA_character_)
  })

  sig <- paste(deparse(args(fn)), collapse = " ")
  sig <- squish(sub("NULL\\s*$", "", sig))
  sig <- sub("^function ?", paste0(v$verb, " "), sig)

  list(verb         = v$verb,
       rd           = v$rd %||% v$verb,
       display      = v$display %||% v$verb,   # what the diagram and card show; S3 methods display as their generic
       label        = v$label,
       lanes        = as.list(v$lanes),     # a list so a single lane still serializes as an array
       order        = v$order,
       optional     = isTRUE(v$optional),
       class_in     = v$class_in,
       class_out    = v$class_out,
       signature    = sig,
       summary      = squish(v$summary),
       args         = args,
       mechanics    = v$mechanics %||% list(),
       records      = v$records %||% list(),
       refuses      = v$refuses %||% list(),
       evidence     = v$evidence %||% list(),
       issues       = v$issues %||% list(),
       notes        = v$notes %||% list(),
       design_ahead = v$design_ahead %||% list())
}

verbs <- lapply(content$verbs, build_verb)
verbs <- verbs[order(vapply(verbs, function(v) v$order, numeric(1)))]

data <- list(
  generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  horizons     = as.character(utils::packageVersion("horizons")),
  lanes        = content$lanes,
  divergences  = content$divergences,
  verbs        = verbs
)

json_path <- file.path(board_dir, "board-data.json")
writeLines(jsonlite::toJSON(data, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"), json_path)
cat("wrote", json_path, "\n")

## ---------------------------------------------------------------------------
## SVG: lanes, nodes, connectors
## ---------------------------------------------------------------------------

esc <- function(s) {
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  gsub(">", "&gt;", s, fixed = TRUE)
}

pal <- list(paper = "#f1f0ea", panel = "#e0ddcf", ink = "#2d232e", muted = "#534b52",
            strong = "#474448", plum = "#7d4f66", ochre = "#b07d4e",
            plum_light = "#c3a6b8", ochre_light = "#d9bd9c")
lane_colour <- function(id) if (id == "library") pal$plum else pal$ochre
lane_light  <- function(id) if (id == "library") pal$plum_light else pal$ochre_light

orders  <- sort(unique(vapply(verbs, function(v) v$order, numeric(1))))
col_w   <- 190; gap <- 30; left <- 250; top <- 30
lane_h  <- 110; lane_gap <- 24
per_row <- 6L                                   # columns per row; the pipeline wraps
n_rows  <- ceiling(length(orders) / per_row)
row_h   <- 2 * lane_h + lane_gap + 70           # both lanes plus the divergence label strip
row_of  <- function(o) (match(o, orders) - 1L) %/% per_row
col_in  <- function(o) (match(o, orders) - 1L) %%  per_row
lane_y  <- function(id, row) top + row * row_h + (if (id == "library") 0 else lane_h + lane_gap)
width   <- left + min(length(orders), per_row) * (col_w + gap) + 20
height  <- top + n_rows * row_h

col_x <- function(o) left + col_in(o) * (col_w + gap)

## wrap a string into lines of at most `n` characters, at spaces
wrap_lines <- function(s, n, max_lines = 2) {
  words <- strsplit(s, " ")[[1]]; lines <- character(); cur <- ""
  for (w in words) {
    if (nchar(paste(cur, w)) > n && nzchar(cur)) { lines <- c(lines, trimws(cur)); cur <- w } else cur <- paste(cur, w)
  }
  lines <- c(lines, trimws(cur))
  if (length(lines) > max_lines) { lines <- lines[seq_len(max_lines)]; lines[max_lines] <- paste0(strtrim(lines[max_lines], n - 1), "…") }
  lines
}

svg <- character()
svg <- c(svg, sprintf('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 %d %d" width="%d" height="%d" font-family="ui-sans-serif, system-ui, -apple-system, Helvetica, Arial, sans-serif" font-size="13">',
                      width, height, width, height))
svg <- c(svg, sprintf('<rect width="%d" height="%d" fill="%s"/>', width, height, pal$paper))

## Lane bands, one pair per row ---------------------------------------------
for (row in seq_len(n_rows) - 1L) {
  n_cols_row <- if (row < n_rows - 1L) per_row else length(orders) - row * per_row
  band_w <- n_cols_row * (col_w + gap) - gap + 20 + 210
  for (ln in content$lanes) {
    y <- lane_y(ln$id, row)
    svg <- c(svg, sprintf('<g class="lane lane-%s"><rect x="%d" y="%d" width="%d" height="%d" rx="10" fill="%s" opacity="0.35"/>',
                          ln$id, left - 230, y, band_w, lane_h, lane_light(ln$id)))
    svg <- c(svg, sprintf('<text x="%d" y="%d" fill="%s" font-weight="600" font-size="15">%s%s</text>',
                          left - 216, y + 26, lane_colour(ln$id), esc(ln$title), if (row > 0) " (continued)" else ""))
    if (row == 0) {
      sub_lines <- wrap_lines(ln$subtitle, 30, 3)
      for (i in seq_along(sub_lines)) {
        svg <- c(svg, sprintf('<text x="%d" y="%d" fill="%s" font-size="11">%s</text>',
                              left - 216, y + 30 + 15 * i, pal$muted, esc(sub_lines[i])))
      }
    }
    svg <- c(svg, '</g>')
  }
}

## Nodes --------------------------------------------------------------------
node_box <- function(v) {
  x <- col_x(v$order); row <- row_of(v$order)
  both <- length(v$lanes) == 2
  y <- if (both) lane_y("library", row) + 8 else lane_y(v$lanes[[1]], row) + 8
  h <- if (both) 2 * lane_h + lane_gap - 16 else lane_h - 16
  list(x = x, y = y, w = col_w, h = h, both = both)
}

for (v in verbs) {
  b <- node_box(v)
  stroke <- if (b$both) pal$strong else lane_colour(v$lanes[[1]])
  dash   <- if (v$optional) ' stroke-dasharray="6 4"' else ""
  svg <- c(svg, sprintf('<g class="node%s" data-verb="%s" style="cursor:pointer">', if (b$both) " node-both" else paste0(" node-", v$lanes[[1]]), v$verb))
  svg <- c(svg, sprintf('<rect x="%d" y="%d" width="%d" height="%d" rx="8" fill="#ffffff" stroke="%s" stroke-width="1.5"%s/>',
                        b$x, b$y, b$w, b$h, stroke, dash))
  ty <- b$y + (if (b$both) b$h / 2 - 12 else 24)
  svg <- c(svg, sprintf('<text x="%d" y="%d" fill="%s" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-weight="600" font-size="13">%s()</text>',
                        b$x + 12, ty, pal$ink, esc(v$display)))
  lines <- wrap_lines(v$label, 27, 2)
  for (i in seq_along(lines)) {
    svg <- c(svg, sprintf('<text x="%d" y="%d" fill="%s" font-size="11">%s</text>', b$x + 12, ty + 16 * i, pal$muted, esc(lines[i])))
  }
  cls <- wrap_lines(paste(v$class_in, "→", v$class_out), 34, 1)
  svg <- c(svg, sprintf('<text x="%d" y="%d" fill="%s" font-size="10">%s</text>',
                        b$x + 12, b$y + b$h - 9, pal$strong, esc(cls)))
  svg <- c(svg, '</g>')
}

## Connectors: between consecutive orders, per lane ---------------------------
## Within a row, a straight arrow. Across a row break, a short stub leaving the
## last node and a short stub entering the first node of the next row.
lane_mid <- function(id, row) lane_y(id, row) + lane_h / 2
arrow <- function(x1, x2, y, ln) c(
  sprintf('<line x1="%d" y1="%d" x2="%d" y2="%d" stroke="%s" stroke-width="1.5" class="edge edge-%s"/>', x1, y, x2, y, lane_colour(ln), ln),
  sprintf('<polygon points="%d,%d %d,%d %d,%d" fill="%s" class="edge edge-%s"/>', x2, y, x2 - 7, y - 4, x2 - 7, y + 4, lane_colour(ln), ln))
for (i in seq_len(length(orders) - 1)) {
  o1 <- orders[i]; o2 <- orders[i + 1]
  from <- Filter(function(v) v$order == o1, verbs); to <- Filter(function(v) v$order == o2, verbs)
  for (ln in c("library", "local")) {
    f <- Filter(function(v) ln %in% v$lanes, from); t <- Filter(function(v) ln %in% v$lanes, to)
    if (!length(f) || !length(t)) next
    if (row_of(o1) == row_of(o2)) {
      svg <- c(svg, arrow(col_x(o1) + col_w, col_x(o2), lane_mid(ln, row_of(o1)), ln))
    } else {
      y1 <- lane_mid(ln, row_of(o1)); y2 <- lane_mid(ln, row_of(o2))
      svg <- c(svg, arrow(col_x(o1) + col_w, col_x(o1) + col_w + gap - 4, y1, ln))
      svg <- c(svg, arrow(left - 4 - gap + 4, left, y2, ln))
    }
  }
}

## Divergence labels under the column that follows `after` -------------------
for (d in content$divergences) {
  after <- Filter(function(v) v$verb == d$after, verbs)
  if (!length(after)) next
  o <- after[[1]]$order
  nxt <- orders[match(o, orders) + 1]
  if (is.na(nxt)) next
  x <- if (row_of(nxt) == row_of(o)) col_x(nxt) - gap / 2 else col_x(nxt) + col_w / 2
  y <- lane_y("local", row_of(nxt)) + lane_h + 24
  svg <- c(svg, sprintf('<g class="divergence" data-divergence="%s" style="cursor:pointer"><text x="%d" y="%d" fill="%s" font-size="11" font-style="italic" text-anchor="middle">▲ %s</text></g>',
                        d$id, as.integer(x), y, pal$strong, esc(d$title)))
}

svg <- c(svg, '</svg>')
svg_path <- file.path(board_dir, "board.svg")
writeLines(svg, svg_path)
cat("wrote", svg_path, "\n")

## ---------------------------------------------------------------------------
## HTML: fragment and standalone
## ---------------------------------------------------------------------------

template <- readLines(file.path(board_dir, "template.html"), warn = FALSE)
css      <- readLines(file.path(board_dir, "board.css"), warn = FALSE)
js       <- readLines(file.path(board_dir, "board.js"), warn = FALSE)
json_txt <- readLines(json_path, warn = FALSE)

fill <- function(tpl) {
  tpl <- sub("{{CSS}}",  paste(css, collapse = "\n"), tpl, fixed = TRUE)
  tpl <- sub("{{SVG}}",  paste(svg, collapse = "\n"), tpl, fixed = TRUE)
  tpl <- sub("{{DATA}}", paste(json_txt, collapse = "\n"), tpl, fixed = TRUE)
  tpl <- sub("{{JS}}",   paste(js, collapse = "\n"), tpl, fixed = TRUE)
  tpl
}

fragment <- fill(paste(template, collapse = "\n"))
frag_path <- file.path(board_dir, "board.html")
writeLines(fragment, frag_path)
cat("wrote", frag_path, "\n")

standalone <- c('<!DOCTYPE html>', '<html lang="en"><head><meta charset="utf-8">',
                '<meta name="viewport" content="width=device-width, initial-scale=1">',
                '<title>horizons — the pipeline board</title>',
                sprintf('<style>html,body{margin:0;background:%s;color:%s;font-family:ui-sans-serif,system-ui,-apple-system,Helvetica,Arial,sans-serif}</style>', pal$paper, pal$ink),
                '</head><body>', fragment, '</body></html>')
sa_path <- file.path(board_dir, "board-standalone.html")
writeLines(standalone, sa_path)
cat("wrote", sa_path, "\n")

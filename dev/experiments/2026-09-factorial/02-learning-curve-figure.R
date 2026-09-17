## ===========================================================================
## 02 — Learning-curve figure (reads the rows 01-learning-curve.R wrote)
## ===========================================================================
##
## Run (from package/):
##   Rscript dev/experiments/2026-09-factorial/02-learning-curve-figure.R
##
## Reads every row-*.csv under results/learning-curve/ (so a partial run
## plots too) and writes learning-curve.png beside them. Two panels sharing
## the x axis (log2 training rows): fixed-test RPD and fixed-test RMSE.
## One hue per config, fixed order; direct labels at the last point.

suppressPackageStartupMessages({ library(dplyr); library(ggplot2) })

file_arg <- grep("^--file=", commandArgs(), value = TRUE)
this_dir <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]))) else getwd()
lc_dir   <- file.path(this_dir, "results", "learning-curve")

rows <- list.files(lc_dir, "^row-.*\\.csv$", full.names = TRUE) |>
  lapply(readr::read_csv, show_col_types = FALSE) |>
  bind_rows() |>
  arrange(config, n_train)

stopifnot(nrow(rows) > 0)

long <- rows |>
  select(config, n_train, frac_train, test_rpd, test_rmse, cv_rpd) |>
  tidyr::pivot_longer(c(test_rpd, test_rmse, cv_rpd), names_to = "metric") |>
  mutate(metric = factor(metric,
                         levels = c("test_rpd", "cv_rpd", "test_rmse"),
                         labels = c("RPD, fixed test (n = 5,239)",
                                    "RPD, 5-fold CV",
                                    "RMSE (% clay), fixed test")))

## Fixed hue order: plsr first, cubist second. Validated 2026-09-16.
cfg_levels <- c("plsr_snv_deriv1_pca", "cubist_snv_pca")
cfg_labels <- c("PLSR, SNV + 1st deriv, PCA", "Cubist, SNV, PCA")
palette    <- c("#2f6db5", "#c7581b")
names(palette) <- cfg_levels
long$config <- factor(long$config, levels = cfg_levels)

ends <- long |> group_by(config, metric) |> slice_max(n_train, n = 1) |> ungroup()

p <- ggplot(long, aes(n_train, value, colour = config, group = config)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2.2) +
  geom_text(data = ends, aes(label = cfg_labels[as.integer(config)]),
            hjust = 1.1, vjust = 1.8, size = 3, show.legend = FALSE) +
  facet_wrap(~ metric, ncol = 1, scales = "free_y") +
  scale_y_continuous(expand = expansion(mult = c(0.12, 0.12))) +
  scale_x_continuous(trans = "log2",
                     breaks = sort(unique(long$n_train)),
                     labels = function(x) format(x, big.mark = ",")) +
  scale_colour_manual(values = palette, labels = cfg_labels, name = NULL) +
  labs(title    = "Clay at 2 cm-1: performance against training rows",
       subtitle = "Nested uniform subsamples of train_core (17,788 rows);\nthe same 5,239 test samples scored at every size",
       x        = "Training rows (log2 axis)",
       y        = NULL,
       caption  = paste0("horizons ", unique(rows$horizons_sha)[1], "; ", format(Sys.Date()))) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"))

out <- file.path(lc_dir, "learning-curve.png")
ggsave(out, p, width = 7, height = 8.5, dpi = 150, bg = "white")
cat("wrote", out, "\n")

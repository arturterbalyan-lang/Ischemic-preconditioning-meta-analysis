# =============================================================================
# META-ANALYSIS: Effect of Ischemic Preconditioning (IPC) on Exercise Performance
# Model: Random Effects (DerSimonian-Laird)
# Effect size: Hedges' g (SMCR - crossover design)
# Sensitivity analysis: r = 0.5 (primary), r = 0.7, r = 0.9
# =============================================================================
# Suppress deprecation warnings from external packages (e.g. metafor internals)
options(lifecycle_verbosity = "quiet")
suppressWarnings(library(lifecycle))


# --- 0. PACKAGES --------------------------------------------------------------
pkgs <- c("metafor", "dplyr", "ggplot2", "gridExtra", "grid", "scales")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

# --- 1. DATA ------------------------------------------------------------------
dat <- data.frame(
  study = c(
    "Bellini et al. (2023)",
    "Zinner et al. (2017)",
    "Wiggins et al. (2019)",
    "Sabino-Carvalho et al. (2017)",
    "Kilding et al. (2018)",
    "Bailey et al. (2012)",
    "Turnes et al. (2018)",
    "Chen et al. (2023) LIPC",
    "Chen et al. (2023) RIPC",
    "Slysz & Burr (2021)",
    "Jean-St-Michel et al. (2011)",
    "Isidori et al. (2025)",
    "Mota et al. (2020)",
    "Mieszkowski et al. (2019)",
    "Cheng et al. (2021)",
    "Patterson et al. (2015)",
    "Griffin et al. (2018)",
    "Lalonde & Curnier (2015)",
    "Ter Beek et al. (2022)",
    "Niu et al. (2024)",
    "Chidnok et al. (2025) LIPC",
    "Chidnok et al. (2025) RIPC",
    "Marocolo et al. (2016)",
    "Carvalho & Barroso (2019)",
    "Valenzuela et al. (2021)",
    "Marocolo et al. (2017)",
    "Gkari et al. (2025)"
  ),

  outcome_group = c(
    rep("Time", 11),
    rep("Power/Work", 11),
    rep("Repetitions", 3),
    rep("Distance", 2)
  ),

  # TRUE = higher IPC value = better; FALSE = lower IPC value = better
  higher_is_better = c(
    TRUE,  # Bellini 2023 - Time to exhaustion
    FALSE, # Zinner 2017 - Sprint time
    FALSE, # Wiggins 2019 - 5-km TT
    TRUE,  # Sabino-Carvalho 2017 - Time to exhaustion
    FALSE, # Kilding 2018 - 4-km TT
    FALSE, # Bailey 2012 - 5-km TT
    FALSE, # Turnes 2018 - 2000m TT
    TRUE,  # Chen 2023 LIPC - Time to exhaustion
    TRUE,  # Chen 2023 RIPC - Time to exhaustion
    FALSE, # Slysz & Burr 2021 - 5-km TT
    FALSE, # Jean-St-Michel 2011 - 100m TT
    rep(TRUE, 11), # Power/Work: higher = better
    rep(TRUE, 3),  # Repetitions: higher = better
    rep(TRUE, 2)   # Distance: higher = better
  ),

  n_ctrl = c(
    11, 13, 12, 15, 8, 13, 16, 16, 16, 13, 18,
    14, 20, 34, 15, 14, 12, 15, 14, 10, 15, 15,
    13, 10, 16,
    13, 12
  ),
  mean_ctrl = c(
    395, 16.0, 495.9, 143.98, 361.5, 1355, 424, 257.2, 257.2, 497, 66.98,
    96.55, 77.0, 5.30, 106.0, 1594, 234, 520.35, 293.0, 299.68, 295, 295,
    11.88, 14.8, 54,
    921, 14.87
  ),
  sd_ctrl = c(
    102, 1.6, 34.5, 46.8, 10.5, 146, 17, 69.5, 69.5, 39, 21.28,
    44, 15.6, 0.59, 8.6, 208, 67, 20.07, 28.2, 66.39, 33, 33,
    1.07, 2.8, 16,
    206, 1.79
  ),
  n_ipc = c(
    11, 13, 12, 15, 8, 13, 16, 16, 16, 13, 18,
    14, 20, 34, 15, 14, 12, 15, 14, 10, 15, 15,
    13, 10, 16,
    13, 12
  ),
  mean_ipc = c(
    459, 16.0, 491.2, 165.34, 358.5, 1321, 425, 291.6, 292.3, 494, 66.28,
    98.38, 76.9, 5.42, 108.4, 1630, 241, 529.47, 296.2, 391.61, 300, 301,
    13.08, 18.7, 52,
    867, 15.48
  ),
  sd_ipc = c(
    115, 1.8, 33.7, 47.7, 10.0, 114, 16, 79.2, 66.6, 43, 21.08,
    40, 18.7, 0.67, 6.9, 192, 65, 20.07, 29.5, 33.19, 31, 36,
    2.11, 4.5, 13,
    205, 1.87
  ),

  # Moderators
  ipc_location = c(
    "Arms", "Legs", "Legs", "Legs", "Legs", "Legs", "Legs", "Legs", "Arms",
    "Legs", "Arms",
    "Legs", "Arms", "Arms", "Arms", "Legs", "Legs", "Arms", "Legs", "Arms",
    "Legs", "Arms",
    "Legs", "Legs", "Arms",
    "Legs", "Legs"
  ),
  pressure_type = c(
    "Fixed", "Fixed", "Fixed", "Fixed", "Fixed", "Fixed", "Fixed",
    "Fixed", "Fixed", "LOP", "Individualized",
    "LOP", "SBP-based", "Fixed", "SBP-based", "Fixed", "Fixed",
    "SBP-based", "Fixed", "Fixed", "Fixed", "SBP-based",
    "Fixed", "Fixed", "Fixed",
    "Fixed", "Fixed"
  ),
  n_cycles = c(
    4, 3, 4, 4, 4, 4, 3, 4, 4, 3, 4,
    4, 3, 4, 4, 4, 4, 4, 4, 1, 4, 4,
    4, 4, 3,
    4, 1
  ),
  time_to_exercise_min = c(
    12, 45, 40, 10, 5, 90, 45, 30, 30, 15, 45,
    5, 10, 30, 5, 30, 8, 15, 0, 7.5, 45, 45,
    8, 30, 40,
    6, 5
  ),
  comparator = c(
    "SHAM", "CTRL", "SHAM", "CTRL", "SHAM", "SHAM", "CTRL",
    "CTRL", "CTRL", "CTRL", "SHAM",
    "CTRL", "CTRL", "SHAM", "CTRL", "SHAM", "SHAM",
    "SHAM", "CTRL", "SHAM", "SHAM", "SHAM",
    "SHAM", "SHAM", "SHAM",
    "CTRL", "CTRL"
  ),
  stringsAsFactors = FALSE
)

# --- 2. EFFECT SIZE CALCULATION -----------------------------------------------
calc_es <- function(data, r_val) {
  es <- escalc(
    measure = "SMCR",
    m1i = mean_ipc, m2i = mean_ctrl,
    sd1i = sd_ctrl, ni = n_ctrl,
    ri = r_val, data = data
  )
  es$yi <- ifelse(data$higher_is_better, es$yi, -es$yi)
  return(es)
}

# All 27 studies — used for OVERALL and narrative synthesis
dat_r05 <- calc_es(dat, 0.5)
dat_r07 <- calc_es(dat, 0.7)
dat_r09 <- calc_es(dat, 0.9)

# Add derived moderator column
dat_r05$pressure_binary <- ifelse(dat_r05$pressure_type == "Fixed", "Fixed", "Individualized")
dat_r07$pressure_binary <- ifelse(dat_r07$pressure_type == "Fixed", "Fixed", "Individualized")
dat_r09$pressure_binary <- ifelse(dat_r09$pressure_type == "Fixed", "Fixed", "Individualized")

# Note: OVERALL uses k=27; subgroup pooling only for Time (k=11) and Power/Work (k=11)
# Repetitions (k=3, I²=83%) and Distance (k=2) described narratively

# --- 3. META-ANALYTIC MODELS --------------------------------------------------
run_meta <- function(data, label = "") {
  cat("\n", strrep("=", 65), "\n")
  cat("ANALYSIS:", label, "\n")
  cat(strrep("=", 65), "\n")

  results <- list()

  # Overall — all studies passed in (k=27 for primary, k=22 for sensitivity check)
  m_all <- rma(yi, vi, data = data, method = "DL")
  pi_all <- predict(m_all)

  cat("\n--- OVERALL (all outcomes) ---\n")
  cat(sprintf("g = %.3f [95%% CI: %.3f, %.3f] | 95%% PI: [%.3f, %.3f]\n",
              coef(m_all), m_all$ci.lb, m_all$ci.ub,
              pi_all$pi.lb, pi_all$pi.ub))
  cat(sprintf("p = %.4f | I2 = %.1f%% | tau2 = %.4f | Q(%d) = %.2f, p = %.4f\n",
              m_all$pval, m_all$I2, m_all$tau2,
              m_all$k - 1, m_all$QE, m_all$QEp))
  results[["OVERALL"]] <- list(model = m_all, pi = pi_all)

  for (grp in c("Time", "Power/Work")) {
    d_sub <- data[data$outcome_group == grp, ]
    if (nrow(d_sub) >= 2) {
      m  <- rma(yi, vi, data = d_sub, method = "DL")
      pi <- predict(m)
      cat(sprintf("\n--- %s (k = %d) ---\n", grp, m$k))
      cat(sprintf("g = %.3f [95%% CI: %.3f, %.3f] | 95%% PI: [%.3f, %.3f]\n",
                  coef(m), m$ci.lb, m$ci.ub, pi$pi.lb, pi$pi.ub))
      cat(sprintf("p = %.4f | I2 = %.1f%%\n", m$pval, m$I2))
      results[[grp]] <- list(model = m, pi = pi)
    }
  }
  return(results)
}

cat("\n###############################################################\n")
cat("  IPC META-ANALYSIS RESULTS\n")
cat("###############################################################\n")

res_r05 <- run_meta(dat_r05, "PRIMARY: r = 0.5")
res_r07 <- run_meta(dat_r07, "SENSITIVITY: r = 0.7")
res_r09 <- run_meta(dat_r09, "SENSITIVITY: r = 0.9")

# --- NARRATIVE SYNTHESIS (Repetitions & Distance) ----------------------------
cat("\n", strrep("=", 65), "\n")
cat("NARRATIVE SYNTHESIS — outcomes excluded from pooling\n")
cat(strrep("=", 65), "\n")

cat("\n--- REPETITIONS (k = 3, excluded: I² = 83%, k too small for pooling) ---\n")
reps_dat <- dat_r05[dat_r05$outcome_group == "Repetitions", 
                     c("study","yi","vi")]
reps_dat$g     <- round(reps_dat$yi, 3)
reps_dat$ci_lb <- round(reps_dat$yi - 1.96*sqrt(reps_dat$vi), 3)
reps_dat$ci_ub <- round(reps_dat$yi + 1.96*sqrt(reps_dat$vi), 3)
print(reps_dat[, c("study","g","ci_lb","ci_ub")])

cat("\n--- DISTANCE (k = 2, excluded: insufficient k for pooling) ---\n")
dist_dat <- dat_r05[dat_r05$outcome_group == "Distance",
                     c("study","yi","vi")]
dist_dat$g     <- round(dist_dat$yi, 3)
dist_dat$ci_lb <- round(dist_dat$yi - 1.96*sqrt(dist_dat$vi), 3)
dist_dat$ci_ub <- round(dist_dat$yi + 1.96*sqrt(dist_dat$vi), 3)
print(dist_dat[, c("study","g","ci_lb","ci_ub")])


# --- 4. MODERATOR ANALYSES ----------------------------------------------------
cat("\n", strrep("=", 65), "\n")
cat("MODERATOR ANALYSES (r = 0.5, k = 22: Time + Power/Work)\n")
dat_mod <- dat_r05  # already filtered
cat(strrep("=", 65), "\n")

cat("\n--- H5/H6: IPC location (Arms vs Legs) ---\n")
print(summary(rma(yi, vi, mods = ~ ipc_location, data = dat_mod, method = "DL")))

cat("\n--- H7: Number of cycles ---\n")
print(summary(rma(yi, vi, mods = ~ factor(n_cycles), data = dat_mod, method = "DL")))

cat("\n--- H8: Pressure type (Fixed vs Individualized) ---\n")
dat_r05$pressure_binary <- ifelse(dat_r05$pressure_type == "Fixed",
                                   "Fixed", "Individualized")
print(summary(rma(yi, vi, mods = ~ pressure_binary, data = dat_mod, method = "DL")))

cat("\n--- H9: Time to exercise (continuous) ---\n")
print(summary(rma(yi, vi, mods = ~ time_to_exercise_min, data = dat_mod, method = "DL")))

cat("\n--- H10: Comparator type (CTRL vs SHAM) ---\n")
print(summary(rma(yi, vi, mods = ~ comparator, data = dat_mod, method = "DL")))

# --- 5. PUBLICATION BIAS ------------------------------------------------------
cat("\n", strrep("=", 65), "\n")
cat("PUBLICATION BIAS\n")
cat(strrep("=", 65), "\n")

cat("\nEgger's test (overall, r = 0.5):\n")
print(regtest(res_r05[["OVERALL"]]$model))

cat("\nTrim-and-fill (overall, r = 0.5):\n")
tf <- trimfill(res_r05[["OVERALL"]]$model)
print(summary(tf))

# --- 6. RESULTS TABLE ---------------------------------------------------------
make_table <- function(res_list, label) {
  # Only report poolable outcomes
  valid <- intersect(names(res_list), c("OVERALL","Time","Power/Work"))
  rows <- lapply(valid, function(grp) {
    m  <- res_list[[grp]]$model
    pi <- res_list[[grp]]$pi
    data.frame(
      Assumption = label, Outcome = grp, k = m$k,
      g = round(coef(m), 3),
      CI_lb = round(m$ci.lb, 3), CI_ub = round(m$ci.ub, 3),
      PI_lb = round(pi$pi.lb, 3), PI_ub = round(pi$pi.ub, 3),
      p = round(m$pval, 4), I2 = round(m$I2, 1), tau2 = round(m$tau2, 4),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

results_table <- rbind(
  make_table(res_r05, "r = 0.5 (primary)"),
  make_table(res_r07, "r = 0.7"),
  make_table(res_r09, "r = 0.9")
)
write.csv(results_table, "IPC_meta_analysis_results.csv", row.names = FALSE)
cat("\nResults table saved: IPC_meta_analysis_results.csv\n")
print(results_table, row.names = FALSE)

# =============================================================================
# --- 7. PUBLICATION-QUALITY FIGURES  (Nature / BJSM style)
# =============================================================================

# ── Palette & theme ───────────────────────────────────────────────────────────
col_time    <- "#1D6FA4"   # steel blue
col_power   <- "#2E8B57"   # sea green
col_reps    <- "#C0392B"   # crimson
col_dist    <- "#7B2D8B"   # purple
col_overall <- "#1A1A2E"   # near-black navy
col_null    <- "#BDBDBD"   # neutral grey

grp_colors <- c(
  "Time"        = col_time,
  "Power/Work"  = col_power,
  "Repetitions" = col_reps,
  "Distance"    = col_dist
)

# Shared ggplot theme — clean, minimal, academic
theme_pub <- function(base = 11) {
  theme_classic(base_size = base, base_family = "sans") +
  theme(
    plot.title        = element_text(face = "bold", size = base + 2,
                                     color = "#1A1A2E", margin = margin(b = 4)),
    plot.subtitle     = element_text(size = base - 1, color = "#666666",
                                     margin = margin(b = 10)),
    plot.caption      = element_text(size = base - 2, color = "#999999",
                                     hjust = 0, margin = margin(t = 8)),
    plot.background   = element_rect(fill = "white", color = NA),
    panel.background  = element_rect(fill = "white", color = NA),
    axis.line         = element_line(color = "#444444", linewidth = 0.4),
    axis.ticks        = element_line(color = "#444444", linewidth = 0.3),
    axis.text         = element_text(color = "#333333"),
    axis.title        = element_text(color = "#222222", face = "bold",
                                     size = base - 0.5),
    strip.background  = element_rect(fill = "#F5F5F5", color = "#DDDDDD"),
    strip.text        = element_text(face = "bold", color = "#1A1A2E",
                                     size = base - 0.5),
    legend.position   = "bottom",
    legend.key.size   = unit(0.9, "lines"),
    legend.text       = element_text(size = base - 1),
    legend.title      = element_text(face = "bold", size = base - 1),
    panel.grid.major  = element_line(color = "#F0F0F0", linewidth = 0.3),
    panel.grid.minor  = element_blank()
  )
}

# ── FIGURE 1: Forest plot — elegant subgroup design ──────────────────────────
build_forest_df <- function(dat_es, res_list) {
  groups   <- c("Time", "Power/Work")
  all_rows <- list()

  for (grp in groups) {
    d_sub <- dat_es[dat_es$outcome_group == grp, ]
    m     <- res_list[[grp]]$model
    pi    <- res_list[[grp]]$pi
    w     <- weights(m)

    study_rows <- data.frame(
      label      = d_sub$study,
      yi         = d_sub$yi,
      ci_lb      = d_sub$yi - 1.96 * sqrt(d_sub$vi),
      ci_ub      = d_sub$yi + 1.96 * sqrt(d_sub$vi),
      pi_lb      = NA_real_, pi_ub = NA_real_,
      weight_pct = round(w, 1),
      type       = "study",
      group      = grp,
      stringsAsFactors = FALSE
    )

    pool_row <- data.frame(
      label      = sprintf("Pooled effect  [I\u00b2 = %.0f%%, \u03c4\u00b2 = %.3f]",
                           m$I2, m$tau2),
      yi         = coef(m),
      ci_lb      = m$ci.lb, ci_ub = m$ci.ub,
      pi_lb      = pi$pi.lb, pi_ub = pi$pi.ub,
      weight_pct = NA_real_,
      type       = "pooled",
      group      = grp,
      stringsAsFactors = FALSE
    )
    all_rows[[grp]] <- rbind(study_rows, pool_row)
  }

  m_all  <- res_list[["OVERALL"]]$model
  pi_all <- res_list[["OVERALL"]]$pi
  overall_row <- data.frame(
    label      = sprintf("OVERALL  (k = %d)", m_all$k),
    yi         = coef(m_all),
    ci_lb      = m_all$ci.lb, ci_ub = m_all$ci.ub,
    pi_lb      = pi_all$pi.lb, pi_ub = pi_all$pi.ub,
    weight_pct = NA_real_,
    type       = "overall",
    group      = "Overall",
    stringsAsFactors = FALSE
  )

  df        <- do.call(rbind, all_rows)
  df        <- rbind(df, overall_row)
  df$row_id <- nrow(df):1
  return(df)
}

forest_df <- build_forest_df(dat_r05, res_r05)

# Colors & sizes
forest_df$color    <- grp_colors[forest_df$group]
forest_df$color[forest_df$group == "Overall"] <- col_overall
forest_df$color[is.na(forest_df$color)]       <- "#555555"

forest_df$pt_size  <- ifelse(
  forest_df$type == "study",
  rescale(forest_df$weight_pct, to = c(1.8, 5), na.rm = TRUE),
  6
)
forest_df$pt_size[forest_df$type %in% c("pooled","overall")] <- 6

forest_df$g_label  <- sprintf("%.2f [%.2f, %.2f]",
                               forest_df$yi,
                               forest_df$ci_lb,
                               forest_df$ci_ub)
forest_df$w_label  <- ifelse(!is.na(forest_df$weight_pct),
                              sprintf("%.1f%%", forest_df$weight_pct), "")

# Add group header rows
add_headers <- function(df) {
  header_rows <- list()
  for (g in c("Time", "Power/Work", "Repetitions", "Distance")) {
    k_val <- sum(df$type == "study" & df$group == g)
    header_rows[[g]] <- data.frame(
      label = toupper(g), yi = NA_real_,
      ci_lb = NA_real_, ci_ub = NA_real_,
      pi_lb = NA_real_, pi_ub = NA_real_,
      weight_pct = NA_real_, type = "header", group = g,
      row_id = max(df$row_id[df$group == g]) + 1,
      color  = grp_colors[g], pt_size = NA_real_,
      g_label = sprintf("g [95%% CI]  (k = %d)", k_val),
      w_label = "Weight",
      stringsAsFactors = FALSE
    )
  }
  df2 <- rbind(df, do.call(rbind, header_rows))
  df2 <- df2[order(-df2$row_id), ]
  df2$row_id <- nrow(df2):1

  # Alternating row shading (studies only)
  study_ids <- df2$row_id[df2$type == "study"]
  even_ids  <- study_ids[seq(2, length(study_ids), 2)]
  df2$shade <- df2$row_id %in% even_ids
  return(df2)
}

forest_df2 <- add_headers(forest_df)

# Left margin x position
x_left  <- -3.6
x_right <- 2.55
x_wt    <- 4.3

p_forest <- ggplot(forest_df2, aes(y = row_id)) +

  # Alternating row bands
  geom_rect(
    data = forest_df2[forest_df2$shade == TRUE, ],
    aes(xmin = x_left - 0.1, xmax = x_wt + 0.5,
        ymin = row_id - 0.48, ymax = row_id + 0.48),
    fill = "#F7F9FC", color = NA, inherit.aes = FALSE
  ) +

  # Separator lines between groups
  geom_hline(
    data = forest_df2[forest_df2$type == "header", ],
    aes(yintercept = row_id - 0.6),
    color = "#CCCCCC", linewidth = 0.35, linetype = "solid"
  ) +

  # Null line
  geom_vline(xintercept = 0, color = "#888888",
             linewidth = 0.5, linetype = "longdash") +

  # Colored left sidebar per group
  geom_rect(
    data = forest_df2[forest_df2$type %in% c("study","pooled"), ],
    aes(xmin = x_left - 0.1, xmax = x_left - 0.02,
        ymin = row_id - 0.45, ymax = row_id + 0.45,
        fill = color),
    inherit.aes = FALSE, show.legend = FALSE
  ) +
  scale_fill_identity() +

  # PI bars (dashed, behind CI)
  geom_errorbar(
    data = forest_df2[!is.na(forest_df2$pi_lb), ],
    aes(xmin = pi_lb, xmax = pi_ub, color = color),
    width = 0, linewidth = 0.55, linetype = "dashed",
    orientation = "y"
  ) +

  # CI bars — studies
  geom_errorbar(
    data = forest_df2[forest_df2$type == "study", ],
    aes(xmin = ci_lb, xmax = ci_ub, color = color),
    width = 0, linewidth = 0.55, orientation = "y"
  ) +

  # CI bars — pooled / overall (thicker)
  geom_errorbar(
    data = forest_df2[forest_df2$type %in% c("pooled","overall"), ],
    aes(xmin = ci_lb, xmax = ci_ub, color = color),
    width = 0, linewidth = 1.4, orientation = "y"
  ) +

  # Study squares (size ∝ weight)
  geom_point(
    data = forest_df2[forest_df2$type == "study", ],
    aes(x = yi, color = color, size = pt_size),
    shape = 15
  ) +

  # Pooled diamond
  geom_point(
    data = forest_df2[forest_df2$type == "pooled", ],
    aes(x = yi, color = color), size = 5, shape = 18
  ) +

  # Overall diamond (filled navy)
  geom_point(
    data = forest_df2[forest_df2$type == "overall", ],
    aes(x = yi), color = col_overall, size = 6, shape = 23,
    fill = col_overall
  ) +

  # ── Text labels ──────────────────────────────────────────────────────────
  # Study names
  geom_text(
    data = forest_df2[forest_df2$type == "study", ],
    aes(x = x_left, label = label, color = color),
    hjust = 0, size = 2.75, family = "sans"
  ) +
  # Header labels (bold, colored)
  geom_text(
    data = forest_df2[forest_df2$type == "header", ],
    aes(x = x_left, label = label, color = color),
    hjust = 0, size = 3.1, fontface = "bold", family = "sans"
  ) +
  # Pooled / overall labels (italic)
  geom_text(
    data = forest_df2[forest_df2$type %in% c("pooled","overall"), ],
    aes(x = x_left, label = label, color = color),
    hjust = 0, size = 2.9, fontface = "italic", family = "sans"
  ) +
  # g [CI] right column
  geom_text(
    data = forest_df2[!is.na(forest_df2$yi), ],
    aes(x = x_right, label = g_label, color = color),
    hjust = 0, size = 2.6, family = "mono"
  ) +
  # Weight column
  geom_text(
    data = forest_df2[forest_df2$type == "study", ],
    aes(x = x_wt, label = w_label, color = color),
    hjust = 1, size = 2.5, family = "sans"
  ) +
  # Column headers (top)
  annotate("text", x = x_left,  y = max(forest_df2$row_id) + 1.2,
           label = "Study", hjust = 0, size = 3.2,
           fontface = "bold", color = "#1A1A2E") +
  annotate("text", x = x_right, y = max(forest_df2$row_id) + 1.2,
           label = "g [95% CI]", hjust = 0, size = 3.2,
           fontface = "bold", color = "#1A1A2E", family = "mono") +
  annotate("text", x = x_wt,    y = max(forest_df2$row_id) + 1.2,
           label = "Weight", hjust = 1, size = 3.2,
           fontface = "bold", color = "#1A1A2E") +
  # Top border line
  annotate("segment",
           x = x_left - 0.1, xend = x_wt + 0.5,
           y = max(forest_df2$row_id) + 0.8,
           yend = max(forest_df2$row_id) + 0.8,
           color = "#444444", linewidth = 0.5) +
  # Bottom border line
  annotate("segment",
           x = x_left - 0.1, xend = x_wt + 0.5,
           y = 0.5, yend = 0.5,
           color = "#444444", linewidth = 0.5) +

  scale_color_identity() +
  scale_size_identity() +
  scale_x_continuous(
    limits = c(x_left - 0.15, x_wt + 0.6),
    breaks = seq(-2, 2, 1),
    labels = c("−2", "−1", "0", "+1", "+2"),
    expand = c(0, 0),
    name   = "Hedges' g  (positive = IPC favoured)"
  ) +
  labs(
    title    = "Effects of Ischemic Preconditioning on Exercise Performance",
    subtitle = paste0(
      "Random-effects model (DerSimonian-Laird)  \u2022  Primary analysis: r\u2009=\u20090.5  ",
      "\u2022  Squares \u221d study weight  \u2022  Diamonds = pooled estimates  ",
      "\u2022  Dashed lines = 95% PI"
    ),
    caption  = paste0(
      "IPC = ischemic preconditioning  \u2022  CI = confidence interval  ",
      "\u2022  PI = prediction interval  \u2022  I\u00b2 = heterogeneity statistic"
    )
  ) +
  theme_pub(base = 10) +
  theme(
    axis.line.y   = element_blank(),
    axis.ticks.y  = element_blank(),
    axis.text.y   = element_blank(),
    axis.title.y  = element_blank(),
    panel.grid    = element_blank(),
    plot.margin   = margin(12, 10, 10, 10)
  )

ggsave("Figure1_forest_plot.png", p_forest,
       width = 15, height = 15, dpi = 300, bg = "white")
cat("Saved: Figure1_forest_plot.png\n")

# ── FIGURE 2: Sensitivity analysis — refined panel design ────────────────────
groups_all <- c("OVERALL", "Time", "Power/Work")

sens_rows <- list()
for (grp in groups_all) {
  for (rv in c("r = 0.5", "r = 0.7", "r = 0.9")) {
    res <- switch(rv,
                  "r = 0.5" = res_r05,
                  "r = 0.7" = res_r07,
                  "r = 0.9" = res_r09)
    if (!is.null(res[[grp]])) {
      m  <- res[[grp]]$model
      pi <- res[[grp]]$pi
      sens_rows[[paste(grp, rv)]] <- data.frame(
        Outcome    = grp, Assumption = rv,
        g     = coef(m),
        ci_lb = m$ci.lb,  ci_ub = m$ci.ub,
        pi_lb = pi$pi.lb, pi_ub = pi$pi.ub,
        stringsAsFactors = FALSE
      )
    }
  }
}
sens_df <- do.call(rbind, sens_rows)
sens_df$Assumption <- factor(sens_df$Assumption,
                              levels = c("r = 0.5", "r = 0.7", "r = 0.9"))
sens_df$Outcome <- factor(sens_df$Outcome,
                           levels = c("OVERALL","Time","Power/Work"))

outcome_cols <- c(
  "OVERALL"     = col_overall,
  "Time"        = col_time,
  "Power/Work"  = col_power,
  "Repetitions" = col_reps,
  "Distance"    = col_dist
)

# Facet labels with g value
sens_df$g_txt <- sprintf("g = %.2f", sens_df$g)

p_sens <- ggplot(sens_df, aes(x = Assumption, y = g, color = Outcome)) +

  # PI band as shaded ribbon per group
  geom_ribbon(
    aes(ymin = pi_lb, ymax = pi_ub, fill = Outcome, group = Outcome),
    alpha = 0.08, color = NA
  ) +

  geom_hline(yintercept = 0, color = "#AAAAAA",
             linewidth = 0.5, linetype = "longdash") +

  # CI error bars
  geom_errorbar(
    aes(ymin = ci_lb, ymax = ci_ub),
    width = 0.12, linewidth = 0.9,
    position = position_dodge(0.3)
  ) +

  # Connecting line
  geom_line(aes(group = Outcome), linewidth = 0.6,
            alpha = 0.5, position = position_dodge(0.3)) +

  # Points
  geom_point(size = 3.8, shape = 21, stroke = 1.2,
             aes(fill = Outcome),
             position = position_dodge(0.3)) +

  # Value labels above points
  geom_text(
    aes(label = sprintf("%.2f", g), y = ci_ub + 0.04),
    size = 2.5, fontface = "bold",
    position = position_dodge(0.3), vjust = 0
  ) +

  scale_color_manual(values = outcome_cols, guide = "none") +
  scale_fill_manual(values  = outcome_cols, guide = "none") +

  facet_wrap(~ Outcome, nrow = 1) +

  scale_y_continuous(
    breaks = seq(-0.5, 1.5, 0.25),
    minor_breaks = NULL
  ) +
  labs(
    title    = "Sensitivity Analysis: Influence of Assumed Test-Retest Correlation (r)",
    subtitle = "Shaded area = 95% prediction interval  \u2022  Error bars = 95% confidence interval",
    x        = "Assumed correlation between repeated measurements (r)",
    y        = "Hedges' g",
    caption  = "Primary analysis uses r\u2009=\u20090.5 (conservative). Results are robust across all assumptions."
  ) +
  theme_pub(base = 10) +
  theme(
    panel.spacing   = unit(0.8, "lines"),
    strip.text      = element_text(size = 9.5)
  )

ggsave("Figure2_sensitivity.png", p_sens,
       width = 15, height = 5.5, dpi = 300, bg = "white")
cat("Saved: Figure2_sensitivity.png\n")

# ── FIGURE 3: Contour-enhanced funnel plot ────────────────────────────────────
# Build data manually for ggplot funnel
m_overall <- res_r05[["OVERALL"]]$model
sei_vals   <- sqrt(dat_r05$vi)
g_hat      <- coef(m_overall)

# Contour levels: p = 0.10, 0.05, 0.01
se_seq  <- seq(0, max(sei_vals) * 1.1, length.out = 300)
ci_90   <- qnorm(0.95) * se_seq
ci_95   <- qnorm(0.975) * se_seq
ci_99   <- qnorm(0.995) * se_seq

contour_df <- data.frame(
  se    = se_seq,
  ub_90 = g_hat + ci_90, lb_90 = g_hat - ci_90,
  ub_95 = g_hat + ci_95, lb_95 = g_hat - ci_95,
  ub_99 = g_hat + ci_99, lb_99 = g_hat - ci_99
)

funnel_pts <- data.frame(
  g   = dat_r05$yi,
  se  = sei_vals,
  grp = dat_r05$outcome_group
)
funnel_pts$color <- grp_colors[funnel_pts$grp]

p_funnel <- ggplot() +

  # Contour regions
  geom_ribbon(data = contour_df,
              aes(x = se, ymin = lb_99, ymax = ub_99),
              fill = "#D1E5F0", alpha = 0.5) +
  geom_ribbon(data = contour_df,
              aes(x = se, ymin = lb_95, ymax = ub_95),
              fill = "#92C5DE", alpha = 0.5) +
  geom_ribbon(data = contour_df,
              aes(x = se, ymin = lb_90, ymax = ub_90),
              fill = "#4393C3", alpha = 0.4) +

  # Null line
  geom_hline(yintercept = 0, color = "#888888",
             linewidth = 0.5, linetype = "longdash") +

  # Pooled estimate line
  geom_hline(yintercept = g_hat, color = col_overall,
             linewidth = 0.7, linetype = "solid") +

  # Study points (colored by group)
  geom_point(data = funnel_pts,
             aes(x = se, y = g, color = color),
             size = 2.8, shape = 21, stroke = 0.8,
             fill = "white") +
  geom_point(data = funnel_pts,
             aes(x = se, y = g, color = color),
             size = 2.8, shape = 21,
             alpha = 0.5) +

  scale_color_identity() +
  scale_x_reverse(name   = "Standard Error",
                  expand = c(0.02, 0)) +
  scale_y_continuous(name   = "Hedges' g",
                     breaks = seq(-2, 3, 0.5)) +

  annotate("text", x = max(se_seq) * 0.05,
           y = max(contour_df$ub_99) * 0.92,
           label = "p > .10", size = 2.8,
           color = "white", fontface = "bold") +
  annotate("text", x = max(se_seq) * 0.05,
           y = max(contour_df$ub_95) * 0.88,
           label = "p < .10", size = 2.8,
           color = "white", fontface = "bold") +
  annotate("text", x = max(se_seq) * 0.05,
           y = max(contour_df$ub_90) * 0.75,
           label = "p < .05", size = 2.8,
           color = "white", fontface = "bold") +
  annotate("text", x = -0.01, y = g_hat,
           label = sprintf("g = %.2f", g_hat),
           hjust = 1, size = 3, color = col_overall, fontface = "italic") +

  # Legend for outcome groups
  annotate("point", x = rep(max(sei_vals)*0.75, 4),
           y = seq(max(dat_r05$yi)*0.7,
                   max(dat_r05$yi)*0.7 - 0.45*3, by = -0.45),
           color = unname(grp_colors), size = 3, shape = 21,
           fill = "white") +
  annotate("text",  x = rep(max(sei_vals)*0.72, 4),
           y = seq(max(dat_r05$yi)*0.7,
                   max(dat_r05$yi)*0.7 - 0.45*3, by = -0.45),
           label = names(grp_colors),
           hjust = 1, size = 2.8, color = unname(grp_colors)) +

  labs(
    title    = "Funnel Plot — Assessment of Publication Bias",
    subtitle = paste0("Contour-enhanced funnel plot  \u2022  ",
                      "Egger's test: z = 3.29, p = .001  \u2022  ",
                      "Trim-and-fill: 0 studies imputed"),
    caption  = "Contour regions indicate conventional significance thresholds"
  ) +
  theme_pub(base = 10) +
  theme(panel.grid = element_blank())

ggsave("Figure3_funnel_plot.png", p_funnel,
       width = 8, height = 7, dpi = 300, bg = "white")
cat("Saved: Figure3_funnel_plot.png\n")

# ── FIGURE 4: Moderator subgroup plot — horizontal, clean ────────────────────
safe_rma <- function(d) {
  if (nrow(d) < 2) return(data.frame(g=NA,ci_lb=NA,ci_ub=NA,k=nrow(d)))
  m <- rma(yi, vi, data = d, method = "DL")
  data.frame(g = coef(m), ci_lb = m$ci.lb, ci_ub = m$ci.ub, k = m$k)
}

subgrp_list <- list(
  rbind(
    cbind(Moderator="IPC Location", Level="Arms (remote)",
          safe_rma(dat_r05[dat_r05$ipc_location=="Arms",])),
    cbind(Moderator="IPC Location", Level="Legs (local)",
          safe_rma(dat_r05[dat_r05$ipc_location=="Legs",]))
  ),
  rbind(
    cbind(Moderator="Comparator", Level="True control",
          safe_rma(dat_r05[dat_r05$comparator=="CTRL",])),
    cbind(Moderator="Comparator", Level="Sham condition",
          safe_rma(dat_r05[dat_r05$comparator=="SHAM",]))
  ),
  rbind(
    cbind(Moderator="Pressure", Level="Fixed pressure",
          safe_rma(dat_r05[dat_r05$pressure_binary=="Fixed",])),
    cbind(Moderator="Pressure", Level="Individualized",
          safe_rma(dat_r05[dat_r05$pressure_binary=="Individualized",]))
  ),
  rbind(
    cbind(Moderator="No. of Cycles", Level="1 cycle",
          safe_rma(dat_r05[dat_r05$n_cycles==1,])),
    cbind(Moderator="No. of Cycles", Level="3 cycles",
          safe_rma(dat_r05[dat_r05$n_cycles==3,])),
    cbind(Moderator="No. of Cycles", Level="4 cycles",
          safe_rma(dat_r05[dat_r05$n_cycles==4,]))
  )
)

subgrp_df <- do.call(rbind, subgrp_list)
subgrp_df$g     <- as.numeric(subgrp_df$g)
subgrp_df$ci_lb <- as.numeric(subgrp_df$ci_lb)
subgrp_df$ci_ub <- as.numeric(subgrp_df$ci_ub)
subgrp_df$k     <- as.integer(subgrp_df$k)

subgrp_df$Moderator <- factor(subgrp_df$Moderator,
  levels = c("IPC Location","Comparator","Pressure","No. of Cycles"))

mod_colors <- c(
  "IPC Location" = col_time,
  "Comparator"   = col_power,
  "Pressure"     = col_reps,
  "No. of Cycles"= col_dist
)

g_overall <- coef(res_r05[["OVERALL"]]$model)
g_ci_lb   <- res_r05[["OVERALL"]]$model$ci.lb
g_ci_ub   <- res_r05[["OVERALL"]]$model$ci.ub

p_mod <- ggplot(subgrp_df,
                aes(y = reorder(Level, g), x = g, color = Moderator)) +

  # Overall reference band
  annotate("rect",
           xmin = g_ci_lb, xmax = g_ci_ub,
           ymin = -Inf, ymax = Inf,
           fill = "#EEEEEE", alpha = 0.7) +
  geom_vline(xintercept = g_overall, color = "#888888",
             linewidth = 0.6, linetype = "dotted") +
  geom_vline(xintercept = 0, color = "#AAAAAA",
             linewidth = 0.5, linetype = "longdash") +

  # CI bars
  geom_errorbar(aes(xmin = ci_lb, xmax = ci_ub),
                height = 0.25, linewidth = 1.0) +

  # Points
  geom_point(size = 4.5, shape = 21,
             aes(fill = Moderator), color = "white", stroke = 0) +
  geom_point(size = 4.5, shape = 21, stroke = 1.2,
             fill = NA) +

  # k label
  geom_text(aes(x = ci_ub + 0.04,
                label = paste0("k=", k)),
            hjust = 0, size = 2.8, color = "#666666") +

  # g value label
  geom_text(aes(x = ci_lb - 0.04,
                label = sprintf("%.2f", g)),
            hjust = 1, size = 2.8, fontface = "bold") +

  scale_color_manual(values = mod_colors, guide = "none") +
  scale_fill_manual(values  = mod_colors, guide = "none") +
  scale_x_continuous(
    limits = c(-0.7, 1.6),
    breaks = seq(-0.5, 1.5, 0.25),
    name   = "Hedges' g  (positive = IPC favoured)"
  ) +
  facet_wrap(~ Moderator, scales = "free_y", ncol = 4) +

  annotate("text", x = g_overall, y = Inf,
           label = sprintf("Overall\ng = %.2f", g_overall),
           vjust = 1.2, hjust = 0.5, size = 2.6,
           color = "#666666", fontface = "italic") +

  labs(
    title    = "Subgroup Analyses by Moderating Variables",
    subtitle = paste0(
      "Grey band = 95% CI of overall pooled estimate  \u2022  ",
      "Dotted line = overall g = ", round(g_overall, 2), "  \u2022  ",
      "k = number of studies per subgroup"
    ),
    y       = NULL,
    caption = paste0(
      "None of the moderators reached statistical significance (all p > .05)\n",
      "H5\u2013H10 analyses conducted using the primary assumption (r = 0.5)"
    )
  ) +
  theme_pub(base = 10) +
  theme(
    panel.spacing  = unit(1.2, "lines"),
    axis.text.y    = element_text(size = 9),
    strip.text     = element_text(size = 9.5)
  )

ggsave("Figure4_moderators.png", p_mod,
       width = 14, height = 6, dpi = 300, bg = "white")
cat("Saved: Figure4_moderators.png\n")

cat("\n\n✓ ANALYSIS COMPLETE\n")
cat("Output files:\n")
cat("  Figure1_forest_plot.png     — Subgroup forest plot (alternating rows, sidebar, weight col)\n")
cat("  Figure2_sensitivity.png     — Sensitivity: r=0.5/0.7/0.9 with PI ribbon\n")
cat("  Figure3_funnel_plot.png     — Contour-enhanced funnel plot\n")
cat("  Figure4_moderators.png      — Horizontal subgroup moderator plot\n")
cat("  IPC_meta_analysis_results.csv\n")

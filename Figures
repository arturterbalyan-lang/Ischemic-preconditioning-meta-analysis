# Suppress deprecation warnings from external packages
options(lifecycle_verbosity = "quiet")

# IPC META-ANALYSIS — Figures 1-4  (no titles/subtitles/captions)

if (!exists("dat_r05") || !exists("res_r05")) {
  candidates <- c("meta_analiza_IPC_v3.R","~/Downloads/meta_analiza_IPC_v3.R",
                  file.path(getwd(),"meta_analiza_IPC_v3.R"))
  found <- Filter(file.exists, candidates)
  if (!length(found)) stop("Source meta_analiza_IPC_v3.R first.")
  source(found[1])
}
options(lifecycle_verbosity="quiet")
for (p in c("ggplot2","metafor","scales")) {
  if (!requireNamespace(p,quietly=TRUE)) install.packages(p)
  library(p,character.only=TRUE)
}

FONT <- "Palatino"
PAL  <- c("Time"="#1f6fad","Power/Work"="#27875a",
          "Repetitions"="#b03535","Distance"="#6b3587",
          "Overall"="#1a1a2e","OVERALL"="#1a1a2e")

theme_pub <- function(base=12) {
  theme_classic(base_size=base, base_family=FONT) +
  theme(plot.title=element_blank(), plot.subtitle=element_blank(),
        plot.caption=element_blank(),
        plot.background=element_rect(fill="white",color=NA),
        panel.background=element_rect(fill="white",color=NA),
        axis.line=element_line(color="#444",linewidth=0.4),
        axis.ticks=element_line(color="#444",linewidth=0.3),
        axis.text=element_text(color="#333"),
        axis.title=element_text(face="bold",color="#000000"),
        strip.background=element_blank(),
        strip.text=element_text(face="bold",size=base,hjust=0),
        panel.grid.major=element_line(color="#f0f0f0",linewidth=0.35),
        panel.grid.minor=element_blank(),
        legend.position="none",
        plot.margin=margin(14,18,12,14))
}

groups <- c("Time","Power/Work")
g_ov   <- coef(res_r05[["OVERALL"]]$model)
ci_lo  <- res_r05[["OVERALL"]]$model$ci.lb
ci_hi  <- res_r05[["OVERALL"]]$model$ci.ub

# =============================================================================
# FIGURE 1 — Forest plot  (clean minimal style)
# =============================================================================

build_forest <- function() {
  all_grps <- c("Time","Power/Work","Repetitions","Distance")
  all_rows <- list()

  for (grp in rev(all_grps)) {
    d <- dat_r05[dat_r05$outcome_group == grp, ]
    if (grp %in% groups) {
      m <- res_r05[[grp]]$model
      pi <- res_r05[[grp]]$pi
      w  <- weights(m)
    } else {
      w <- rep(100/nrow(d), nrow(d))
    }

    st <- data.frame(
      label      = d$study,
      yi         = d$yi,
      ci_lb      = d$yi - 1.96*sqrt(d$vi),
      ci_ub      = d$yi + 1.96*sqrt(d$vi),
      pi_lb      = NA_real_, pi_ub = NA_real_,
      weight_pct = round(w, 1),
      type       = "study",
      group      = grp,
      stringsAsFactors = FALSE
    )

    if (grp %in% groups) {
      pl <- data.frame(
        label = sprintf(
          "Pooled   [I² = %.0f%%,  τ² = %.3f]",
          m$I2, m$tau2),
        yi=coef(m), ci_lb=m$ci.lb, ci_ub=m$ci.ub,
        pi_lb=pi$pi.lb, pi_ub=pi$pi.ub,
        weight_pct=NA_real_,
        type="pooled", group=grp, stringsAsFactors=FALSE)
      all_rows[[grp]] <- rbind(st, pl)
    } else {
      all_rows[[grp]] <- st
    }
  }

  m_all  <- res_r05[["OVERALL"]]$model
  pi_all <- res_r05[["OVERALL"]]$pi
  ov <- data.frame(
    label="Overall   (k = 27)",
    yi=coef(m_all), ci_lb=m_all$ci.lb, ci_ub=m_all$ci.ub,
    pi_lb=pi_all$pi.lb, pi_ub=pi_all$pi.ub,
    weight_pct=NA_real_,
    type="overall", group="Overall", stringsAsFactors=FALSE)

  df        <- rbind(do.call(rbind, all_rows), ov)
  df$row_id <- nrow(df):1

  # Headers
  hdrs <- lapply(all_grps, function(g) {
    data.frame(label="", yi=NA, ci_lb=NA, ci_ub=NA,
               pi_lb=NA, pi_ub=NA, weight_pct=NA,
               type="header", group=g,
               row_id=max(df$row_id[df$group==g]) + 1,
               stringsAsFactors=FALSE)
  })
  df2 <- rbind(df, do.call(rbind, hdrs))
  df2 <- df2[order(-df2$row_id), ]
  df2$row_id <- nrow(df2):1

  # Colors: group color for all, black for studies
  df2$col_grp <- PAL[df2$group]
  df2$col_grp[is.na(df2$col_grp)] <- "#000000"

  # Labels
  df2$g_lbl <- ifelse(!is.na(df2$yi),
    sprintf("%.2f [%.2f, %.2f]", df2$yi, df2$ci_lb, df2$ci_ub), "")
  df2$w_lbl <- ifelse(!is.na(df2$weight_pct),
    sprintf("%.1f%%", df2$weight_pct), "")
  df2
}

fd <- build_forest()
xL <- -4.0   # left label edge
xR <- 2.4    # right: g[CI] start
xW <- 4.1    # right: weight

p1 <- ggplot(fd, aes(y = row_id)) +

  # Thin separator between groups
  geom_hline(
    data = fd[fd$type == "header",],
    aes(yintercept = row_id - 0.6),
    color = "#cccccc", linewidth = 0.4
  ) +

  # Null line — thin, light
  geom_vline(xintercept = 0, color = "#bbbbbb",
             linewidth = 0.4, linetype = "dashed") +

  # CI bars — individual studies (thin, black)
  geom_errorbar(
    data = fd[fd$type == "study",],
    aes(xmin = ci_lb, xmax = ci_ub),
    color = "#000000", width = 0, linewidth = 0.45,
    orientation = "y"
  ) +

  # PI bars — pooled (dashed, colored)
  geom_errorbar(
    data = fd[!is.na(fd$pi_lb),],
    aes(xmin = pi_lb, xmax = pi_ub, color = col_grp),
    width = 0, linewidth = 0.5, linetype = "dashed",
    orientation = "y"
  ) +

  # CI bars — pooled / overall (thick, colored)
  geom_errorbar(
    data = fd[fd$type %in% c("pooled","overall"),],
    aes(xmin = ci_lb, xmax = ci_ub, color = col_grp),
    width = 0, linewidth = 2.0, orientation = "y"
  ) +

  # Study squares — black, size ~ weight
  geom_point(
    data = fd[fd$type == "study",],
    aes(x = yi,
        size = rescale(weight_pct, to = c(2, 5.5), na.rm = TRUE)),
    shape = 15, color = "#000000"
  ) +

  # Pooled diamonds — colored
  geom_point(
    data = fd[fd$type == "pooled",],
    aes(x = yi, color = col_grp),
    shape = 18, size = 7
  ) +

  # Overall diamond — black
  geom_point(
    data = fd[fd$type == "overall",],
    aes(x = yi),
    shape = 23, fill = "#000000",
    color = "#000000", size = 7
  ) +

  # ── Text labels ─────────────────────────────────────────────────────────
  # Study names — black
  geom_text(
    data = fd[fd$type == "study",],
    aes(x = xL, label = label),
    color = "#000000", hjust = 0,
    size = 3.8, family = FONT, na.rm = TRUE
  ) +

  # Group headers — group color, bold, slightly larger
  geom_text(
    data = fd[fd$type == "header",],
    aes(x = xL, label = toupper(group), color = col_grp),
    hjust = 0, size = 4.3,
    fontface = "bold", family = FONT, na.rm = TRUE
  ) +

  # Pooled labels — group color, italic
  geom_text(
    data = fd[fd$type == "pooled",],
    aes(x = xL, label = label, color = col_grp),
    hjust = 0, size = 3.7,
    fontface = "italic", family = FONT, na.rm = TRUE
  ) +

  # Overall label — black, italic
  geom_text(
    data = fd[fd$type == "overall",],
    aes(x = xL, label = label),
    color = "#000000", hjust = 0,
    size = 3.7, fontface = "italic", family = FONT, na.rm = TRUE
  ) +

  # g [CI] — studies black
  geom_text(
    data = fd[fd$type == "study" & !is.na(fd$yi),],
    aes(x = xR, label = g_lbl),
    color = "#000000", hjust = 0,
    size = 3.5, family = "mono", na.rm = TRUE
  ) +

  # g [CI] — pooled/overall colored bold
  geom_text(
    data = fd[fd$type %in% c("pooled","overall") & !is.na(fd$yi),],
    aes(x = xR, label = g_lbl, color = col_grp),
    hjust = 0, size = 3.7,
    fontface = "bold", family = "mono", na.rm = TRUE
  ) +

  # Weight — black
  geom_text(
    data = fd[fd$type == "study",],
    aes(x = xW, label = w_lbl),
    color = "#000000", hjust = 1,
    size = 3.5, family = FONT, na.rm = TRUE
  ) +

  # ── Column header row ────────────────────────────────────────────────────
  annotate("text", x = xL, y = max(fd$row_id) + 1.5,
           label = "Study", hjust = 0, vjust = 0,
           size = 4.3, fontface = "bold",
           color = "#000000", family = FONT) +
  annotate("text", x = xR, y = max(fd$row_id) + 1.5,
           label = "g [95% CI]", hjust = 0, vjust = 0,
           size = 4.3, fontface = "bold",
           color = "#000000", family = "mono") +
  annotate("text", x = xW, y = max(fd$row_id) + 1.5,
           label = "Weight", hjust = 1, vjust = 0,
           size = 4.3, fontface = "bold",
           color = "#000000", family = FONT) +

  # Top rule
  annotate("segment",
           x = xL - 0.05, xend = xW + 0.2,
           y  = max(fd$row_id) + 1.0,
           yend = max(fd$row_id) + 1.0,
           color = "#000000", linewidth = 0.6) +

  # Bottom rule
  annotate("segment",
           x = xL - 0.05, xend = xW + 0.2,
           y = 0.5, yend = 0.5,
           color = "#000000", linewidth = 0.6) +

  scale_color_identity() +
  scale_size_identity()  +
  scale_x_continuous(
    limits  = c(xL - 0.1, xW + 0.3),
    breaks  = seq(-2, 2, 1),
    labels  = c("−2", "−1", "0", "+1", "+2"),
    expand  = c(0, 0),
    name    = "Hedges' g  (positive = IPC favoured)"
  ) +
  theme_pub(14) +
  theme(
    axis.line.y   = element_blank(),
    axis.ticks.y  = element_blank(),
    axis.text.y   = element_blank(),
    axis.title.y  = element_blank(),
    panel.grid    = element_blank(),
    axis.text.x   = element_text(size = 13, color = "#000000"),
    axis.title.x  = element_text(size = 14, color = "#000000", face = "bold")
  )

ggsave("Figure1_forest.png", p1,
       width = 20, height = 20, dpi = 300, bg = "white")
cat("Saved: Figure1_forest.png\n")

if (!exists("dat_r05") || !exists("res_r05")) {
  candidates <- c("meta_analiza_IPC_v3.R","~/Downloads/meta_analiza_IPC_v3.R",
                  file.path(getwd(),"meta_analiza_IPC_v3.R"))
  found <- Filter(file.exists, candidates)
  if (!length(found)) stop("Source meta_analiza_IPC_v3.R first.")
  source(found[1])
}
options(lifecycle_verbosity="quiet")
for (p in c("ggplot2","metafor","scales")) {
  if (!requireNamespace(p,quietly=TRUE)) install.packages(p)
  library(p,character.only=TRUE)
}

FONT <- "Palatino"
PAL  <- c("Time"="#1f6fad","Power/Work"="#27875a",
          "Repetitions"="#b03535","Distance"="#6b3587",
          "Overall"="#1a1a2e","OVERALL"="#1a1a2e")

theme_pub <- function(base=12) {
  theme_classic(base_size=base, base_family=FONT) +
  theme(plot.title=element_blank(), plot.subtitle=element_blank(),
        plot.caption=element_blank(),
        plot.background=element_rect(fill="white",color=NA),
        panel.background=element_rect(fill="white",color=NA),
        axis.line=element_line(color="#444",linewidth=0.4),
        axis.ticks=element_line(color="#444",linewidth=0.3),
        axis.text=element_text(color="#333"),
        axis.title=element_text(face="bold",color="#000000"),
        strip.background=element_blank(),
        strip.text=element_text(face="bold",size=base,hjust=0),
        panel.grid.major=element_line(color="#f0f0f0",linewidth=0.35),
        panel.grid.minor=element_blank(),
        legend.position="none",
        plot.margin=margin(14,18,12,14))
}

groups <- c("Time","Power/Work")
g_ov   <- coef(res_r05[["OVERALL"]]$model)
ci_lo  <- res_r05[["OVERALL"]]$model$ci.lb
ci_hi  <- res_r05[["OVERALL"]]$model$ci.ub

# =============================================================================
# FIGURE 1 — Forest plot
# =============================================================================

# =============================================================================
# FIGURE 1 — Forest plot  (clean, minimal, Palatino, black text)
# Style: metafor base R — handles all placement automatically
# =============================================================================

all_grps <- c("Time","Power/Work","Repetitions","Distance")
grp_cols <- c("Time"="#1f6fad","Power/Work"="#27875a",
               "Repetitions"="#b03535","Distance"="#6b3587")

# Assign colors to each study row
study_cols <- grp_cols[dat_r05$outcome_group]

# Row positions per group (bottom to top)
# Distance(2) | Reps(3) | Power(11) | Time(11)
# with gaps between groups
n_dist <- sum(dat_r05$outcome_group=="Distance")
n_reps <- sum(dat_r05$outcome_group=="Repetitions")
n_pow  <- sum(dat_r05$outcome_group=="Power/Work")
n_tim  <- sum(dat_r05$outcome_group=="Time")

row_dist <- 2:(1+n_dist)
row_reps <- (max(row_dist)+3):(max(row_dist)+2+n_reps)
row_pow  <- (max(row_reps)+3):(max(row_reps)+2+n_pow)
row_tim  <- (max(row_pow)+3):(max(row_pow)+2+n_tim)

rows_all <- c(row_dist, row_reps, row_pow, row_tim)
top_row  <- max(row_tim) + 4   # space for overall

png("Figure1_forest.png", width=3200, height=3400, res=220,
    bg="white")

par(family=FONT, bg="white",
    mar=c(5, 0, 3, 0),
    col.axis="#000000", col.lab="#000000",
    cex.axis=1.2, cex.lab=1.4, font.lab=2)

forest(
  res_r05[["OVERALL"]]$model,
  ylim     = c(-3, top_row + 3),
  rows     = rows_all,
  slab     = dat_r05$study,
  xlab     = "Hedges' g  (positive = IPC favoured)",
  header   = c("Study", "g [95% CI]"),
  mlab     = sprintf("Overall  (k = 27,  g = %.2f,  I\u00b2 = %.0f%%)",
                     coef(res_r05[["OVERALL"]]$model),
                     res_r05[["OVERALL"]]$model$I2),
  col      = study_cols,
  border   = study_cols,
  pch      = 15,
  cex      = 1.2,
  cex.lab  = 1.4,
  efac     = c(0.7, 1.3),
  fonts    = c(FONT, FONT),
  refline  = 0,
  lty.refline = 2
)

# ── Pooled diamonds — all 4 groups ───────────────────────────────────────────
addpoly(res_r05[["Time"]]$model,
        row=min(row_tim)-1.5, cex=1.1, efac=0.9,
        col=grp_cols["Time"], border=grp_cols["Time"],
        mlab=sprintf("Pooled Time  (k = 11,  g = %.2f,  I\u00b2 = %.0f%%)",
                     coef(res_r05[["Time"]]$model),
                     res_r05[["Time"]]$model$I2))

addpoly(res_r05[["Power/Work"]]$model,
        row=min(row_pow)-1.5, cex=1.1, efac=0.9,
        col=grp_cols["Power/Work"], border=grp_cols["Power/Work"],
        mlab=sprintf("Pooled Power/Work  (k = 11,  g = %.2f,  I\u00b2 = %.0f%%)",
                     coef(res_r05[["Power/Work"]]$model),
                     res_r05[["Power/Work"]]$model$I2))

m_reps <- rma(yi, vi,
  data=dat_r05[dat_r05$outcome_group=="Repetitions",], method="DL")
addpoly(m_reps,
        row=min(row_reps)-1.5, cex=1.1, efac=0.9,
        col=grp_cols["Repetitions"], border=grp_cols["Repetitions"],
        mlab=sprintf("Pooled Repetitions  (k = 3,  g = %.2f)*",
                     coef(m_reps)))

m_dist <- rma(yi, vi,
  data=dat_r05[dat_r05$outcome_group=="Distance",], method="DL")
addpoly(m_dist,
        row=min(row_dist)-1.5, cex=1.1, efac=0.9,
        col=grp_cols["Distance"], border=grp_cols["Distance"],
        mlab=sprintf("Pooled Distance  (k = 2,  g = %.2f)*",
                     coef(m_dist)))

# ── Group headers (colored, bold, larger) ────────────────────────────────────
text(-8.5, max(row_tim)+2.0,  "TIME",
     pos=4, font=2, cex=1.3, col=grp_cols["Time"])
text(-8.5, max(row_pow)+2.0,  "POWER / WORK",
     pos=4, font=2, cex=1.3, col=grp_cols["Power/Work"])
text(-8.5, max(row_reps)+2.0, "REPETITIONS",
     pos=4, font=2, cex=1.3, col=grp_cols["Repetitions"])
text(-8.5, max(row_dist)+2.0, "DISTANCE",
     pos=4, font=2, cex=1.3, col=grp_cols["Distance"])

# ── Footnote ─────────────────────────────────────────────────────────────────
text(-8.5, -2.2,
     "* Not included in primary pooled analysis (k < 3 or I\u00b2 > 75%); shown for reference.",
     pos=4, font=3, cex=0.95, col="#333333")

dev.off()
cat("Saved: Figure1_forest.png\n")



# =============================================================================
# FIGURE 2 — Sensitivity  (labels above CI bars)
# =============================================================================

sens_rows <- list()
for (g in c("OVERALL","Time","Power/Work")) {
  for (rv in c("r = 0.5","r = 0.7","r = 0.9")) {
    res <- switch(rv,"r = 0.5"=res_r05,"r = 0.7"=res_r07,"r = 0.9"=res_r09)
    if (!is.null(res[[g]])) {
      m <- res[[g]]$model
      sens_rows[[paste(g,rv)]] <- data.frame(
        group=g,r=rv,g=coef(m),ci_lb=m$ci.lb,ci_ub=m$ci.ub,
        stringsAsFactors=FALSE)
    }
  }
}
sens       <- do.call(rbind,sens_rows)
sens$r     <- factor(sens$r,levels=c("r = 0.5","r = 0.7","r = 0.9"))
sens$group <- factor(sens$group,levels=c("OVERALL","Time","Power/Work"))
sens$col   <- PAL[as.character(sens$group)]

p2 <- ggplot(sens,aes(x=r,y=g,group=1)) +
  geom_hline(yintercept=0,color="#ddd",linewidth=.4) +
  geom_errorbar(aes(ymin=ci_lb,ymax=ci_ub,color=col),width=.08,linewidth=1.1) +
  geom_line(aes(color=col),linewidth=.6,alpha=.4) +
  geom_point(aes(color=col),size=3.5,shape=19) +
  geom_text(aes(y=ci_ub+0.07, label=sprintf("%.2f",g), color=col),
            vjust=0, hjust=.5, size=3.2, family=FONT, na.rm=TRUE) +
  scale_color_identity() +
  facet_wrap(~group,nrow=1) +
  scale_y_continuous(name="Hedges' g",breaks=seq(-.25,1.0,.25),limits=c(-.35,1.15)) +
  scale_x_discrete(name="Assumed correlation (r)") +
  theme_pub(11) +
  theme(panel.spacing=unit(1.4,"lines"),
        axis.text.x=element_text(size=9,angle=10,hjust=1),
        panel.grid.major.x=element_blank())

ggsave("Figure2_sensitivity.png",p2,width=10,height=5,dpi=300,bg="white")
cat("Saved: Figure2_sensitivity.png\n")

# =============================================================================
# FIGURE 3 — Funnel plot
# =============================================================================

m_ov <- res_r05[["OVERALL"]]$model
png("Figure3_funnel.png",width=1200,height=1000,res=180)
par(family=FONT,bg="white",mar=c(5,5,2,2),
    col.axis="#333",col.lab="#111",cex.axis=.9,cex.lab=1.0)
funnel(m_ov,xlab="Hedges' g",ylab="Standard Error",
       pch=21,col="#1f6fad",bg="#cce0f5",cex=1.1,
       shade=c("white","#e8f4fb","#c5e3f5"),back="white")
dev.off()
cat("Saved: Figure3_funnel.png\n")

# =============================================================================
# FIGURE 4 — Moderators
# =============================================================================

safe_pool <- function(d) {
  if (nrow(d)<2) return(NULL)
  m <- rma(yi,vi,data=d,method="DL")
  data.frame(g=coef(m),ci_lb=m$ci.lb,ci_ub=m$ci.ub,k=m$k)
}
dat_mod <- dat_r05[dat_r05$outcome_group %in% c("Time","Power/Work"),]
make_mod <- function(var,lvls,labs,name) {
  do.call(rbind,lapply(seq_along(lvls),function(i) {
    r <- safe_pool(dat_mod[dat_mod[[var]]==lvls[i],])
    if (is.null(r)) return(NULL)
    cbind(moderator=name,level=labs[i],r,stringsAsFactors=FALSE)
  }))
}
mod4 <- rbind(
  make_mod("ipc_location",c("Arms","Legs"),c("Arms","Legs"),"IPC Location"),
  make_mod("comparator",c("CTRL","SHAM"),c("True control","Sham"),"Comparator"),
  make_mod("pressure_binary",c("Fixed","Individualized"),
           c("Fixed","Individualized"),"Cuff Pressure"),
  do.call(rbind,lapply(c(1,3,4),function(cyc) {
    r <- safe_pool(dat_mod[dat_mod$n_cycles==cyc,])
    if (is.null(r)) return(NULL)
    cbind(moderator="No. of Cycles",level=paste0(cyc,"\u00d75 min"),r,
          stringsAsFactors=FALSE)
  }))
)
mod4$g     <- as.numeric(mod4$g)
mod4$ci_lb <- as.numeric(mod4$ci_lb)
mod4$ci_ub <- as.numeric(mod4$ci_ub)
mod4$k     <- as.integer(mod4$k)
mod4$moderator <- factor(mod4$moderator,
  levels=c("IPC Location","Comparator","Cuff Pressure","No. of Cycles"))
pcol <- c("IPC Location"="#1f6fad","Comparator"="#27875a",
          "Cuff Pressure"="#b03535","No. of Cycles"="#6b3587")
mod4$col    <- pcol[as.character(mod4$moderator)]
mod4$ylabel <- paste0(mod4$level,"  (k = ",mod4$k,")")

p4 <- ggplot(mod4,aes(x=g,y=reorder(ylabel,g),color=col)) +
  annotate("rect",xmin=ci_lo,xmax=ci_hi,ymin=-Inf,ymax=Inf,
           fill="#f5f5f5",color=NA) +
  geom_vline(xintercept=g_ov,color="#aaa",linewidth=.5,linetype="dashed") +
  geom_vline(xintercept=0,color="#ddd",linewidth=.4) +
  geom_errorbar(aes(xmin=ci_lb,xmax=ci_ub),width=.25,linewidth=1.3,
                orientation="y") +
  geom_point(size=4.5,shape=21,fill="white",stroke=1.6) +
  scale_color_identity() +
  facet_wrap(~moderator,scales="free",ncol=2) +
  scale_x_continuous(name="Hedges' g") +
  scale_y_discrete(name=NULL) +
  theme_pub(12) +
  theme(panel.spacing=unit(1.8,"lines"),axis.text.y=element_text(size=11))

ggsave("Figure4_moderators.png",p4,width=11,height=7,dpi=300,bg="white")
cat("Saved: Figure4_moderators.png\n")
cat("\n\u2713 Done — Figure1_forest.png, Figure2_sensitivity.png, Figure3_funnel.png, Figure4_moderators.png\n")

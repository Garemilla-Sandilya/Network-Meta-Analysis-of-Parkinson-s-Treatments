# ============================================================
# Parkinson’s Disease Network Meta-Analysis Project
# Clean, reproducible, GitHub-ready analysis
# ============================================================

# ---------------------------
# 0) Packages
# ---------------------------
library(NeuroDataSets)
library(tidyverse)
library(netmeta)
library(ggplot2)
library(patchwork)

# ---------------------------
# 1) Load data
# ---------------------------
data("parkinsons_dopamine_list")
d <- parkinsons_dopamine_list

pd <- tibble(
  study   = as.character(d$Study),
  treat   = d$Treat,
  outcome = d$Outcomes,
  se      = d$SE
)

# Treatment order
treat_levels <- unique(d$Treat.order)
pd <- pd %>%
  mutate(treat = factor(treat, levels = treat_levels))

cat("Studies:", n_distinct(pd$study), "\n")
cat("Treatments:", n_distinct(pd$treat), "\n")

# ---------------------------
# 2) Add CI
# ---------------------------
pd <- pd %>%
  mutate(
    ci_lo = outcome - 1.96 * se,
    ci_hi = outcome + 1.96 * se
  )

# ---------------------------
# 3) Plot A: Arm-level effects
# ---------------------------
pal <- c(
  "Placebo"       = "#B4B2A9",
  "Pramipexole"   = "#7F77DD",
  "Ropinirole"    = "#D85A30",
  "Bromocriptine" = "#1D9E75",
  "Cabergoline"   = "#BA7517"
)

pA <- ggplot(pd, aes(outcome, reorder(treat, outcome), color = treat)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(xmin = ci_lo, xmax = ci_hi),
                height = 0.25, orientation = "y") +
  geom_point(size = 3) +
  scale_color_manual(values = pal) +
  facet_wrap(~study, ncol = 1,
             labeller = labeller(.default = label_both)) +
  theme_minimal()

# ---------------------------
# 4) Plot B: Funnel-style
# ---------------------------
pB <- ggplot(pd, aes(abs(outcome), se, color = treat)) +
  geom_point(size = 3) +
  scale_color_manual(values = pal) +
  theme_minimal()

print(pA | pB)

# ---------------------------
# 5) Build ALL pairwise contrasts (FIXED)
# ---------------------------
make_pw <- function(df) {
  combs <- combn(nrow(df), 2, simplify = FALSE)
  
  bind_rows(lapply(combs, function(i) {
    tibble(
      study  = df$study[1],
      treat1 = df$treat[i[1]],
      treat2 = df$treat[i[2]],
      TE     = df$outcome[i[1]] - df$outcome[i[2]],
      seTE   = sqrt(df$se[i[1]]^2 + df$se[i[2]]^2)
    )
  }))
}

nma_input <- pd %>%
  group_by(study) %>%
  group_modify(~make_pw(.x)) %>%
  ungroup() %>%
  mutate(studlab = paste0("Study_", study))

# ---------------------------
# 6) Check structure
# ---------------------------
check <- nma_input %>%
  group_by(studlab) %>%
  summarise(
    arms = n_distinct(c(treat1, treat2)),
    comparisons = n(),
    expected = arms * (arms - 1) / 2
  )

print(check)

# ---------------------------
# 7) Run Network Meta-Analysis
# ---------------------------
net1 <- netmeta(
  TE, seTE,
  treat1, treat2,
  studlab,
  data = nma_input,
  sm = "SMD",
  reference.group = "Placebo",
  random = TRUE
)

summary(net1)

# ---------------------------
# 8) Forest plot
# ---------------------------
forest(net1,
       reference.group = "Placebo",
       smlab = "SMD vs Placebo")

# ---------------------------
# 9) Network graph
# ---------------------------
netgraph(net1,
         points = TRUE,
         col.points = pal[names(pal) %in% net1$trts])

# ---------------------------
# 10) Ranking (P-scores)
# ---------------------------
rank <- netrank(net1, small.values = "good")

rank_tbl <- tibble(
  Treatment = names(rank$Pscore.random),
  P_score = rank$Pscore.random
) %>%
  arrange(desc(P_score)) %>%
  mutate(rank = row_number())

print(rank_tbl)

# ---------------------------
# 11) ADDITIONAL ANALYSIS
# ---------------------------

# 🔹 A) Leave-one-out sensitivity
loo_results <- lapply(unique(nma_input$studlab), function(s) {
  netmeta(
    TE, seTE,
    treat1, treat2,
    studlab,
    data = subset(nma_input, studlab != s),
    sm = "SMD",
    random = TRUE
  )
})

cat("Leave-one-out analysis completed\n")

# 🔹 B) Heterogeneity
cat("Tau^2:", net1$tau^2, "\n")

# 🔹 C) Inconsistency
try(print(netsplit(net1)))

# 🔹 D) Ranking plot
ggplot(rank_tbl, aes(reorder(Treatment, P_score), P_score)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Treatment Ranking")

# ---------------------------
# 12) Save outputs
# ---------------------------
dir.create("outputs", showWarnings = FALSE)

write_csv(pd, "outputs/arm_data.csv")
write_csv(nma_input, "outputs/nma_input.csv")
write_csv(rank_tbl, "outputs/ranking.csv")

pdf("outputs/results.pdf")
print(pA)
print(pB)
forest(net1)
netgraph(net1)
dev.off()

cat("All results saved in /outputs folder\n")

# ============================================================
# ADVANCED ANALYSES FOR NETWORK META-ANALYSIS
# ============================================================

cat("\n================ ADVANCED ANALYSIS ================\n")

# ---------------------------
# 1) LEAGUE TABLE
# ---------------------------
cat("\n--- League Table ---\n")

league <- netleague(
  net1,
  digits = 2,
  bracket = "[",
  common = FALSE
)

print(league)

# Save league table
capture.output(league, file = "outputs/league_table.txt")


# ---------------------------
# 2) RANKING PLOT (P-scores)
# ---------------------------
cat("\n--- Ranking Plot ---\n")

rank <- netrank(net1, small.values = "good")

rank_tbl <- tibble(
  Treatment = names(rank$Pscore.random),
  P_score = as.numeric(rank$Pscore.random)
) %>%
  arrange(desc(P_score))

print(rank_tbl)

p_rank <- ggplot(rank_tbl,
                 aes(x = reorder(Treatment, P_score), y = P_score)) +
  geom_col(fill = "#4C72B0") +
  coord_flip() +
  labs(
    title = "Treatment Ranking (P-scores)",
    x = "",
    y = "P-score"
  ) +
  theme_minimal()

print(p_rank)
ggsave("outputs/ranking_plot.png", p_rank, width = 6, height = 4)


# ---------------------------
# 3) FUNNEL PLOT (Publication bias)
# ---------------------------
cat("\n--- Funnel Plot ---\n")

png("outputs/funnel_plot.png", width = 600, height = 500)

funnel(
  net1,
  order = net1$trts   # uses all treatments in correct order
)

dev.off()

# ---------------------------
# 4) INCONSISTENCY (NETSPLIT)
# ---------------------------
cat("\n--- Netsplit (Direct vs Indirect) ---\n")

netsplit_df <- as.data.frame(netsplit_res$random)

write.csv(
  netsplit_df,
  "outputs/netsplit_results.csv",
  row.names = FALSE
)


# ---------------------------
# 5) LEAVE-ONE-OUT ANALYSIS
# ---------------------------
cat("\n--- Leave-One-Out Analysis ---\n")

studies <- unique(nma_input$studlab)

loo_results <- lapply(studies, function(s) {
  model <- netmeta(
    TE, seTE,
    treat1, treat2,
    studlab,
    data = subset(nma_input, studlab != s),
    sm = "SMD",
    random = TRUE
  )
  
  tibble(
    removed_study = s,
    Pramipexole = model$TE.random["Pramipexole"],
    Cabergoline = model$TE.random["Cabergoline"],
    Bromocriptine = model$TE.random["Bromocriptine"],
    Ropinirole = model$TE.random["Ropinirole"]
  )
})

loo_df <- bind_rows(loo_results)

print(loo_df)

write_csv(loo_df, "outputs/leave_one_out_results.csv")


# ---------------------------
# 6) HETEROGENEITY SUMMARY
# ---------------------------
cat("\n--- Heterogeneity ---\n")

cat("Tau^2:", net1$tau^2, "\n")
cat("I^2:", net1$I2, "\n")

heterogeneity <- tibble(
  tau2 = net1$tau^2,
  I2 = net1$I2
)

write_csv(heterogeneity, "outputs/heterogeneity.csv")


# ---------------------------
# DONE
# ---------------------------
cat("\nAll advanced analyses completed and saved in /outputs\n")

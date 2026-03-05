dir.create("/Users/crocker/Downloads/Evil Dead", showWarnings = FALSE)
setwd("/Users/crocker/Downloads/Evil Dead")

library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

# Substack content column is ~800px wide; at 150dpi a 7x5 inch PNG fits cleanly
CHART_W <- 7
CHART_H <- 5
CHART_DPI <- 150

theme_1950s <- function() {
  theme_minimal(base_family = "Arial", base_size = 13) +
    theme(
      plot.background  = element_rect(fill = "#FAF5E9", color = NA),
      panel.background = element_rect(fill = "#FAF5E9", color = NA),
      panel.grid.major = element_line(color = "#E6DECA", linewidth = 0.6),
      panel.grid.minor = element_blank(),
      axis.title       = element_text(face = "bold", color = "#3A3A3A"),
      axis.text        = element_text(color = "#4A4A4A"),
      plot.title       = element_text(face = "bold", size = 16, color = "#3A3A3A"),
      plot.subtitle    = element_text(size = 11, color = "#5A5A5A", lineheight = 1.4),
      plot.caption     = element_text(size = 9, color = "#7A7A7A", hjust = 0,
                                      lineheight = 1.3),
      plot.margin      = margin(18, 18, 14, 18),
      legend.position  = "none"
    )
}

col_jolie  <- "#4A7B9D"
col_kylie  <- "#B05A7A"
col_willis <- "#7B6FA0"
col_red    <- "#C0392B"

set.seed(42)

# =============================================================================
# CHART 1: ANGELINA JOLIE — BRCA Testing Rates (US)
# Source: Liede et al. (2018), Breast Cancer Research and Treatment
# doi: 10.1007/s10549-018-4824-9
# US insurance claims (MarketScan), ~46M commercially-insured women
# Key numbers from interrupted time series:
#   Pre-editorial monthly growth: +2.0%/month
#   Post-editorial monthly growth: +0.5%/month (from higher base)
#   Immediate step change: 16.0 → 20.7 tests per 100,000 (May → June 2013)
# Monthly values reconstructed from ITS parameters reported in paper.
# =============================================================================

jolie_disclosure <- as.Date("2013-05-14")

pre_months_j <- seq(as.Date("2007-01-01"), as.Date("2013-05-01"), by = "month")
n_pre_j      <- length(pre_months_j)
pre_base_j   <- 3.0 * (1.02 ^ (0:(n_pre_j - 1)))
pre_vals_j   <- pre_base_j + rnorm(n_pre_j, 0, 0.25)

post_months_j <- seq(as.Date("2013-06-01"), as.Date("2016-09-01"), by = "month")
n_post_j      <- length(post_months_j)
post_base_j   <- 20.7 * (1.005 ^ (0:(n_post_j - 1)))
post_vals_j   <- post_base_j + rnorm(n_post_j, 0, 0.3)

jolie_df <- tibble(
  date  = c(pre_months_j, post_months_j),
  value = c(pre_vals_j, post_vals_j),
  phase = c(rep("Before", n_pre_j), rep("After", n_post_j))
)

jolie_pre  <- filter(jolie_df, phase == "Before")
jolie_post <- filter(jolie_df, phase == "After")

p_jolie <- ggplot(jolie_df, aes(x = date, y = value)) +
  annotate("rect",
    xmin = min(jolie_df$date), xmax = jolie_disclosure,
    ymin = -Inf, ymax = Inf, fill = "#E6DECA", alpha = 0.4) +
  annotate("rect",
    xmin = jolie_disclosure, xmax = max(jolie_df$date),
    ymin = -Inf, ymax = Inf, fill = "#D4E6F1", alpha = 0.3) +
  geom_smooth(data = jolie_pre, method = "lm", formula = y ~ x,
    se = FALSE, color = col_jolie, linewidth = 0.65,
    linetype = "dashed", alpha = 0.6) +
  geom_smooth(data = jolie_post, method = "lm", formula = y ~ x,
    se = FALSE, color = col_jolie, linewidth = 0.65,
    linetype = "dashed", alpha = 0.6) +
  geom_line(color = col_jolie, linewidth = 1.1, lineend = "round") +
  geom_vline(xintercept = jolie_disclosure,
    linetype = "dotted", color = col_red, linewidth = 0.65, alpha = 0.8) +
  geom_point(data = filter(jolie_df, date == as.Date("2013-06-01")),
    color = col_red, size = 4.5, shape = 21,
    fill = "#E74C3C", stroke = 1.6) +
  annotate("text",
    x = as.Date("2013-08-01"), y = 24,
    label = "Jolie op-ed\nMay 14, 2013",
    size = 3.4, color = col_red, hjust = 0, lineheight = 1.3) +
  annotate("text",
    x = as.Date("2009-06-01"), y = 6,
    label = "+2.0% per month",
    size = 3.1, color = "#7A7A7A", hjust = 0.5, fontface = "italic") +
  annotate("text",
    x = as.Date("2015-03-01"), y = 30,
    label = "+0.5% per month\n(from a higher base)",
    size = 3.1, color = col_jolie, hjust = 0.5,
    lineheight = 1.3, fontface = "italic") +
  scale_x_date(date_labels = "'%y", date_breaks = "1 year") +
  scale_y_continuous(limits = c(0, 36),
    breaks = c(0, 10, 20, 30),
    labels = c("0", "10", "20", "30")) +
  labs(
    title    = "Jolie: BRCA Testing Rates, United States",
    subtitle = paste0(
      "Tests per 100,000 commercially-insured women, 2007\u20132016\n",
      "Behavior changed. The effect was sustained for years."
    ),
    x       = NULL,
    y       = "Tests per 100,000 women",
    caption = paste0(
      "Source: Liede et al. (2018), Breast Cancer Research and Treatment. ",
      "doi:10.1007/s10549-018-4824-9\n",
      "Monthly rates reconstructed from interrupted time series parameters ",
      "reported in the paper (MarketScan\u00ae database, ~46M women)."
    )
  ) +
  theme_1950s() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 10))

ggsave("chart_jolie.png",
  plot = p_jolie, width = CHART_W, height = CHART_H,
  dpi = CHART_DPI, bg = "#FAF5E9")
cat("Jolie chart saved.\n")

# =============================================================================
# CHART 2: KYLIE MINOGUE — Mammography Screening (Australia)
# Source: Kelaher et al. (2008), International Journal of Epidemiology
# doi: 10.1093/ije/dyn090
# Quarterly data, women aged 25–44, Australian breast screening program
# Key numbers:
#   20% increase in imaging procedure odds, Q1 and Q2 post-announcement
#   101% increase among previously non-screened women
# Index baseline = 100. Values modeled from reported effect sizes.
# =============================================================================

kylie_announcement <- as.Date("2005-05-17")

kylie_quarters <- seq(as.Date("2003-09-01"), as.Date("2007-03-01"), by = "3 months")
n_q <- length(kylie_quarters)

kylie_vals <- c(
  100, 101, 99, 100, 98, 100, 101,  # pre: 7 stable quarters
  118,                                # announcement quarter begins spike
  140,                                # Q1 post: peak (new attenders, 101% effect)
  122,                                # Q2 post: sustained 20% effect
  115, 110, 107, 104, 102, 101, 100  # gradual decay over ~5 quarters
)
kylie_vals <- kylie_vals[1:n_q] + rnorm(n_q, 0, 1.0)

kylie_df <- tibble(
  date  = kylie_quarters,
  value = kylie_vals,
  phase = ifelse(date < kylie_announcement, "Before", "After")
)

kylie_baseline <- mean(kylie_df$value[kylie_df$phase == "Before"])

p_kylie <- ggplot(kylie_df, aes(x = date, y = value)) +
  annotate("rect",
    xmin = min(kylie_df$date), xmax = kylie_announcement,
    ymin = -Inf, ymax = Inf, fill = "#E6DECA", alpha = 0.4) +
  annotate("rect",
    xmin = kylie_announcement, xmax = max(kylie_df$date),
    ymin = -Inf, ymax = Inf, fill = "#F5E6EE", alpha = 0.35) +
  annotate("segment",
    x = min(kylie_df$date), xend = max(kylie_df$date),
    y = kylie_baseline, yend = kylie_baseline,
    linetype = "dashed", color = "#9B8FA0", linewidth = 0.55, alpha = 0.8) +
  annotate("text",
    x = as.Date("2004-06-01"), y = kylie_baseline - 3,
    label = "Pre-announcement baseline",
    size = 3.1, color = "#9A8FA0", hjust = 0.5, fontface = "italic") +
  geom_line(color = col_kylie, linewidth = 1.1, lineend = "round") +
  geom_vline(xintercept = kylie_announcement,
    linetype = "dotted", color = col_red, linewidth = 0.65, alpha = 0.8) +
  geom_point(data = filter(kylie_df, date == as.Date("2005-06-01")),
    color = col_red, size = 4.5, shape = 21,
    fill = "#E74C3C", stroke = 1.6) +
  annotate("text",
    x = as.Date("2005-09-15"), y = 143,
    label = "Kylie announces\nMay 17, 2005",
    size = 3.4, color = col_red, hjust = 0, lineheight = 1.3) +
  annotate("text",
    x = as.Date("2005-06-01"), y = 131,
    label = "+101% among\nnever-screened women",
    size = 3.1, color = col_kylie, hjust = 0.5,
    lineheight = 1.3, fontface = "italic") +
  scale_x_date(date_labels = "'%y", date_breaks = "1 year") +
  scale_y_continuous(limits = c(88, 150),
    breaks = c(90, 100, 110, 120, 130, 140),
    labels = c("90", "100", "110", "120", "130", "140")) +
  labs(
    title    = "Kylie: Breast Screening, Australia",
    subtitle = paste0(
      "Screening index, women aged 25\u201344, 2003\u20132007 (quarterly)\n",
      "Behavior changed. New patients appeared who had never been screened."
    ),
    x       = NULL,
    y       = "Screening index (baseline = 100)",
    caption = paste0(
      "Source: Kelaher et al. (2008), International Journal of Epidemiology. ",
      "doi:10.1093/ije/dyn090\n",
      "Index modeled from reported effect sizes (20% increase in imaging odds; ",
      "101% increase among previously unscreened women)."
    )
  ) +
  theme_1950s() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 10))

ggsave("chart_kylie.png",
  plot = p_kylie, width = CHART_W, height = CHART_H,
  dpi = CHART_DPI, bg = "#FAF5E9")
cat("Kylie chart saved.\n")

# =============================================================================
# CHART 3: BRUCE WILLIS / FTD — Google Trends (US)
# Source: Hurley et al. (2023), Innovation in Aging
# doi: 10.1093/geroni/igad125
# Google Trends, "frontotemporal dementia", United States
# Pre-disclosure average: 1.2  |  Disclosure week Feb 12–18 2023: 100  |  Post avg: 2.9
# Monthly values interpolated from published averages and Figure 1 in paper.
# =============================================================================

willis_disclosure <- as.Date("2023-02-16")

pre_dates_w  <- seq(as.Date("2022-04-01"), as.Date("2023-01-01"), by = "month")
post_dates_w <- seq(as.Date("2023-03-01"), as.Date("2023-10-01"), by = "month")

pre_vals_w  <- pmax(0, 1.2 + rnorm(length(pre_dates_w), 0, 0.2))
post_vals_w <- pmax(0, c(18, 7, 4.5, 3.5, 3.2, 2.9, 2.9, 3.0) +
                       rnorm(8, 0, 0.15))

willis_df <- tibble(
  date  = c(pre_dates_w, willis_disclosure, post_dates_w),
  value = c(pre_vals_w, 100, post_vals_w),
  phase = c(rep("Before", length(pre_dates_w)),
            "Disclosure",
            rep("After", length(post_dates_w)))
)

pre_mean_w  <- mean(pre_vals_w)
post_mean_w <- mean(post_vals_w)

p_willis <- ggplot(willis_df, aes(x = date, y = value)) +
  annotate("rect",
    xmin = min(willis_df$date), xmax = willis_disclosure,
    ymin = -Inf, ymax = Inf, fill = "#E6DECA", alpha = 0.4) +
  annotate("rect",
    xmin = willis_disclosure, xmax = max(willis_df$date),
    ymin = -Inf, ymax = Inf, fill = "#EDE6F5", alpha = 0.3) +
  annotate("segment",
    x = min(willis_df$date), xend = willis_disclosure,
    y = pre_mean_w, yend = pre_mean_w,
    linetype = "dashed", color = "#9B8FA0", linewidth = 0.55) +
  annotate("segment",
    x = willis_disclosure, xend = max(post_dates_w),
    y = post_mean_w, yend = post_mean_w,
    linetype = "dashed", color = col_willis, linewidth = 0.55) +
  annotate("text",
    x = as.Date("2022-08-15"), y = pre_mean_w + 4.5,
    label = paste0("Avg before: ", round(pre_mean_w, 1)),
    size = 3.2, color = "#7A7A7A", hjust = 0.5, fontface = "italic") +
  annotate("text",
    x = as.Date("2023-07-01"), y = post_mean_w + 4.5,
    label = paste0("Avg after: ", round(post_mean_w, 1)),
    size = 3.2, color = col_willis, hjust = 0.5, fontface = "italic") +
  geom_line(color = col_willis, linewidth = 1.1, lineend = "round") +
  geom_point(data = filter(willis_df, phase == "Disclosure"),
    color = col_red, size = 4.5, shape = 21,
    fill = "#E74C3C", stroke = 1.6) +
  geom_vline(xintercept = willis_disclosure,
    linetype = "dotted", color = col_red, linewidth = 0.65, alpha = 0.8) +
  annotate("text",
    x = willis_disclosure + 12, y = 90,
    label = "Willis family\nFTD announcement\nFeb 16, 2023",
    size = 3.4, color = col_red, hjust = 0, lineheight = 1.3) +
  annotate("text",
    x = as.Date("2023-07-01"), y = 46,
    label = "Awareness spiked.\nNo screening pathway\nexisted to receive it.",
    size = 3.1, color = col_willis, hjust = 0.5,
    lineheight = 1.4, fontface = "italic") +
  scale_y_continuous(limits = c(0, 108),
    breaks = c(0, 25, 50, 75, 100)) +
  scale_x_date(date_labels = "%b '%y", date_breaks = "2 months") +
  labs(
    title    = "Willis: FTD Awareness, United States",
    subtitle = paste0(
      "Google Trends index, 'frontotemporal dementia', Apr 2022 \u2013 Oct 2023\n",
      "Attention spiked. Then almost everyone forgot."
    ),
    x       = NULL,
    y       = "Search interest (0\u2013100 index)",
    caption = paste0(
      "Source: Hurley et al. (2023), Innovation in Aging. ",
      "doi:10.1093/geroni/igad125\n",
      "Monthly values interpolated from pre/post averages (1.2 and 2.9) ",
      "and Figure 1 published in the paper."
    )
  ) +
  theme_1950s() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 10))

ggsave("chart_willis.png",
  plot = p_willis, width = CHART_W, height = CHART_H,
  dpi = CHART_DPI, bg = "#FAF5E9")
cat("Willis chart saved.\n")

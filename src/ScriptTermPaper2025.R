#DISCLAIMER!!!!!!!
#The final output can have tiny differences for the post-2008 models because
#the FRED data may have been revised between time of writing and the present
#time



############################################################
# 📦 Load packages
############################################################

library(quantmod)
library(tidyverse)
library(lubridate)
library(zoo)
library(readr)
library(writexl)
library(car)
library(mFilter)
library(stargazer)
library(modelsummary)
library(pandoc)
library(RCurl)
library(fredr)
library(xts)

############################################################
# 🔑 FRED key & helper
############################################################

# Replace with your own key if needed
fredr_set_key("6cb8d40b57cfd2b2ae1bcd5d541a86af")

fredr_getSymbols <- function(series_id,
                             from = as.Date("1900-01-01"),
                             to   = Sys.Date()) {
  if (!requireNamespace("fredr", quietly = TRUE))
    stop("Package 'fredr' is required.")
  if (!requireNamespace("xts", quietly = TRUE))
    stop("Package 'xts' is required.")
  
  data <- fredr::fredr(
    series_id        = series_id,
    observation_start = from,
    observation_end   = to
  )
  
  if (nrow(data) == 0) {
    warning(paste("No data found for", series_id))
    return(NULL)
  }
  
  ts_xts <- xts::xts(data$value, order.by = data$date)
  colnames(ts_xts) <- series_id
  return(ts_xts)
}

############################################################
# 📊 Step 1: Load and reduce FRED Data
############################################################

FEDFUNDS <- fredr_getSymbols("FEDFUNDS")
CPIAUCSL <- fredr_getSymbols("CPIAUCSL")
INDPRO   <- fredr_getSymbols("INDPRO")
WALCL    <- fredr_getSymbols("WALCL")
UNRATE   <- fredr_getSymbols("UNRATE")

# FEDFUNDS
df_fedfunds <- data.frame(
  date_raw = index(FEDFUNDS),
  rate     = as.numeric(coredata(FEDFUNDS))  # force numeric vector, name = "rate"
) %>%
  mutate(month = floor_date(date_raw, "month")) %>%
  group_by(month) %>%
  arrange(date_raw, .by_group = TRUE) %>%  # earliest day in each month
  slice(1) %>%
  ungroup() %>%
  transmute(date = month, rate = rate)



# CPI
df_inflation <- data.frame(
  date = index(CPIAUCSL),
  cpi  = coredata(CPIAUCSL)
) %>%
  rename(cpi = CPIAUCSL) %>%
  mutate(date = floor_date(date, "month")) %>%
  group_by(date) %>%
  summarise(cpi = mean(cpi, na.rm = TRUE), .groups = "drop")

# Industrial production
df_output <- data.frame(
  date    = index(INDPRO),
  indprod = coredata(INDPRO)
) %>%
  rename(indprod = INDPRO) %>%
  mutate(date = floor_date(date, "month")) %>%
  group_by(date) %>%
  summarise(indprod = mean(indprod, na.rm = TRUE), .groups = "drop")

# Fed balance sheet (end-of-month)
df_assets <- data.frame(
  date          = index(WALCL),
  balance_sheet = coredata(WALCL)
) %>%
  rename(balance_sheet = WALCL) %>%
  mutate(date = floor_date(date, "month")) %>%
  group_by(date) %>%
  filter(row_number() == n()) %>%
  ungroup()

# Unemployment rate
df_unrate <- data.frame(
  date  = index(UNRATE),
  unemp = coredata(UNRATE)
) %>%
  rename(unemp = UNRATE) %>%
  mutate(date = floor_date(date, "month")) %>%
  group_by(date) %>%
  summarise(unemp = mean(unemp, na.rm = TRUE), .groups = "drop")

############################################################
# 🧾 Step 1.5: Load disapproval data
############################################################

disapproval <- read_delim(
  "C:/Users/Timothy/Desktop/TermPaper Resurection/Disapproval_Rate.txt",  # <- update path if needed
  delim     = "\t",                       # tab-separated
  col_names = c("date", "disapproval_raw"),
  locale    = locale(decimal_mark = ","), # numbers use comma as decimal
  col_types = cols(
    date           = col_date(format = "%Y-%m-%d"),
    disapproval_raw = col_double()
  )
) %>%
  mutate(
    # just in case: align to month (no harm if already monthly)
    date = floor_date(date, "month")
  ) %>%
  group_by(date) %>%
  summarise(
    disapproval_rate = mean(disapproval_raw, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(date >= as.Date("1950-01-01"))

############################################################
# 🔄 Step 2: Merge datasets
############################################################

df <- df_fedfunds %>%
  full_join(df_inflation,  by = "date") %>%
  full_join(df_output,     by = "date") %>%
  full_join(df_assets,     by = "date") %>%
  full_join(disapproval,   by = "date") %>%
  full_join(df_unrate,     by = "date") %>%
  arrange(date)

############################################################
# 📉 Step 3: Compute Gaps
############################################################

# Inflation YoY (annualized, in %)
df <- df %>%
  mutate(
    inflation_yoy = 1200 * log(cpi / lag(cpi, 12)),
    # Original version most likely used a 2% target:
    inflation_gap = inflation_yoy
  )

# Output gap via HP filter (log INDPRO)
df <- df %>%
  arrange(date) %>%
  mutate(log_indprod = log(indprod)) %>%
  filter(!is.na(log_indprod))

hp_out <- hpfilter(df$log_indprod, freq = 14400)
df$output_gap <- as.numeric(hp_out$cycle)

############################################################
# 🗳️ Step 4: Add political variables
############################################################

# Election years & continuation indicator
elections <- tibble(
  year = c(1952, 1956, 1960, 1964, 1968,
           1972, 1976, 1980, 1984, 1988,
           1992, 1996, 2000, 2004, 2008,
           2012, 2016, 2020, 2024),
  admin_continued = c(
    FALSE, TRUE,  FALSE, TRUE,  TRUE,
    TRUE,  TRUE,  TRUE,  TRUE,  TRUE,
    TRUE,  TRUE,  TRUE,  TRUE,  TRUE,
    TRUE,  TRUE,  TRUE,  TRUE
  )
)

# Presidential terms
presidents <- tibble(
  start = as.Date(c(
    "1953-01-20", "1961-01-20", "1963-11-22", "1969-01-20", "1974-08-09",
    "1977-01-20", "1981-01-20", "1989-01-20", "1993-01-20", "2001-01-20",
    "2009-01-20", "2017-01-20", "2021-01-20"
  )),
  end = as.Date(c(
    "1961-01-20", "1963-11-22", "1969-01-20", "1974-08-09", "1977-01-20",
    "1981-01-20", "1989-01-20", "1993-01-20", "2001-01-20", "2009-01-20",
    "2017-01-20", "2021-01-20", "2025-01-20"
  )),
  party = c(
    "Republican", "Democrat", "Democrat", "Republican", "Republican",
    "Democrat", "Republican", "Republican", "Democrat", "Republican",
    "Democrat", "Republican", "Democrat"
  )
)

# Fed chairs
fed_chairs <- tibble(
  start = as.Date(c(
    "1951-04-01", "1970-02-01", "1978-08-06", "1979-08-06", "1987-08-11",
    "2006-02-01", "2014-02-03", "2018-02-05", "2022-02-01"
  )),
  end = as.Date(c(
    "1970-02-01", "1978-08-06", "1979-08-06", "1987-08-11", "2006-02-01",
    "2014-02-03", "2018-02-05", "2022-02-01", "2026-01-01"
  )),
  chair_party = c(
    "Democrat", "Republican", "Democrat", "Democrat",
    "Republican", "Republican", "Democrat", "Republican", "Democrat"
  )
)

set.seed(123)

df <- df %>%
  mutate(
    year               = year(date),
    month              = month(date),
    months_to_election = if_else(
      year %% 4 == 0 & month <= 11,
      11 - month,
      NA_real_
    ),
    proximity      = if_else(!is.na(months_to_election),
                             1 - months_to_election / 11, 0),
    fake_proximity = runif(n())
  ) %>%
  left_join(elections, by = "year") %>%
  mutate(
    admin_continued = if_else(is.na(admin_continued), FALSE, admin_continued),
    president_party = map_chr(date, function(d) {
      p <- presidents %>% filter(start <= d & end > d)
      if (nrow(p) == 0) NA_character_ else p$party
    }),
    fed_chair_party = map_chr(date, function(d) {
      f <- fed_chairs %>% filter(start <= d & end > d)
      if (nrow(f) == 0) NA_character_ else f$chair_party
    }),
    party_alignment = president_party == fed_chair_party
  ) %>%
  filter(date >= as.Date("1954-07-01") & date <= as.Date("2025-01-01"))

############################################################
# 📈 Step 5: Run Regime-Specific Models
############################################################

df <- df %>%
  mutate(
    regime = case_when(
      date <= as.Date("1979-10-06") ~ "pre_1979",
      date >  as.Date("1979-10-06") & date <= as.Date("2008-12-15") ~ "1979_2008",
      date >= as.Date("2008-12-16") ~ "post_2008"
    )
  )

# Era 1: pre_1979
regime1 <- df %>%
  filter(regime == "pre_1979") %>%
  drop_na(rate, inflation_gap, output_gap,
          disapproval_rate, unemp, party_alignment)

taylor_fit1 <- lm(rate ~ inflation_gap + output_gap, data = regime1)
regime1$residual_taylor1 <- residuals(taylor_fit1)

model1_1 <- lm(
  rate ~ inflation_gap + output_gap + proximity +
    admin_continued + disapproval_rate + unemp + party_alignment,
  data = regime1
)

model1_2 <- lm(
  residual_taylor1 ~ proximity + admin_continued +
    disapproval_rate + unemp + party_alignment,
  data = regime1
)

# Era 2: 1979_2008
regime2 <- df %>%
  filter(regime == "1979_2008") %>%
  drop_na(rate, inflation_gap, output_gap,
          disapproval_rate, unemp, party_alignment)

taylor_fit2 <- lm(rate ~ inflation_gap + output_gap, data = regime2)
regime2$residual_taylor2 <- residuals(taylor_fit2)

model2_1 <- lm(
  rate ~ inflation_gap + output_gap + proximity +
    admin_continued + disapproval_rate + unemp + party_alignment,
  data = regime2
)

model2_2 <- lm(
  residual_taylor2 ~ proximity + admin_continued +
    disapproval_rate + unemp + party_alignment,
  data = regime2
)

# Era 3: post_2008
regime3 <- df %>%
  filter(regime == "post_2008") %>%
  arrange(date) %>%
  mutate(
    log_bal      = log(balance_sheet),
    log_bal_diff = c(NA, diff(log_bal))
  ) %>%
  drop_na(log_bal_diff, inflation_gap, output_gap,
          disapproval_rate, unemp, party_alignment)

taylor_fit3 <- lm(rate ~ inflation_gap + output_gap, data = regime3)
regime3$residual_taylor3 <- residuals(taylor_fit3)

model3_1 <- lm(
  rate ~ inflation_gap + output_gap + proximity +
    admin_continued + disapproval_rate + unemp + party_alignment,
  data = regime3
)

model3_2 <- lm(
  residual_taylor3 ~ proximity + admin_continued +
    disapproval_rate + unemp + party_alignment,
  data = regime3
)

model3_3 <- lm(
  log_bal_diff ~ inflation_gap + output_gap + proximity +
    admin_continued + disapproval_rate + unemp + party_alignment,
  data = regime3
)

############################################################
# 🧪 Step 6: Placebo tests (commented in original)
############################################################

# model1_placebo <- lm(
#   rate ~ inflation_gap + output_gap + fake_proximity +
#     admin_continued + disapproval_rate + unemp + party_alignment,
#   data = regime1
# )
#
# model2_placebo <- lm(
#   residual_taylor2 ~ inflation_gap + output_gap + fake_proximity +
#     admin_continued + disapproval_rate + unemp + party_alignment,
#   data = regime2
# )
#
# model3_placebo <- lm(
#   log_bal_diff ~ inflation_gap + output_gap + fake_proximity +
#     admin_continued + disapproval_rate + unemp + party_alignment,
#   data = regime3
# )
#
# set.seed(11)
# df <- df %>%
#   mutate(fake_disapproval_rate = sample(disapproval_rate, replace = FALSE))
#
# regime1 <- regime1 %>%
#   mutate(fake_disapproval_rate = df$fake_disapproval_rate[match(date, df$date)])
#
# model1_placebo_pop <- lm(
#   rate ~ inflation_gap + output_gap + proximity + admin_continued +
#     fake_disapproval_rate + unemp + party_alignment,
#   data = regime1
# )
#
# regime2 <- regime2 %>%
#   mutate(fake_disapproval_rate = df$fake_disapproval_rate[match(date, df$date)])
#
# model2_placebo_pop <- lm(
#   residual_taylor2 ~ proximity + admin_continued +
#     fake_disapproval_rate + unemp + party_alignment,
#   data = regime2
# )
#
# regime3 <- regime3 %>%
#   mutate(fake_disapproval_rate = df$fake_disapproval_rate[match(date, df$date)])
#
# model3_placebo_pop <- lm(
#   log_bal_diff ~ proximity + admin_continued +
#     fake_disapproval_rate + unemp + party_alignment,
#   data = regime3
# )

############################################################
# ✅ Check uniqueness of time index
############################################################

if (any(duplicated(df$date))) {
  stop("❌ There are duplicated dates in the dataset. You must inspect and fix aggregation.")
} else {
  cat("✅ All dates are unique. The dataset is clean and suitable for regression.\n")
}

############################################################
# 🔍 Multicollinearity diagnostics
############################################################

vif(model1_1)
vif(model1_2)
vif(model2_1)
vif(model2_2)
vif(model3_1)
vif(model3_2)
vif(model3_3)

############################################################
# 📑 Regression table export
############################################################

modelsummary(
  list(
    "Pre-1979_1"   = model1_1,
    "Pre-1979_2"   = model1_2,
    "1979–2008_1"  = model2_1,
    "1979-2008_2"  = model2_2,
    "Post-2008_1"  = model3_1,
    "Post-2008_2"  = model3_2,
    "Post-2008_3"  = model3_3
  ),
  output = "regression_table.docx",
  stars  = TRUE
)


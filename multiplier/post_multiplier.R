#### STEP 0: Initialize workspace ----------------------------------------------
library(readxl)
library(magrittr)
library(zoo)
library(dplyr)
library(roll)
library(tibble)
library(ggplot2)

# We assume your current working directory is fim/. If not, please adjust 
# accordingly before proceeding with code. You can check by running:
# getwd()

#### STEP 1: Load & clean FIM data ---------------------------------------------
# We use 2024-04 FIM results here. Feel free to adjust according to your needs.
fim_raw <- read_xlsx("results/04-2024/fim-04-2024.xlsx",
                    range = "A1:HU260",
                    col_names = TRUE,
                    # The first column is a date column, the second column "id"
                    # has data with either "historic" or "forecast" and is text, 
                    # the rest are values. There are 229 columns total.
                    # If you try to read in date as a date, it produced NAs. So 
                    # these will be post-processed to be parsed as dates.
                    col_types = c("text", "text", rep("numeric", times = 227)))

data <- fim_raw[c("date", "id", "gdp", "real_gdp", "gdp_deflator", "fiscal_impact")] %>%
  # Format `date` as yearquarter data type
  mutate(date = as.yearqtr(date, format = "%Y Q%q")) %>%
  # Rename `gdp` as `nom_gdp` to better reflect is nature
  rename(nom_gdp = gdp) %>%
  # Convert fiscal_impact to a decimal from percent
  mutate(fiscal_impact = fiscal_impact/100)

#### STEP 2: Data transformation -----------------------------------------------
# 2.A: Define a function that calculates rolling MPCs (or multipliers: they both
# have the same arithmetic backbone)
mpc_lorae <- function (x, # A vector of cash disbursement data
                       mpc) # A vector of MPCs
{
  1 * roll::roll_sum(x, 
                     width = length(mpc), 
                     weights = rev(mpc), 
                     online = FALSE, 
                     min_obs = 1)
}

# 2.B: Calculate nominal fiscal impulse
data <- data %>%
  # Produce nominal nominal fiscal impulse (in $ B)
  mutate(nom_impulse = (nom_gdp*fiscal_impact) / (1 + fiscal_impact))

# 2.C: Define & calculate post-multiplier nominal fiscal impulse
# Define the multiplier as a vector. Based upon Table 2 from:
# https://www.cbo.gov/sites/default/files/114th-congress-2015-2016/workingpaper/49925-FiscalMultiplier_1.pdf
# Low multiplier scenario
mult_low <- c(0.5, -0.03, -0.04, -0.05, -0.06, -0.06, -0.05, -0.05) 
# High multiplier scenario
mult_high <- c(1.43, 0.48, 0.10, -0.10, -0.30, -0.28, -0.25, -0.25)

# Generate nominal post-multiplier time series
data <- data %>%
  mutate(
    # Generate col representing low multiplier scenario
    nom_post_multiplier_low = mpc_lorae(x = nom_impulse, mpc = mult_low),
    # Generate col representing high multiplier scenario
    nom_post_multiplier_high = mpc_lorae(x = nom_impulse, mpc = mult_high)
    )


# 2.D: Prepare data for graphing
data$date <- yq(data$date)
data_filtered <- data %>%
  filter(year(date) >= 2000 & year(date) <= 2025)

# 2.E: Graph nominal FIM, post multiplier

ggplot(data_filtered) +
  geom_line(aes(x = date, y = nom_post_multiplier_low, color = "Low")) +
  geom_point(aes(x = date, y = nom_post_multiplier_low, color = "Low")) +
  geom_line(aes(x = date, y = nom_post_multiplier_high, color = "High")) +
  geom_point(aes(x = date, y = nom_post_multiplier_high, color = "High")) +
  labs(title = "Nominal FIM, Post-Multiplier (2000-2025)",
       x = "Date",
       y = "Billions USD, Nominal",
       color = "Legend") +
  theme_minimal()

ggplot(data_filtered) +
  geom_ribbon(aes(x = date, ymin = nom_post_multiplier_low, ymax = nom_post_multiplier_high), fill = "grey70", alpha = 0.5) +
  geom_line(aes(x = date, y = nom_impulse), color = "black") +
  labs(title = "Nominal FIM, Post-Multiplier (2000-2025)",
       x = "Date",
       y = "Billions USD, Nominal",
       color = "Legend") +
  theme_minimal()

# 2.F: Convert to nominal FIM, post-multiplier to reals
data <- data %>%
  mutate(
    real_post_multiplier_low = nom_post_multiplier_low / (gdp_deflator/100),
    real_post_multiplier_high = nom_post_multiplier_high / (gdp_deflator/100),
    real_impulse = nom_impulse / (gdp_deflator/100)
  )

# 2.G: Graph real FIM, post multiplier
ggplot(data_filtered) +
  geom_line(aes(x = date, y = real_post_multiplier_low, color = "Low")) +
  geom_point(aes(x = date, y = real_post_multiplier_low, color = "Low")) +
  geom_line(aes(x = date, y = real_post_multiplier_high, color = "High")) +
  geom_point(aes(x = date, y = real_post_multiplier_high, color = "High")) +
  labs(title = "Real FIM, Post-Multiplier (2000-2025)",
       x = "Date",
       y = "Billions USD, Nominal",
       color = "Legend") +
  theme_minimal()

ggplot(data_filtered) +
  geom_ribbon(aes(x = date, ymin = real_post_multiplier_low, ymax = real_post_multiplier_high), fill = "grey70", alpha = 0.5) +
  geom_line(aes(x = date, y = real_impulse), color = "black") +
  labs(title = "Real FIM, Post-Multiplier (2000-2025)",
       x = "Date",
       y = "Billions USD, Real",
       color = "Legend") +
  theme_minimal()


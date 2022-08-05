# Data cleanup

# libraries
library(tidyverse)
library(magrittr)
library(readxl)
library(readr)

# read
week4 <- read_excel("WICVOCDataAll_080422.xlsx",
  guess_max = 20000
)

# Clean up ####
# numbers to numeric
week4 %<>%
  mutate(
    across(
      c(
        total_value_guldens,
        total_value_stuivers,
        total_value_penningen,
        textile_quantity
      ),
      ~ as.numeric(.x)
    )
  )

# Muslins
week4 %<>%
  mutate(
    textile_name = if_else(textile_name %in% c(
      "adathaies",
      "adathaies",
      "bethilles",
      "camcanys",
      "mallemolens",
      "sollogesjes",
      "tanjeebs"
    ),
    "muslin",
    textile_name
    )
  )

# convert NAs to 0
# will convert back to NAs after
week4 %<>%
  mutate(
    across(
      c(
        total_value_guldens,
        total_value_stuivers,
        total_value_penningen
      ),
      ~ replace_na(.x, 0)
    ),
    # total value calculations
    # in decimal guldens
    total_value = total_value_guldens +
      total_value_stuivers / 20 +
      total_value_penningen / 320,
    # els per ps.
    els_per_ps = parse_number(els_per_ps)
  )

# Currency ####
# 1 Indian Guilder = 0.7 Dutch Guilder
# It's Dutch when it only says "guilders"
week4 %<>%
  mutate(
    total_value = if_else(
      startsWith(total_value_currency, "Indian"),
      total_value * 0.7,
      total_value
    )
  )

# Geo ####
# assumes "Java" is "JavaNW" as nothing else is
# week4 %<>%
#   mutate(
#     loc_orig = if_else(
#       loc_orig == "Java",
#       "JavaNW",
#       loc_orig
#     )
#   )

# Quantity ####
# deal with ells.
# other units dealt with below
week4 %<>%
  mutate(
    textile_quantity = case_when(
      # ells.
      textile_unit == "el" ~ textile_quantity / els_per_ps,
      # deal with half ps.
      textile_unit == "half ps." ~ textile_quantity / 2,
      textile_unit == "halve ps." ~ textile_quantity / 2,
      TRUE ~ textile_quantity
    ),
    # back to NA if 0
    total_value = na_if(total_value, 0),
    # calculate per-unit prices
    price_per_unit = (total_value / textile_quantity),
    # Unify Case
    textile_color_arch = str_to_lower(textile_color_arch),
    textile_pattern_arch = str_to_lower(textile_pattern_arch),
    total_value_currency = str_to_lower(total_value_currency)
  )

# Mode as unit
# Unit that appears the most frequent for one textile
# Because unit should be the same for one textile

# Function from https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
Mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

unitvec <- week4 %>%
  group_by(textile_name) %>%
  summarize(textile_unit = Mode(textile_unit)) %>%
  pull(var = textile_unit, name = textile_name)

# Export ####
# select columns
week4 %<>%
  dplyr::select(
    exchange_nr,
    source,
    company,
    means_of_exchange,
    orig_yr,
    dest_yr,
    orig_loc_abr,
    dest_loc_abr,
    starts_with("textile"),
    total_value,
    price_per_unit
  )

# Make year discrete
week4$orig_yr <- factor(week4$orig_yr)

# Export to .rds
# no compression, space is cheaper than time
write_rds(week4, "week4.rds")

write_rds(unitvec, "unitvec.rds")

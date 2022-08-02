# Data cleanup ####

# libraries
library(tidyverse)
library(magrittr)
library(readxl)
library(readr)

# read
week4 <- read_excel("WICVOCDataAll_080222.xlsx",
  guess_max = 20000
)

# clean up
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

# currency
# 1 Indian Guilder = 0.7 Dutch Guilder
# TODO assume Dutch when it only says "guilders"
week4 %<>%
  mutate(
    total_value = if_else(
      startsWith(total_value_currency, "Indian"),
      total_value * 0.7,
      total_value
    )
  )

# geo
# assumes "Java" is "JavaNW" as nothing else is
week4 %<>%
  mutate(
    loc_orig = if_else(
      loc_orig == "Java",
      "JavaNW",
      loc_orig
    )
  )

# quantity
# deal with non-ps units
# TODO other units to deal with
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
    # calculate per-piece prices
    price_per_piece = (total_value / textile_quantity),
    # Unify Case
    textile_color_arch = str_to_lower(textile_color_arch),
    textile_pattern_arch = str_to_lower(textile_pattern_arch),
    total_value_currency = str_to_lower(total_value_currency)
  )

# select columns
week4 %<>%
  dplyr::select(
    orig_yr,
    dest_yr,
    loc_orig,
    loc_dest,
    starts_with("textile"),
    total_value,
    price_per_piece
  )

# Export to .rds
# no compression, space is cheaper than time
write_rds(week4, "week4.rds")

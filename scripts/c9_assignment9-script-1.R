#c9_assignment9-script-1.R

#install packages as needed:
# install.packages(c("tidyverse","readxl","janitor","lubridate","stringr", "forcats"), quiet = TRUE)

# ====================================================================
# NYC Rolling Sales — Cleaning and Organization - Section 1
# ====================================================================

library(readxl)
library(tidyverse)
library(janitor)
library(stringr)
library(dplyr)
library(forcats)
library(ggplot2)
library(scales)
library(tidyr)

files <- c(
  "~/Desktop/c9_assignment/data/rollingsales_bronx.xlsx",
  "~/Desktop/c9_assignment/data/rollingsales_brooklyn.xlsx",
  "~/Desktop/c9_assignment/data/rollingsales_manhattan.xlsx",
  "~/Desktop/c9_assignment/data/rollingsales_queens.xlsx",
  "~/Desktop/c9_assignment/data/rollingsales_statenisland.xlsx"
)

# ============================================
# NYC Rolling Sales — keep sale_date as *text* in MM/DD/YYYY
# ============================================

.anchor_tokens <- c(
  "borough","neighborhood","building class category","tax class at present",
  "block","lot","address","zip code","residential units","commercial units",
  "total units","land square feet","gross square feet","year built",
  "tax class at time of sale","building class at time of sale","sale price","sale date"
)

find_header_row <- function(path, sheet, scan_rows = 80) {
  raw <- read_excel(path, sheet = sheet, col_names = FALSE, n_max = scan_rows, trim_ws = TRUE)
  text_lines <- apply(as.matrix(raw), 1, function(r) paste(r[!is.na(r)], collapse = " | "))
  token_hits <- vapply(text_lines, function(s) sum(str_detect(str_to_lower(s), fixed(.anchor_tokens, TRUE))), integer(1))
  best <- which.max(token_hits)
  if (length(best) == 0 || is.na(best) || token_hits[best] < 3) NA_integer_ else best
}

make_safe_names <- function(dat) {
  nm <- names(dat); bad <- is.na(nm) | nm == ""
  if (any(bad)) nm[bad] <- paste0("x", which(bad))
  names(dat) <- nm
  tibble::as_tibble(dat, .name_repair = "unique")
}

# --- Coercers (no date classing) ------------------------------------
to_number  <- function(x) {
  if (is.numeric(x)) x
  else if (is.character(x)) readr::parse_number(x)
  else if (is.factor(x)) readr::parse_number(as.character(x))
  else suppressWarnings(as.numeric(x))
}
to_integer <- function(x) suppressWarnings(as.integer(to_number(x)))

# Keep sale_date as character; normalize to "MM/DD/YYYY"
fix_sale_date_text <- function(x) {
  if (is.null(x)) return(x)
  s <- as.character(x)
  
  # Excel serials: all digits, length 5–6 (very likely)
  is_serial <- grepl("^[0-9]{5,6}$", s) & !is.na(s)
  out <- s
  
  # Convert serials -> Date -> formatted text
  if (any(is_serial)) {
    d <- as.Date(as.numeric(s[is_serial]), origin = "1899-12-30")
    out[is_serial] <- ifelse(is.na(d), NA_character_, format(d, "%m/%d/%Y"))
  }
  
  # Strings with slashes/dashes: try to parse then format, still as text
  needs_parse <- !is_serial & grepl("[-/]", s) & !is.na(s)
  if (any(needs_parse)) {
    # Try common patterns; never keep Date type
    parsed <- suppressWarnings(as.Date(s[needs_parse], format = "%m/%d/%Y"))
    # Fallback attempts if NA:
    fallback <- is.na(parsed)
    if (any(fallback)) parsed[fallback] <- suppressWarnings(as.Date(s[needs_parse][fallback], format = "%Y-%m-%d"))
    fallback2 <- is.na(parsed)
    if (any(fallback2)) parsed[fallback2] <- suppressWarnings(as.Date(s[needs_parse][fallback2], format = "%d/%m/%Y"))
    
    out[needs_parse] <- ifelse(is.na(parsed), s[needs_parse], format(parsed, "%m/%d/%Y"))
  }
  
  # Already looks like mm/dd/yyyy or something else: leave as-is
  out
}

borough_from_path <- function(path) {
  case_when(
    str_detect(path, regex("bronx", TRUE))             ~ "Bronx",
    str_detect(path, regex("brooklyn", TRUE))          ~ "Brooklyn",
    str_detect(path, regex("manhattan", TRUE))         ~ "Manhattan",
    str_detect(path, regex("queens", TRUE))            ~ "Queens",
    str_detect(path, regex("staten[_ ]?island", TRUE)) ~ "Staten Island",
    TRUE ~ NA_character_
  )
}

normalize_names <- function(df) {
  df %>%
    janitor::clean_names() %>%
    rename_with(~ str_replace_all(.x, "_+", "_")) %>%
    rename(
      borough                           = any_of("borough"),
      neighborhood                      = any_of("neighborhood"),
      building_class_category           = any_of("building_class_category"),
      tax_class_at_present              = any_of(c("tax_class_at_present","tax_class_at_present_")),
      block                             = any_of("block"),
      lot                               = any_of("lot"),
      easement                          = any_of("easement"),
      building_class_at_present         = any_of("building_class_at_present"),
      address                           = any_of("address"),
      apartment_number                  = any_of(c("apartment_number","apartment")),
      zip_code                          = any_of(c("zip_code","zip")),
      residential_units                 = any_of("residential_units"),
      commercial_units                  = any_of("commercial_units"),
      total_units                       = any_of("total_units"),
      land_square_feet                  = any_of(c("land_square_feet","land_sq_ft","land_square_feet_")),
      gross_square_feet                 = any_of(c("gross_square_feet","gross_sq_ft","gross_square_feet_")),
      year_built                        = any_of("year_built"),
      tax_class_at_time_of_sale         = any_of("tax_class_at_time_of_sale"),
      building_class_at_time_of_sale    = any_of("building_class_at_time_of_sale"),
      sale_price                        = any_of("sale_price"),
      sale_date                         = any_of("sale_date")   # stays character; we only normalize text
    )
}

coerce_types <- function(df) {
  df %>%
    mutate(
      zip_code = if ("zip_code" %in% names(.)) {
        z <- as.character(zip_code)
        str_pad(z, width = 5, pad = "0")
      } else zip_code,
      
      across(any_of(c("residential_units","commercial_units","total_units")), to_integer),
      
      land_square_feet  = if ("land_square_feet"  %in% names(.)) to_number(land_square_feet)  else land_square_feet,
      gross_square_feet = if ("gross_square_feet" %in% names(.)) to_number(gross_square_feet) else gross_square_feet,
      sale_price        = if ("sale_price"        %in% names(.)) to_number(sale_price)        else sale_price,
      
      year_built        = if ("year_built" %in% names(.)) to_integer(year_built) else year_built,
      
      # NEW: keep sale_date as character, normalize to "MM/DD/YYYY" text (or leave as-is if unparseable)
      sale_date         = if ("sale_date" %in% names(.)) fix_sale_date_text(sale_date) else sale_date
    )
}

read_one_sheet <- function(path, sheet) {
  header_row <- find_header_row(path, sheet)
  if (is.na(header_row)) {
    message(sprintf("⚠️  %s [%s]: No confident header row — skipping.", basename(path), sheet))
    return(tibble())
  }
  
  dat <- read_excel(
    path, sheet = sheet,
    col_names   = TRUE,
    skip        = header_row - 1,
    col_types   = "text",   # read EVERYTHING as text first
    trim_ws     = TRUE,
    .name_repair = "unique"
  ) %>%
    make_safe_names() %>%
    janitor::remove_empty("rows")
  
  if (nrow(dat) == 0) return(tibble())
  
  dat %>%
    normalize_names() %>%
    coerce_types() %>%
    mutate(sheet = sheet, .before = 1)
}

read_one_file <- function(path) {
  if (!file.exists(path)) {
    message(sprintf("⏭️  Not found on disk, skipping: %s", path))
    return(tibble())
  }
  sheets <- readxl::excel_sheets(path)
  out <- purrr::map(sheets, ~ read_one_sheet(path, .x)) %>% list_rbind()
  if (nrow(out) == 0) return(tibble())
  
  out %>% mutate(borough = borough_from_path(path), .before = 1) %>% relocate(borough, sheet)
}

# ---- Execute with pre-combine counts --------------------------------
file_results <- purrr::map(files, read_one_file)

precombine_counts <- purrr::map2_dfr(
  file_results, files,
  ~ tibble(
    borough      = borough_from_path(.y),
    precombine_n = nrow(.x)
  )
) %>%
  group_by(borough) %>%
  summarise(precombine_n = sum(precombine_n), .groups = "drop") %>%
  arrange(borough)

rolling_sales <- file_results %>% list_rbind() %>%
  filter(!(str_to_upper(as.character(sale_price)) %in% c("SALE PRICE","NA"))) %>%
  clean_names()

# Sanity: ensure sale_date is character & looks like MM/DD/YYYY where possible
if ("sale_date" %in% names(rolling_sales)) {
  message(sprintf("sale_date class: %s", paste(class(rolling_sales$sale_date), collapse = ", ")))
  print(rolling_sales %>% count(str_detect(sale_date, "^[0-9]{2}/[0-9]{2}/[0-9]{4}$"), name = "rows") %>% rename(is_mmddyyyy = 1))
}

message("# ---------------- Summary ----------------")
message(sprintf("Rows combined (all boroughs): %s", scales::comma(nrow(rolling_sales))))
print(rolling_sales %>% select(borough, sheet, address, zip_code, sale_price, sale_date) %>% head(10))

cat("\n# ---------------- Notice: Pre-combine counts by borough ----------------\n")
print(precombine_counts)

# Optional saves:
# saveRDS(rolling_sales, "~/Desktop/c9_assignment/data/rolling_sales_clean.rds")
# write_csv(rolling_sales, "~/Desktop/c9_assignment/data/rolling_sales_clean.csv")

# ====================================================================
# NYC Rolling Sales - Parse sale_date to Date (lubridate) - Section 2
# ====================================================================

# Expect: rolling_sales already in memory, with sale_date as "MM/DD/YYYY" text
stopifnot("sale_date" %in% names(rolling_sales))

rolling_sales_dates <- rolling_sales %>%
  mutate(
    sale_date_chr  = as.character(sale_date),                  # keep raw text
    sale_date_date = suppressWarnings(lubridate::mdy(sale_date_chr))  # parse to Date
  )

# ---- Validation ------------------------------------------------------
# 1) Confirm the new column is a Date
if (!inherits(rolling_sales_dates$sale_date_date, "Date")) {
  stop("sale_date_date is not a Date type. Check the upstream normalization step.")
} else {
  message("✅ sale_date_date is a Date.")
}

# 2) Basic diagnostics
na_n <- sum(is.na(rolling_sales_dates$sale_date_date))
message(sprintf("Parsed dates: %s; NAs after parse: %s",
                scales::comma(nrow(rolling_sales_dates) - na_n),
                scales::comma(na_n)))

# 3) If there are NAs, show a small sample of problematic raw values
if (na_n > 0) {
  message("Examples of unparsed sale_date values:")
  print(
    rolling_sales_dates %>%
      filter(is.na(sale_date_date)) %>%
      distinct(sale_date_chr) %>%
      head(10)
  )
}

# 4) Quick sanity summary
print(
  rolling_sales_dates %>%
    summarise(min_date = min(sale_date_date, na.rm = TRUE),
              max_date = max(sale_date_date, na.rm = TRUE))
)

# (Optional) time helpers for next steps
# rolling_sales_dates <- rolling_sales_dates %>%
#   mutate(
#     sale_year   = year(sale_date_date),
#     sale_month  = floor_date(sale_date_date, "month"),
#     sale_quarter= paste0(year(sale_date_date), " Q", quarter(sale_date_date))
#   )

# Your parsed tibble is `rolling_sales_dates`

# ====================================================================
# NYC Rolling Sales -Shape dates with forcats - Section 3
# ====================================================================

# Work from parsed dates only
rs <- rolling_sales_dates %>%
  filter(!is.na(sale_date_date))


# Helper categorical fields -------------------------------------------
rs_cat <- rs %>%
  mutate(
    # Weekly bucket (Mon-start)
    week_start  = floor_date(sale_date_date, "week", week_start = 1),
    # Monthly bucket
    month_date  = floor_date(sale_date_date, "month"),
    # Month label (ordered factor: Jan … Dec)
    month_label = month(sale_date_date, label = TRUE, abbr = TRUE)
  )

# ====================================================================
# NYC Rolling Sales -ggplot visualization #1 - Section 4
# ====================================================================

# ---------------------------------------------------------------------
# A) Compare total sales per borough (bar; borough ordered by volume)
# ---------------------------------------------------------------------
sales_by_boro <- rs_cat %>%
  count(borough, name = "sales") %>%
  mutate(borough = fct_reorder(borough, sales, .desc = TRUE))

plot_sales_by_boro <- ggplot(sales_by_boro, aes(borough, sales)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Total Sales by Borough",
    x = NULL, y = "Number of Sales"
  ) +
  theme_minimal(base_size = 12)

print(plot_sales_by_boro)

# ---------------------------------------------------------------------
# B) All sales across the last 52 weeks
# ---------------------------------------------------------------------

stopifnot("sale_date_date" %in% names(rs_cat), "week_start" %in% names(rs_cat))

# First date in 2024 that actually exists in your data
first_2024 <- rs_cat %>%
  filter(sale_date_date >= as.Date("2024-01-01")) %>%
  summarise(d = suppressWarnings(min(sale_date_date, na.rm = TRUE))) %>%
  pull(d)

if (is.infinite(first_2024) || is.na(first_2024)) {
  stop("No data in 2024+. Adjust the start year or inspect upstream parsing.")
}

# Choose the latest year that has any data on/before Aug 31 of that year
candidate_years <- sort(unique(year(rs_cat$sale_date_date)))
years_ok <- vapply(candidate_years, function(y) {
  any(rs_cat$sale_date_date <= as.Date(sprintf("%d-08-31", y)) & year(rs_cat$sale_date_date) == y, na.rm = TRUE)
}, logical(1))

end_year <- max(candidate_years[years_ok])
end_date <- as.Date(sprintf("%d-08-31", end_year))   # end of August that year

# Weekly aggregate over this fixed window (no September)
start_week <- floor_date(first_2024, "week", week_start = 1)
end_week   <- floor_date(end_date,   "week", week_start = 1)

weekly_aug_window <- rs_cat %>%
  filter(sale_date_date >= start_week, sale_date_date <= end_date) %>%
  count(week_start, name = "sales") %>%
  complete(
    week_start = seq(start_week, end_week, by = "1 week"),
    fill = list(sales = 0)
  )

plot_weekly_aug_window <- ggplot(weekly_aug_window, aes(week_start, sales)) +
  geom_line(linewidth = 0.8) +
  scale_x_date(date_breaks = "8 weeks", date_labels = "%b %d") +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "All Sales — Weekly Totals",
    subtitle = sprintf("From %s to %s (September excluded)",
                       format(start_week, "%b %d, %Y"),
                       format(end_date,  "%b %d, %Y")),
    x = NULL, y = "Weekly Sales"
  ) +
  theme_minimal(base_size = 12)

print(plot_weekly_aug_window)

# ---------------------------------------------------------------------
# C) Bar plots for months
#   C1) Overall seasonal pattern (Jan–Dec)
#   C2) Faceted by borough (seasonality by borough)
# ---------------------------------------------------------------------
# C1: Overall
monthly_overall <- rs_cat %>%
  count(month_label, name = "sales")

plot_monthly_overall <- ggplot(monthly_overall, aes(month_label, sales)) +
  geom_col() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Sales by Month (All Boroughs, Pooled Across Years)",
    x = NULL, y = "Sales"
  ) +
  theme_minimal(base_size = 12)

print(plot_monthly_overall)

# C2: Faceted by borough (ordered by each borough's total volume)
# ---- Faceted by borough (ordered by total sales) ----
monthly_by_boro <- rs_cat %>%
  count(borough, month_label, name = "sales")

# Compute borough order by total volume (desc)
boro_levels <- monthly_by_boro %>%
  group_by(borough) %>%
  summarise(total_sales = sum(sales), .groups = "drop") %>%
  arrange(desc(total_sales)) %>%
  pull(borough)

monthly_by_boro <- monthly_by_boro %>%
  mutate(
    borough = factor(borough, levels = boro_levels),         # order facets
    month_label = forcats::fct_match(month_label, levels(month_label)) %>% 
      { monthly_by_boro$month_label }                        # keep month order as Jan..Dec (already ordered)
  )

plot_monthly_by_boro <- ggplot(monthly_by_boro, aes(month_label, sales)) +
  geom_col() +
  facet_wrap(~ borough, scales = "free_y", ncol = 2) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Sales by Month, Faceted by Borough",
    x = NULL, y = "Sales"
  ) +
  theme_minimal(base_size = 12)

print(plot_monthly_by_boro)

# ====================================================================
# NYC Rolling Sales - Filter invalids, compute PPSF, and plot - Section 5
# ====================================================================

# Start from parsed dataset (Section 2 output)
stopifnot(exists("rolling_sales_dates"))

# ---- 5.1 Filter to valid records -----------------------------------
rs_valid <- rolling_sales_dates %>%
  filter(
    !is.na(sale_price), sale_price > 0,
    !is.na(gross_square_feet), gross_square_feet >= 100
  )

# ---- 5.2 Compute metrics -------------------------------------------
rs_metrics <- rs_valid %>%
  mutate(
    # Standard: dollars per square foot
    ppsf = sale_price / gross_square_feet,
    
    # OPTIONAL: "square feet per $100" (if you truly wanted numerator=100*gsf / denominator=sale_price)
    sqft_per_100_dollars = (100 * gross_square_feet) / sale_price
  )

# ---- 5.3 Plot 1: Median PPSF by Borough ----------------------------
ppsf_by_boro <- rs_metrics %>%
  group_by(borough) %>%
  summarise(
    median_ppsf = median(ppsf, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(borough = fct_reorder(borough, median_ppsf, .desc = TRUE))

plot_ppsf_boro <- ggplot(ppsf_by_boro, aes(borough, median_ppsf)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(accuracy = 1)) +
  labs(
    title = "Median Price per Square Foot by Borough",
    subtitle = "Filtered to sale_price > 0 and gross_square_feet ≥ 100",
    x = NULL,
    y = "Median $/sqft"
  ) +
  theme_minimal(base_size = 12)

print(plot_ppsf_boro)

# ---- 5.4 Plot 2: Median PPSF by Building Class Category (Top 15) ---
ppsf_by_cat <- rs_metrics %>%
  group_by(building_class_category) %>%
  summarise(
    n = n(),
    median_ppsf = median(ppsf, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # keep the 15 most common categories to keep the plot readable
  slice_max(n, n = 15, with_ties = FALSE) %>%
  mutate(building_class_category = fct_reorder(building_class_category, median_ppsf))

plot_ppsf_cat <- ggplot(ppsf_by_cat, aes(building_class_category, median_ppsf)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(accuracy = 1)) +
  labs(
    title = "Median Price per Square Foot by Building Class Category",
    subtitle = "Top 15 categories by number of valid sales; all boroughs pooled",
    x = NULL,
    y = "Median $/sqft"
  ) +
  theme_minimal(base_size = 12)

print(plot_ppsf_cat)


# ================================
# Stacked proportions by Borough × Tax Class (custom labels)
# ================================

stopifnot(exists("rs_valid"))

# --- Updated tax class labels & stacked proportions plot ---

tax_labs <- c(
  `1`     = "1 = Single, Two and Three Family Homes and (Condos <= 3 Stories)",
  `2`     = "2 = Rentals, Coops and (Condos > 3 Stories)",
  `4`     = "4 = Commercial and Industrial Real Estate",
  `Other` = "Other"
)

rs_tax <- rs_valid %>%
  filter(!is.na(tax_class_at_present), tax_class_at_present != "") %>%
  mutate(
    borough  = factor(borough, levels = rs_valid %>% count(borough, name="n") %>% arrange(desc(n)) %>% pull(borough)),
    tax_code = stringr::str_extract(as.character(tax_class_at_present), "\\d"),
    tax_code = dplyr::if_else(is.na(tax_code), "Other", tax_code),
    tax_code = dplyr::if_else(tax_code %in% c("1","2","4"), tax_code, "Other"),
    tax_class_labeled = dplyr::recode(tax_code, !!!tax_labs),
    tax_class_labeled = forcats::fct_relevel(
      tax_class_labeled,
      tax_labs["1"], tax_labs["2"], tax_labs["4"], tax_labs["Other"]
    )
  )

plot_tax_stacked <- ggplot(rs_tax, aes(borough, fill = tax_class_labeled)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Tax Class Composition by Borough (Valid Sales Only)",
    subtitle = "Proportions within each borough; custom tax class groupings",
    x = "Borough", y = "Share of Sales", fill = "Tax Class"
  ) +
  theme_minimal(base_size = 12)

print(plot_tax_stacked)


# ---------------------------------------------------------------------
# (Optional) Save plots to disk as PNGs
# ---------------------------------------------------------------------

output_dir <- "~/Desktop/c9_assignment/results"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

save_plot <- function(plot_obj, filename_stub, width = 9, height = 5, dpi = 300) {
  stopifnot(inherits(plot_obj, "ggplot"))
  out_path <- file.path(output_dir, paste0(filename_stub, ".png"))
  ggsave(out_path, plot = plot_obj, width = width, height = height, dpi = dpi)
  message("✓ Saved: ", out_path)
}

# Collect plots in a named list (only those that exist)
plots <- list(
  sales_by_boro        = if (exists("plot_sales_by_boro"))        plot_sales_by_boro        else NULL,
  weekly_aug_window    = if (exists("plot_weekly_aug_window"))    plot_weekly_aug_window    else NULL,
  monthly_overall      = if (exists("plot_monthly_overall"))      plot_monthly_overall      else NULL,
  monthly_by_boro      = if (exists("plot_monthly_by_boro"))      plot_monthly_by_boro      else NULL,
  ppsf_boro            = if (exists("plot_ppsf_boro"))            plot_ppsf_boro            else NULL,
  ppsf_cat             = if (exists("plot_ppsf_cat"))             plot_ppsf_cat             else NULL,
  tax_class_stacked    = if (exists("plot_tax_stacked"))          plot_tax_stacked          else NULL
)

# Save each non-NULL plot with sensible dimensions
if (length(plots)) {
  if (!is.null(plots$sales_by_boro))     save_plot(plots$sales_by_boro,     "sales_by_boro",        width = 8,  height = 5)
  if (!is.null(plots$weekly_aug_window)) save_plot(plots$weekly_aug_window, "weekly_totals_aug_end",width = 10, height = 4.5)
  if (!is.null(plots$monthly_overall))   save_plot(plots$monthly_overall,   "monthly_overall",      width = 8,  height = 4.5)
  if (!is.null(plots$monthly_by_boro))   save_plot(plots$monthly_by_boro,   "monthly_by_borough",   width = 10, height = 7)
  if (!is.null(plots$ppsf_boro))         save_plot(plots$ppsf_boro,         "ppsf_by_borough",      width = 8,  height = 5)
  if (!is.null(plots$ppsf_cat))          save_plot(plots$ppsf_cat,          "ppsf_by_category",     width = 9,  height = 6)
  if (!is.null(plots$tax_class_stacked)) save_plot(plots$tax_class_stacked, "tax_class_stacked",    width = 9,  height = 6)
}






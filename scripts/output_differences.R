# compare_outputs.R
#
# Compares previous and current contributions and inputs Excel files,
# outputting a three-sheet workbook: previous, current, and differences

# ---- load files ----

# Read current and previous contributions
cur_contrib  <- readxl::read_xlsx(glue('results/{month_year}/beta/contributions-{month_year}.xlsx'))
prev_contrib <- readxl::read_xlsx(glue('results/04-2026/04.09/beta/contributions-04-2026.xlsx'))
# cur_contrib  <- readxl::read_xlsx(glue('results/{month_year}/beta/contributions-{month_year}.xlsx'))
# prev_contrib <- readxl::read_xlsx(glue('results/{last_month_year}/beta/contributions-{last_month_year}.xlsx'))

# Read current and previous inputs
cur_inputs  <- readxl::read_xlsx(glue('results/{month_year}/beta/inputs-{month_year}.xlsx'))
prev_inputs <- readxl::read_xlsx(glue('results/04-2026/04.09/beta/inputs-04-2026.xlsx'))
# cur_inputs  <- readxl::read_xlsx(glue('results/{month_year}/beta/inputs-{month_year}.xlsx'))
# prev_inputs <- readxl::read_xlsx(glue('results/{last_month_year}/beta/inputs-{last_month_year}.xlsx'))

# ---- process contributions ----

# Find columns that exist in both files (skip col 1, which is the label column)
shared <- intersect(names(cur_contrib)[-1], names(prev_contrib)[-1])

# Keep only shared columns from previous, trimmed/padded to match current's row count
# (indexing beyond nrow will fill with NA automatically)
prev <- prev_contrib[1:nrow(cur_contrib), shared]

# Re-attach the label column from current
prev <- bind_cols(cur_contrib[, 1], prev)

# Compute current - previous for numeric columns; NA for non-numeric
diff <- cur_contrib[, 1]
for (col in shared) {
  diff[[col]] <- if (is.numeric(cur_contrib[[col]])) cur_contrib[[col]] - prev[[col]] else NA_real_
}

# Write all three sheets to one workbook
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "current");     openxlsx::writeData(wb, "current", cur_contrib)
openxlsx::addWorksheet(wb, "previous");    openxlsx::writeData(wb, "previous", prev)
openxlsx::addWorksheet(wb, "differences"); openxlsx::writeData(wb, "differences", diff)
openxlsx::saveWorkbook(wb, glue('results/{month_year}/beta/contributions-comparison-{month_year}.xlsx'), overwrite = TRUE)

# ---- process inputs (same logic as above) ----

shared <- intersect(names(cur_inputs)[-1], names(prev_inputs)[-1])

diff <- cur_inputs[, 1]
for (col in shared) {
  diff[[col]] <- if (is.numeric(cur_inputs[[col]])) cur_inputs[[col]] - prev[[col]] else NA_real_
}

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "current");     openxlsx::writeData(wb, "current", cur_inputs)
openxlsx::addWorksheet(wb, "previous");    openxlsx::writeData(wb, "previous", prev)
openxlsx::addWorksheet(wb, "differences"); openxlsx::writeData(wb, "differences", diff)
openxlsx::saveWorkbook(wb, glue('results/{month_year}/beta/inputs-comparison-{month_year}.xlsx'), overwrite = TRUE)

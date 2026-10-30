# IRA clean-energy tax credits after OBBBA (P.L. 119-21): source data

Collected 2026-10-30 for rebuilding the "supply-side IRA" path in the Hutchins Center FIM. This folder holds collected data only. Nothing here is modeled or adjusted. CSV values are copied exactly as published, with only the thousands separators removed. Published cell codes are kept as they appear (`---`, `[2]`, `[3]`, `Negligible Revenue Effect`). Each CSV starts with `#` comment lines that give the source, URL, units and caveats, so read it with `comment="#"` in pandas or `comment.char="#"` in R.

All years are **federal fiscal years** (Oct-Sep). None of these sources reports calendar years or quarters.

## Files

| File | Source / date | What it measures |
|---|---|---|
| `JCX-35-25.pdf` | JCT, JCX-35-25, July 1, 2025. https://www.jct.gov/publications/2025/jcx-35-25/ | Official revenue estimate of Title VII of the Senate-passed bill, enacted unchanged as P.L. 119-21 on July 4, 2025. |
| `JCX-35-25_energy_provisions_FY2025-2034.csv` | From the PDF above: pp. 5-6 (Ch. 5, Subch. A-B), p. 2 (Ch. 3 item 8, 48D), and the outlay rows of footnote [1] (pp. 8-9) | **Change** from the present-law (pre-OBBBA) baseline, which is CBO's January 2025 baseline. $ millions, FY2025-2034. Positive = revenue gain (savings). The main rows include outlay (direct-pay/refundable) effects, which are broken out in the `footnote1` rows. |
| `JCX-45-25.pdf` | JCT, JCX-45-25, *Estimates of Federal Tax Expenditures for FY2025-2029*, Dec. 3, 2025. https://www.jct.gov/publications/2025/jcx-45-25/ | Latest JCT tax-expenditure report. It is **post-OBBBA** (law enacted through Aug. 31, 2025). JCT's listing shows no 2026 report as of 2026-10-30. |
| `JCX-45-25_energy_tax_expenditures_FY2025-2029.csv` | From the PDF above: Table 1, Energy function (pp. 34-35) and refundability footnote [3] | **Post-OBBBA levels** in $ billions, FY2025-2029, split into corporations and individuals as published. Tax expenditure means static revenue loss, with no behavioral response. |
| `JCX-48-24.pdf` | JCT, JCX-48-24, *Tax Expenditures FY2024-2028*, Dec. 11, 2024. https://www.jct.gov/publications/2024/jcx-48-24/ | The last **pre-OBBBA** tax-expenditure report. Uses the Feb. 2024 CBO baseline and law through Nov. 1, 2024. |
| `JCX-48-24_energy_tax_expenditures_FY2024-2028_preOBBBA.csv` | From the PDF above: Table 1, Energy function (pp. 24-26) | **Pre-OBBBA levels** in $ billions, FY2024-2028. Section 48 is split by technology. The table has no 45Y row. |
| `CBO-61188-Business-Tax-Credits-Wind-Solar_report.md` | CBO, *Business Tax Credits for Wind and Solar Power*, Apr. 11, 2025. https://www.cbo.gov/publication/61329 | Full report text converted from HTML to markdown. The PDF (https://www.cbo.gov/system/files/2025-04/61188-solar-wind-incentives.pdf) could not be downloaded because cbo.gov blocks scripted downloads with a bot check. |
| `CBO-61188_wind-solar-and-manufacturing-credits_Jan2025baseline.csv` | Table 3 and Table A-2, via https://www.cbo.gov/system/files/2025-04/61188-data.xlsx | **Pre-OBBBA levels** (Jan. 2025 baseline), split into revenue and outlays, $ billions. **Only FY2025 and the FY2026-2035 total are published. There is no year-by-year path.** |
| `CBO-61882_TableA-2_OBBBA_conventional_estimate_FY2025-2034.csv` | CBO, *Budget and Economic Outlook: 2026 to 2036*, Feb. 11, 2026, Table A-2. https://www.cbo.gov/publication/61882 (data: 61882-data.xlsx) | **Change** in the primary deficit from OBBBA relative to the Jan. 2025 baseline, $ billions. The "Energy-related taxes" row covers Ch. 5, Subch. A-B. **The sign is opposite to JCT's**: negative = deficit reduction. |
| `CBO_Feb2026_baseline_energy_credit_outlays_by_account_FY2026-2036.csv` | CBO Feb. 2026 baseline spending projections by budget account. https://www.cbo.gov/system/files/2026-02/51142-2026-02-Spending-Projections.xlsx | **Post-OBBBA outlay levels** in $ millions, FY2026-2036, for the IRS energy-credit payment accounts. Most relevant is 020-0963, sec. 6417 elective payments. This file holds outlays only, not the revenue side. |
| `CBO_Jan2025_baseline_energy_credit_outlays_by_account_FY2025-2035_preOBBBA.csv` | Same CBO series, Jan. 2025 baseline. https://www.cbo.gov/system/files/2025-01/51142-2025-01-Spending-Projections.xlsx | **Pre-OBBBA outlay levels** in $ millions, FY2025-2035, for the same accounts. |

## Supply-side vs consumer credits

The JCT CSVs carry a `category_compiler_note` column. The compiler added it; it is not part of the published tables.

- **Business / supply-side** (likely the FIM "supply-side IRA" series):
  - 45Y (clean electricity PTC) and 48E (clean electricity ITC)
  - legacy 45 (PTC) and 48 (ITC "Energy credit")
  - 45X (advanced manufacturing production)
  - 45Q (carbon sequestration)
  - 45Z (clean fuel)
  - 45V (hydrogen)
  - 45U (existing nuclear)
  - 48C (advanced energy project)
- **Consumer / household** (probably *not* in a supply-side series): 30D (clean vehicle), 25E (used EV), 25C (home improvement), 25D (residential clean energy).
- **Borderline:**
  - 45W (commercial clean vehicles): a business credit, but tied to vehicle purchases.
  - 30C (refueling property).
  - 45L (new energy-efficient homes).
  - 179D (commercial buildings deduction).
- **Not IRA clean energy:** 48D (CHIPS "advanced manufacturing investment credit"). It appears in every source, and its name is easy to confuse with 45X.

## Key caveats

1. **A post-OBBBA level series exists directly only for FY2025-2029.** JCX-45-25 gives it, by credit. JCT publishes no post-OBBBA levels past 2029. CBO's Feb. 2026 baseline gives post-OBBBA *outlays*, which are only the direct-pay portion, through 2036. It does not publish the *revenue* side by credit.
2. **Building levels as baseline + change mixes vintages.**
   - JCX-35-25 changes are relative to CBO's **Jan. 2025** baseline.
   - The only by-credit pre-OBBBA year-by-year levels are JCX-48-24. That report uses the **Feb. 2024** baseline and runs only through FY2028.
   - CBO 61188 publishes only FY2025 and a 10-year total.
   - Neither CBO nor JCT publishes the Jan. 2025 baseline cost of each credit year by year.
3. **JCX-45-25 and JCX-48-24 differ by more than OBBBA explains.** Example: 45X in FY2025 is $5.1B in JCX-45-25 versus $18.3B in JCX-48-24 (corporate plus individual), yet JCX-35-25 shows no OBBBA effect on 45X in FY2025. Likely causes are new return data and the claiming split (credit, transfer or direct pay), but JCT does not explain the gap. Do not treat the difference between the two reports as the OBBBA effect.
4. **Direct pay (sec. 6417) and tax expenditures.**
   - JCX-45-25 footnote [3] lists refundability/outlay effects only for 45V, 45Z, 30D and 25E. The report does not say whether elective payments for 45X, 45Y, 48E or 45Q are inside the tax-expenditure figures.
   - CBO's account 020-0963 carries those payments as outlays: about $2.8B in FY2026, down from $16.7B for FY2026 in the Jan. 2025 baseline.
   - CBO (Outlook ch. 5) says most of that drop is a **technical reclassification**: it now expects more of the remaining credits to be claimed against tax or transferred rather than paid directly. Outlays and revenues fall by the same amount, so total cost is not lower.
   - Check for double counting before adding 020-0963 to JCT tax-expenditure levels.
5. **Tax expenditures are not revenue estimates.** They are static, with no behavioral response, and are measured against a no-provision counterfactual. For credits, the result is close to the credit claimed.
6. **Source oddities, reproduced exactly as printed:**
   - JCX-45-25, "Clean electricity production credit" (45Y): the published cells (corp `--- --- [2] 0.2 0.5`, indiv `--- 0.8 [2] [2] [2]`) do not sum to the printed total of 0.8, so one cell is probably a misprint.
   - JCX-48-24, Clean electricity ITC: the components sum to 32.2 against a printed 31.9.
   - In JCX-35-25, several rows differ from their printed totals by a few $M. That is rounding.
   - CBO account titles are truncated in CBO's file. The contents of account 020-0931 are not documented and may not be energy-related.
7. **JCX-35-25 effective dates** are in the `effective` column, using JCT's legend codes. In the 45Y/48E rows, "generally tyba DOE" does not by itself show the July 4, 2026 begin-construction safe harbor or the placed-in-service-by-end-of-2027 rule. Those come from the statute.
8. **Verification.** Every CSV row was read from the PDF page images and cross-checked against an independent text parse of the PDF (Firecrawl). The CBO numbers come straight from CBO's own xlsx files.

## Not found / not collected

- No CBO or JCT **year-by-year** pre-OBBBA (Jan. 2025 baseline) cost path by credit on the revenue side. CBO 61188 gives only FY2025 plus the 2026-2035 total.
- No JCT tax-expenditure report newer than JCX-45-25. JCT's 2026 list is empty.
- No CBO post-OBBBA revenue-side projection of energy credits by credit or by year.
- Not collected: CBO's July 21, 2025 section-by-section estimate of P.L. 119-21 (https://www.cbo.gov/publication/61570). It repeats JCT's revenue numbers and splits revenue from outlays. It could be useful if JCX-35-25's footnote [1] outlay split is not enough.

## Added 2026-10-30: FIM path options and Census comparison

- `supply_side_ira_paths.csv` / `supply_side_ira_paths.xlsx`: option 1 (the FIM's 2026 Q3 level grown at JCT's FY-over-FY change in total post-OBBBA business supply-side credits) and option 2 (JCT credit cost, reference only). The xlsx also holds a verbatim copy of the `Supply Side IRA` sheet from `data/forecast.xlsx` and a second copy with option 1 placed in row 12.
- `FRED_PRMFGCONS.csv`, `FRED_TLMFGCONS.csv`: Census value of construction put in place, manufacturing (private / total), $M SAAR, monthly, from FRED.
- `FRED_WPUID612.csv`: PPI, materials and components for construction (the deflator the IRA sheet uses), monthly, from FRED.
- `census_vs_gs_structures.csv`: Census private manufacturing construction (75% of excess over the 2019 average, in 2022$) next to the IRA sheet's structures column, which is a read of a Goldman Sachs chart published Aug 2023 (values after mid-2023 are GS forecasts). Includes two rebuilt row-14 paths run through the sheet's own equipment and deflator mechanics. The script checks that the mechanics reproduce the current row 14.
- **Important:** the FIM `supply_side_ira` series measures IRA/CHIPS-induced manufacturing investment (structures plus equipment), not tax-credit cost. JCT figures are a proxy for direction only.
- Option 3 (added to `supply_side_ira_paths.csv/.xlsx`): published history kept through 2026 Q3 (no revisions); from 2026 Q4 the level grows at the quarterly growth rate of the Census-based rebuild in `census_vs_gs_structures.csv`. The xlsx tab `IRA sheet (option 3)` places it in row 12 of the sheet format.
- Scope note: IRA subsidies (Subsidies sheet), IRA grants incl. "IRA investment grants" (Grants sheet), and CBO's IRA score by provision (IRA and CHIPS sheet) already enter the FIM through other forecast-sheet lines, throughout history. The JCT/CBO credit files here are reference only and must not be added to `supply_side_ira`, which measures investment. Option 1 (JCT shape) mixes those concepts; option 3 (Census) stays within the supply-side investment concept.

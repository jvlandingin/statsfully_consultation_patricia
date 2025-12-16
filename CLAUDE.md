# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a statistical consulting project for an undergraduate thesis examining tax structure composition and SDG outcomes in 29 Asia-Pacific countries (2014-2023). The project provides consultation services for panel data econometric analysis.

## Key Commands

### R Analysis
```bash
# Run the main analysis script (requires Google Sheets authentication)
Rscript main.R
```

### Quarto Documents
```bash
# Render client quote to PDF
quarto render claude_points/client_quote.qmd
```

## Project Structure

- `main.R` - Core R script for data extraction from Google Sheets and tidying
- `claude_points/` - Client-facing documents (quotes, methodology evaluation, email drafts)
- `files/` - Reference materials (thesis proposal, credentials, resume)
- `email_messages/` - Client correspondence
- `assets/` - Logo and branding assets

## Data Pipeline Architecture

The main analysis follows this flow:

1. **Google Sheets Authentication** - Uses `googlesheets4` with email auth
2. **Data Extraction** - Pulls 11 indicator tables from specific cell ranges in the "DATA SHEET"
3. **Data Tidying** - `tidy_googlesheet_extract()` function converts raw extracts to long format with country/year keys
4. **Data Validation** - Uses `pointblank` for column type checks and value validation
5. **Data Integration** - Sequential `full_join` with one-to-one relationship enforcement

## R Dependencies

Uses `box` for modular imports:
- `googlesheets4` - Google Sheets API access
- `dplyr`, `tidyr` - Data manipulation
- `pointblank` - Data validation

## Data Variables

**Tax Structure (Independent Variables):**
- `tax_goods_and_services`
- `tax_general_consumption`
- `tax_income_and_profits`

**SDG 3 Health Indicators:**
- `under_5_mortality_rate`, `neonatal_mortality_rate`, `maternal_mortality_ratio`
- `prop_births_with_skilled_personnel`, `coverage_of_essential_health_services`

**SDG 4 Education Indicators:**
- `lower_secondary`, `adult_literacy_rate`

**Control Variable:**
- `macroeconomic_population`

## Statistical Methods (Planned)

- PCA for composite SDG indices
- Panel data regression (Fixed/Random Effects)
- Diagnostic tests: VIF, Breusch-Pagan, Wooldridge, Pesaran CD, Unit Root
- Model selection: BP-LM, Hausman, Chow tests

## Study Framework Reference

See `study_framework.md` for the complete research design including:

### Research Question
How does tax structure composition affect health (SDG 3) and education (SDG 4) outcomes in Asia-Pacific economies, with moderating effects of population, GDP per capita, and debt-to-GDP ratio?

### Hypotheses Summary (14 Total)
**SDG 3 Health (H1-H7):**
- H1: Direct effect of tax structure on health
- H2-H4: Pairwise comparisons (income-predominant vs consumption-predominant vs balanced)
- H5-H7: Moderating effects (population, GDP per capita, debt-to-GDP)

**SDG 4 Education (H8-H14):**
- H8: Direct effect of tax structure on education
- H9-H11: Pairwise comparisons (income-predominant vs consumption-predominant vs balanced)
- H12-H14: Moderating effects (population, GDP per capita, debt-to-GDP)

### Tax Classification Rule
- **Income-Predominant**: Income tax share > Consumption by ≥10pp
- **Consumption-Predominant**: Consumption tax share > Income by ≥10pp
- **Balanced**: Difference < 10pp

### Panel Regression Models
Base model tests direct tax→SDG relationships. Full model adds interaction terms for moderating effects. Model selection via Breusch-Pagan LM, Chow, and Hausman tests.

### Theoretical Expectation
Income-predominant (progressive) systems → greater redistribution → better SDG outcomes
Consumption-predominant (regressive) systems → less redistribution → weaker SDG outcomes
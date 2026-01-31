# SDG3 Health Imputation Methodology

## Executive Summary

This document explains the methodological decisions for imputing missing values in SDG3 health indicators prior to creating a composite index via Principal Component Analysis (PCA). The imputation strategy uses multilevel panel imputation (2l.pan method) on 3 highly correlated SDG3 mortality indicators, while intentionally excluding moderating and tax variables to preserve the relationships we intend to study in subsequent regression analysis.

**Key Decisions:**
- **Variables imputed**: 3 SDG3 mortality indicators (r = 0.74-0.97)
- **Variables excluded**: Moderators, tax variables, 2 coverage indicators
- **Method**: Multilevel panel imputation (2l.pan) respecting country clustering
- **Datasets created**: 5 multiply imputed datasets (m=5) for sensitivity analysis
- **Final index**: PC1 from PCA on first imputed dataset

## Variable Selection Rationale

### Correlation-Based Selection

Based on correlation analysis (see `sdg3_correlation_analysis` target), we selected 3 SDG3 mortality indicators with strong intercorrelations for the imputation model:

| Variable | Description | Correlation with under-5 mortality | Missing (%) |
|----------|-------------|-------------------------------------|-------------|
| `sdg3_under_5_mortality_rate` | Under-5 mortality per 1,000 live births | 1.000 (baseline) | 15.9% (46/290) |
| `sdg3_neonatal_mortality_rate` | Neonatal mortality per 1,000 live births | 0.967 | 6.9% (20/290) |
| `sdg3_maternal_mortality_ratio` | Maternal deaths per 100,000 live births | 0.744 | 6.9% (20/290) |

**Correlation matrix for 3 mortality indicators:**
- Under-5 ↔ Neonatal: **r = 0.967** (extremely strong)
- Under-5 ↔ Maternal: **r = 0.744** (strong)
- Neonatal ↔ Maternal: **r = 0.757** (strong)

**Rationale for selection:**
1. **Very high intercorrelations (r = 0.74-0.97)**: These variables measure closely related aspects of health system performance and can reliably predict each other's missing values
2. **Conceptual coherence**: All 3 variables measure mortality outcomes (not system inputs), representing the same underlying construct
3. **Manageable missingness**: 6.9-15.9% missing data is moderate and appropriate for imputation
4. **Complete coverage**: Together these variables capture both child health (under-5, neonatal) and maternal health

### Excluded Variables: Other SDG3 Indicators

Two additional SDG3 variables were **excluded** from the imputation model:

| Excluded Variable | Correlation with under-5 mortality | Missing (%) | Reason for Exclusion |
|-------------------|-------------------------------------|-------------|---------------------|
| `sdg3_prop_births_with_skilled_personnel` | -0.797 | 59% (171/290) | Excessive missingness, conceptual incoherence |
| `sdg3_coverage_of_essential_health_services` | -0.876 | 64% (186/290) | Structural biennial pattern, excessive missingness |

**Despite strong correlations**, these variables were excluded for compelling reasons:

#### 1. Excessive Missingness
- **Skilled birth attendance**: 59% missing (171/290 observations)
- **Health coverage**: 64% missing (186/290 observations)

With >50% missing, including these variables would:
- Further reduce effective sample size
- Increase imputation uncertainty
- Risk model instability and convergence issues

#### 2. Structural Missingness Pattern
**Health coverage indicator** has structural biennial (2-year) collection:
- **100% missing in even years** (2014, 2016, 2018, 2020, 2022)
- Data only collected in odd years (2015, 2017, 2019, 2021, 2023)

This is **NOT random missingness** but a systematic data collection design. Cannot impute values for unmeasured years without strong assumptions about temporal stability.

#### 3. Conceptual Incoherence
**Coverage indicators measure system INPUTS** (healthcare access, skilled personnel), while **mortality indicators measure OUTPUTS** (health outcomes).

These represent different constructs:
- **Inputs** (coverage, personnel): Institutional quality, healthcare system capacity
- **Outputs** (mortality): Population health outcomes

**Why this matters for PCA:**
- PCA assumes variables measure the same underlying dimension
- Mixing inputs and outputs violates this assumption
- Would create an uninterpretable composite index (what does "health quality" mean if it mixes access and outcomes?)
- Countries can have:
  - High coverage but high mortality (system inefficiency, other determinants)
  - Low coverage but low mortality (cultural factors, low disease burden)

**Comparison to SDG4:**
- SDG4 imputation uses only **outcome variables** (completion rates, literacy) - conceptually coherent
- Excluded teacher training (input variable) for same conceptual reasons
- SDG3 follows this principle: use only mortality outcomes, exclude coverage inputs

### Excluded Variables: Moderators and Tax Variables

**CRITICAL METHODOLOGICAL DECISION**: Moderating variables (GDP per capita, population, debt-to-GDP ratio) and tax structure variables (goods/services tax, consumption tax, income tax) are **intentionally excluded** from the imputation model.

**Correlation with under-5 mortality:**
| Variable | Correlation | Relationship |
|----------|-------------|--------------|
| `mod_gdp_per_capita` | -0.716 | Strong negative |
| `mod_macroeconomic_population` | 0.104 | Weak positive |
| `mod_debt_to_gdp_ratio` | -0.087 | Weak negative |
| `tax_goods_and_services` | -0.268 | Weak negative |
| `tax_general_consumption` | -0.289 | Weak negative |
| `tax_income_and_profits` | -0.258 | Weak negative |

**Why exclude despite moderate-strong correlations?**

**Methodological Integrity - Preserving Relationships for Regression Analysis:**

The core purpose of this thesis is to study the **relationships** between tax structure and SDG outcomes, with moderating effects from economic/demographic variables. Including these variables in the imputation model would **contaminate** the very relationships we want to estimate.

**Problem if included:**
1. **Circularity**: We would use tax→SDG3 relationships to impute missing SDG3 values, then use those imputed values to estimate tax→SDG3 relationships. Results would partially reflect our imputation model assumptions rather than observed data.

2. **Synthetic relationships**: The imputed SDG3 values would be mathematically constructed to correlate with tax/moderator variables in a specific way (determined by the imputation model). Regression analysis would then "discover" relationships we built into the data.

3. **Inability to distinguish**: We could not determine whether regression results reflect:
   - Real observed patterns in the data (what we want to study)
   - Synthetic patterns created by imputation assumptions (methodological artifact)

4. **Committee defensibility**: Thesis committee would reasonably question: "Are these results from your data or from your imputation model?"

**Fundamental Principle:**
> Impute outcome variables using **ONLY** related outcome variables that measure the same construct. **NEVER** use predictor variables (moderators, tax) that will later be used to explain that outcome in regression analysis.

This principle maintains the distinction between:
- **Imputation model**: Uses only SDG3 mortality variables to predict each other (r = 0.74-0.97)
- **Regression model**: Uses tax/moderator variables to explain SDG3 index (independent analysis)

**Trade-off accepted:**
We sacrifice potential predictive power in imputation (GDP r = -0.716 could help predict) to maintain methodological integrity for inference. The 3 SDG3 variables have sufficient intercorrelation (r = 0.74-0.97) to provide reliable imputation without compromising our ability to study causal relationships.

## Imputation Method: Multilevel Panel Imputation (2l.pan)

### Panel Data Structure

The dataset has a **nested panel structure**:
- **Level 1**: Observations (country-year combinations)
- **Level 2**: Countries (29 Asia-Pacific economies)
- **Time dimension**: Years (2014-2023)

Each observation is nested within a country, and observations from the same country are more similar than observations from different countries (intraclass correlation). Standard imputation methods (PMM, norm) ignore this clustering and can produce biased estimates and underestimated standard errors.

### Why 2l.pan (Two-Level Pan Method)?

The `2l.pan` method from the `mice` package implements **multilevel imputation** that respects the panel structure:

**Method characteristics:**
- **Two-level**: Accounts for clustering of observations within countries
- **Pan**: Uses the algorithm from Pan & Rubin (1999) for continuous variables
- **Random effects**: Allows each country to have its own baseline level (random intercept)
- **Fixed effects**: Year and other SDG3 variables act as fixed effect predictors

**Advantages for our data:**
1. **Respects country heterogeneity**: Recognizes that Philippines' mortality patterns differ from Singapore's in systematic ways
2. **Prevents cross-contamination**: Imputed values for Philippines are informed primarily by Philippines' own observed data, not Singapore's
3. **Appropriate uncertainty**: Standard errors account for clustering, avoiding false precision
4. **Theoretically justified**: Health systems have strong country-specific institutional characteristics (healthcare policy, infrastructure, cultural factors) that create within-country correlation

**Alternative methods considered and rejected:**
- **PMM (Predictive Mean Matching)**: Ignores country clustering, treats all observations as independent
- **2l.norm**: Similar to 2l.pan but assumes normal distribution (2l.pan more robust)
- **Fixed effects by dummy variables**: Creates 29 country dummies, causing convergence issues with small sample
- **Imputation by country**: Insufficient observations per country (n=10 years) for reliable imputation

### Predictor Matrix Specification

The predictor matrix defines how variables predict each other in the imputation model:

```r
# Type codes in mice predictor matrix:
# 0  = Don't use this variable
# 1  = Use as fixed effect predictor
# -2 = Use as cluster identifier (class variable)

predictor_matrix:
                                  country  country_id  year  sdg3_under5  sdg3_neonatal  sdg3_maternal
sdg3_under_5_mortality_rate            0          -2     1            0              1              1
sdg3_neonatal_mortality_rate           0          -2     1            1              0              1
sdg3_maternal_mortality_ratio          0          -2     1            1              1              0
```

**Rationale for each specification:**

1. **country = 0**: Character variable (country name) not used. Clustering captured by numeric country_id instead.

2. **country_id = -2**: Numeric cluster identifier (1-29) defining Level 2 units. Code `-2` tells mice this is the grouping variable for multilevel imputation.
   - **Why numeric?**: 2l.pan requires numeric cluster ID, not factor
   - **Created as**: `country_id = as.numeric(as.factor(country))`

3. **year = 1**: Time trend used as fixed effect predictor. Captures secular improvements in health over 2014-2023 (e.g., SDG implementation effects, global health interventions, economic development).
   - **Why not -2?**: Year is not a clustering variable; we don't model random effects across years
   - **Why not 0?**: Temporal patterns exist (mortality generally declining); year helps predict missing values

4. **SDG3 variables = 1 or 0**: Each SDG3 variable predicted by the other 2 SDG3 variables (self-prediction = 0).
   - **Very high correlations (r = 0.74-0.97)** mean each variable provides strong predictive information for the others
   - **Mutual prediction**: All 3 variables have missing data, so they impute each other iteratively

### Imputation Parameters

```r
m = 5       # Number of multiply imputed datasets
maxit = 20  # Maximum iterations for MICE algorithm
seed = 123  # Random seed for reproducibility
```

**Rationale:**

1. **m = 5 (Multiple Imputation Datasets)**:
   - Standard recommendation: m = 5-10 for ~10% missingness (Rubin 1987, van Buuren 2018)
   - Creates 5 complete datasets reflecting uncertainty in imputed values
   - Used for sensitivity analysis: Does SDG3 index vary substantially across imputations?
   - **Implementation note**: Primary analysis uses first imputation (action=1) for PCA, but all 5 are retained for robustness checks

2. **maxit = 20 (Convergence Iterations)**:
   - MICE uses iterative algorithm: cycles through variables, imputing each conditional on others
   - 20 iterations typically sufficient for convergence with 3 variables
   - **Validation**: Convergence plots (trace plots) visually confirm stabilization

3. **seed = 123 (Reproducibility)**:
   - Ensures identical results across runs
   - Critical for thesis defense: committee can replicate exact results

## Why Imputation → PCA (not PCA → Imputation)?

**Sequential approach:** First impute missing values using multilevel panel imputation, then apply PCA to the completed dataset.

**Rationale:**

1. **PCA requires complete data**: `prcomp()` in R cannot handle missing values. Must either:
   - Listwise deletion: Drop all observations with any missing data (would lose 16% of data)
   - Imputation: Fill in missing values before PCA (chosen approach)

2. **High correlations enable reliable imputation**: Strong intercorrelations (r = 0.74-0.97) among SDG3 variables mean each variable provides substantial information for predicting others' missing values.

3. **Standard practice**: Imputation → dimension reduction is well-established in multivariate analysis (van Buuren 2018, Little & Rubin 2019).

4. **PCA assumptions met**: After imputation, the completed data satisfies PCA requirements:
   - Sufficient correlations (validated by KMO test)
   - Linear relationships (validated by Bartlett test)
   - Complete data matrix (by construction)

**Alternative considered: Single imputation with PCA projection**
- Could impute only `sdg3_under_5_mortality_rate`, then join with complete cases for PCA
- Rejected because: Reduces effective sample size, doesn't leverage correlations among all 3 variables

## Validation and Diagnostics

### Pre-Imputation: Sample Size and Missingness Patterns

**Original dataset**: 290 observations (29 countries × 10 years)

**Missingness for 3 SDG3 variables**:
- sdg3_under_5_mortality_rate: 46 missing (15.9%)
- sdg3_neonatal_mortality_rate: 20 missing (6.9%)
- sdg3_maternal_mortality_ratio: 20 missing (6.9%)

**Complete cases** (all 3 variables observed): 243 observations (83.8% retention)

**Missingness pattern**:
- No systematic clustering by country or year (visual inspection via missingness heatmap)
- Assumption: Missing At Random (MAR) conditional on observed SDG3 variables and year
- **Cannot test**: Missing Not At Random (MNAR) requires external validation data

### Post-Imputation: Convergence Diagnostics

**Trace plots** (`plot(imp_model)`):
- Shows mean and standard deviation of imputed values across iterations
- **Good convergence**: Lines stabilize (no trends or oscillations) after ~5-10 iterations
- **Bad convergence**: Lines show trends, cycles, or divergence (would indicate model misspecification)

**Visual check**: Included in `sdg3_imputation_diagnostics` target for Quarto reporting

### Post-Imputation: Distribution Comparison

**Density plots** (`densityplot(imp_model, ~variable)`):
- Compares distribution of observed values (blue) vs imputed values (red/pink)
- **Ideal result**: Imputed distribution overlaps well with observed distribution
- **Red flag**: Imputed distribution substantially different (bimodal, shifted mean, different variance)
- **Purpose**: Validates that imputation model produces plausible values within observed range

**Visual check**: Generated for all 3 SDG3 variables in `sdg3_imputation_diagnostics` target

### Post-Imputation: PCA Suitability Tests

Before applying PCA to imputed data, validate that assumptions are met:

**1. Kaiser-Meyer-Olkin (KMO) Test** - Sampling Adequacy
- Measures whether variables share enough common variance for PCA
- **Interpretation**:
  - KMO > 0.9: Marvelous
  - KMO > 0.8: Meritorious
  - KMO > 0.7: Middling (acceptable)
  - KMO > 0.6: Mediocre (questionable)
  - KMO < 0.6: Unacceptable
- **Expected result**: KMO > 0.7 given strong correlations (r = 0.74-0.97)

**2. Bartlett's Test of Sphericity**
- Tests whether correlation matrix is significantly different from identity matrix
- **Null hypothesis**: Variables are uncorrelated (correlation matrix = identity)
- **Interpretation**: p < 0.05 rejects null, confirms correlations exist, PCA appropriate
- **Expected result**: p < 0.001 (highly significant)

**3. Variance Explained by PC1**
- First principal component should capture majority of variance
- **Rule of thumb**: PC1 > 60% variance for good composite index
- **Expected result**: PC1 > 75% given very strong correlations

**Implementation**: `validate_pca_suitability()` function runs all 3 tests on first imputed dataset

### Sensitivity Analysis: Cross-Imputation Comparison

**Strategy**: Compare SDG3 indices across all 5 imputed datasets

**Implementation**:
```r
# Extract all 5 imputed datasets
all_imputations <- extract_sdg3_imputed_data(sdg3_imputation_fit, action = "all")

# Compute SDG3 index for each
indices <- lapply(all_imputations, function(d) {
  pca <- prcomp(d[, c("sdg3_under_5_mortality_rate", "sdg3_neonatal_mortality_rate",
                       "sdg3_maternal_mortality_ratio")],
                scale = TRUE)
  pca$x[, "PC1"]
})

# Check correlation among indices
cor_matrix <- cor(do.call(cbind, indices))
```

**Interpretation**:
- **High correlation (r > 0.95)**: Indices are robust to imputation uncertainty
- **Low correlation (r < 0.90)**: Imputation choices strongly influence results; consider reporting sensitivity analysis in thesis

**Expected result**: Very high correlations (r > 0.98) given strong predictor variables and 2l.pan method

## PCA on Imputed Data

### Why PCA?

**Purpose**: Create a single composite index from 3 SDG3 mortality indicators for use as dependent variable in regression analysis.

**Advantages**:
1. **Data reduction**: Simplifies analysis from 3 outcomes to 1 composite measure
2. **Captures common variance**: PC1 represents shared "health system performance" dimension
3. **Weights variables optimally**: Variables with more variance/correlation contribute more
4. **Reduces multicollinearity**: Regression on 3 correlated outcomes would have inflated standard errors

**Alternative considered: Simple average**
- Could average the 3 variables with equal weights
- Rejected because: Ignores differential variance, doesn't capture latent structure, arbitrary weighting

### PCA Specification

```r
pca_model <- prcomp(imputed_data[, sdg3_variables], scale. = TRUE)
```

**Key parameter: `scale. = TRUE`** (standardization)
- Standardizes each variable to mean=0, sd=1 before PCA
- **Why necessary**: Variables measured on different scales (e.g., under-5 mortality 5-60 per 1000, maternal mortality 10-200 per 100,000)
- **Effect**: Ensures each variable contributes equally to PC1 based on correlations, not raw variances
- **Standard practice**: Always standardize for PCA when variables have different units

### Interpreting PC1 as SDG3 Index

**PC1 loadings** (expected):
- All 3 variables load positively (same direction) on PC1
- Indicates PC1 represents general "mortality burden" dimension
- Higher PC1 → higher mortality rates (worse health outcomes)

**Variance explained**:
- Expected: PC1 explains 75-85% of total variance
- Validates that a single composite index adequately captures the 3 variables

**Index directionality**:
- PC1 direction is arbitrary (eigenvector sign ambiguity)
- **Check**: Correlation between PC1 and original variables
  - If positive: Higher PC1 = higher mortality (worse health)
  - If negative: Reverse sign with `sdg3_index = -PC1` so higher = better health
- **For consistency with SDG4**: May reverse sign so higher values = better outcomes for both indices

### Using SDG3 Index in Regression

**Final outcome variable**: `sdg3_index` extracted from `pca_model$x[, "PC1"]`

**Interpretation in regression**:
- Unit: Standard deviations of PC1 (since standardized PCA)
- Coefficient interpretation: "A 1-unit increase in [tax variable] is associated with a β-standard-deviation change in the SDG3 health index, holding moderators constant"
- Substantive meaning: Change in overall health quality composite (weighted combination of mortality rates)

**Advantage over using raw variables**:
- Single clear estimate instead of 3 separate coefficients
- Avoids multiple testing issues
- Easier interpretation for policy recommendations

## Comparison with SDG4 Implementation

### Similarities (Methodological Consistency)

| Aspect | SDG3 | SDG4 |
|--------|------|------|
| **Method** | 2l.pan multilevel panel | 2l.pan multilevel panel |
| **Excluded from imputation** | Moderators, tax variables | Moderators, tax variables |
| **Variable type** | Outcome indicators only | Outcome indicators only |
| **Number of imputations** | m = 5 | m = 5 |
| **Iterations** | maxit = 20 | maxit = 20 |
| **PCA specification** | Standardized (scale=TRUE) | Standardized (scale=TRUE) |
| **Diagnostics** | Convergence, density plots | Convergence, density plots |
| **Validation** | KMO, Bartlett tests | KMO, Bartlett tests |

### Differences (Data-Driven)

| Aspect | SDG3 | SDG4 |
|--------|------|------|
| **Variables imputed** | 3 mortality indicators | 4 education indicators |
| **Missingness** | 6.9-15.9% | 33-35% |
| **Correlations** | r = 0.74-0.97 | r = 0.74-0.93 |
| **Complete cases before** | 243/290 (83.8%) | 167/290 (57.6%) |
| **Complete cases after** | 290/290 (100%) | 290/290 (100%) |
| **Expected PC1 variance** | 75-85% | 70-80% |
| **Excluded indicators** | 2 coverage (59-64% missing) | 7 education (weak r or >50% missing) |

**Key insight**: Both SDG3 and SDG4 use identical methodology, adapted to data-specific characteristics (missingness, correlations). Ensures methodological consistency across thesis.

## Limitations and Assumptions

### 1. Missing At Random (MAR) Assumption

**Assumption**: Missing data mechanism is MAR conditional on observed SDG3 variables and year.

**Meaning**:
- Probability of missingness for `sdg3_under_5_mortality_rate` depends on observed values of other SDG3 variables, year, and country
- Does NOT depend on the unobserved value of `sdg3_under_5_mortality_rate` itself

**Cannot test**: MAR is untestable without external validation data

**Violation scenario** (MNAR - Missing Not At Random):
- If countries with extremely high mortality systematically fail to report data
- If missing data is due to political instability/conflict correlated with health crisis
- Would lead to biased imputation (underestimate mortality burden)

**Mitigation**:
- Used multiple correlated SDG3 variables (reduces bias if MAR holds)
- Multilevel structure captures country-specific patterns
- Sensitivity analysis across 5 imputations checks robustness

### 2. Model-Based Imputation Dependency

**Issue**: Imputed values are **synthetic data** generated by a statistical model, not observed reality.

**Implications**:
1. **Model misspecification**: If 2l.pan assumptions wrong (e.g., non-linear relationships, non-normal errors), imputed values biased
2. **Uncertainty understatement**: Treating imputed values as if observed can underestimate standard errors in regression
3. **Synthetic relationships**: Imputed data reflects our modeling assumptions, not necessarily true data patterns

**Mitigation**:
- Used well-established method (2l.pan) with strong theoretical justification
- Diagnostic plots validate plausibility of imputed values
- Multiple imputation (m=5) acknowledges uncertainty
- Sensitivity analysis checks robustness

**Reporting transparency**: Thesis should clearly state:
- Proportion of data imputed (6.9-15.9% for each variable)
- Method used (multilevel panel imputation)
- Validation checks performed
- Sensitivity analysis results

### 3. PCA Interpretation Ambiguity

**Issue**: PC1 is a mathematical construct, not a directly measured quantity.

**Challenges**:
1. **Arbitrary direction**: Sign of PC1 arbitrary (can reverse)
2. **Weighting opacity**: PC1 weights variables by eigenvalues, not theoretical importance
3. **Latent construct**: Assumes a single underlying "health quality" dimension exists

**Mitigation**:
- Validate with KMO, Bartlett tests (confirms single dimension reasonable)
- Check variance explained (PC1 > 60% indicates dominant dimension)
- Inspect loadings (all variables load similarly → coherent construct)
- Sensitivity analysis (compare indices across imputations)

**Reporting**: Thesis should report:
- PC1 variance explained
- Variable loadings on PC1
- Validation test results (KMO, Bartlett)
- Interpretation as "general health system performance" composite

### 4. Excluded Coverage Indicators

**Issue**: Dropped 2 SDG3 coverage indicators due to missingness and conceptual concerns.

**Consequence**: SDG3 index represents only **mortality outcomes**, not healthcare **access/coverage**.

**Justification**:
- Conceptual coherence (all outcomes vs mixing outcomes + inputs)
- Methodological constraints (59-64% missing, biennial pattern)
- Consistency with SDG4 approach (outcomes only)

**Alternative interpretation**: Could argue that comprehensive health index should include coverage.

**Mitigation**: Thesis should:
- Clearly label SDG3 index as "mortality-based health index"
- Acknowledge coverage indicators excluded
- Justify on methodological and conceptual grounds
- Note as limitation

## References

### Methodological Literature

**Multiple Imputation:**
- van Buuren, S., & Groothuis-Oudshoorn, K. (2011). mice: Multivariate Imputation by Chained Equations in R. *Journal of Statistical Software*, 45(3), 1-67. https://doi.org/10.18637/jss.v045.i03
- Little, R. J., & Rubin, D. B. (2019). *Statistical Analysis with Missing Data* (3rd ed.). Wiley.
- Rubin, D. B. (1987). *Multiple Imputation for Nonresponse in Surveys*. Wiley.

**Multilevel Imputation:**
- Pan, J., & Rubin, D. B. (1999). *Markov Chain Monte Carlo Methods for Missing Data*. Technical report, Department of Statistics, Harvard University.
- Grund, S., Lüdtke, O., & Robitzsch, A. (2018). Multiple Imputation of Missing Data for Multilevel Models: Simulations and Recommendations. *Organizational Research Methods*, 21(1), 111-149.

**Principal Component Analysis:**
- Kaiser, H. F. (1974). An index of factorial simplicity. *Psychometrika*, 39(1), 31-36. [KMO test]
- Bartlett, M. S. (1950). Tests of significance in factor analysis. *British Journal of Psychology, Statistical Section*, 3(2), 77-85.

### Project-Specific Documentation

**Correlation analysis**:
- `sdg3_correlation_analysis` target - Documents variable selection based on correlations

**Function documentation**:
- [R/functions.R](functions.R) - All 3 SDG3 imputation functions with roxygen2 documentation

**Pipeline definition**:
- [_targets.R](../_targets.R) - Complete targets pipeline with SDG3 and SDG4 workflows

**Parallel implementation**:
- [R/documentation_sdg4_imputation.md](documentation_sdg4_imputation.md) - SDG4 methodology (same approach)

---

## Document Metadata

**Purpose**: Thesis methodology chapter reference, committee defense preparation, replication documentation

**Author**: Statistical consulting project for undergraduate thesis

**Last updated**: 2026-01-01

**Corresponding functions**:
- `analyze_sdg3_correlations()`
- `fit_sdg3_panel_imputation()`
- `extract_sdg3_imputed_data()`
- `generate_sdg3_imputation_diagnostics()`
- `fit_sdg3_pca()` (modified)
- `validate_pca_suitability()` (shared)
- `extract_sdg3_index()` (unchanged)

**Targets**: 10 SDG3-related targets in `_targets.R`:
- `sdg3_correlation_analysis`
- `sdg3_imputation_fit`
- `sdg3_imputed_datasets`
- `sdg3_imputed_primary`
- `sdg3_imputed_long`
- `sdg3_imputation_diagnostics`
- `sdg3_pca_validation`
- `sdg3_pca_fit`
- `sdg3_index`

**For questions or clarifications**, refer to function roxygen2 documentation or contact project consultant.

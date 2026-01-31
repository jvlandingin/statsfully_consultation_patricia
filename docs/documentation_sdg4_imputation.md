# SDG4 Education Imputation Methodology

## Executive Summary

This document explains the methodological decisions for imputing missing values in SDG4 education indicators prior to creating a composite index via Principal Component Analysis (PCA). The imputation strategy uses multilevel panel imputation (2l.pan method) on 4 highly correlated SDG4 variables, while intentionally excluding moderating and tax variables to preserve the relationships we intend to study in subsequent regression analysis.

**Key Decisions:**
- **Variables imputed**: 4 SDG4 education indicators (r = 0.74-0.93)
- **Variables excluded**: Moderators, tax variables, weakly correlated SDG4 variables
- **Method**: Multilevel panel imputation (2l.pan) respecting country clustering
- **Datasets created**: 5 multiply imputed datasets (m=5) for sensitivity analysis
- **Final index**: PC1 from PCA on first imputed dataset

## Variable Selection Rationale

### Correlation-Based Selection

Based on correlation analysis ([R/analyze_sdg4_correlations.R](analyze_sdg4_correlations.R)), we selected 4 SDG4 education variables with strong intercorrelations for the imputation model:

| Variable | Description | Correlation with sdg4_lower_secondary | Missing (%) |
|----------|-------------|--------------------------------------|-------------|
| `sdg4_lower_secondary` | Lower secondary completion rate | 1.000 (baseline) | 33% (96/290) |
| `sdg4_prop_primary_secondary` | Primary to secondary transition rate | 0.926 | 34% (99/290) |
| `sdg4_adult_literacy_rate` | Adult literacy rate | 0.776 | 35% (101/290) |
| `sdg4_upper_secondary` | Upper secondary completion rate | 0.744 | 34% (99/290) |

**Rationale for selection:**
1. **High intercorrelations (r > 0.74)**: These variables measure closely related aspects of educational attainment and can reliably predict each other's missing values
2. **Conceptual coherence**: All 4 variables measure educational outcomes (completion rates, literacy), not system inputs
3. **Reasonable missingness**: 33-35% missing data is substantial but not excessive for imputation
4. **Complete coverage**: Together these variables capture both basic literacy and secondary education progression

### Excluded Variables: Other SDG4 Indicators

Several SDG4 variables were **excluded** from the imputation model despite being available:

| Excluded Variable | Correlation | Missing (%) | Reason for Exclusion |
|-------------------|-------------|-------------|---------------------|
| `sdg4_prop_students_proficient_math` | 0.191 | 69% | Weak correlation, excessive missingness |
| `sdg4_prop_students_proficient_reading` | 0.193 | 69% | Weak correlation, excessive missingness |
| `sdg4_prop_trained_teachers_pre_primary` | -0.025 | 51% | Near-zero correlation, excessive missingness |
| `sdg4_prop_trained_teachers_primary` | 0.165 | 39% | Weak correlation, conceptual incoherence |
| `sdg4_prop_trained_teachers_lower_secondary` | 0.066 | 42% | Near-zero correlation, conceptual incoherence |
| `sdg4_prop_trained_teachers_upper_secondary` | -0.072 | 42% | Near-zero correlation, conceptual incoherence |
| `sdg4_prop_trained_teachers_secondary` | 0.004 | 42% | Near-zero correlation, conceptual incoherence |
| `sdg4_gender_gap_in_completion` | -0.114 | 69% | Weak correlation, excessive missingness |

**Rationale for exclusion:**
1. **Weak predictive power**: Correlations < 0.2 provide little information for imputing missing values
2. **Excessive missingness**: Variables with >50% missing would further reduce effective sample size
3. **Conceptual incoherence**: Teacher training measures system **inputs** (institutional quality), while completion rates measure **outcomes** (student attainment). These represent different constructs that don't necessarily correlate:
   - Countries can have highly trained teachers but low completion (retention issues, poverty)
   - Countries can have untrained teachers but high completion (cultural factors, family support)
4. **PCA assumption violation**: PCA assumes variables measure the same underlying dimension. Mixing inputs and outputs violates this assumption and creates an uninterpretable composite index

**Reference**: See [main.R lines 116-156](../main.R#L116-L156) for detailed rationale documented during exploratory analysis.

### Excluded Variables: Moderators and Tax Variables

**CRITICAL METHODOLOGICAL DECISION**: Moderating variables (GDP per capita, population, debt-to-GDP ratio) and tax structure variables (goods/services tax, consumption tax, income tax) are **intentionally excluded** from the imputation model.

**Correlation with sdg4_lower_secondary:**
| Variable | Correlation | Relationship |
|----------|-------------|--------------|
| `mod_gdp_per_capita` | 0.609 | Strong positive |
| `mod_macroeconomic_population` | -0.063 | Weak negative |
| `mod_debt_to_gpt_ratio` | 0.198 | Weak positive |
| `tax_goods_and_services` | 0.261 | Weak positive |
| `tax_general_consumption` | 0.248 | Weak positive |
| `tax_income_and_profits` | 0.136 | Weak positive |

**Why exclude despite moderate correlations?**

**Methodological Integrity - Preserving Relationships for Regression Analysis:**

The core purpose of this thesis is to study the **relationships** between tax structure and SDG outcomes, with moderating effects from economic/demographic variables. Including these variables in the imputation model would **contaminate** the very relationships we want to estimate.

**Problem if included:**
1. **Circularity**: We would use tax→SDG4 relationships to impute missing SDG4 values, then use those imputed values to estimate tax→SDG4 relationships. Results would partially reflect our imputation model assumptions rather than observed data.

2. **Synthetic relationships**: The imputed SDG4 values would be mathematically constructed to correlate with tax/moderator variables in a specific way (determined by the imputation model). Regression analysis would then "discover" relationships we built into the data.

3. **Inability to distinguish**: We could not determine whether regression results reflect:
   - Real observed patterns in the data (what we want to study)
   - Synthetic patterns created by imputation assumptions (methodological artifact)

4. **Committee defensibility**: Thesis committee would reasonably question: "Are these results from your data or from your imputation model?"

**Fundamental Principle:**
> Impute outcome variables using **ONLY** related outcome variables that measure the same construct. **NEVER** use predictor variables (moderators, tax) that will later be used to explain that outcome in regression analysis.

This principle maintains the distinction between:
- **Imputation model**: Uses only SDG4 education variables to predict each other (r = 0.74-0.93)
- **Regression model**: Uses tax/moderator variables to explain SDG4 index (independent analysis)

**Trade-off accepted:**
We sacrifice potential predictive power in imputation (GDP r = 0.609 could help predict) to maintain methodological integrity for inference. The 4 SDG4 variables have sufficient intercorrelation (r = 0.74-0.93) to provide reliable imputation without compromising our ability to study causal relationships.

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
- **Fixed effects**: Year and other SDG4 variables act as fixed effect predictors

**Advantages for our data:**
1. **Respects country heterogeneity**: Recognizes that Vietnam's education patterns differ from Singapore's in systematic ways
2. **Prevents cross-contamination**: Imputed values for Vietnam are informed primarily by Vietnam's own observed data, not Singapore's
3. **Appropriate uncertainty**: Standard errors account for clustering, avoiding false precision
4. **Theoretically justified**: Educational systems have strong country-specific institutional characteristics (language, policy, culture) that create within-country correlation

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
                                  country  country_id  year  sdg4_lower  sdg4_primary  sdg4_literacy  sdg4_upper
sdg4_lower_secondary                   0          -2     1            0             1              1           1
sdg4_prop_primary_secondary            0          -2     1            1             0              1           1
sdg4_adult_literacy_rate               0          -2     1            1             1              0           1
sdg4_upper_secondary                   0          -2     1            1             1              1           0
```

**Rationale for each specification:**

1. **country = 0**: Character variable (country name) not used. Clustering captured by numeric country_id instead.

2. **country_id = -2**: Numeric cluster identifier (1-29) defining Level 2 units. Code `-2` tells mice this is the grouping variable for multilevel imputation.
   - **Why numeric?**: 2l.pan requires numeric cluster ID, not factor
   - **Created as**: `country_id = as.numeric(as.factor(country))`

3. **year = 1**: Time trend used as fixed effect predictor. Captures secular improvements in education over 2014-2023 (e.g., SDG implementation effects, global education expansion).
   - **Why not -2?**: Year is not a clustering variable; we don't model random effects across years
   - **Why not 0?**: Temporal patterns exist (education generally improving); year helps predict missing values

4. **SDG4 variables = 1 or 0**: Each SDG4 variable predicted by the other 3 SDG4 variables (self-prediction = 0).
   - **High correlations (r = 0.74-0.93)** mean each variable provides strong predictive information for the others
   - **Mutual prediction**: All 4 variables have missing data, so they impute each other iteratively

### Imputation Parameters

```r
m = 5       # Number of multiply imputed datasets
maxit = 20  # Maximum iterations for MICE algorithm
seed = 123  # Random seed for reproducibility
```

**Rationale:**

1. **m = 5 (Multiple Imputation Datasets)**:
   - Standard recommendation: m = 5-10 for ~30% missingness (Rubin 1987, van Buuren 2018)
   - Creates 5 complete datasets reflecting uncertainty in imputed values
   - Used for sensitivity analysis: Does SDG4 index vary substantially across imputations?
   - **Implementation note**: Primary analysis uses first imputation (action=1) for PCA, but all 5 are retained for robustness checks

2. **maxit = 20 (Convergence Iterations)**:
   - MICE uses iterative algorithm: cycles through variables, imputing each conditional on others
   - 20 iterations typically sufficient for convergence with 4 variables
   - **Validation**: Convergence plots (trace plots) visually confirm stabilization

3. **seed = 123 (Reproducibility)**:
   - Ensures identical results across runs
   - Critical for thesis defense: committee can replicate exact results

## Why Imputation → PCA (not PCA → Imputation)?

**Sequential approach:** First impute missing values using multilevel panel imputation, then apply PCA to the completed dataset.

**Rationale:**

1. **PCA requires complete data**: `prcomp()` in R cannot handle missing values. Must either:
   - Listwise deletion: Drop all observations with any missing data (would lose 67% of data)
   - Imputation: Fill in missing values before PCA (chosen approach)

2. **High correlations enable reliable imputation**: Strong intercorrelations (r = 0.74-0.93) among SDG4 variables mean each variable provides substantial information for predicting others' missing values.

3. **Standard practice**: Imputation → dimension reduction is well-established in multivariate analysis (van Buuren 2018, Little & Rubin 2019).

4. **PCA assumptions met**: After imputation, the completed data satisfies PCA requirements:
   - Sufficient correlations (validated by KMO test)
   - Linear relationships (validated by Bartlett test)
   - Complete data matrix (by construction)

**Alternative considered: Single imputation with PCA projection**
- Could impute only `sdg4_lower_secondary`, then join with complete cases for PCA
- Rejected because: Reduces effective sample size, doesn't leverage correlations among all 4 variables

## Validation and Diagnostics

### Pre-Imputation: Sample Size and Missingness Patterns

**Original dataset**: 290 observations (29 countries × 10 years)

**Missingness for 4 SDG4 variables**:
- sdg4_lower_secondary: 96 missing (33%)
- sdg4_prop_primary_secondary: 99 missing (34%)
- sdg4_adult_literacy_rate: 101 missing (35%)
- sdg4_upper_secondary: 99 missing (34%)

**Complete cases** (all 4 variables observed): 167 observations (58% retention)

**Missingness pattern analysis** (see `main.R` lines 159-181):
- Visual inspection: Heatmap shows missing data not systematically clustered by country or year
- Assumption: Missing At Random (MAR) conditional on observed SDG4 variables and year
- **Cannot test**: Missing Not At Random (MNAR) requires external validation data

### Post-Imputation: Convergence Diagnostics

**Trace plots** (`plot(imp_model)`):
- Shows mean and standard deviation of imputed values across iterations
- **Good convergence**: Lines stabilize (no trends or oscillations) after ~5-10 iterations
- **Bad convergence**: Lines show trends, cycles, or divergence (would indicate model misspecification)

**Visual check**: Included in `sdg4_imputation_diagnostics` target for Quarto reporting

### Post-Imputation: Distribution Comparison

**Density plots** (`densityplot(imp_model, ~variable)`):
- Compares distribution of observed values (blue) vs imputed values (red/pink)
- **Ideal result**: Imputed distribution overlaps well with observed distribution
- **Red flag**: Imputed distribution substantially different (bimodal, shifted mean, different variance)
- **Purpose**: Validates that imputation model produces plausible values within observed range

**Visual check**: Generated for all 4 SDG4 variables in `sdg4_imputation_diagnostics` target

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
- **Expected result**: KMO > 0.7 given strong correlations (r = 0.74-0.93)

**2. Bartlett's Test of Sphericity**
- Tests whether correlation matrix is significantly different from identity matrix
- **Null hypothesis**: Variables are uncorrelated (correlation matrix = identity)
- **Interpretation**: p < 0.05 rejects null, confirms correlations exist, PCA appropriate
- **Expected result**: p < 0.001 (highly significant)

**3. Variance Explained by PC1**
- First principal component should capture majority of variance
- **Rule of thumb**: PC1 > 60% variance for good composite index
- **Expected result**: PC1 > 70% given strong correlations

**Implementation**: `validate_sdg4_pca_suitability()` function runs all 3 tests on first imputed dataset

### Sensitivity Analysis: Cross-Imputation Comparison

**Strategy**: Compare SDG4 indices across all 5 imputed datasets

**Implementation**:
```r
# Extract all 5 imputed datasets
all_imputations <- extract_sdg4_imputed_data(sdg4_imputation_fit, action = "all")

# Compute SDG4 index for each
indices <- lapply(all_imputations, function(d) {
  pca <- prcomp(d[, c("sdg4_lower_secondary", "sdg4_prop_primary_secondary",
                       "sdg4_adult_literacy_rate", "sdg4_upper_secondary")],
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

**Purpose**: Create a single composite index from 4 SDG4 education variables for use as dependent variable in regression analysis.

**Advantages**:
1. **Data reduction**: Simplifies analysis from 4 outcomes to 1 composite measure
2. **Captures common variance**: PC1 represents shared "education quality" dimension
3. **Weights variables optimally**: Variables with more variance/correlation contribute more
4. **Reduces multicollinearity**: Regression on 4 correlated outcomes would have inflated standard errors

**Alternative considered: Simple average**
- Could average the 4 variables with equal weights
- Rejected because: Ignores differential variance, doesn't capture latent structure, arbitrary weighting

### PCA Specification

```r
pca_model <- prcomp(imputed_data[, sdg4_variables], scale. = TRUE)
```

**Key parameter: `scale. = TRUE`** (standardization)
- Standardizes each variable to mean=0, sd=1 before PCA
- **Why necessary**: Variables measured on different scales (e.g., literacy rate 60-100%, completion rate 20-90%)
- **Effect**: Ensures each variable contributes equally to PC1 based on correlations, not raw variances
- **Standard practice**: Always standardize for PCA when variables have different units

### Interpreting PC1 as SDG4 Index

**PC1 loadings** (expected):
- All 4 variables load positively (same direction) on PC1
- Indicates PC1 represents general "education attainment" dimension
- Higher PC1 → higher completion rates, higher literacy

**Variance explained**:
- Expected: PC1 explains 70-80% of total variance
- Validates that a single composite index adequately captures the 4 variables

**Index directionality**:
- PC1 direction is arbitrary (eigenvector sign ambiguity)
- **Check**: Correlation between PC1 and original variables
  - If positive: Higher PC1 = better education (desired)
  - If negative: Reverse sign with `sdg4_index = -PC1`

### Using SDG4 Index in Regression

**Final outcome variable**: `sdg4_index` extracted from `pca_model$x[, "PC1"]`

**Interpretation in regression**:
- Unit: Standard deviations of PC1 (since standardized PCA)
- Coefficient interpretation: "A 1-unit increase in [tax variable] is associated with a β-standard-deviation change in the SDG4 education index, holding moderators constant"
- Substantive meaning: Change in overall education quality composite (weighted combination of completion rates and literacy)

**Advantage over using raw variables**:
- Single clear estimate instead of 4 separate coefficients
- Avoids multiple testing issues
- Easier interpretation for policy recommendations

## Limitations and Assumptions

### 1. Missing At Random (MAR) Assumption

**Assumption**: Missing data mechanism is MAR conditional on observed SDG4 variables and year.

**Meaning**:
- Probability of missingness for `sdg4_lower_secondary` depends on observed values of other SDG4 variables, year, and country
- Does NOT depend on the unobserved value of `sdg4_lower_secondary` itself

**Cannot test**: MAR is untestable without external validation data

**Violation scenario** (MNAR - Missing Not At Random):
- If countries with extremely low completion rates systematically fail to report data
- If missing data is due to political censorship correlated with education quality
- Would lead to biased imputation (overestimate education levels)

**Mitigation**:
- Used multiple correlated SDG4 variables (reduces bias if MAR holds)
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
- Proportion of data imputed (33-35% for each variable)
- Method used (multilevel panel imputation)
- Validation checks performed
- Sensitivity analysis results

### 3. PCA Interpretation Ambiguity

**Issue**: PC1 is a mathematical construct, not a directly measured quantity.

**Challenges**:
1. **Arbitrary direction**: Sign of PC1 arbitrary (can reverse)
2. **Weighting opacity**: PC1 weights variables by eigenvalues, not theoretical importance
3. **Latent construct**: Assumes a single underlying "education quality" dimension exists

**Mitigation**:
- Validate with KMO, Bartlett tests (confirms single dimension reasonable)
- Check variance explained (PC1 > 60% indicates dominant dimension)
- Inspect loadings (all variables load similarly → coherent construct)
- Sensitivity analysis (compare indices across imputations)

**Reporting**: Thesis should report:
- PC1 variance explained
- Variable loadings on PC1
- Validation test results (KMO, Bartlett)
- Interpretation as "general education attainment" composite

### 4. Listwise Deletion for PCA

**Issue**: Even after imputation, some observations may have missing data in non-SDG4 variables (tax, moderators).

**Consequence**: Final regression analysis uses only observations with complete data on all variables (tax, moderators, SDG indices).

**Sample size reduction**:
- Original: 290 observations
- After PCA: 290 (imputation fills SDG4 gaps)
- After merging with tax/moderators: Depends on missingness in those variables

**Mitigation**:
- Imputation focused on SDG4 variables (dependent variable priority)
- Tax and moderator variables may need separate imputation (future work)
- Report effective sample size in regression analysis

## Implementation in Targets Pipeline

### Function Workflow

```
tax_structure_and_sdg (290 obs, 29 countries, 2014-2023)
    │
    ├─► fit_sdg4_panel_imputation()
    │   └─► Returns: imp_model (mice object), panel_data, predictor_matrix, method
    │
    ├─► extract_sdg4_imputed_data(action = "all")
    │   └─► Returns: List of 5 imputed datasets
    │
    ├─► extract_sdg4_imputed_data(action = 1)
    │   └─► Returns: Primary imputed dataset (for PCA)
    │
    ├─► extract_sdg4_imputed_data(action = "long")
    │   └─► Returns: Long format with .imp and .id (for pooled analysis)
    │
    ├─► generate_sdg4_imputation_diagnostics()
    │   └─► Returns: List with convergence plot + 4 density plots
    │
    ├─► validate_sdg4_pca_suitability()
    │   └─► Returns: KMO, Bartlett, correlation matrix, variance explained
    │
    ├─► fit_sdg4_pca()
    │   └─► Returns: pca_model (prcomp object) + data with country/year
    │
    └─► extract_sdg4_index()
        └─► Returns: Tibble with country, year, sdg4_index (PC1 scores)
```

### Targets Pipeline Structure

**8 SDG4 targets** (mirroring SDG3 pattern):

1. `sdg4_imputation_fit` - Fitted mice model
2. `sdg4_imputed_datasets` - All 5 imputations (sensitivity analysis)
3. `sdg4_imputed_primary` - First imputation (for PCA)
4. `sdg4_imputed_long` - Long format (for pooled analysis)
5. `sdg4_imputation_diagnostics` - Convergence/density plots
6. `sdg4_pca_validation` - KMO, Bartlett tests
7. `sdg4_pca_fit` - PCA model
8. `sdg4_index` - Final composite index

**Integration with analysis-ready data**:
```r
analysis_ready_data <- tax_structure_and_sdg |>
  left_join(sdg3_index, by = c("country", "year")) |>
  left_join(sdg4_index, by = c("country", "year"))
```

**Package dependencies**:
- `mice` - Multilevel panel imputation
- `lattice` - Density plots for mice diagnostics
- `psych` - KMO and Bartlett tests

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
- [R/analyze_sdg4_correlations.R](analyze_sdg4_correlations.R) - Documents variable selection based on correlations

**Reference implementation**:
- [main.R lines 343-427](../main.R#L343-L427) - Working panel imputation code
- [main.R lines 116-156](../main.R#L116-L156) - Rationale for NOT using PCA on incomplete data

**Function documentation**:
- [R/functions.R](functions.R) - All 6 SDG4 functions with roxygen2 documentation

**Pipeline definition**:
- [_targets.R](_targets.R) - Complete targets pipeline with SDG3 and SDG4 workflows

---

## Document Metadata

**Purpose**: Thesis methodology chapter reference, committee defense preparation, replication documentation

**Author**: Statistical consulting project for undergraduate thesis

**Last updated**: 2026-01-01

**Corresponding functions**:
- `fit_sdg4_panel_imputation()`
- `extract_sdg4_imputed_data()`
- `generate_sdg4_imputation_diagnostics()`
- `validate_sdg4_pca_suitability()`
- `fit_sdg4_pca()`
- `extract_sdg4_index()`

**Targets**: 8 SDG4 targets in `_targets.R` (lines 212-242)

**For questions or clarifications**, refer to function roxygen2 documentation or contact project consultant.

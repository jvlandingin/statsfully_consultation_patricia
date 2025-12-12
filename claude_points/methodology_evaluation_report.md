# Methodology Evaluation Report

## Thesis: Tax Structure Composition and Sustainable Development Goals in Asia-Pacific Countries

**Authors:** Mira, Paco, and Reyes
**Institution:** De La Salle University
**Evaluation Date:** December 2024

---

## Executive Summary

This report evaluates the research methodology proposed in the thesis examining how tax structure composition (income tax vs. consumption tax mix) affects SDG 3 (Health) and SDG 4 (Education) outcomes across 29 Asia-Pacific countries from 2014-2023.

**Overall Assessment: B+ / Acceptable with Revisions**

The methodology demonstrates competent understanding of panel data econometric methods with a comprehensive diagnostic testing framework. However, several concerns—particularly regarding endogeneity and temporal dynamics—should be addressed to strengthen causal inference claims.

---

## Part 1: Methodology Strengths

### 1.1 Robust Econometric Framework

The proposed methodology employs a comprehensive battery of diagnostic tests:

| Test | Purpose | Threshold |
|------|---------|-----------|
| Variance Inflation Factor (VIF) | Multicollinearity detection | VIF < 10 |
| Breusch-Pagan Test | Heteroscedasticity detection | p < 0.05 |
| Wooldridge Test | Serial correlation in panel data | p < 0.05 |
| Pesaran CD Test | Cross-sectional dependence | p < 0.05 |
| Unit Root Tests | Stationarity verification | p < 0.05 |

This testing battery follows established best practices in panel data econometrics.

### 1.2 Appropriate Model Selection Process

The three-test approach for model selection is methodologically sound:

1. **Breusch-Pagan LM Test** → Pooled OLS vs. Random Effects
2. **Hausman Test** → Fixed Effects vs. Random Effects
3. **Chow/F-Test** → Pooled OLS vs. Fixed Effects

This systematic approach ensures the most appropriate estimator is selected based on data characteristics rather than researcher preference.

### 1.3 Sound Variable Operationalization

- **PCA for Composite Indices:** Using Principal Component Analysis to aggregate multiple SDG indicators into composite indices is methodologically defensible and reduces dimensionality while preserving variance.

- **Z-Score Standardization:** The formula Z_ijt = (X_ijt - X̄_j) / s_j allows meaningful comparison across indicators with different scales.

- **Tax Mix Ratio:** The ratio of Income Tax to Consumption Tax provides a clear, interpretable measure of tax structure composition.

### 1.4 Proposed Regression Models

**Fixed Effects Model:**
```
SDG_it = α_i + β₁(TaxMix_it) + γ₁(TaxMix × Pop_it) + δ₁(TaxMix × GDPpc_it) + θ₁(TaxMix × DebtGDP_it) + λ_t + ε_it
```

**Random Effects Model:**
```
SDG_it = β₀ + β₁(TaxMix_it) + γ₁(TaxMix × Pop_it) + δ₁(TaxMix × GDPpc_it) + θ₁(TaxMix × DebtGDP_it) + λ_t + ε_it
```

Both specifications appropriately include time fixed effects (λ_t) and interaction terms for moderating variables.

---

## Part 2: Methodology Concerns and Recommendations

### 2.1 Sample Size Limitations

**Severity: Medium | Difficulty to Fix: Hard**

#### The Problem

The dataset comprises 29 countries observed over 10 years, yielding a maximum of 290 observations. While this appears substantial, the model complexity strains available degrees of freedom:

- Main effect of tax mix: 1 parameter
- Three interaction terms: 3 parameters
- Country fixed effects: 28 parameters (in FE model)
- Time fixed effects: 9 parameters
- **Total: 41+ parameters**

With approximately 290 observations, the ratio of observations to parameters is relatively low, potentially compromising estimate reliability.

#### Why It Matters

Insufficient degrees of freedom lead to:
- Large standard errors
- Unstable coefficient estimates
- Reduced statistical power to detect true effects
- Overfitting risk

#### Recommendation

1. Test moderating variables in separate models rather than simultaneously
2. Consider a more parsimonious specification for primary analysis
3. Use the full model as a robustness check
4. Report adjusted R² and information criteria (AIC/BIC) to assess model fit

---

### 2.2 Arbitrary Classification Threshold

**Severity: Low-Medium | Difficulty to Fix: Easy**

#### The Problem

The proposal classifies countries using a 10% threshold:
- **Income-dominant:** Income tax share exceeds consumption tax share by >10 percentage points
- **Consumption-dominant:** Consumption tax share exceeds income tax share by >10 percentage points
- **Balanced:** Difference within 10 percentage points

This threshold lacks theoretical or empirical justification.

#### Why It Matters

Consider two countries:
- **Country A:** 54% income tax, 46% consumption tax (8% difference → "Balanced")
- **Country B:** 56% income tax, 44% consumption tax (12% difference → "Income-dominant")

These nearly identical tax structures receive fundamentally different classifications. Small measurement errors or year-to-year fluctuations could reclassify countries, introducing noise into the analysis.

#### Recommendation

1. **Primary approach:** Use the continuous tax ratio (Income Tax / Consumption Tax) rather than categorical classification
2. **Sensitivity analysis:** Test results with alternative thresholds (5%, 10%, 15%, 20%)
3. **Report robustness:** Demonstrate that conclusions hold across threshold specifications
4. **Theoretical grounding:** If categorical analysis is preferred, provide literature-based justification for the chosen threshold

---

### 2.3 Endogeneity Risk (Reverse Causality)

**Severity: High | Difficulty to Fix: Medium**

#### The Problem

The thesis assumes unidirectional causation:

**Tax Structure → SDG Outcomes**

However, reverse causality is plausible:

**SDG Outcomes → Tax Structure**

Additionally, omitted variable bias may create spurious correlations:

**Unobserved Factor (e.g., institutional quality) → Both Tax Structure AND SDG Outcomes**

#### Examples of Reverse Causality

1. **Health → Tax Capacity:** Countries with healthier populations have more productive workers, generating higher incomes and enabling more sophisticated income tax systems.

2. **Education → Tax Administration:** Higher educational attainment produces citizens capable of navigating complex income tax systems and bureaucrats capable of administering them. Countries with poor education may default to simpler consumption taxes.

3. **Wealth as Confounder:** Wealthy countries can afford both progressive income tax systems and robust health/education infrastructure. The observed correlation between income tax reliance and SDG performance may reflect wealth rather than tax policy effects.

#### Why It Matters

If endogeneity exists, OLS/GLS estimates are biased and inconsistent. Coefficient estimates do not represent causal effects, undermining the study's policy implications.

#### Recommendation

1. **Lagged Independent Variables:** Use tax structure from period t-1 or t-2 to predict SDG outcomes in period t. This temporal ordering strengthens causal inference.

2. **Instrumental Variables:** Identify variables that:
   - Correlate with tax structure
   - Do not directly affect SDG outcomes except through tax structure
   - Potential instruments: Historical colonial tax systems, geographic/trade characteristics

3. **System GMM Estimation:** Arellano-Bond or Blundell-Bond estimators are designed for dynamic panel data with endogeneity concerns.

4. **Granger Causality Tests:** Test whether tax structure "Granger-causes" SDG outcomes (i.e., past tax values predict current SDG values after controlling for past SDG values).

5. **Explicit Acknowledgment:** At minimum, discuss endogeneity limitations in the methodology and interpretation sections.

---

### 2.4 Missing Time Lag Consideration

**Severity: High | Difficulty to Fix: Easy**

#### The Problem

The model tests contemporaneous effects:

```
SDG_t = f(TaxMix_t)
```

This specification assumes tax policy changes immediately affect health and education outcomes within the same year. This is implausible.

#### Realistic Causal Timeline

| Year | Event |
|------|-------|
| 2020 | Government increases education budget via income tax revenue |
| 2021 | Schools hire teachers, purchase materials, build facilities |
| 2022-2024 | Students receive improved instruction |
| 2025 | Educational attainment metrics improve measurably |

Testing same-year effects may show "no relationship" simply because insufficient time has passed for effects to materialize.

#### Health Example

- Increased healthcare funding → Hospital construction (Year 1)
- Staff hiring and training (Year 2)
- Improved patient care (Year 3)
- Mortality rate reductions observable (Years 3-5)

#### Recommendation

1. **Include Lagged Variables:**
   ```
   SDG_it = β₀ + β₁(TaxMix_i,t-1) + β₂(TaxMix_i,t-2) + ... + ε_it
   ```

2. **Distributed Lag Models:** Test multiple lag lengths to identify when effects materialize.

3. **Theoretical Justification:** Specify expected lag length based on literature (e.g., "Based on [citation], education policy effects typically materialize within 3-5 years").

4. **Dynamic Panel Specification:** Include lagged dependent variable to model persistence:
   ```
   SDG_it = ρ(SDG_i,t-1) + β₁(TaxMix_i,t-1) + ε_it
   ```

---

### 2.5 PCA Application Ambiguity

**Severity: Low | Difficulty to Fix: Easy**

#### The Problem

The proposal does not fully specify PCA implementation:

1. **Component Retention:** How many principal components will be retained?
   - Kaiser criterion (eigenvalue > 1)?
   - Scree plot elbow method?
   - Predetermined variance threshold (e.g., 70%)?

2. **Application Scope:** Is PCA applied:
   - Separately to SDG 3 indicators and SDG 4 indicators?
   - Jointly across all indicators?
   - By country? By year? Pooled?

3. **Index Construction:** Will the composite index use:
   - Only the first principal component?
   - Weighted combination of multiple components?
   - Component scores or factor loadings?

#### Why It Matters

Different PCA decisions produce different composite indices:

| Decision | SDG 3 Index Composition |
|----------|------------------------|
| First PC only | May weight maternal mortality heavily |
| First two PCs | May balance mortality and life expectancy |
| Varimax rotation | Different interpretive structure |

Results and conclusions may differ based on these methodological choices.

#### Recommendation

Pre-specify in the methodology section:

1. "Principal Component Analysis will be applied separately to SDG 3 and SDG 4 indicator sets."

2. "Components with eigenvalues exceeding 1.0 (Kaiser criterion) will be retained."

3. "The first principal component, explaining [X]% of total variance, will serve as the composite index for each SDG."

4. "Varimax rotation [will/will not] be applied."

5. Report factor loadings to demonstrate which indicators drive each component.

---

### 2.6 Country Heterogeneity

**Severity: Medium | Difficulty to Fix: Medium**

#### The Problem

The 29 Asia-Pacific countries span vastly different economic contexts:

| Category | Countries | Characteristics |
|----------|-----------|-----------------|
| High Income | Japan, Australia, South Korea, Singapore, New Zealand | Advanced tax systems, strong institutions |
| Upper-Middle Income | China, Thailand, Malaysia, Fiji | Developing tax capacity, mixed institutions |
| Lower-Middle Income | Philippines, Indonesia, Vietnam, India | Informal economies, weaker tax administration |
| Low Income | Cambodia, Myanmar, Nepal, Timor-Leste | Limited tax infrastructure, aid-dependent |

#### Why It Matters

Pooling these countries assumes the tax-SDG relationship is identical across all contexts. This assumption is questionable:

**In Japan:**
- Sophisticated income tax system with high compliance
- Marginal tax policy changes may have modest SDG effects
- Strong institutions mediate policy transmission

**In Cambodia:**
- Large informal economy escapes income taxation
- Consumption taxes more effectively capture economic activity
- Weak institutions may prevent revenue from reaching SDG programs

The "average" effect estimated across all 29 countries may not represent the true effect in any individual country.

#### Recommendation

1. **Stratified Analysis:** Run separate regressions by income group:
   - High-income countries (n ≈ 8)
   - Middle-income countries (n ≈ 12)
   - Low-income countries (n ≈ 9)

2. **Interaction Terms:** Include income-level interactions:
   ```
   SDG_it = β₁(TaxMix) + β₂(TaxMix × HighIncome) + β₃(TaxMix × LowIncome) + ε_it
   ```

3. **Multilevel Modeling:** Hierarchical Linear Models allow coefficients to vary by country group while borrowing strength across groups.

4. **Coefficient Heterogeneity Tests:** Test whether imposing common coefficients across countries is statistically valid.

5. **Subgroup Reporting:** Even if pooled analysis is primary, report country-group-specific effects as supplementary analysis.

---

## Part 3: Summary of Recommendations

### Priority Matrix

| Concern | Severity | Effort | Priority |
|---------|----------|--------|----------|
| Endogeneity | High | Medium | **Critical** |
| Time Lags | High | Low | **Critical** |
| Sample Size | Medium | High | Important |
| Country Heterogeneity | Medium | Medium | Important |
| 10% Threshold | Low-Medium | Low | Recommended |
| PCA Clarity | Low | Low | Recommended |

### Minimum Required Revisions

To strengthen the methodology to acceptable standards:

1. **Add lagged independent variables** (at minimum t-1)
2. **Acknowledge endogeneity limitations** explicitly
3. **Specify PCA retention criteria** in methodology section

### Recommended Enhancements

To elevate the methodology to rigorous standards:

4. **Conduct sensitivity analysis** on classification threshold
5. **Perform subgroup analysis** by country income level
6. **Consider instrumental variables or GMM** estimation
7. **Test multiple lag specifications** with theoretical justification

---

## Part 4: Conclusion

The proposed methodology demonstrates solid grounding in panel data econometrics. The diagnostic testing framework, model selection procedure, and variable operationalization follow established practices.

However, the study's ability to make causal claims is compromised by:
1. Potential reverse causality between tax structure and SDG outcomes
2. Contemporaneous specification that ignores realistic policy lag effects

Addressing these concerns—particularly by incorporating lagged variables and acknowledging endogeneity—would substantially strengthen the research contribution.

**Final Verdict:** The methodology is fundamentally sound but requires targeted revisions before the research can support strong policy conclusions.

---

## Appendix: Suggested Model Specifications

### A. Baseline Model with Lags

```
SDG_it = α_i + β₁(TaxMix_i,t-1) + β₂(TaxMix_i,t-2) + λ_t + ε_it
```

### B. Full Model with Lagged Interactions

```
SDG_it = α_i + β₁(TaxMix_i,t-1) + γ₁(TaxMix_i,t-1 × Pop_it) + δ₁(TaxMix_i,t-1 × GDPpc_it) + θ₁(TaxMix_i,t-1 × DebtGDP_it) + λ_t + ε_it
```

### C. Dynamic Panel Model

```
SDG_it = ρ(SDG_i,t-1) + β₁(TaxMix_i,t-1) + α_i + λ_t + ε_it
```

*Note: Dynamic specification requires GMM estimation to address Nickell bias.*

### D. Heterogeneous Effects Model

```
SDG_it = α_i + β₁(TaxMix_it) + β₂(TaxMix_it × HighIncome_i) + β₃(TaxMix_it × LowIncome_i) + λ_t + ε_it
```

---

*Report prepared for methodology review purposes. December 2024.*

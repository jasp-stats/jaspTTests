One Sample T-Test
==========================

The one sample t-test allows the user to estimate the effect size and test the null hypothesis that the population mean equals specific constant, i.e., the test value.

### Assumptions
- The dependent variable is continuous.
- The data are a random sample from the population.
- The dependent variable is normally distributed in the population.

### Input
-------
#### Assignment Box
- Variables: In this box, the variables are selected that are included in the analysis.

#### Tests  
- Student: The student's t-test. This options is selected by default.
- Wilcoxon signed-rank: Wilcoxon signed-rank test.
- Z test: The Z test.
  - Test value: The test value for the Z test. By default this is set to 0.
  - Std. deviation: The standard deviation applied in the Z test. By default this is set to 1.

#### Hypothesis
- &ne; Test value: Two-sided alternative hypothesis that the population mean is not equal to the test value. This options is selected by default.
- &gt; Test value: One-sided alternative hypothesis that the population mean is larger than the test value.
- &lt; Test value: One sided alternative hypothesis that the population mean is smaller than the test value.

#### Assumption Checks
- Normality tests: Shapiro-Wilk test of normality.

#### Additional Statistics
- Location difference: Average difference between the data points and the test value. For the Student's t-test and the Z test the location difference estimate is given by mean difference divided by the (hypothesized) standard deviation d; for the Wilcoxon signed-rank test, the location difference estimate is given by the Hodges-Lehmann estimate.
  - Confidence interval: Confidence interval for the location parameter. By default, the confidence interval is set to 95%. This can be changed into the desired percentage.
- Effect Size: For the Student t-test, the effect size is given by Cohen's d; for the Wilcoxon test, the effect size is given by the matched rank biserial correlation; for the Z test, the effect size is given by Cohen's d (based on the provided population standard deviation).
  - Confidence interval: Confidence interval for the effect size.
- Descriptives: Sample size, sample mean, sample standard deviation, standard error of the mean for each measure.
- Descriptive plots: Displays the sample mean and the confidence interval.
  - Confidence interval: Coverage of the confidence intervals in percentages. By default, the confidence interval is set to 95%. This can be changed into the desired percentage.
- Bar plots: Displays the sample mean as a bar and the confidence interval or standard error as the error bar. 
  - Confidence interval: Coverage of the confidence intervals in percentages. By default, the confidence interval is set to 95%. This can be changed into the desired percentage.
  - Standard error: By selecting this option, the error bars will represent standard errors of the mean.
  - Fix horizontal axis to 0: Forces the graph to show the default x-axis at y = 0.
- Raincloud plots: Displays the individual cases, box plot, and density.
  - Horizontal display: Changes the orientation of the raincloud plot so that the x-axis represents the dependent variable.
- Vovk-Sellke Maximum *p*-Ratio: The bound 1/(-e *p* log(*p*)) is derived from the shape of the *p*-value distribution. Under the null hypothesis (H<sub>0</sub>) it is uniform(0,1), and under the alternative (H<sub>1</sub>) it is decreasing in *p*, e.g., a beta(&#945;, 1) distribution, where 0 < &#945; < 1. The Vovk-Sellke MPR is obtained by choosing the shape &#945; of the distribution under H<sub>1</sub> such that the obtained *p*-value is *maximally diagnostic*. The value is then the ratio of the densities at point *p* under H<sub>0</sub> and H<sub>1</sub>. For example, if the two-sided *p*-value equals .05, the Vovk-Sellke MPR equals 2.46, indicating that this *p*-value is at most 2.46 times more likely to occur under H<sub>1</sub> than under H<sub>0</sub>.

#### Missing Values
 - Exclude cases per dependent variable: In case of multiple t-tests within a single analysis, each test will be conducted using all cases with valid data for the dependent variable for the particular t-test. Sample sizes may therefore vary across the tests. This option is selected by default.
 - Exclude cases listwise: In case of multiple t-tests within a single analysis, each t-test will be conducted using only cases with valid data for all dependent variables. Sample size is therefore constant across the tests.

### Output
-------

#### One Sample T-Test
- The first column contains the variables for which the analysis is performed.
- Test: The type of test that is selected. If only one test is selected, this column will not be displayed. In this scenario, the table only displays the results for the selected test.
- Statistic: The test statistic. For the Student's t-test this is the value of the t-statistic. For the Wilcoxon signed-rank test this is the value of the W-statistic. For the Z test this is the value of the Z-statistic.
- df: Degrees of freedom.
- p: The p-value.
- Mean difference: Average difference between the data points and the test value. This column is only named 'Mean difference' when the test `Student` is selected. When the test `Wilcoxon signed-rank` and/or the Z test is selected, this column is called 'Location difference'.
- Location difference: For the Student's t-test and the Z test, the location difference estimate is given by mean difference; for the Wilcoxon signed-rank test, the location difference estimate is given by the Hodges-Lehmann estimate. This column is only called 'Location difference' when `Wilcoxon signed-rank` test and/or the Z test is selected, otherwise this column is called 'Mean difference'.
- SE Difference: Standard error of the mean of the difference scores.
- % CI for Mean difference/location difference parameter: The confidence interval for the mean difference/location difference parameter of the difference scores. By default this is set to 95%.
  - Lower: The lower bound of the confidence interval.
  - Upper: The upper bound of the confidence interval.
- Effect Size: For the Student t-test, the effect size is given by Cohen's d; for the Wilcoxon test, the effect size is given by the matched rank biserial correlation; for the Z test, the effect size is given by Cohen's d (based on the provided population standard deviation).
- SE Effect Size: Standard error of the effect size.
- % CI for Effect Size: The confidence interval for effect size. By default this is set to 95%.
    - Lower: The lower bound of the confidence interval.
    - Upper: The upper bound of the confidence interval.

#### Assumption Checks
Test of Normality (Shapiro-Wilk)
- The first column contains the variables for which the analysis is performed.
- W: The value of the W test statistic.
- p: The p-value.

#### Descriptives
- The first column contains the variable.
- N: The sample size per variable.
- Mean: The mean of the variable.
- SD: Standard deviation of the mean.
- SE: Standard error of the mean.

#### Descriptive Plots
- Displays the sample mean (black bullet), the % confidence interval (whiskers), and the value of the test statistic (dashed line).

#### Bar Plots 
- Displays the sample mean (grey bar), the x% confidence interval or standard error (whiskers), and the value of the test statistic (dashed line).

##### Raincloud Plots
- Displays the individual cases (colored dots), box plot, and density of the sample. The y-axis represents the dependent variable. Within the box plots, the bold black line shows the sample median, the hinges indicate the 25th and 75th quantile, and the whiskers point to 1.5 interquartile ranges beyond the hinges. Densities are estimated using a Gaussian kernel and the bandwidth is determined with the 'nrd0' method (Silverman, 1986).

### References
-------
- Moore, D. S., McCabe, G. P., & Craig, B. A. (2012). *Introduction to the practice of statistics (7th ed.)*. New York, NY: W. H. Freeman and Company.
- Sellke, T., Bayarri, M. J., & Berger, J. O. (2001). Calibration of *p* values for testing precise null hypotheses. *The American Statistician, 55*(1), 62-71.
- Silverman, B. W. (1986). *Density Estimation*. London: Chapman and Hall.
- Whitlock, M. C., & Schluter, D. (2015). *The analysis of biological data (2nd ed.)*. Greenwood Village, Colorado: Roberts and Company Publishers.
- Borenstein, M., Hedges, L. V., Higgins, J. P. T., & Rothstein, H. R. (2009). *Introduction to Meta-Analysis (1st ed.)*. Chichester, UK: John Wiley & Sons, Ltd.

### R-packages
---
- stats

### Example
---
- For an example go to `Open` --> `Data Library` --> `T-Tests` --> `Weight Gain`.  

Paired Samples T-Test
==========================

The paired samples t-test allows the user to estimate the effect size  and test the null hypothesis that the population mean of the difference between paired observations equals 0.

### Assumptions
- The difference score is continuous.
- The difference scores are a random sample from the population.
- The difference scores are normally distributed in the population.

### Input 
-------
#### Assignment Box 
- Variables: In this box the variables are selected for which the difference is computed. Multiple differences can be analysed at the same time by specifying different rows with two variables for which the difference is computed. In other words, each row represents other difference scores. 

#### Tests 
- Student: Student's t-test. This option is selected by default. 
- Wilcoxon signed-rank: Wilcoxon signed-rank test. 

#### Alt. Hypothesis 
- Measure 1 &ne; Measure 2: Two-sided alternative hypothesis that the population mean of the difference is not equal to 0. This option is selected by default. 
- Measure 1 &gt; Measure 2: One-sided alternative hypothesis that the population mean of the difference is larger than 0.
- Measure 1 &lt; Measure 2: One sided alternative hypothesis that the population mean of the difference is smaller than 0.

#### Assumption Checks 
- Normality: Shapiro-Wilk test of normality. 

#### Additional Statistics 
- Location parameter: For the Student's t-test the location parameter is given by mean difference d; for the Wilcoxon signed-rank test, the location parameter is given by the Hodges-Lehmann estimate.
  - Confidence interval: Confidence interval for the location parameter. By default, the confidence interval is set to 95%. This can be changed into the desired percentage.
- Effect Size: For the Student t-test, the effect size is given by Cohen's d; for the Wilcoxon test, the effect size is given by the matched rank biserial correlation.  
  - Confidence interval: Confidence interval for the effect size.
- Descriptives: Sample size, sample mean, sample standard deviation, standard error of the mean for each measure. 
- Descriptive plots: Displays the sample means and the confidence intervals for each measure (see Morey [2008] for the computation of the standard error of the mean in paired designs).
  - Confidence interval: Coverage of the confidence intervals in percentages. By default, the confidence interval is set to 95%. This can be changeed into the desired percentage.
  - Normalize error bars: In order to get accurate confidence intervals and standard errors, the data are normalized by subtracting the appropriate participantʹs mean performance from each observation, and then adding the grand mean score to every observation. The variances of the resulting normalized values in each condition, and thus the size of the bars, no longer depend on the participant effects and are therefore a more accurate representation of the experimental manipulation. See Morey (2008) for a thorough discussion of this procedure.
- Bar plots: Displays the sample means as bars and the confidence intervals or standard errors as error bars for each measure (see Morey [2008] for the computation of the standard error of the mean in paired designs). 
  - Confidence interval: Coverage of the confidence intervals in percentages. By default, the confidence interval is set to 95%. This can be changed into the desired percentage.
  - Standard error: By selecting this option, the error bars will represent standard errors of the mean of each condition.
  - Fix horizontal axis to 0: Forces the graph to show the default x-axis at y = 0.
  - Normalize error bars: Same as for descriptive plots. 
- Raincloud plots: Displays the individual cases (colored dots), box plots, and densities for each measure. 
- Raincloud difference plots: Displays a raincloud plot of the differences between the two measures.
  - Horizontal display: Changes the orientation of the raincloud difference plot so that the x-axis represents the dependent variable and the y-axis the difference between measures.
- Vovk-Sellke Maximum *p*-Ratio: The bound 1/(-e *p* log(*p*)) is derived from the shape of the *p*-value distribution. Under the null hypothesis (H<sub>0</sub>) it is uniform(0,1), and under the alternative (H<sub>1</sub>) it is decreasing in *p*, e.g., a beta(&#945;, 1) distribution, where 0 < &#945; < 1. The Vovk-Sellke MPR is obtained by choosing the shape &#945; of the distribution under H<sub>1</sub> such that the obtained *p*-value is *maximally diagnostic*. The value is then the ratio of the densities at point *p* under H<sub>0</sub> and H<sub>1</sub>. For example, if the two-sided *p*-value equals .05, the Vovk-Sellke MPR equals 2.46, indicating that this *p*-value is at most 2.46 times more likely to occur under H<sub>1</sub> than under H<sub>0</sub>.

#### Missing Values 
 - Exclude cases per dependent variable: In case of multiple t-tests within a single analysis, each test will be conducted using all cases with valid data for the difference score for the particular t-test. Sample sizes may therefore vary across the tests. This option is selected by default.
 - Exclude cases listwise: In case of multiple t-tests within a single analysis, each t-test will be conducted using only cases with valid data for all difference scores.
 Sample size is therefore constant across the tests.

### Output 
--- 

#### Paired Samples T-Test
- The first column contains the two variables for which the difference is calculated.
- Test: The type of test that is selected. If only one test is selected, this column will not be displayed. In this scenario, the table only displays the results for the selected test. 
- Statistic: The test statistic. For the Student's t-test this is the value of the t-statistic. For the Wilcoxon signed-rank test this is the value of the W-statistic. 
- df: Degrees of freedom.
- p: The p-value.
- Mean difference: The mean of the difference scores. This column is only named 'Mean difference' when the test `Student` is selected. When the test `Wilcoxon signed-rank` is selected, this column is called 'Location parameter'. 
- Location parameter: For the Student's t-test, the location parameter is given by mean difference; for the Wilcoxon signed-rank test, the location parameter is given by the Hodges-Lehmann estimate. This column is only called 'Location parameter' when `Wilcoxon signed-rank` test is selected, otherwise this column is called 'Mean difference'. 
- SE Difference: Standard error of the mean of the difference scores. 
- % CI for Mean difference/location parameter: The confidence interval for the mean difference/location parameter of the difference scores. By default this is set to 95%. 
  - Lower: The lower bound of the confidence interval. 
  - Upper: The upper bound of the confidence interval. 
- Effect Size: For the Student's t-test, the effect size is given by Cohen's d; for the Wilcoxon test, the effect size is given by the matched rank biserial correlation.
- SE Effect Size: Standard error of the effect size.
- % CI for Effect Size: The confidence interval for the effect size. By default this is set to 95%. 
    - Lower: The lower bound of the confidence interval. 
    - Upper: The upper bound of the confidence interval.

#### Assumption Checks 
Test of Normality (Shapiro-Wilk)
- The first column contains the two variables from which the difference is calculated.
- W: The value of the W test statistic. 
- p: The p-value. 

#### Descriptives 
- The first column contains the variable. 
- N: The sample size per variable. 
- Mean: The mean of the variable. 
- SD: Standard deviation of the mean. 
- SE: Standard error of the mean. 

##### Descriptive Plots 
- Displays the sample means (black bullet), the % confidence intervals (whiskers) for each measure.  

##### Bar Plots 
- Displays the sample means (grey bars), the x% confidence intervals or standard errors (whiskers) for each measure. The x-axis represents the paired variables, and the y-axis the dependent variable. 

##### Raincloud Plots
- Displays the individual cases (colored dots), box plots, and densities for each measure. The cases from both measures are connected with individual lines. The x-axis and color represent the measures, and the y-axis represents the dependent variable. Within the box plots, the bold black line shows the sample median, the hinges indicate the 25th and 75th quantile, and the whiskers point to 1.5 interquartile ranges beyond the hinges. Densities are estimated using a Gaussian kernel and the bandwidth is determined with the 'nrd0' method (Silverman, 1986).

##### Raincloud Difference Plots
- Displays the individual cases (colored dots), box plot, and density for the difference between the measures. The x-axis and color represent the measures, and the y-axis represents the dependent variable. Within the box plots, the bold black line shows the sample median, the hinges indicate the 25th and 75th quantile, and the whiskers point to 1.5 interquartile ranges beyond the hinges. Densities are estimated using a Gaussian kernel and the bandwidth is determined with the 'nrd0' method (Silverman, 1986).

### References
-------
- Moore, D. S., McCabe, G. P., & Craig, B. A. (2012). *Introduction to the practice of statistics (7th ed.)*. New York, NY: W. H. Freeman and Company.
- Morey, R. D. (2008). Confidence intervals from normalized data: A correction to Cousineau (2005). *Tutorials in Quantitative Methods for Psychology, 4*, 61-64.
- Sellke, T., Bayarri, M. J., & Berger, J. O. (2001). Calibration of *p* values for testing precise null hypotheses. *The American Statistician, 55*(1), 62-71.
- Silverman, B. W. (1986). *Density Estimation*. London: Chapman and Hall.
- Whitlock, M. C., & Schluter, D. (2015). *The analysis of biological data (2nd ed.)*. Greenwood Village, Colorado: Roberts and Company Publishers.
- Borenstein, M., Hedges, L. V., Higgins, J. P. T., & Rothstein, H. R. (2009). *Introduction to Meta-Analysis (1st ed.)*. Chichester, UK: John Wiley & Sons, Ltd.

### R-packages 
---
- stats 

### Example 
--- 
- For an example go to `Open` --> `Data Library` --> `T-Tests` --> `Moon and Agression` 

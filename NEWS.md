# jaspTTests Changelog

> **HOW TO READ AND UPDATE THIS CHANGELOG:**
> 
> This document follows a modified [Keep a Changelog](https://keepachangelog.com/) format adapted for the R/JASP ecosystem. Releases are listed in reverse chronological order (newest first).
> As an example see [jaspModuleTemplate](https://github.com/jasp-stats/jaspModuleTemplate/blob/master/NEWS.md)
> * **Adding New Changes (For Contributors):** All new commits should be logged at the very top of the file under the `# jaspModuleTemplate (development version)` header. Place your bullet point under the appropriate category (`## Added`, `## Fixed`, etc.). 
> * **Issue References:** Please reference the relevant GitHub Issue (if any) at the end of your line (e.g., `([Issue #19](https://github.com/jasp-stats/jaspModuleTemplate/issues/19)`). 
> * **Format Categories:** >   * **Added:** New template features, QML examples, or build tools.
>   * **Changed:** Updates to default configurations, boilerplate code, or dependencies. 
>   * **Fixed:** Bug fixes in the build pipeline, R wrappers, or QML layouts.
>   * **Deprecated / Removed:** Outdated template components or legacy code.


---

# jaspTTests (development version)

## Changed
* Normality assumption in the help and info text now refers to the residuals being normally distributed (matching the residual-based Q-Q plots and Shapiro-Wilk test) instead of the dependent variable, across one-sample, independent-samples, and paired-samples t-tests (frequentist and Bayesian).
* Normality tests do not crash for sample sizes larger than 5000, but instead return an info message in a footnote.

## Fixed
* The one-sample t-test no longer stops with "object 'rowResults' not found" when a variable contains infinite values; the table shows the footnote about it instead.
* Independent samples t-test: rows with a score but no group no longer count towards the Mann-Whitney ranks in the descriptives table, and no longer make the Bayesian robustness and sequential analysis plots fail ([Issue #4719](https://github.com/jasp-stats/jasp-issues/issues/4719)).
* Bayesian t-tests: with BF01 chosen, the Bayes factor robustness check now shows the maximum BF01 and its prior width; before, it showed the minimum ([Issue #4511](https://github.com/jasp-stats/jasp-issues/issues/4511)).
* Paired samples t-test (classical and Bayesian): a pair where one measure is constant now gets its tests and Bayes factor, because only the difference between the measures needs variance ([INTERNAL-jasp#2871](https://github.com/jasp-stats/INTERNAL-jasp/issues/2871)).

---

# jaspModuleTemplate 0.2.0
## Added
* Added NEWS.md
* Added workflow to remind users to update their `NEWS.md`.
* Added workflow to auto-bump version when user does not do so.

---

# jaspModuleTemplate 0.1.0

## Added
* Initial examples to showcase JASP module development

## Changed
* Use best practices for checking input ([Issue #19](https://github.com/jasp-stats/jaspModuleTemplate/issues/19)).
* The main results table now defaults to displaying 95% Confidence Intervals for effect sizes.

## Fixed
* Remove deprecated dependencies from qml files ([Issue #14](https://github.com/jasp-stats/jaspModuleTemplate/issues/14)).

//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls
import "./common" as Common

Form
{
	info: qsTr("The paired samples t-test allows the user to estimate the effect size and test the null hypothesis that the population mean of the difference between observations equals 0 in dependent groups.\n") +
	"## " + qsTr("Assumptions") + "\n" + "- The difference score is continuous.\n" + "- The difference scores are a random sample from the population.\n" + "- The difference scores are normally distributed in the population."
	id: form
	property int framework:	Common.Type.Framework.Classical

	plotHeight: 300
	plotWidth:  350

	VariablesForm
	{
		infoLabel: qsTr("Input")
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "allVariablesList" ;  }
		AssignedPairsVariablesList { name: "pairs"; title: qsTr("Variable Pairs"); info: qsTr("The variables here have their difference computed. Multiple differences can be analysed at the same time by specifying different rows. In other words, each row represents a difference score."); allowedColumns: ["scale"];	minNumericLevels: 2 }
	}

	Group
	{
		title: qsTr("Tests")
		CheckBox { name: "student";			label: qsTr("Student"); info: qsTr("Student's paired sample t-test.This option is selected by default."); checked: true	}
		CheckBox { name: "wilcoxon";	label: qsTr("Wilcoxon signed-rank"); info: qsTr("Non-parametric version of the paired samples t-test. Use when model residuals (i.e., group differences) are not normally distributed.")	}
	}

	Group
	{
		title: qsTr("Additional Statistics")
		Layout.rowSpan: 2
		CheckBox
		{
			name: "meanDifference";	label: qsTr("Location parameter"); info: qsTr ("For the Student's t-test the location parameter is given by the mean difference 'd'; for the Wilcoxon signed-rank test, the location parameter is given by the Hodges-Lehmann estimate.")
			CheckBox
			{
				name: "meanDifferenceCi";	label: qsTr("Confidence interval"); info: qsTr ("Confidence interval for the location parameter. By default, the confidence interval is set to 95%. This can be changed into the desired percentage.")
				childrenOnSameRow: true
				CIField { name: "meanDifferenceCiLevel" }
			}
		}

		CheckBox
		{
			name: "effectSize";	label: qsTr("Effect size"); info: qsTr("For the Student t-test, the effect size is given by Cohen's d; for the Wilcoxon test, the effect size is given by the matched rank biserial correlation.")
			CheckBox
			{
				name: "effectSizeCi";	label: qsTr("Confidence interval"); info: qsTr("Confidence interval for the effect size.")
				childrenOnSameRow: true
				CIField { name: "effectSizeCiLevel" }
			}
			CheckBox
			{				
				name: "effectSizeCorrection";	label: qsTr("Correct for correlation"); info: qsTr("Correct the effect size for the correlation between the observed values, to prevent overestimating the effect (Dunlap et al., 1996).")
			}
		}
		CheckBox { name: "descriptives";					label: qsTr("Descriptives")	; info: qsTr("Sample size, sample mean, sample standard deviation, standard error of the mean for each measure.")									}
		CheckBox { name: "vovkSellke";					label: qsTr("Vovk-Sellke maximum p-ratio")		; info: qsTr("Shows the maximum ratio of the likelihood of the obtained p value under H1 vs the likelihood of the obtained p value under H0. For example, if the two-sided p-value equals .05, the Vovk-Sellke MPR equals 2.46, indicating that this p-value is at most 2.46 times more likely to occur under H1 than under H0.")				}
	}

	RadioButtonGroup
	{
		name: "alternative"
		title: qsTr("Alternative Hypothesis")
		RadioButton { value: "twoSided";	label: qsTr("Measure 1 ≠ Measure 2"); info: qsTr("Two-sided alternative hypothesis that the population mean of the difference is not equal to 0. This option is selected by default."); checked: true	}
		RadioButton { value: "greater";	label: qsTr("Measure 1 > Measure 2"); info: qsTr("One-sided alternative hypothesis that the population mean of the difference is larger than 0.")				}
		RadioButton { value: "less";	label: qsTr("Measure 1 < Measure 2"); info: qsTr("One-sided alternative hypothesis that the population mean of the difference is smaller than 0.")				}
	}

	Group
	{
		title: qsTr("Assumption Checks")
		CheckBox { name: "normalityTest";	label: qsTr("Normality"); info: qsTr("Shapiro-Wilk test of normality.") }
		CheckBox 
		{ 
			name: "qqPlot";		 	label: qsTr("Q-Q plot residuals"); info: qsTr("Displays Q-Q plot of the standardized residuals. The confidence band shows the expected range of residuals under normality; points outside the band suggest deviations from normality.") 
			CheckBox
            {
                name:               "qqPlotCi"
                label:              qsTr("Confidence interval")
                childrenOnSameRow:  true
                CIField{ name: "qqPlotCiLevel" }
            }		
		}
	}
	
	Group
	{
		title: qsTr("Plots")
		CheckBox
		{
			name: "descriptivesPlot";						label: qsTr("Descriptives plots"); info: qsTr("Displays the sample means and the confidence intervals for each measure (see Morey [2008] for the computation of the standard error of the mean in paired designs.)") 
			CIField { name: "descriptivesPlotCiLevel";	label: qsTr("Confidence interval")						}
		}
		CheckBox{ name: "raincloudPlot";		label: qsTr("Raincloud plots")	; info:qsTr("Displays the individual data points (colored dots), box plots, and densities for each measure.")								}
		CheckBox
		{
			name: "differenceRaincloudPlot";	label: qsTr("Raincloud difference plots"); info: qsTr("Displays a raincloud plot of the differences between the two measures.") 
			CheckBox { name: "differenceRaincloudPlotHorizontal"; label: qsTr("Horizontal display"); info: qsTr("Changes the orientation of the raincloud difference plot so that the x-axis represents the dependent variable.")		}
		}
		Common.BarPlots
		{
			framework:	form.framework
		}
	}

	RadioButtonGroup
	{
		name: "naAction"
		title: qsTr("Missing Values")
		RadioButton { value: "perDependent";			label: qsTr("Exclude cases per variable"); info: qsTr("In case of multiple t-tests within a single analysis, each test will be conducted using all cases with valid data for the difference score for the particular t-test. Sample sizes may therefore vary across the tests. This option is selected by default.") ; checked: true		}
		RadioButton { value: "listwise";						label: qsTr("Exclude cases listwise"); info: qsTr("In case of multiple t-tests within a single analysis, each t-test will be conducted using only cases with valid data for all difference scores. Sample size is therefore constant across the tests.")								}
	}

}

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
	info: qsTr("The independent samples t-test allows the user to estimate the effect size and test the null hypothesis that the population means of two independent groups are equal.") + "\n" +
		"## " + qsTr("Assumptions") + "\n" +
		"- " + qsTr("The dependent variable is continuous.") + "\n" +
		"- " + qsTr("The observations in both groups are a random sample from the population.") + "\n" +
		"- " + qsTr("The dependent variable is normally distributed in each group of the independent variable.") + "\n" +
		"- " + qsTr("The population variances in the two groups are homogeneous.")
	id: form
	property int framework:	Common.Type.Framework.Classical

	plotHeight: 300
	plotWidth:  350

	Formula { rhs: "dependent" }

	VariablesForm
	{
		infoLabel: qsTr("Input")
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "allVariablesList" }
		AssignedVariablesList { name: "dependent";			title: qsTr("Dependent Variables");	info: qsTr("In this box the dependent variable is selected."); 												allowedColumns: ["scale"];			minNumericLevels: 2}
		AssignedVariablesList { name: "group";				title: qsTr("Grouping Variable"); info: qsTr("In this box the variable defining the groups is selected. e.g., experimental condition.");			allowedColumns: ["nominal"];		minLevels: 2; maxLevels: 2; singleVariable: true }
	}

	Group
	{
		title: qsTr("Tests")
		CheckBox { name: "student";	label: qsTr("Student"); info: qsTr("Good old fashioned t-test. Selected by default.");	 checked: true	}
		CheckBox { name: "welch";			label: qsTr("Welch"); info: qsTr("Welch's unequal variances test. Use when the group variances cannot be assumed to be equal.");					}
		CheckBox { name: "mannWhitneyU";	label: qsTr("Mann-Whitney"); info:qsTr("Non-parametric independent t-test. Use when the model residuals are not normally distributed.");				}
	}

	Group
	{
		title: qsTr("Additional Statistics")
		Layout.rowSpan: 2
		CheckBox
		{
			name: "meanDifference"; label: qsTr("Location parameter"); info: qsTr("For the Student's t-test and Welch's t-test, the location parameter is given by mean difference; for the Mann-Whitney test, the location parameter is given by the Hodges-Lehmann estimate.")
			CheckBox
			{
				name: "meanDifferenceCi"; label: qsTr("Confidence interval"); info: qsTr("Confidence interval for the location parameter. By default, the confidence interval is set to 95%. This can be changed into the desired percentage.")
				childrenOnSameRow: true
				CIField { name: "meanDifferenceCiLevel" }
			}
		}
		CheckBox
		{
			name: "effectSize"; label: qsTr("Effect size"); info: qsTr("For the Student t-test and Welch t-test, the effect size can be selected below; for the Mann-Whitney test, the effect size is given by the rank biserial correlation.")
			RadioButtonGroup {
				name: "effectSizeType"
				RadioButton { value: "cohen"; label: qsTr("Cohen's d") ; checked: true ; info: qsTr("For the Student's t-test, uses the pooled standard deviation to standardize the mean difference. For the Welch's t-test, uses the square-root of the average variance to standardize the mean difference.")}
				RadioButton { value: "glass"; label: qsTr("Glass' delta") ; info: qsTr("Uses the standard deviation of group 2 to standardize the mean difference. In order to change which group is used as group 2, you can change the order of the levels by clicking on the name of the grouping variable in the data window, click on one of the levels and then click the arrow buttons to switch the order.")}
				RadioButton { value: "hedges"; label: qsTr("Hedges' g") ; info: qsTr("Applies a correction factor to Cohen's d to make it unbiased.") }
				CheckBox
				{
					name: "effectSizeCi"; label: qsTr("Confidence interval"); info: qsTr("Confidence interval for the chosen effect size, based on the non-central t-distribution for Cohen's d, Glass' delta and Hedges' g, and normal approximation of the Fisher transformed rank biserial correlation.")
					childrenOnSameRow: true
					PercentField { name: "effectSizeCiLevel"; defaultValue: 95 }
				}
			}
		}
		CheckBox { name: "descriptives";	label: qsTr("Descriptives") ; info: qsTr("Sample size, sample mean, sample standard deviation, standard error of the mean for each group.")							}
		CheckBox { name: "vovkSellke";	label: qsTr("Vovk-Sellke maximum p-ratio"); info: qsTr("Shows the maximum ratio of the likelihood of the obtained p value under H1 vs the likelihood of the obtained p value under H0. For example, if the two-sided p-value equals .05, the Vovk-Sellke MPR equals 2.46, indicating that this p-value is at most 2.46 times more likely to occur under H1 than under H0.") }
	}

	RadioButtonGroup
	{
		name: "alternative"
		title: qsTr("Alternative Hypothesis")
		RadioButton { value: "twoSided";	label: qsTr("Group 1 ≠ Group 2"); info: qsTr("Two-sided alternative hypothesis that the population means are not equal. Selected by default."); checked: true	}
		RadioButton { value: "greater";	label: qsTr("Group 1 > Group 2"); info: qsTr("One-sided alternative hypothesis that the population mean of Group 1 is larger than the population mean of Group 2.");					}
		RadioButton { value: "less"; label: qsTr("Group 1 < Group 2"); info: qsTr("One-sided alternative hypothesis that the population mean of Group 1 is smaller than the population mean of Group 2.");				}
	}

	Group
	{
		title: qsTr("Assumption Checks")
		CheckBox { name: "normalityTest";	label: qsTr("Normality"); info: qsTr("Shapiro-Wilk test of normality."); }
		CheckBox
		{
			name: "equalityOfVariancesTest";	label: qsTr("Equality of variances"); info: qsTr("BrownForsythe or Levene's tests to check if variances are equally distributed across groups.")
			RadioButtonGroup
			{
				name: "equalityOfVariancesTestType"
				RadioButton { value: "brownForsythe";	label: qsTr("Brown-Forsythe") ; checked: true }
				RadioButton { value: "levene";			label: qsTr("Levene's") }
			}
		}
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
		Layout.rowSpan: 2
		CheckBox
		{
			name: "descriptivesPlot";		label: qsTr("Descriptives plots") ; info: qsTr("Displays the sample means and the confidence intervals for each group. By default it is set at 95%. This can be changed into the desired percentage.")
			CIField { name: "descriptivesPlotCiLevel"; label: qsTr("Confidence interval")  }
		}
		CheckBox
		{
			name: "raincloudPlot"; label: qsTr("Raincloud plots") ; info: qsTr("Displays the individual cases (colored dots), box plots, and densities for each group.")
			CheckBox { name: "raincloudPlotHorizontal"; label: qsTr("Horizontal display") ; info: qsTr("Changes the orientation of the raincloud plot so that the x-axis represents the dependent variable.")}
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
		RadioButton { value: "perDependent";	label: qsTr("Exclude cases per dependent variable"); info: qsTr("Exclude cases per dependent variable: In case of multiple t-tests within a single analysis, each test will be conducted using all cases with valid data for the dependent variable for the particular t-test. Sample sizes may therefore vary across the tests. This option is selected by default.") ; checked: true	}
		RadioButton { value: "listwise";				label: qsTr("Exclude cases listwise") ; info: qsTr("In case of multiple t-tests within a single analysis, each t-test will be conducted using only cases with valid data for all dependent variables. Sample size is therefore constant across the tests.")							}
	}
}

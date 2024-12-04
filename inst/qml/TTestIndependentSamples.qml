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
	info: qsTr("The independent samples t-test allows the user to estimate the effect size and test the null hypothesis that the population means of two independent groups are equal.")
	id: form
	property int framework:	Common.Type.Framework.Classical

	plotHeight: 300
	plotWidth:  350

	Formula { rhs: "dependent" }

	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "allVariablesList" }
		AssignedVariablesList { name: "dependent";			title: qsTr("Dependent Variables");	info: qsTr("In this box the dependent variable is selected"); 												allowedColumns: ["scale"];			minNumericLevels: 2}
		AssignedVariablesList { name: "group";				title: qsTr("Grouping Variable"); info: qsTr("In this box the variable defining the groups is selected. e.g experimental condition");			allowedColumns: ["nominal"];		minLevels: 2; maxLevels: 2; singleVariable: true }
	}

	Group
	{
		title: qsTr("Tests")
    	CheckBox { name: "student";	label: qsTr("Student"); info: qsTr("Good old fashioned T-test. Selected by default");	 checked: true	}
		CheckBox { name: "welch";			label: qsTr("Welch"); info: qsTr("Use when variances are not equally distributed accross groups");					}
		CheckBox { name: "mannWhitneyU";	label: qsTr("Mann-Whitney"); info:qsTr("Non-parametric independent T-test. Use when data is not normally distributed");				}
	}

	Group
	{
		title: qsTr("Additional Statistics")
		Layout.rowSpan: 2
		CheckBox
		{
			name: "meanDifference"; label: qsTr("Location parameter"); info: qsTr(" For the Student's t-test and Welch's t-test, the location parameter is given by mean difference; for the Mann-Whitney test, the location parameter is given by the Hodges-Lehmann estimate.")
			CheckBox
			{
				name: "meanDifferenceCi"; label: qsTr("Confidence interval"); info: qsTr(" Confidence interval for the location parameter. By default, the confidence interval is set to 95%. This can be changed into the desired percentage.")
				childrenOnSameRow: true
				CIField { name: "meanDifferenceCiLevel" }
			}
		}
		CheckBox
		{
			name: "effectSize"; label: qsTr("Effect size"); info: qsTr(" For the Student t-test and Welch t-test, the effect size can be selected below; for the Mann-Whitney test, the effect size is given by the rank biserial correlation.")
			RadioButtonGroup {
				name: "effectSizeType"
				RadioButton { value: "cohen"; label: qsTr("Cohen's d") ; checked: true ; info: qsTr("For the Student's t-test, uses the pooled standard deviation to standardize the mean difference. For the Welch's t-test, uses the square-root of the average variance to standardize the mean difference.")}
				RadioButton { value: "glass"; label: qsTr("Glass' delta") ; info: qsTr("Uses the standard deviation of group 2 to standardize the mean difference. In order to change which group is used as group 2, you can change the order of the levels by clicking on the name of the grouping variable in the data window, click on one of the levels and then click the arrow buttons to switch the order.")}
				RadioButton { value: "hedges"; label: qsTr("Hedges' g") ; info: qsTr("Applies a correction factor to Cohen's d to make it unbiased.") }
				CheckBox
				{
					name: "effectSizeCi"; label: qsTr("Confidence interval"); info: qsTr("Confidence interval for the effect size based on the non-central t-distribution for Cohen's d, Glass' delta and Hedges' g, and normal approximation of the Fisher transformed rank biserial correlation.")
					childrenOnSameRow: true
					PercentField { name: "effectSizeCiLevel"; defaultValue: 95 }
				}
			}
		}
		CheckBox { name: "descriptives";	label: qsTr("Descriptives")								}
		CheckBox { name: "vovkSellke";	label: qsTr("Vovk-Sellke maximum p-ratio") }
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
			name: "equalityOfVariancesTest";	label: qsTr("Equality of variances"); info: qsTr("BrownForsythe or Levene's tests to check if variances are qually distributed accross groups")
			RadioButtonGroup
			{
				name: "equalityOfVariancesTestType"
				RadioButton { value: "brownForsythe";	label: qsTr("Brown-Forsythe") ; checked: true }
				RadioButton { value: "levene";			label: qsTr("Levene's") }
			}
		}
		CheckBox { name: "qqPlot";		 	label: qsTr("Q-Q plot residuals"); info: qsTr("Displays Q-Q plot of the standardized residuals.") }
	}
	Group
	{
		title: qsTr("Plots")
		Layout.rowSpan: 2
		CheckBox
		{
			name: "descriptivesPlot";		label: qsTr("Descriptives plots")
			CIField { name: "descriptivesPlotCiLevel"; label: qsTr("Confidence interval") }
		}
		CheckBox
		{
			name: "raincloudPlot"; label: qsTr("Raincloud plots")
			CheckBox { name: "raincloudPlotHorizontal"; label: qsTr("Horizontal display") }
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
		RadioButton { value: "perDependent";	label: qsTr("Exclude cases per dependent variable"); checked: true	}
		RadioButton { value: "listwise";				label: qsTr("Exclude cases listwise")							}
	}
}

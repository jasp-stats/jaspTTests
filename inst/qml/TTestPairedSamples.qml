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
	info: qsTr("The paired samples t-test allows the user to estimate the effect size and test the null hypothesis that the population mean of the difference between observations equals 0 in dependent groups.")
	id: form
	property int framework:	Common.Type.Framework.Classical

	plotHeight: 300
	plotWidth:  350

	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "allVariablesList"; info: qsTr("All variables are displayed here. Drag the relevant variables for the analysis to the variable pairs box.") }
		AssignedPairsVariablesList { name: "pairs"; title: qsTr("Variable Pairs"); info: qsTr("The variables here have their difference computed. Multiple differences can be analysed at the same time by specifying different rows. In other words, each row represents a difference score."); allowedColumns: ["scale"];	minNumericLevels: 2 }
	}

	Group
	{
		title: qsTr("Tests")
		CheckBox { name: "student";			label: qsTr("Student"); info: qsTr("Student's paired sample t-test.This option is selected by default"); checked: true	}
		CheckBox { name: "wilcoxon";	label: qsTr("Wilcoxon signed-rank"); info: ("Non-parametric version of paired samples t-test. Use when data is not normally distributed.")	}
	}

	Group
	{
		title: qsTr("Additional Statistics")
		Layout.rowSpan: 2
		CheckBox
		{
			name: "meanDifference";	label: qsTr("Location parameter")
			CheckBox
			{
				name: "meanDifferenceCi";	label: qsTr("Confidence interval")
				childrenOnSameRow: true
				CIField { name: "meanDifferenceCiLevel" }
			}
		}

		CheckBox
		{
			name: "effectSize";	label: qsTr("Effect size"); info: qsTr ("For the Student t-test, the effect size is given by Cohen's d; for the Wilcoxon test, the effect size is given by the matched rank biserial correlation.")
			CheckBox
			{
				name: "effectSizeCi";	label: qsTr("Confidence interval")
				childrenOnSameRow: true
				CIField { name: "effectSizeCiLevel" }
			}
			CheckBox
			{				
				name: "effectSizeCorrection";	label: qsTr("Correct for correlation")
			}
		}
		CheckBox { name: "descriptives";					label: qsTr("Descriptives")										}
		CheckBox { name: "vovkSellke";					label: qsTr("Vovk-Sellke maximum p-ratio")						}
	}

	RadioButtonGroup
	{
		name: "alternative"
		title: qsTr("Alternative Hypothesis")
		RadioButton { value: "twoSided";	label: qsTr("Measure 1 ≠ Measure 2"); info: qsTr("Two-sided alternative hypothesis that the population mean of the difference is not equal to 0. This option is selected by default."); checked: true	}
		RadioButton { value: "greater";	label: qsTr("Measure 1 > Measure 2"); info: qsTr(" One-sided alternative hypothesis that the population mean of the difference is larger than 0.")				}
		RadioButton { value: "less";	label: qsTr("Measure 1 < Measure 2"); info: qsTr("One sided alternative hypothesis that the population mean of the difference is smaller than 0.")				}
	}

	Group
	{
		title: qsTr("Assumption Checks")
		CheckBox { name: "normalityTest";	label: qsTr("Normality"); info: qsTr("Shapiro-Wilk test of normality.") }
		CheckBox { name: "qqPlot";		 	label: qsTr("Q-Q plot residuals"); info: qsTr("Q-Q plot of the standardized residuals.") }

	}
	
	Group
	{
		title: qsTr("Plots")
		CheckBox
		{
			name: "descriptivesPlot";						label: qsTr("Descriptives plots")
			CIField { name: "descriptivesPlotCiLevel";	label: qsTr("Confidence interval")						}
		}
		CheckBox{ name: "raincloudPlot";		label: qsTr("Raincloud plots")									}
		CheckBox
		{
			name: "differenceRaincloudPlot";	label: qsTr("Raincloud difference plots")
			CheckBox { name: "differenceRaincloudPlotHorizontal"; label: qsTr("Horizontal display")		}
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
		RadioButton { value: "perDependent";			label: qsTr("Exclude cases per variable"); checked: true		}
		RadioButton { value: "listwise";						label: qsTr("Exclude cases listwise")								}
	}

}

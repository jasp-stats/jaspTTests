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

Form {
	info: qsTr("The paired samples t-test allows you to estimate the effect size and test the null hypothesis that the population mean of the difference between paired (dependent) observations equals 0.\n") +
	"## " + "Assumptions" + "\n" + "- Continuous difference score.\n" + "- The difference scores are a random sample from the population.\n" + "- The difference scores are normally distributed in the population."
	id: form
	property int framework:	Common.Type.Framework.Bayesian

	plotHeight: 340
	plotWidth:  420

	VariablesForm
	{
		infoLabel: qsTr("Input")
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "allVariablesList" }
		AssignedPairsVariablesList { name: "pairs"; title: qsTr("Variable Pairs"); info: qsTr("In this box the variables are selected for which the difference is computed. Multiple differences can be analysed at the same time by specifying different rows with two variables for which the difference is computed. In other words, each row represents a separate set of difference scores.") ;allowedColumns: ["scale"]; minNumericLevels: 2 }
	}

	RadioButtonGroup
	{
		id:		hypothesis
		name:	"alternative"
		title:	qsTr("Alternative Hypothesis")
		RadioButton { value: "twoSided";	label: qsTr("Measure 1 ≠ Measure 2"); info: qsTr("Two-sided alternative hypothesis that the population mean of the difference is not equal to 0. Selected by default.") ; checked: true	}
		RadioButton { value: "greater";	label: qsTr("Measure 1 > Measure 2"); info: qsTr("One-sided alternative hypothesis that the population mean of the difference is larger than 0.")				}
		RadioButton { value: "less";	label: qsTr("Measure 1 < Measure 2"); info: qsTr("One-sided alternative hypothesis that the population mean of the difference is smaller than 0.")			}
	}

	Group
	{
		title: qsTr("Plots")
		Layout.rowSpan: 2

		CheckBox
		{
			name: "priorAndPosteriorPlot";		label: qsTr("Prior and posterior"); info: qsTr("Displays the prior and posterior distributions of the effect size after observing the data.")
			CheckBox { name: "priorAndPosteriorPlotAdditionalInfo";		label: qsTr("Additional info"); info: qsTr("Adds the Bayes factor computed with the user-defined prior; adds a probability wheel depicting how likely the data is under the null vs. alternative hypothesis; adds the median and the 95% credible interval of the posterior distribution of the effect size.") ; checked: true }
			CIField  { name: "priorAndPosteriorPlotCiLevel";	label: qsTr("Credible interval") }
		}

		CheckBox
		{
			enabled: student.checked && priors.defaultPriorsChecked
			name: "bfRobustnessPlot";	label: qsTr("Bayes factor robustness check"); info: qsTr("Displays the Bayes factor across different values of cauchy prior width. The scale of the Cauchy prior is varied between 0 and 1.5, creating progressively more uninformative priors.")
			CheckBox { name: "bfRobustnessPlotAdditionalInfo";	label: qsTr("Additional info"); info: qsTr("Adds the Bayes factor computed with the user-defined prior; adds a probability wheel depicting how likely the data is under the null vs. alternative hypothesis; adds the median and the 95% credible interval of the posterior distribution of the effect size.") ; checked: true }
		}

		CheckBox
		{
			enabled: student.checked && priors.defaultPriorsChecked
			name: "bfSequentialPlot";		label: qsTr("Sequential analysis"); info: qsTr("Displays the development of the Bayes factor as the data come in using the user-defined prior.")
			CheckBox { name: "bfSequentialPlotRobustness";		label: qsTr("Robustness check"); info: qsTr("Adds the results of the sequential analysis using the wide (scale=1) and ultrawide prior (scale=sqrt(2)).") }
		}

		CheckBox
		{
			name: "descriptivesPlot";			label: qsTr("Descriptives"); info: qsTr("Display descriptives plots. Includes central credible interval, which shows the probability (conditional on the alternative model) that the true effect size lies within certain values. The default credible interval is set at 95% and can be changed by the user.")
			CIField { name: "descriptivesPlotCiLevel";	label: qsTr("Credible interval") }
		}

		Common.BarPlots
		{
			framework:	form.framework
		}

		CheckBox
		{
			name: "raincloudPlot"; label: qsTr("Raincloud plots"); info: qsTr("Displays the individual cases (colored dots), box plots, and densities for each group.") 
		}

		CheckBox
		{
			name: "differenceRaincloudPlot"; label: qsTr("Raincloud difference plots"); info: qsTr("Displays a raincloud plot of the differences between the two measures.")
			CheckBox { name: "differenceRaincloudPlotHorizontal"; label: qsTr("Horizontal display"); info: qsTr("Changes the orientation of the raincloud plot so that the x-axis represents the dependent variable.") }
		}
	}

	BayesFactorType { correlated: hypothesis.value }

	RadioButtonGroup
	{
		name: "test"
		id: testStatistic
		title: qsTr("Tests")
		RadioButton
		{
			id: student
			value: "student";	label: qsTr("Student"); info: qsTr("Student's t-test. This option is selected by default.");  checked: true }
		RadioButton
		{
			value: "wilcoxon";	label: qsTr("Wilcoxon signed-rank"); info: qsTr("Non-parametric version of paired samples t-test. Use when data is not normally distributed. The number of MCMC samples is set at 1000 by default and can be changed.") ; id: testWilcoxon
			IntegerField { name: "wilcoxonSamples"; label: qsTr("No. samples"); defaultValue: 1000; min: 100; max: 10000; fieldWidth: 60 }
		}
	}

	Group
	{
		title: qsTr("Additional Statistics"); info: qsTr("Sample size, sample mean, sample standard deviation, and standard error of the mean for each group.")
		CheckBox { name: "descriptives"; label: qsTr("Descriptives") }
	}

	RadioButtonGroup
	{
		name: "naAction"
		title: qsTr("Missing Values")
		RadioButton { value: "perDependent";	label: qsTr("Exclude cases per dependent variable"); info: qsTr("In case of multiple t-tests within a single analysis, each test will be conducted using all cases with valid data for the dependent variable for the particular t-test. Sample sizes may therefore vary across the multiple t-tests."); checked: true }
		RadioButton { value: "listwise";				label: qsTr("Exclude cases listwise"); info: qsTr("In case of multiple t-tests within a single analysis, each t-test will be conducted using only cases with valid data for all dependent variables. Sample size is therefore constant across the multiple t-tests.")												}
	}

	SubjectivePriors { id: priors }

}

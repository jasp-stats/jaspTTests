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
	info: qsTr("The paired samples t-test allows you to estimate the effect size and test the null hypothesis that the population mean equals a specific constant,i.e., the test value.\n") +
	"## " + "Assumptions" + "\n" + "- Continuous dependent variable.\n" + "- The data are a random sample from the population.\n" + "- The dependent variable is normally distributed in the population."
	id: form
	property int framework:	Common.Type.Framework.Bayesian

	plotHeight: 240
	plotWidth:  320

	CheckBox { name: "standardizedEffectSize"; checked: true; visible: false }

	Formula { rhs: "dependent" }

	VariablesForm
	{
		infoLabel: qsTr("Input")
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "allVariablesList" }
		AssignedVariablesList { name: "dependent"; title: qsTr("Variables"); info: qsTr("In this box the dependent variable is selected.\n") + "- Test value: Test value specified in the null hypothesis." ; allowedColumns: ["scale"]; minNumericLevels: 2 }
	}

	DoubleField { name: "testValue"; label: qsTr("Test value:"); defaultValue: 0; negativeValues: true }

	Group
	{
		title: qsTr("Plots")
		Layout.rowSpan: 3

		CheckBox
		{
			name: "priorAndPosteriorPlot";		label: qsTr("Prior and posterior"); info: qsTr("Displays the prior and posterior distribution of the effect size after the data is seen.\n") + "\t" + "- Additional info: Adds the Bayes factor computed with the user-defined prior; adds a probability wheel depicting how likely the data is under the null vs. alternative hypothesis; adds the median and the 95% credible interval of the posterior distribution of the effect size."
			CheckBox { name: "priorAndPosteriorPlotAdditionalInfo";		label: qsTr("Additional info"); checked: true }
			CIField  { name: "priorAndPosteriorPlotCiLevel";	label: qsTr("Credible interval") }
		}

		CheckBox
		{
			enabled: student.checked && priors.defaultPriorsChecked
			name: "bfRobustnessPlot";	label: qsTr("Bayes factor robustness check"); info: qsTr("Displays the Bayes factor accross different values of cauchy prior width. The scale of the Cauchy prior is varied between 0 and 1.5, creating progressively more uninformative priors.\n") +
			"\t" + "- Additional info: Adds the Bayes factor computed with the user-defined prior; adds a probability wheel depicting how likely the data is under the null vs. alternative hypothesis; adds the median and the 95% credible interval of the posterior distribution of the effect size."
			CheckBox { name: "bfRobustnessPlotAdditionalInfo";	label: qsTr("Additional info"); checked: true }
		}

		CheckBox
		{
			enabled: student.checked && priors.defaultPriorsChecked
			name: "bfSequentialPlot";		label: qsTr("Sequential analysis"); info: qsTr("Displays the development of the Bayes factor as the data come in using the user-defined prior.\n") +
			"\t" + "- Robustness check: Adds the results of the sequential analysis using the wide (scale=1) and ultrawide prior (scale=sqrt(2))."
			CheckBox { name: "bfSequentialPlotRobustness";		label: qsTr("Robustness check") }
		}

		CheckBox
		{
			name: "descriptivesPlot";			label: qsTr("Descriptives"); info: qsTr("\n") + "\t" + "- Credible interval: Display central credible intervals. A credible interval shows the probability that the true effect size lies within certain values. The default credible interval is set at 95%.\n"
			 + "- Bar plots: Displays the sample means as bars and the credible intervals or standard errors as error bars for each measure.\n" + "\t" + "- Credible interval: Displays the central credible intervals. By default, the credible interval is set to 95%. This can be changed into the desired percentage.\n" +
			"\t" + "- Standard error: By selecting this option, the error bars will represent standard errors of the mean of each condition.\n" + "\t" + "- Fix horizontal axis to 0: Forces the graph to show the default x-axis at y = 0."
			CIField { name: "descriptivesPlotCiLevel";	label: qsTr("Credible interval") }
		}

		Common.BarPlots
		{
			framework:	form.framework
		}

		CheckBox
		{
			name: "raincloudPlot"; label: qsTr("Raincloud plots"); info: qsTr("Displays the individual cases (colored dots), box plots, and densities for each group.\n") +
			"\t" + "- Horizontal display: Changes the orientation of the raincloud plot so that the x-axis represents the dependent variable."
			CheckBox { name: "raincloudPlotHorizontal"; label: qsTr("Horizontal display") }
		}
	}


	RadioButtonGroup
	{
		id:		hypothesis
		name:	"alternative"
		title:	qsTr("Alternative Hypothesis")
		RadioButton { value: "twoSided";		label: qsTr("≠ Test value"); info: qsTr("Two-sided alternative hypothesis that the population mean is not equal to the test value. Selected by default."); checked: true		}
		RadioButton { value: "greater";	label: qsTr("> Test value"); info: qsTr("One-sided alternative hypothesis that the population mean is larger than the test value.")	}
		RadioButton { value: "less";		label: qsTr("< Test value"); info: qsTr("One sided alternative hypothesis that the population mean is smaller than the test value.")					}
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
			value: "student";	label: qsTr("Student"); info: qsTr("Student's t-test. This option is selected by default.") ; checked: true }
		RadioButton
		{
			value: "wilcoxon";	label: qsTr("Wilcoxon signed-rank"); info: qsTr("Non-parametric version of one-sample t-test. Use when data is not normally distributed.\n") +
			"\t" + "- No.samples: Number of MCMC samples to use"; id: testWilcoxon
			IntegerField { name: "wilcoxonSamples"; label: qsTr("No. samples"); defaultValue: 1000; min: 100; max: 10000; fieldWidth: 60 }
		}
	}

	Group
	{
		title: qsTr("Additional Statistics"); info: qsTr("\n") + "- Descriptives: Sample size, sample mean, sample standard deviation, and standard error of the mean for each group."
		CheckBox { name: "descriptives";	label: qsTr("Descriptives") }
	}

	RadioButtonGroup
	{
		name: "naAction"
		title: qsTr("Missing Values")
		RadioButton { value: "perDependent";	label: qsTr("Exclude cases per dependent variable"); info: qsTr("In case of multiple t-tests within a single analysis, each test will be conducted using all cases with valid data for the dependent variable for the particular t-test. Sample sizes may therefore vary across the multiple t-tests.; checked: true") }
		RadioButton { value: "listwise";				label: qsTr("Exclude cases listwise"); info: qsTr(" In case of multiple t-tests within a single analysis, each t-test will be conducted using only cases with valid data for all dependent variables. Sample size is therefore constant across the multiple t-tests.\n") +
		"## " + "prior" + "\n" + "\t" + "Standardized effect size\n" + "\t" + "\t" + "Default\n" + "\t" + "\t" + "\t" + "- Cauchy: Scale of the Cauchy prior distribution on effect size under the alternative hypothesis; the default is 0.707.\n" +
 "\t" + "\t" + "Informed\n" + "\t" + "\t" + "\t" + "- Cauchy: Scale and location.\n" + "\t" + "\t" + "\t" + "- Normal: Mean and standard deviation." + "\t" + "\t" + "\t" + "- Student's t: Scale, location and degrees of freedom (df)."							}
	}


	SubjectivePriors { id: priors }



}

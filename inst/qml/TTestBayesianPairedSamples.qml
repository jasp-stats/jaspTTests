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

import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0
import "./common" as Common

Form {
	id: form
	property int framework:	Common.Type.Framework.Bayesian

	plotHeight: 340
	plotWidth:  420

	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "allVariablesList" }
		AssignedPairsVariablesList { name: "pairs"; title: qsTr("Variable Pairs"); suggestedColumns: ["scale"] }
	}

	RadioButtonGroup
	{
		name: "alternative"
		title: qsTr("Alternative Hypothesis")
		RadioButton { value: "twoSided";	label: qsTr("Measure 1 ≠ Measure 2"); checked: true	}
		RadioButton { value: "greater";	label: qsTr("Measure 1 > Measure 2");				}
		RadioButton { value: "less";	label: qsTr("Measure 1 < Measure 2");				}
	}

	Group
	{
		title: qsTr("Plots")
		Layout.rowSpan: 2

		CheckBox
		{
			name: "priorAndPosteriorPlot";		label: qsTr("Prior and posterior")
			CheckBox { name: "priorAndPosteriorPlotAdditionalInfo";		label: qsTr("Additional info"); checked: true }
			CIField  { name: "priorAndPosteriorPlotCiLevel";	label: qsTr("Credible interval") }
		}

		CheckBox
		{
			enabled: student.checked && priors.defaultPriorsChecked
			name: "bayesFactorRobustnessPlot";	label: qsTr("Bayes factor robustness check")
			CheckBox { name: "bayesFactorRobustnessPlotAdditionalInfo";	label: qsTr("Additional info"); checked: true }
		}

		CheckBox
		{
			enabled: student.checked && priors.defaultPriorsChecked
			name: "sequentialAnalysisPlot";		label: qsTr("Sequential analysis")
			CheckBox { name: "sequentialAnalysisPlotRobustness";		label: qsTr("Robustness check") }
		}

		CheckBox
		{
			name: "descriptivesPlot";			label: qsTr("Descriptives")
			CIField { name: "descriptivesPlotCiLevel";	label: qsTr("Credible interval") }
		}

		Common.BarPlots
		{
			framework:	form.framework
		}

		CheckBox
		{
			name: "raincloudPlot"; label: qsTr("Raincloud plots")
		}

		CheckBox
		{
			name: "differenceRaincloudPlot"; label: qsTr("Raincloud difference plots")
			CheckBox { name: "differenceRaincloudPlotHorizontal"; label: qsTr("Horizontal display") }
		}
	}

	BayesFactorType { }

	RadioButtonGroup
	{
		name: "test"
		id: testStatistic
		title: qsTr("Tests")
		RadioButton
		{
			id: student
			value: "student";	label: qsTr("Student"); checked: true }
		RadioButton
		{
			value: "wilcoxon";	label: qsTr("Wilcoxon signed-rank"); id: testWilcoxon
			IntegerField { name: "wilcoxonSamples"; label: qsTr("No. samples"); defaultValue: 1000; min: 100; max: 10000; fieldWidth: 60 }
		}
	}

	Group
	{
		title: qsTr("Additional Statistics")
		CheckBox { name: "descriptives"; label: qsTr("Descriptives") }
	}

	RadioButtonGroup
	{
		name: "naAction"
		title: qsTr("Missing Values")
		RadioButton { value: "perDependent";	label: qsTr("Exclude cases per dependent variable"); checked: true }
		RadioButton { value: "listwise";				label: qsTr("Exclude cases listwise")							}
	}

	SubjectivePriors { id: priors }

}

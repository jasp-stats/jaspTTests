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
	id: form
	property int framework:	Common.Type.Framework.Bayesian

	plotHeight: 240
	plotWidth:  320

	CheckBox { name: "standardizedEffectSize"; checked: true; visible: false }

	Formula { rhs: "dependent" }

	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "allVariablesList" }
		AssignedVariablesList { name: "dependent"; title: qsTr("Variables"); suggestedColumns: ["scale"] }
	}

	DoubleField { name: "testValue"; label: qsTr("Test value:"); defaultValue: 0; negativeValues: true }

	Group
	{
		title: qsTr("Plots")
		Layout.rowSpan: 3

		CheckBox
		{
			name: "priorAndPosteriorPlot";		label: qsTr("Prior and posterior")
			CheckBox { name: "priorAndPosteriorPlotAdditionalInfo";		label: qsTr("Additional info"); checked: true }
			CIField  { name: "priorAndPosteriorPlotCiLevel";	label: qsTr("Credible interval") }
		}

		CheckBox
		{
			enabled: student.checked && priors.defaultPriorsChecked
			name: "bfRobustnessPlot";	label: qsTr("Bayes factor robustness check")
			CheckBox { name: "bfRobustnessPlotAdditionalInfo";	label: qsTr("Additional info"); checked: true }
		}

		CheckBox
		{
			enabled: student.checked && priors.defaultPriorsChecked
			name: "bfSequentialPlot";		label: qsTr("Sequential analysis")
			CheckBox { name: "bfSequentialPlotRobustness";		label: qsTr("Robustness check") }
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
			CheckBox { name: "raincloudPlotHorizontal"; label: qsTr("Horizontal display") }
		}
	}


	RadioButtonGroup
	{
		name: "alternative"
		title: qsTr("Alternative Hypothesis")
		RadioButton { value: "twoSided";		label: qsTr("≠ Test value"); checked: true		}
		RadioButton { value: "greater";	label: qsTr("> Test value");					}
		RadioButton { value: "less";		label: qsTr("< Test value");					}
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
		CheckBox { name: "descriptives";	label: qsTr("Descriptives") }
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

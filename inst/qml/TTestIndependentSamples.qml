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
	property int framework:	Common.Type.Framework.Classical

	plotHeight: 300
	plotWidth:  350

	Formula { rhs: "dependent" }

	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "allVariablesList" }
		AssignedVariablesList { name: "dependent";			title: qsTr("Dependent Variables");			suggestedColumns: ["scale"]	}
		AssignedVariablesList { name: "group";	title: qsTr("Grouping Variable");	suggestedColumns: ["ordinal", "nominal"]; singleVariable: true }
	}

	Group
	{
		title: qsTr("Tests")
    	CheckBox { name: "student";		label: qsTr("Student"); checked: true	}
		CheckBox { name: "welch";			label: qsTr("Welch")					}
		CheckBox { name: "mannWhitneyU";	label: qsTr("Mann-Whitney")				}
	}

	Group
	{
		title: qsTr("Additional Statistics")
		Layout.rowSpan: 2
		CheckBox
		{
			name: "meanDifference"; label: qsTr("Location parameter")
			CheckBox
			{
				name: "meanDifferenceCi"; label: qsTr("Confidence interval")
				childrenOnSameRow: true
				CIField { name: "meanDifferenceCiLevel" }
			}
		}
		CheckBox
		{
			name: "effectSize"; label: qsTr("Effect size")
			RadioButtonGroup {
				name: "effectSizeType";
				RadioButton { value: "cohen"; label: qsTr("Cohen's d") ; checked: true }
				RadioButton { value: "glass"; label: qsTr("Glass' delta") }
				RadioButton { value: "hedges"; label: qsTr("Hedges' g") }
				CheckBox
				{
					name: "effectSizeCi"; label: qsTr("Confidence interval")
					childrenOnSameRow: true
					PercentField { name: "effectSizeCiLevel"; defaultValue: 95 }
				}
			}
		}
		CheckBox { name: "descriptives";	label: qsTr("Descriptives")								}
		CheckBox
		{
			name: "descriptivesPlot";		label: qsTr("Descriptives plots")
			CIField { name: "descriptivesPlotCiLevel"; label: qsTr("Confidence interval") }
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
		CheckBox { name: "vovkSellke";	label: qsTr("Vovk-Sellke maximum p-ratio") }
	}

	RadioButtonGroup
	{
		name: "alternative"
		title: qsTr("Alternative Hypothesis")
		RadioButton { value: "twoSided";	label: qsTr("Group 1 ≠ Group 2"); checked: true	}
		RadioButton { value: "greater";	label: qsTr("Group 1 > Group 2")					}
		RadioButton { value: "less"; label: qsTr("Group 1 < Group 2")					}
	}

	Group
	{
		title: qsTr("Assumption Checks")
		CheckBox { name: "normalityTest";	label: qsTr("Normality") }
		CheckBox
		{
			name: "equalityOfVariancesTest";	label: qsTr("Equality of variances")
			RadioButtonGroup
			{
				name: "equalityOfVariancesTestType"
				RadioButton { value: "brownForsythe";	label: qsTr("Brown-Forsythe") ; checked: true }
				RadioButton { value: "levene";			label: qsTr("Levene's") }
			}
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

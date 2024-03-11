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

	Formula { rhs: "dependent" }

	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "allVariablesList" }
		AssignedVariablesList { name: "dependent"; title: qsTr("Variables"); suggestedColumns: ["scale"] }
	}

	Group
	{
		title: qsTr("Tests")
		CheckBox { name: "student";		label: qsTr("Student"); checked: true	}
		CheckBox { name: "wilcoxon";	label: qsTr("Wilcoxon signed-rank")		}
		CheckBox { name: "zTest";			label: qsTr("Z Test"); id: zTest		}
		DoubleField { name: "testValue";	label: qsTr("Test value:");		defaultValue: 0;	negativeValues: true	}
		DoubleField { name: "zTestSd";		label: qsTr("Std. deviation:");	defaultValue: 1.0;	enabled: zTest.checked	}
	}

	Group
	{
		title: qsTr("Additional Statistics")
		Layout.rowSpan: 2
		CheckBox
		{
            name: "meanDifference";			label: qsTr("Location estimate")
			CheckBox
			{
				name: "meanDifferenceCi";	label: qsTr("Confidence interval")
				childrenOnSameRow: true
				CIField { name: "meanDifferenceCiLevel" }
			}
		}

		CheckBox
		{
			name: "effectSize";				label: qsTr("Effect size")
			CheckBox
			{
				name: "effectSizeCi"; label: qsTr("Confidence interval")
				childrenOnSameRow: true
				CIField { name: "effectSizeCiLevel" }
			}
		}
		CheckBox { name: "descriptives";	label: qsTr("Descriptives") }
		CheckBox
		{
			name: "descriptivesPlot";		label: qsTr("Descriptives plots")
			CIField { name: "descriptivesPlotCi";	label: qsTr("Confidence interval") }
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
		RadioButton { value: "twoSided";		label: qsTr("≠ Test value"); checked: true	}
		RadioButton { value: "greater";	label: qsTr("> Test value")					}
		RadioButton { value: "less";		label: qsTr("< Test value")					}
	}

	Group
	{
		title: qsTr("Assumption checks")
		CheckBox { name: "normalityTest"; label: qsTr("Normality") }
	}

	RadioButtonGroup
	{
		name: "naAction"
		title: qsTr("Missing Values")
		RadioButton { value: "perVariable";	label: qsTr("Exclude cases per variable"); checked: true }
		RadioButton { value: "listwise";				label: qsTr("Exclude cases listwise")							}
	}
}

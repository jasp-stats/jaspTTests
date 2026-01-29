//
// Copyright (C) 2013-2022 University of Amsterdam
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
import JASP.Controls
import JASP
import "./" as Common


CheckBox
{
	name: 						"barPlot"
	label: 						qsTr("Bar plots")
	property int framework:		Common.Type.Framework.Classical


	RadioButtonGroup
	{
		name: "barPlotErrorType"
		info: qsTr("Displays a bar plot of the sample mean(s), including error bars.")

		RadioButton
		{
			value: 				"ci"
			label: 				framework === Common.Type.Framework.Classical ? qsTr("Confidence interval") : qsTr("Credible interval")
			checked: 			true
			childrenOnSameRow: 	true
			info:				qsTr("Coverage of the confidence intervals (Or credible intervals in case of a Bayesian analysis) in percentages. By default, the confidence interval is set to 95%. This can be changed into the desired percentage.")

			CIField { name: 	"barPlotCiLevel"}
		}
		RadioButton { value: 	"se";	label: qsTr("Standard error"); info: qsTr("By selecting this option, the error bars will represent standard errors of the mean of each condition." )}
	}

	CheckBox
	{
		name: "barPlotYAxisFixedToZero";
		label: qsTr("Fix horizontal axis to 0"); checked: true;
		info: qsTr("Forces the graph to show the default x-axis at y = 0.")
	}
}

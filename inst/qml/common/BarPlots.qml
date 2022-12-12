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

			RadioButton
			{
				value: 				"ci"
				label: 				framework === Common.Type.Framework.Classical ? qsTr("Confidence interval") : qsTr("Credible interval")
				checked: 			true
				childrenOnSameRow: 	true

				CIField { name: 	"barPlotCiLevel" }
			}
			RadioButton { value: 	"se";	label: qsTr("Standard error") }
		}

	CheckBox { name: "barPlotYAxisFixedToZero";		label: qsTr("Fix horizontal axis to 0");	checked: true }
}

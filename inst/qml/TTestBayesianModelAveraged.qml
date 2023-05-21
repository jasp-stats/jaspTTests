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
	id: form
	property int framework:	Common.Type.Framework.Bayesian

	plotHeight: 340
	plotWidth:  420

	Formula { rhs: "dependent" }

	VariablesForm
	{
		AvailableVariablesList
		{
			name:				"allVariablesList"
		}
		
		AssignedVariablesList
		{
			name:				"dependent"
			title:				qsTr("Dependent Variable")
			suggestedColumns:	["scale"]
			singleVariable:		true
		}
		
		AssignedVariablesList
		{
			name:				"group"
			title:				qsTr("Grouping Variable")
			suggestedColumns:	["ordinal", "nominal"]
			singleVariable:		true
		}
	}

	CheckBox
	{
		name:		"priorDistributionPlot"
		label:		qsTr("Prior distribution plots")
	}

	//// Inference ////
	Section
	{
		title: qsTr("Inference")

		Group
		{

			CheckBox
			{
				label:		qsTr("Conditional parameter estimates")
				name:		"inferenceConditionalParameterEstimates"
				checked: 	true
			}

			CheckBox
			{
				columns:	2
				label:		qsTr("Models overview")
				name:		"inferenceModelsOverview"

				RadioButtonGroup
				{
					name: "inferenceModelsOverviewBfComparison"
					title: qsTr("BF comparison")

					RadioButton
					{
						name: 		"inclusion"
						label: 		qsTr("Inclusion")
						checked: 	true
					}

					RadioButton
					{
						name: 		"best"
						label: 		qsTr("vs. Best")
					}

					RadioButton
					{
						name: 		"previous"
						label: 		qsTr("vs. Previous")
						enabled:	inferenceModelsOverviewOrderMarglik.checked
					}
				}

				RadioButtonGroup
				{
					name: 		"inferenceModelsOverviewOrder"
					title:		qsTr("Order")

					RadioButton
					{
						name: 		"modelNumber"
						label: 		qsTr("Model number")
						checked:	true
					}

					RadioButton
					{
						name: 		"marginalLikelihood"
						label: 		qsTr("Marginal likelihood")
						id:			inferenceModelsOverviewOrderMarglik
					}

					RadioButton
					{
						name: 		"posteriorProbability"
						label: 		qsTr("Posterior probability")

					}
				}
			}

			CheckBox
			{
				label:		qsTr("Individual models")
				name:		"inferenceIndividualModels"

				CheckBox
				{
					label:		qsTr("Single model")
					name:		"inferenceIndividualModelsSingleModel"
					childrenOnSameRow: true
					IntegerField
					{
						name:	"inferenceIndividualModelsSingleModelNumber"
						defaultValue:	1
					}
				}
			}

		}

		Group
		{

			BayesFactorType{}

			CIField
			{
				name: "inferenceCiWidth"
				label: qsTr("CI width")
			}

			CheckBox
			{
				label:		qsTr("Shorten prior names")
				name:		"inferenceShortenPriorName"
			}

			CheckBox
			{
				label:		qsTr("Heterogeneity as standard deviation ratio")
				name:		"inferenceHeterogeneityAsStandardDeviationRatio"
				checked:	true
			}

		}
	}

	//// Plots section ////
	Section
	{
		title: 		qsTr("Plots")


		Group
		{
			title:	qsTr("Pooled estimates")
			columns: 1

			CheckBox
			{
				label:	qsTr("Effect")
				name:	"plotsPooledEstimatesEffect"
			}

			CheckBox
			{
				label:	qsTr("Heterogeneity (precision allocation)")
				name:	"plotsPooledEstimatesHeterogeneity"
			}

			CheckBox
			{
				label:	qsTr("Outliers (degrees of freedom)")
				name:	"plotsPooledEstimatesOutliers"
			}

		}

		Group
		{
			title:		" " // Add a line to align with the first column
			columns:	1

			RadioButtonGroup
			{
				name:		"plotsPooledEstimatesType"
				title:		qsTr("Type")
				columns:	2

				RadioButton
				{
					value:		"averaged"
					label:		qsTr("Model averaged")
					checked:	true
				}

				RadioButton
				{
					value:		"conditional"
					label:		qsTr("Conditional")
				}

			}

			CheckBox
			{
				label:		qsTr("Prior distribution")
				name:		"plotsPooledEstimatesPriorDistribution"
				checked:	true
			}
		}

	}

	//// Diagnostics section ////
	Section
	{
		title: qsTr("MCMC Diagnostics")

		CheckBox
		{
			Layout.columnSpan: 2
			label:		qsTr("Overview table")
			name:		"mcmcDiagnosticsOverviewTable"
		}

		Group
		{
			title:			qsTr("Plot")
			CheckBox
			{
				label:		qsTr("Effect")
				name:		"mcmcDiagnosticsPlotEffect"
			}

			CheckBox
			{
				label:		qsTr("Heterogeneity")
				name:		"mcmcDiagnosticsPlotHeterogeneity"
			}

			CheckBox
			{
				label:		qsTr("Degrees of freedom")
				name:		"mcmcDiagnosticsPlotOutliers"
			}
		}

		Group
		{
			Group
			{
				title:			qsTr("Type")
				CheckBox
				{
					label:		qsTr("Trace")
					name:		"mcmcDiagnosticsPlotTypeTrace"
				}

				CheckBox
				{
					label:		qsTr("Autocorrelation")
					name:		"mcmcDiagnosticsPlotTypeAutocorrelation"
				}

				CheckBox
				{
					label:		qsTr("Posterior samples density")
					name:		"mcmcDiagnosticsPlotTypePosteriorSamplesDensity"
				}
			}

			CheckBox
			{
				label:		qsTr("Single model")
				name:		"mcmcDiagnosticsPlotSingleModel"
				childrenOnSameRow: true

				IntegerField
				{
					name:			"mcmcDiagnosticsPlotSingleModelNumber"
					defaultValue:	1
				}
			}
		}

	}

	//// Priors ////
	Section
	{
		title: 				qsTr("Models")
		columns:			1


		// effect prior
		Common.ModelAveragedTTestPriors
		{
			Layout.preferredWidth:	parent.width
			componentType:			"modelsEffect"
		}

		// heterogeneity prior
		Common.ModelAveragedTTestPriors
		{
			Layout.preferredWidth:	parent.width
			componentType:			"modelsHeterogeneity"
		}

		// df priors
		Common.ModelAveragedTTestPriors
		{
			Layout.preferredWidth:	parent.width
			componentType:			"modelsOutliers"
		}

		Divider { }

		CheckBox
		{
			id:						priorsNull
			name:					"priorsNull"
			label:					qsTr("Set null priors")
		}

		// effect prior
		Common.ModelAveragedTTestPriors
		{
			Layout.preferredWidth:	parent.width
			componentType:			"modelsEffectNull"
			visible:				priorsNull.checked
		}

		// heterogeneity prior
		Common.ModelAveragedTTestPriors
		{
			Layout.preferredWidth:	parent.width
			componentType:			"modelsHeterogeneityNull"
			visible:				priorsNull.checked
		}

		// df priors
		Common.ModelAveragedTTestPriors
		{
			Layout.preferredWidth:	parent.width
			componentType:			"modelsOutliersNull"
			visible:				priorsNull.checked
		}

	}

	//// Advanced section for prior model probabilities sampling settings ////
	Section
	{
		title: 				qsTr("Advanced")
		columns: 			2
		
		Group
		{
			rowSpacing: 10 * preferencesModel.uiScale

			Group
			{
				title: 		qsTr("MCMC")

				IntegerField
				{
					name:			"advancedMcmcAdaptation"
					label:			qsTr("Adaptation")
					defaultValue:	5000
					min:			1000
					fieldWidth:		55 * preferencesModel.uiScale
				}
				IntegerField
				{
					name:			"advancedMcmcSamples"
					label:			qsTr("Samples")
					defaultValue:	5000
					min:			4000
					fieldWidth:		55 * preferencesModel.uiScale
				}
				IntegerField
				{
					name:			"advancedMcmcChains"
					label:			qsTr("Chains")
					defaultValue:	4
					min:			1
					fieldWidth:		55 * preferencesModel.uiScale
				}
				IntegerField
				{
					name:			"advancedMcmcThin"
					label:			qsTr("Thin")
					defaultValue:	1
					min:			1
					fieldWidth:		55 * preferencesModel.uiScale
				}

			}

			SetSeed{}
		}
	}

}
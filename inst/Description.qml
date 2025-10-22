import QtQuick
import JASP.Module

Description
{
	title		: qsTr("T-Tests")
	description	: qsTr("Evaluate the difference between two means")
	icon		: "analysis-classical-ttest.svg"
	hasWrappers	: true

	GroupTitle
	{
		title:	qsTr("Classical")
		icon:	"analysis-classical-ttest.svg"
	}
	Analysis
	{
		title:	qsTr("Independent Samples T-Test")
		func:	"TTestIndependentSamples"
	}
	Analysis
	{
		title:	qsTr("Paired Samples T-Test")
		func:	"TTestPairedSamples"
	}
	Analysis
	{
		title:	qsTr("One Sample T-Test")
		func:	"TTestOneSample"
	}

	Separator{}

	GroupTitle
	{
		title:	qsTr("Bayesian")
		icon:	"analysis-bayesian-ttest.svg"
	}
	Analysis
	{
		menu:	qsTr("Independent Samples T-Test")
		title:	qsTr("Bayesian Independent Samples T-Test")
		func:	"TTestBayesianIndependentSamples"
	}
	Analysis
	{
		menu:	qsTr("Paired Samples T-Test")
		title:	qsTr("Bayesian Paired Samples T-Test")
		func:	"TTestBayesianPairedSamples"
	}
	Analysis
	{
		menu:	qsTr("One Sample T-Test")
		title:	qsTr("Bayesian One Sample T-Test")
		func:	"TTestBayesianOneSample"
	}
}

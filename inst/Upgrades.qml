import QtQuick          2.12
import JASP.Module      1.0

Upgrades
{
	Upgrade
	{
				functionName: "TTestIndependentSamples"
				fromVersion:	"0.17.0"
				toVersion:		"0.17.1"

				ChangeRename { from:	"wilcoxon";										to:		"mannWhitneyU"								}
	}

	Upgrade
		{
				functionName: "TTestIndependentSamples"
				fromVersion: "0.16.4"
				toVersion: "0.17.0"

				ChangeRename { from:	"variables";										to:		"dependent"								}
				ChangeRename { from:	"groupingVariable";									to:		"group"									}
				ChangeRename { from:	"students";											to:		"student"								}
				ChangeRename { from:	"welchs";											to:		"welch"									}
				ChangeRename { from:	"mannWhitneyU";										to:		"wilcoxon"								}
				ChangeRename { from:	"meanDiffConfidenceIntervalCheckbox";				to:		"meanDifferenceCi"						}
				ChangeRename { from:	"descriptivesMeanDiffConfidenceIntervalPercent";	to:		"meanDifferenceCiLevel"					}
				ChangeRename { from:	"effectSizesType";									to:		"effectSizeType"						}

				ChangeJS
                {
	                    name:		"effectSizeType"
	                    jsFunction:	function(options)
	                    {
		                        switch(options["effectSizeType"])
		                        {
			                            case "cohensD":	return "cohen";
			                            case "glassD":	return "glass";
			                            case "hedgesG":	return "hedges";
		                        }
	                    }
                }

				ChangeRename { from:	"effSizeConfidenceIntervalCheckbox";				to:		"effectSizeCi"							}
				ChangeRename { from:	"descriptivesEffectSizeConfidenceIntervalPercent";	to:		"effectSizeCiLevel"						}
				ChangeRename { from:	"descriptivesPlots";								to:		"descriptivesPlot"						}
				ChangeRename { from:	"descriptivesPlotsConfidenceInterval";				to:		"descriptivesPlotCiLevel"				}
				ChangeRename { from:	"descriptivesBarPlots";								to:		"barPlot"								}
				ChangeRename { from:	"errorBarType";										to:		"barPlotErrorType"						}

				ChangeJS
                {
	                    name:		"barPlotErrorType"
	                    jsFunction:	function(options)
	                    {
		                        switch(options["barPlotErrorType"])
		                        {
			                            case "confidenceInterval":	return "ci";
			                            case "standardError":		return "se";
		                        }
	                    }
                }

				ChangeRename { from:	"descriptivesBarPlotsConfidenceInterval";			to:		"barPlotCiLevel"						}
				ChangeRename { from:	"descriptivesBarPlotsZeroFix";						to:		"barPlotYAxisFixedToZero"				}
				ChangeRename { from:	"descriptivesPlotsRainCloud";						to:		"raincloudPlot"							}	
				ChangeRename { from:	"descriptivesPlotsRainCloudHorizontalDisplay";		to:		"raincloudPlotHorizontal"				}
				ChangeRename { from:	"VovkSellkeMPR";									to:		"vovkSellke"							}
				ChangeRename { from:	"hypothesis";										to:		"alternative"							}

				ChangeJS
                {
	                    name:		"alternative"
	                    jsFunction:	function(options)
	                    {
		                        switch(options["alternative"])
		                        {
			                            case "groupsNotEqual":	return "twoSided";
			                            case "groupOneGreater":	return "greater";
			                            case "groupTwoGreater":	return "less";
		                        }
	                    }
                }

				ChangeRename { from:	"normalityTests";									to:		"normalityTest"							}
				ChangeRename { from:	"equalityOfVariancesTests";							to:		"equalityOfVariancesTest"				}
				ChangeRename { from:	"missingValues";									to:		"naAction"								}

				ChangeJS
                {
	                    name:		"naAction"
	                    jsFunction:	function(options)
	                    {
		                        switch(options["naAction"])
		                        {
			                            case "excludeAnalysisByAnalysis":	return "perDependent";
			                            case "excludeListwise":				return "listwise";
		                        }
	                    }
                }
		}

		Upgrade
        {
	            functionName: "TTestPairedSamples"
				fromVersion: "0.16.4"
				toVersion: "0.17.0"

				ChangeRename { from:	"students";													to:		"student"						}
				ChangeRename { from:	"wilcoxonSignedRank";										to:		"wilcoxon"						}
				ChangeRename { from:	"meanDiffConfidenceIntervalCheckbox";						to:		"meanDifferenceCi"				}
				ChangeRename { from:	"meanDiffConfidenceIntervalPercent";						to:		"meanDifferenceCiLevel"			}
				ChangeRename { from:	"effSizeConfidenceIntervalCheckbox";						to:		"effectSizeCi"					}
				ChangeRename { from:	"effSizeConfidenceIntervalPercent";							to:		"effectSizeCiLevel"				}
				ChangeRename { from:	"descriptivesPlots";										to:		"descriptivesPlot"				}
				ChangeRename { from:	"descriptivesPlotsConfidenceInterval";						to:		"descriptivesPlotCiLevel"		}
				ChangeRename { from:	"descriptivesBarPlots";										to:		"barPlot"						}
				ChangeRename { from:	"errorBarType";												to:		"barPlotErrorType"				}

				ChangeJS
                {
	                    name:		"barPlotErrorType"
	                    jsFunction:	function(options)
	                    {
		                        switch(options["barPlotErrorType"])
		                        {
			                            case "confidenceInterval":	return "ci";
			                            case "standardError":		return "se";
		                        }
	                    }
                }

				ChangeRename { from:	"descriptivesBarPlotsConfidenceInterval";					to:		"barPlotCiLevel"				}
				ChangeRename { from:	"descriptivesBarPlotsZeroFix";								to:		"barPlotYAxisFixedToZero"		}
				ChangeRename { from:	"descriptivesPlotsRainCloud";								to:		"raincloudPlot"					}
				ChangeRename { from:	"descriptivesPlotsRainCloudDifference";						to:		"differenceRaincloudPlot"		}
				ChangeRename { from:	"descriptivesPlotsRainCloudDifferenceHorizontalDisplay";	to:	"differenceRaincloudPlotHorizontal"	}
				ChangeRename { from:	"VovkSellkeMPR";											to:		"vovkSellke"					}
				ChangeRename { from:	"hypothesis";												to:		"alternative"					}

				ChangeJS
                {
	                    name:		"alternative"
	                    jsFunction:	function(options)
	                    {
		                        switch(options["alternative"])
		                        {
			                            case "groupsNotEqual":	return "twoSided";
			                            case "groupOneGreater":	return "greater";
			                            case "groupTwoGreater":	return "less";
		                        }
	                    }
                }

				ChangeRename { from:	"normalityTests";											to:		"normalityTest"					}
				ChangeRename { from:	"missingValues";											to:		"naAction"						}

				ChangeJS
                {
	                    name:		"naAction"
	                    jsFunction:	function(options)
	                    {
		                        switch(options["naAction"])
		                        {
			                            case "excludeAnalysisByAnalysis":	return "perDependent";
			                            case "excludeListwise":				return "listwise";
		                        }
	                    }
                }
		}

		Upgrade
        {
	            functionName: "TTestOneSample";	fromVersion: "0.16.4";		toVersion: "0.17.0";

				ChangeRename { from:	"variables";									to:		"dependent"									}
				ChangeRename { from:	"students";										to:		"student"									}
				ChangeRename { from:	"mannWhitneyU";									to:		"wilcoxon"									}
				ChangeRename { from:	"stddev";										to:		"zTestSd"									}
				ChangeRename { from:	"meanDiffConfidenceIntervalCheckbox";			to:		"meanDifferenceCi"							}
				ChangeRename { from:	"meanDiffConfidenceIntervalPercent";			to:		"meanDifferenceCiLevel"						}
				ChangeRename { from:	"effSizeConfidenceIntervalCheckbox";			to:		"effectSizeCi"								}
				ChangeRename { from:	"effSizeConfidenceIntervalPercent";				to:		"effectSizeCiLevel"							}
				ChangeRename { from:	"descriptivesPlots";							to:		"descriptivesPlot"							}
				ChangeRename { from:	"descriptivesPlotsConfidenceInterval";			to:		"descriptivesPlotCi"						}
				ChangeRename { from:	"descriptivesBarPlots";							to:		"barPlot"									}
				ChangeRename { from:	"errorBarType";									to:		"barPlotErrorType"							}

				ChangeJS
                {
	                    name:		"barPlotErrorType"
	                    jsFunction:	function(options)
	                    {
		                        switch(options["barPlotErrorType"])
		                        {
			                            case "confidenceInterval":	return "ci";
			                            case "standardError":		return "se";
		                        }
	                    }
                }

				ChangeRename { from:	"descriptivesBarPlotsConfidenceInterval";		to:		"barPlotCiLevel"							}
				ChangeRename { from:	"descriptivesBarPlotsZeroFix";					to:		"barPlotYAxisFixedToZero"					}
				ChangeRename { from:	"descriptivesPlotsRainCloud";					to:		"raincloudPlot"								}
				ChangeRename { from:	"descriptivesPlotsRainCloudHorizontalDisplay";	to:		"raincloudPlotHorizontal"					}
				ChangeRename { from:	"VovkSellkeMPR";								to:		"vovkSellke"								}
				ChangeRename { from:	"hypothesis";									to:		"alternative"								}

				ChangeJS
                {
	                    name:		"alternative"
	                    jsFunction:	function(options)
	                    {
		                        switch(options["alternative"])
		                        {
			                            case "notEqualToTestValue":		return "twoSided";
			                            case "greaterThanTestValue":	return "greater";
			                            case "lessThanTestValue":		return "less";
		                        }
	                    }
                }

				ChangeRename { from:	"normalityTests";								to:		"normalityTest"								}
				ChangeRename { from:	"missingValues";								to:		"naAction"									}

				ChangeJS
                {
	                    name:		"naAction"
	                    jsFunction:	function(options)
	                    {
		                        switch(options["naAction"])
		                        {
			                            case "excludeAnalysisByAnalysis":	return "perVariable";
			                            case "excludeListwise":				return "listwise";
		                        }
	                    }
                }
		}

		Upgrade
        {
	            functionName: "TTestBayesianIndependentSamples";	fromVersion: "0.16.4";		toVersion: "0.17.0";

				ChangeRename { from:	"variables";									to:		"dependent"									}
				ChangeRename { from:	"groupingVariable";								to:		"group"										}
				ChangeRename { from:	"hypothesis";									to:		"alternative"								}

				ChangeJS
                {
	                    name:		"alternative"
	                    jsFunction:	function(options)
	                    {
		                        switch(options["alternative"])
		                        {
			                            case "groupsNotEqual":	return "twoSided";
			                            case "groupOneGreater":	return "greater";
										case "groupTwoGreater":	return "less";
		                        }
	                    }
                }

				ChangeRename { from:	"plotPriorAndPosterior";						to:		"priorAndPosteriorPlot"						}
				ChangeRename { from:	"plotPriorAndPosteriorAdditionalInfo";			to:		"priorAndPosteriorPlotAdditionalInfo"		}
				ChangeRename { from:	"priorAndPosteriorPlotsCredibleInterval";		to:		"priorAndPosteriorPlotCiLevel"				}
				ChangeRename { from:	"plotBayesFactorRobustness";					to:		"bfRobustnessPlot"							}
				ChangeRename { from:	"plotBayesFactorRobustnessAdditionalInfo";		to:		"bfRobustnessPlotAdditionalInfo"			}
				ChangeRename { from:	"plotSequentialAnalysis";						to:		"bfSequentialPlot"							}
				ChangeRename { from:	"plotSequentialAnalysisRobustness";				to:		"bfSequentialPlotRobustness"				}
				ChangeRename { from:	"descriptivesPlots";							to:		"descriptivesPlot"							}
				ChangeRename { from:	"descriptivesPlotsCredibleInterval";			to:		"descriptivesPlotCiLevel"					}
				ChangeRename { from:	"descriptivesBarPlots";							to:		"barPlot"									}
				ChangeRename { from:	"errorBarType";									to:		"barPlotErrorType"							}

				ChangeJS
                {
	                    name:		"barPlotErrorType"
	                    jsFunction:	function(options)
	                    {
		                        switch(options["barPlotErrorType"])
		                        {
			                            case "confidenceInterval":	return "ci";
			                            case "standardError":		return "se";
		                        }
	                    }
                }

				ChangeRename { from:	"descriptivesBarPlotsConfidenceInterval";		to:		"barPlotCiLevel"							}
				ChangeRename { from:	"descriptivesBarPlotsZeroFix";					to:		"barPlotYAxisFixedToZero"					}
				ChangeRename { from:	"descriptivesPlotsRainCloud";					to:		"raincloudPlot"								}
				ChangeRename { from:	"descriptivesPlotsRainCloudHorizontalDisplay";	to:		"raincloudPlotHorizontal"					}
				ChangeRename { from:	"testStatistic";								to:		"test"										}

				ChangeJS
                {
	                    name:		"test"
	                    jsFunction:	function(options)
	                    {
		                        switch(options["test"])
		                        {
			                            case "Student":		return "student";
			                            case "Wilcoxon":	return "wilcoxon";
		                        }
	                    }
                }

				ChangeRename { from:	"wilcoxonSamplesNumber";						to:		"wilcoxonSamples"							}
				ChangeRename { from:	"missingValues";								to:		"naAction"									}

				ChangeJS
                {
	                    name:		"naAction"
	                    jsFunction:	function(options)
	                    {
		                        switch(options["naAction"])
		                        {
			                            case "excludeAnalysisByAnalysis":	return "perDependent";
			                            case "excludeListwise":				return "listwise";
		                        }
	                    }
                }
		}

		Upgrade
        {
	            functionName: "TTestBayesianPairedSamples";	fromVersion: "0.16.4";		toVersion: "0.17.0";

				ChangeRename { from:	"hypothesis";									to:		"alternative"								}

				ChangeJS
                {
	                    name:		"alternative"
	                    jsFunction:	function(options)
	                    {
		                        switch(options["alternative"])
		                        {
			                            case "groupsNotEqual":	return "twoSided";
			                            case "groupOneGreater":	return "greater";
										case "groupTwoGreater":	return "less";
		                        }
	                    }
                }

				ChangeRename { from:	"plotPriorAndPosterior";						to:		"priorAndPosteriorPlot"						}
				ChangeRename { from:	"plotPriorAndPosteriorAdditionalInfo";			to:		"priorAndPosteriorPlotAdditionalInfo"		}
				ChangeRename { from:	"priorAndPosteriorPlotsCredibleInterval";		to:		"priorAndPosteriorPlotCiLevel"				}
				ChangeRename { from:	"plotBayesFactorRobustness";					to: 	"bfRobustnessPlot"							}
				ChangeRename { from:	"plotBayesFactorRobustnessAdditionalInfo";		to:		"bfRobustnessPlotAdditionalInfo"			}
				ChangeRename { from:	"plotSequentialAnalysis";						to:		"bfSequentialPlot"							}
				ChangeRename { from:	"plotSequentialAnalysisRobustness";				to:		"bfSequentialPlotRobustness"				}
				ChangeRename { from:	"descriptivesPlots";							to:		"descriptivesPlot"							}
				ChangeRename { from:	"descriptivesPlotsCredibleInterval";			to:		"descriptivesPlotCiLevel"					}
				ChangeRename { from:	"descriptivesBarPlots";							to:		"barPlot"									}
				ChangeRename { from:	"errorBarType";									to:		"barPlotErrorType"							}

				ChangeJS
                {
	                    name:		"barPlotErrorType"
	                    jsFunction:	function(options)
	                    {
		                        switch(options["barPlotErrorType"])
		                        {
			                            case "confidenceInterval":	return "ci";
			                            case "standardError":		return "se";
		                        }
	                    }
                }

				ChangeRename { from:	"descriptivesBarPlotsConfidenceInterval";					to:		"barPlotCiLevel"				}
				ChangeRename { from:	"descriptivesBarPlotsZeroFix";								to:		"barPlotYAxisFixedToZero"		}
				ChangeRename { from:	"descriptivesPlotsRainCloud";								to:		"raincloudPlot"					}
				ChangeRename { from:	"descriptivesPlotsRainCloudDifference";						to:		"differenceRaincloudPlot"		}
				ChangeRename { from:	"descriptivesPlotsRainCloudDifferenceHorizontalDisplay";	to:	"differenceRaincloudPlotHorizontal"	}
				ChangeRename { from:	"testStatistic";											to:		"test"							}

				ChangeJS
                {
	                    name:		"test"
	                    jsFunction:	function(options)
	                    {
		                        switch(options["test"])
		                        {
			                            case "Student":		return "student";
			                            case "Wilcoxon":	return "wilcoxon";
		                        }
	                    }
                }

				ChangeRename { from:	"wilcoxonSamplesNumber";									to:		"wilcoxonSamples"				}
				ChangeRename { from:	"missingValues";											to:		"naAction"						}

				ChangeJS
                {
	                    name:		"naAction"
	                    jsFunction:	function(options)
	                    {
		                        switch(options["naAction"])
		                        {
			                            case "excludeAnalysisByAnalysis":	return "perDependent";
			                            case "excludeListwise":				return "listwise";
		                        }
	                    }
                }
		}

		Upgrade
        {
	            functionName: "TTestBayesianOneSample";	fromVersion: "0.16.4";		toVersion: "0.17.0";

				ChangeRename { from:	"variables";									to:		"dependent"									}
				ChangeRename { from:	"plotPriorAndPosterior";						to:		"priorAndPosteriorPlot"						}
				ChangeRename { from:	"plotPriorAndPosteriorAdditionalInfo";			to:		"priorAndPosteriorPlotAdditionalInfo"		}
				ChangeRename { from:	"priorAndPosteriorPlotsCredibleInterval";		to:		"priorAndPosteriorPlotCiLevel"				}
				ChangeRename { from:	"plotBayesFactorRobustness";					to: 	"bfRobustnessPlot"							}
				ChangeRename { from:	"plotBayesFactorRobustnessAdditionalInfo";		to:		"bfRobustnessPlotAdditionalInfo"			}
				ChangeRename { from:	"plotSequentialAnalysis";						to:		"bfSequentialPlot"							}
				ChangeRename { from:	"plotSequentialAnalysisRobustness";				to:		"bfSequentialPlotRobustness"				}
				ChangeRename { from:	"descriptivesPlots";							to:		"descriptivesPlot"							}
				ChangeRename { from:	"descriptivesPlotsCredibleInterval";			to:		"descriptivesPlotCiLevel"					}
				ChangeRename { from:	"descriptivesBarPlots";							to:		"barPlot"									}
				ChangeRename { from:	"errorBarType";									to:		"barPlotErrorType"							}

				ChangeJS
                {
	                    name:		"barPlotErrorType"
	                    jsFunction:	function(options)
	                    {
		                        switch(options["barPlotErrorType"])
		                        {
			                            case "confidenceInterval":	return "ci";
			                            case "standardError":		return "se";
		                        }
	                    }
                }

				ChangeRename { from:	"descriptivesBarPlotsConfidenceInterval";		to:		"barPlotCiLevel"							}
				ChangeRename { from:	"descriptivesBarPlotsZeroFix";					to:		"barPlotYAxisFixedToZero"					}
				ChangeRename { from:	"descriptivesPlotsRainCloud";					to:		"raincloudPlot"								}
				ChangeRename { from:	"descriptivesPlotsRainCloudHorizontalDisplay";	to:		"raincloudPlotHorizontal"					}
				ChangeRename { from:	"hypothesis";									to:		"alternative"								}

				ChangeJS
                {
	                    name:		"alternative"
	                    jsFunction:	function(options)
	                    {
		                        switch(options["alternative"])
		                        {
			                            case "notEqualToTestValue":		return "twoSided";
			                            case "greaterThanTestValue":	return "greater";
										case "lessThanTestValue":		return "less";
		                        }
	                    }
                }

				ChangeRename { from:	"testStatistic";								to:		"test"										}

				ChangeJS
                {
	                    name:		"test"
	                    jsFunction:	function(options)
	                    {
		                        switch(options["test"])
		                        {
			                            case "Student":		return "student";
			                            case "Wilcoxon":	return "wilcoxon";
		                        }
	                    }
                }

				ChangeRename { from:	"wilcoxonSamplesNumber";						to:		"wilcoxonSamples"							}
				ChangeRename { from:	"missingValues";								to:		"naAction"									}

				ChangeJS
                {
	                    name:		"naAction"
	                    jsFunction:	function(options)
	                    {
		                        switch(options["naAction"])
		                        {
			                            case "excludeAnalysisByAnalysis":	return "perDependent";
			                            case "excludeListwise":				return "listwise";
		                        }
	                    }
                }
		}
}

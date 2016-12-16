# Analyses R-scripts for the manuscript: "New risk and protective factors for severe hypoglycaemia in people with type 1 diabetes"

**The manuscript is in the peer-review process of the journal: [Nutrition, Metabolism & Cardiovascular Diseases] (http://www.nmcd-journal.com/)**

## Description
Here we provide the R-code which was written for the analyses of the above-mentioned manuscript.
Since the analyses for table 1 and table 3 are very basic, we did not include this.
The scripts are related to the following part of the statistical analyses protocol of the manuscript: page 5, line 7-25; page 6, line 1-25.

Due to the fact that Kaplan-Meier and related analyses are not as common as a logistic regression, we further provide an example data-set for this part.
One can execute the whole Kaplan-Meier-part with our example data-set. 

## Location
The scripts can be found in the folder "scripts".
The data-set can be found in folder "example_dataset".

## Script execution
Since two scripts are referring to external data (example data and an external function), it is not recommended to conduct the scripts via the source-command.
Please check the comments carefully and load the respective data or function if necessary.

Note: We do not provide any example-data for logistic regression since this is a very straight forward analysis.

## Code comments
We extensively commented our code-blocks, to follow our analyses.
However, R-knowledge is necessary to fully understand all the steps. 

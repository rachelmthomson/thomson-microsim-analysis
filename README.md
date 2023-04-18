# Thomson microsimulation analysis

*Analysis files for 2023 Thomson et al. paper using microsimulation to explore mental health effects of Universal Basic Income (UBI)*

Currently includes R files used to analyse outputs from SimPaths, an existing microsimulation model, after it was used to model UBI effects on mental health in UK working-age adults.

The SimPaths code is available on GitHub (link tbc) and a technical description of the model is available here: (link tbc)

Data to run the files are not freely available as the SimPaths population at the time of modelling was based on real-world data from Understanding Society (https://ukdataservice.ac.uk/deposit-data/understanding-society/). However, the files could be adapted to run on any outputs from SimPaths.

Permissions to access SimPaths can be requested here: (link tbc)

PREPARATORY FILES

* generating_fixed_poverty_lines.R = calculates the poverty line in each SimPaths simulation of the baseline policy scenario, to be applied in subsequent runs of UBI intervention scenarios

* validation_graphs.R = generates validation graphs comparing SimPaths outputs after running the baseline scenario in 2012-2018 with data from the Health Survey for England

DATA CLEANING FILES

* processing_multirun_output.R = aggregates individual-level SimPaths outputs and generates summary statistics (mean, relative/slope index of inequality)

* creating_merged_summary_data.R = combines outputs from several runs of processing_multirun_output.R to create one file with summary data for all analyses

ANALYSIS FILES

* checking_variance.R = graphs variance in prevalence of common mental disorder and poverty by number of model iterations

* outputting_graph_and_tables.R = generates an excel file of all results and all graphs in the paper/appendix

* graphing_functions.R = contains R functions used to generate graphs in outputting_graphs_and_tables.R

Outline of Scalable GIS manuscript

# Introduction
Increasing volume of environmental epidemiology & health geography literature focuses on the large-scale exposure assessment either from static spatial data and spatiotemporally dynamic data such as trajectories.
Geospatial exposure assessment was done with geographic information systems (GIS) software or open source packages such as R and Python [@eumComputationGeographicVariables2015;@kimNationalscaleExposurePrediction2017]. More general software for geospatial exposure assessment or covariate generation for land use modeling has been developed [@morleyLandUseRegression2018;@gulliverDevelopmentOpensourceRoad2015;@khanRoadTrafficAir2018;@reedGISToolModeling2012;@evansOpenSourceWebbasedGeographical2012], extending in-house and objective-specific function sets regardless of its opensourcing. An example … the other… another ... 


Computation of geospatial exposures and their contributing factors is mostly the subject of technicality rather than an academic subject. 
This subject is indeed a "grey zone" between the geospatial data processing and the health effect research as efficient exposure assessment can serve as a body of evidence for prompt response and policy development/implementation to pronounced/important risk factors to conventional and emerging adverse health outcomes especially following climate change.

Parallel computing environment is virtually ubiquitous, where ordinary consumer laptops are equipped with multi-core processors. It was long before such advances opened a computational breakthrough for general users that geographic information science (GIS) communities involved in leveraging parallel computing to process large volume of geospatial datasets in a fast and efficient way.

Existing approach is largely for an application or study-specific, which means that the computation methods are based on the generic function where study area's local data dependent solutions are employed. Toward generalized efficient exposure assessment, the common approach is to leverage increasingly available ample computing assets such as cloud computing and high-performance computing. The latter is already or increasingly available in most academic institutions around the world, and similar environments are available for general users thanks to the popularization of performant consumer computers. 

The narrowest bottleneck to make the full-utilization of computing power is the technical intricacies in parallel computing. Parallel computing requires understanding software configuration to distribute computation demands to multiple computing units (threads, cores, nodes, etc.). Although the user-friendly solutions are increasingly available, geospatial exposure assessment cases are very scarse. It is of utmost importance to improve useability of efficient way to encourage and expedite health effect analysis to accumulate evidence for environmental impact on health outcomes in relation to global human-environment changes.

In this study, we present spatial computation at scale (scomps) package. This package provides a succinct and efficient solution to parallelize spatial computation for exposure assessment in R, which is a popular tool which health communities use.

# Methods

Basic strategy is … divide and process… division rationale could be selected by users in three ways according to data availability and situations: multiple rasters, grids, and hierarchy.

Parallelization part is implemented in future framework. Divided sub-datasets are exported to each compute nodes/threads for further processing.

Grid processing requires a pre-generation of computational grids, where sub-datasets are extracted. Comm

We identified common types of geospatial overlay operations for exposure assessment and/or health effect analysis. Three types of operations are succinctly written in dedicated functions that are interoperable with the parallel processing functions in `chopin`.

## Prerequisites
We assume that we use two-dimensional base data structure, which means that three-dimensional structures are managed by attributes from the two-dimensional spatial structure.
The target design 

[Diagram: layout and function structures]


# Major usage


# Benchmark
## Case 1:

## Case 2:


## Case 3:


# Discussion
## Efficient implementation of subsetting

## Data-agnostic function runs.






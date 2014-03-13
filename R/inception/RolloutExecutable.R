#sort NetworkPlanner's "grid" settlements nodes based on a global priority scheme 
#while denoting each unique branch they're originating from 
#using an implementation of Prim's algorithm on the proposed.grid network.  

#Load custome functions written for phasing, etc
source('~/code/networkplanner.R/R/inception/CommonRolloutFunctions.R') 

#Set working directory to scenario of interest here
setwd("~/Dropbox/Myanmar_GIS/Modeling/Tests/644-Chin-HHDem240-13c:KWh-10kWDslMin-$1.15Dsl-SN10/")


##1.0 - Import DATA
#Import metrics.local for only grid-proposed nodes -> local.grid
#load metrics.local to associated settlement points with proposed grid data
local <- read.csv("metrics-local.csv", skip=1) #RUNTIME ~ 00:28 mins
local$Settlement.id <- rownames(local) #use generic row names for unique ID of each unique settlement point

proj4 <- read.csv("metrics-local.csv", nrows=1, header = FALSE)

proposed <- readShapeLines("networks-proposed.shp") #RUNTIME ~ 00:08 mins


#2. Use output of priortized.grid function as input to far-sighted optimized rollout algorithim 
#takes a shapefile (network) and csv (nodal descriptions and weights) 
#and suggests a sequential, phased roll-out of the system based on a greedy, one step ahead view
#***RUNTIME ~08:00***********
nearsighted_grid <- prioritized.grid.nearsighted(local, proposed, proj4)
##***************************

#Explicitly define greedy grid output as a dataframe
#Sometimes I need to explicitly call the fataframe for greedy.grid - arghhhh
if (length(nearsighted_grid)==2){
  print("Houston, we have a problem with our dataframe")
  nearsighted_grid  <- as.data.frame(nearsighted_grid[1])
}

#3. Function to determine downstream summations for nearsighted grid AKA Demands and Capacties 
nearsighted_grid_cumulatives <- downstream.sum.calculator(nearsighted_grid)

#4. Far Sighted function to improve near-sighted greedy grid
#**********Broken!*************
farsighted_grid <- far_sighted_rollout(nearsighted_grid_cumulatives)
#*****


#5. Assign Phase bins by distance threshold to each settlement
#But first, determine cummulative values

#How many total new connections?? 
new_grid_connections <- sum(farsighted_grid$Demand..household....Target.household.count)


#Develop cummulative sum of network length metric
farsighted_grid <- mutate(farsighted_grid, 
                          CumulativeNetworkExtent.m = cumsum(dist),
                          CumulativeHousesConnected.qty = cumsum(Demand..household....Target.household.count),
                          MVLinePerConnection = dist/Demand..household....Target.household.count,
                          TransformerCostPerConnection = System..grid....Transformer.cost/Demand..household....Target.household.count,
                          PercentOfNewGridConnections = CumulativeHousesConnected.qty/new_grid_connections)

#That lets us develop Phase bins
farsighted_grid$PhaseByMVQuintile <- NA
total_phases <- 5
total_MV_dist <- sum(farsighted_grid$dist)

for (j in 1:total_phases){
  
  lower_cutoff <- (j-1)/total_phases*total_MV_dist
  upper_cutoff <- j/total_phases*total_MV_dist
  
  farsighted_grid$PhaseByMVQuintile[which((farsighted_grid$CumulativeNetworkExtent.m >= lower_cutoff) &
                                (farsighted_grid$CumulativeNetworkExtent.m <= upper_cutoff))] <- j
  
}

#output csv for analysess 
write.csv(farsighted_grid, "metrics-local-grid-only-rollout_sequence.csv", row.names=F)

#PAss through ALL metric local values
shared_column_names <- colnames(local)[which(colnames(local) %in% colnames(farsighted_grid))]
farsighted_grid_full <- merge(local, farsighted_grid, by = shared_column_names, all.x=T, all.y=T)

#Order output by Phase
farsighted_grid_full <- arrange(farsighted_grid_full, PhaseByMVQuintile)
#Correct for NA Phases in Mini and Off grid systems
farsighted_grid_full$PhaseByMVQuintile[which(farsighted_grid_full$Metric...System == 'mini-grid')] <- 'MiniGrid Systems'
farsighted_grid_full$PhaseByMVQuintile[which(farsighted_grid_full$Metric...System == 'off-grid')] <- 'OffGrid Systems'
farsighted_grid_full$PhaseByMVQuintile[which(farsighted_grid_full$Metric...System == 'unelectrified')] <- 'Pre-Electrified'

#Output it all with useful phases
write.csv(farsighted_grid_full, "metrics-local-rollout_sequence-FULL.csv", row.names=F)

# #Output a new shapefile with all the attirbute data of interest
#remove multiple nodes on line segments
metrics_local_with_sequence <- (farsighted_grid[which(!(duplicated(farsighted_grid$id))),])
proposed_with_rollout <- merge(proposed, metrics_local_with_sequence, by.x = "FID", by.y = "id")
writeLinesShape(proposed_with_rollout, "networks-proposed-with-rollout.shp")

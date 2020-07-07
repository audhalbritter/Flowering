#**********************************************************************
library(drake)

r_make(source = "R/FloweringDrakePlan.R")
loadd()
failed()

#view dependency graph
r_vis_drake_graph(source = "R/AudsDrakePlan.R", targets_only = TRUE)

#**********************************************************************

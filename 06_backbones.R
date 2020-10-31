#### Description ####
## This code is an adaptation of a script written by Zachary Neal, Rachel Domagalski, and Bruce Sagan (Michigan State University)
## For more details about backbone, visit http://www.zacharyneal.com/backbone
## Questions about backbone? Please contact Zachary Neal at zpneal@msu.edu

#### Helper function for visualizations ####
viz.helper <- function(B) {
  #Extract just the positive edges
  B[B<0] <- 0

  #Convert it into an igraph object
  iB <- graph_from_adjacency_matrix(B, mode = "undirected", diag = FALSE)  #Create igraph object

  #Add each senator's political party as a node attribute
  # V(iB)$topic <- str_sub(gsub(".*\\[(.+)-.*", "\\1",rownames(B)),-2,-2)

  #Define node and edge colors based on political parties
  # V(iB)$color[V(iB)$topic == ""] <- rgb(1,0,0)
  # V(iB)$color[V(iB)$topic == ""] <- rgb(0,0,1)
  # E(iB)$color <- rgb(.5,0,.5)
  # polymers <- V(iB)[V(iB)$topic == ""]
  # polymers.edges <- E(iB)[polymers%--%polymers]
  # E(iB)[polymers.edges]$color <- rgb(1,0,0)
  # catalysis <- V(iB)[V(iB)$topic == ""]
  # catalysis.edges <- E(iB)[catalysis%--%catalysis]
  # E(iB)[catalysis.edges]$color <- rgb(0,0,1)

  #Return the compiled igraph object, ready for visualizing
  return(iB)
}



#### Install and Load packages ####
#install.packages("backbone")
#install.packages("igraph)
#install.packages("stringr")
set.seed(19)
library(backbone)
library(igraph)  #Used for visualization
library(stringr) #Used for visualization

#### Load example dataset ####

# load adjacency matrices built by script 02
load("data-net/unweighted-2mode.rda")

#### Examine dataset ####
dim(a2017)
a2017[1:5, 1:5]

#### Examine bipartite projection ####
G <- a2017%*%t(a2017)
dim(G)
G[1:5, 1:2]
G["PRINCE, AMANIAMPONG", "ANA M, FERREIRA"]
G["FRANCOIS, JEROME", "ERIC, MONFLIER"]
G["PEDRO, ROCHA FILHO", "MARINA, OLIVEIRA"]
G["PEDRO, ROCHA FILHO", "CARLOS, FERNANDEZ"]

#### Universal Threshold Set at 0 ####
## Extract
universal_bb1 <- universal(a2017, upper = 0, bipartite = TRUE)
universal_bb1$backbone[1:5, 1:2]

## Visualize backbone
universal_bb1 <- universal_bb1$backbone
universal_bb1_viz <- viz.helper(universal_bb1)
plot(universal_bb1_viz,
     vertex.color = V(universal_bb1_viz)$color,
     vertex.size = 3,
     vertex.frame.color = NA,
     vertex.label = NA,
     edge.color = E(universal_bb1_viz)$color,
     layout = layout_with_fr)

plot.new()

#### Universal Threshold Set at mean +/- sd ####
## Extract
universal_bb2 <- universal(a2017, upper = function(x) mean(x)+sd(x), lower = function(x) mean(x)-sd(x), bipartite = TRUE)

## Visualize backbone
universal_bb2 <- universal_bb2$backbone
universal_bb2_viz <- viz.helper(universal_bb2)
plot(universal_bb2_viz,
     vertex.color = V(universal_bb2_viz)$color,
     vertex.size = 3,
     vertex.frame.color = NA,
     vertex.label = NA,
     edge.color = E(universal_bb2_viz)$color,
     layout = layout_with_fr)

#### Hypergeometric Filter ####
## Extract
hyperg_probs <- hyperg(a2017)
hyperg_bb <- backbone.extract(hyperg_probs, alpha = .01)

## Visualize backbone
hyperg_bb_viz <- viz.helper(hyperg_bb)
plot(hyperg_bb_viz,
     vertex.color = V(hyperg_bb_viz)$color,
     vertex.size = 3,
     vertex.frame.color = "black",
     vertex.label = NA,
     edge.color = E(hyperg_bb_viz)$color,
     layout = layout_with_fr)

#### Stochastic Degree Sequence Model (SDSM) ####
## Extract
sdsm <- sdsm(a2017, model = "polytope")
sdsm_bb <- backbone.extract(sdsm, alpha = .01, narrative = TRUE)

## Visualize backbone
sdsm_bb_viz <- viz.helper(sdsm_bb)
plot(sdsm_bb_viz,
     vertex.color = V(sdsm_bb_viz)$color,
     vertex.size = 3,
     vertex.frame.color = NA,
     vertex.label = NA,
     edge.color = E(sdsm_bb_viz)$color,
     layout = layout_with_fr)

#### Fixed Degree Sequence Model (FDSM) ####
## Extract
fdsm <- fdsm(a2017, trials = 1000, dyad = c("PEDRO, ROCHA FILHO", "MARINA, OLIVEIRA"), progress = TRUE)
fdsm_bb <- backbone.extract(fdsm, signed = TRUE, alpha = 0.01)

## Visualize histogram
hist(fdsm$dyad_values,
     freq = FALSE,
     xlab = "Expected Number of Co-attendance under FDSM",
     main = NA,
     col = "gray",
     ylim=c(0,10))
lines(density(fdsm$dyad_values))

## Visualize backbone
fdsm_bb_viz <- viz.helper(fdsm_bb)
plot(fdsm_bb_viz,
     vertex.color = V(fdsm_bb_viz)$color,
     vertex.size = 3,
     vertex.frame.color = NA,
     vertex.label = NA,
     edge.color = E(fdsm_bb_viz)$color,
     layout = layout_with_gem)

g <- graph_from_adjacency_matrix(fdsm_bb, mode = "undirected", diag = FALSE)
components(g)
V(g)$degree <- degree(g)
gtop <- induced.subgraph(g,v = V(g)$degree > 0)

plot(gtop,
     vertex.color = V(gtop)$color,
     vertex.size = 3,
     vertex.frame.color = NA,
     vertex.label = word(str_to_lower(V(gtop)$name), -1),
     vertex.label.cex = 0.6,
     edge.color = E(fdsm_bb_viz)$color,
     layout = layout_with_fr)

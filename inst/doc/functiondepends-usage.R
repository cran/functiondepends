## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%",
  message = FALSE,
  warning = FALSE,
  fig.width = 10, 
  fig.height = 5,
  fig.fullwidth = TRUE
)

## ----setup--------------------------------------------------------------------
library(functiondepends)

## ---- include = FALSE---------------------------------------------------------
envir <- functiondepends:::envir_example
functions <- functiondepends:::functions_example

## ---- eval = FALSE------------------------------------------------------------
#  # Create environment for loaded functions
#  envir <- new.env()
#  # Search recursively source files
#  functions <- find_functions(".", envir = envir, recursive = TRUE)

## -----------------------------------------------------------------------------
functions

## ----dependency---------------------------------------------------------------
dependency <- find_dependencies("find_functions", envir = envir, in_envir = TRUE)
dependency

## ----functions_in_path--------------------------------------------------------
library(ggplot2)
library(dplyr)

dependency <- find_dependencies("find_functions", envir = envir, in_envir = FALSE)
dependency %>% 
  slice_max(SourceRep, n = 10) %>% 
  mutate(Source = reorder(Source, SourceRep)) %>% 
  ggplot(aes(x = Source, y = SourceRep, fill = SourceNamespace)) +
  geom_col() +
  coord_flip() +
  labs(caption = "Top 10 most repeated calls in 'find_functions'.")

## ----function_in_path_info----------------------------------------------------
library(tidyr)

dependency <- find_dependencies("find_functions", envir = envir, in_envir = FALSE, add_info = TRUE)
dependency %>% 
  filter(SourceNamespace == "stats") %>% 
  select(Source, SourcePosition, SourceContext) %>% 
  unnest(c(SourcePosition, SourceContext)) 

## ----target_degree------------------------------------------------------------
dependency <- find_dependencies(unique(functions$Function), envir = envir, in_envir = FALSE)
dependency %>% 
  distinct(Target, TargetInDegree) %>%
  mutate(Target = reorder(Target, TargetInDegree)) %>%
  ggplot(aes(x = Target, y = TargetInDegree)) +
  geom_col() +
  coord_flip() + 
  labs(caption = "Functions with most function calls.")

## ----namespace_count----------------------------------------------------------
dependency <- find_dependencies(unique(functions$Function), envir = envir, in_envir = FALSE)
dependency %>% 
  group_by(SourceNamespace) %>% 
  tally(name = "Count") %>% 
  slice_max(Count, n = 10) %>% 
  mutate(SourceNamespace = reorder(SourceNamespace, Count)) %>% 
  ggplot(aes(x = SourceNamespace, y = Count)) +
  geom_col() +
  coord_flip() +
  labs(caption = "Top 10 used namespaces.")

## ----degree-------------------------------------------------------------------
dependency <- find_dependencies(unique(functions$Function), envir = envir, in_envir = TRUE)
dependency %>% 
  distinct(Target, TargetInDegree) %>% 
  arrange(-TargetInDegree)

## ----network------------------------------------------------------------------
library(igraph)

edges <- dependency %>% 
  select(Source, Target) %>% 
  filter(!is.na(.))

vertices <- unique(c(dependency$Source, dependency$Target))
vertices <- vertices[!is.na(vertices)]

g <- graph_from_data_frame(d = edges, vertices = vertices)
deg <- degree(g, mode = "in")
V(g)$size <- deg * 10 + 5
V(g)$label.cex <- (degree(g, mode = "in", normalized = TRUE) + 1)

plot(
  g,
  vertex.color = "grey",
  edge.color = "grey",
  edge.arrow.size = .4,
  main = "Functions dependency graph"
)

## ----network_full-------------------------------------------------------------
dependency <- find_dependencies(unique(functions$Function), envir = envir, in_envir = FALSE)
edges <- dependency %>% 
  select(Source, Target) %>% 
  filter(!is.na(.))
vertices <- unique(c(edges$Source, edges$Target))

g <- graph_from_data_frame(edges)
deg <- degree(g, mode = "in")
V(g)$size <- deg
V(g)$label.cex <- (degree(g, mode = "in", normalized = TRUE) + 1) / 1.8

plot(
  g,
  vertex.color = "grey",
  edge.color = "grey",
  edge.arrow.size = .4,
  main = "Full functions dependency graph"
)


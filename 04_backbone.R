## ISGC NETCONF SCRIPT 4 - SUNBELT 2020 - 04_backbone.R
## 2020-07-14 Bastille Day
## M. Maisonobe & F. Briatte

# --------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------
# AFSP equivalent: https://github.com/briatte/congres-afsp/blob/master/04_backbone.r

# Domagalski, R., Neal, Z. P., & Sagan, B. (2019). backbone: An R package for backbone extraction of weighted graphs. arXiv:1912.12779v1.
# https://arxiv.org/pdf/1912.12779.pdf
#
# Neal, Z. P. (2014). The backbone of bipartite networks: Inferring relationships from co-authorship, co-sponsorship, co-attendance and other co-behaviors. Social Networks, 39, 84 – 97. https://doi.org/10.1016/j.socnet.2014.06.001


library(tidyverse)
library(igraph)
library(ggraph)
library(graphlayouts)
library(backbone)

# load adjacency matrices built by script 02
load("data-net/unweighted-2mode.rda")

for (i in ls(pattern = "^a\\d{4}")) {

  cat("\nEdge weights for year", str_remove(i, "\\D"), ":\n")

  # two-mode adj. matrix
  A <- get(i)

  # one-mode projection
  G <- A %*% t(A)
#  diag(G) <- 0 # not necessary according to Neal et al. replicationcode.R (https://osf.io/r9gvh/)
  print(table(G))

  # backbone from one-mode and two-mode projections - universal threshold method
  bb <- backbone::universal(G, upper = 1)
  bb2 <- backbone::universal(A, upper = 1, bipartite = TRUE)
  stopifnot(all.equal(bb$backbone, bb2$backbone)) # what it's for?

  assign(str_replace(i, "a", "bb"), bb)
  print(bb$summary)

  # backbone from two-mode projections - hypergeometric method
  ## Extract
  hyperg_probs <- hyperg(G)
  hyperg_bb <- backbone.extract(hyperg_probs, alpha = .01)

  # assign(str_replace(i, "a", "bb"), hyperg_bb)
  # print(hyperg_bb$summary)

  # backbone from two-mode projections - sdsm method
  ## Extract
  sdsm <- sdsm(G, model = "polytope")
  sdsm_bb <- backbone.extract(sdsm, alpha = .01, narrative = F)

  # assign(str_replace(i, "a", "bb"), sdsm_bb)
  # print(sdsm_bb$summary)

  # backbone from two-mode projections - sdsm method
  ## Extract
  sdsm <- sdsm(G, model = "polytope")
  sdsm_bb <- backbone.extract(sdsm, alpha = .01, narrative = F)

  # assign(str_replace(i, "a", "bb"), sdsm_bb)
  # print(sdsm_bb$summary)

  # backbone from two-mode projections - fdsm method
  ## Extract
  fdsm <- fdsm(G, trials = 1000, dyad = c("PRINCE, AMANIAMPONG", "ANA M, FERREIRA"), progress = TRUE)
  fdsm_bb <- backbone.extract(fdsm, signed = TRUE, alpha = 0.01)

  # assign(str_replace(i, "a", "bb"), fdsm_bb)
  # print(fdsm_bb$summary)

  ## Visualize histogram
  hist(fdsm$dyad_values,
       freq = FALSE,
       xlab = "Expected Number of Co-attendance under FDSM",
       main = NA,
       col = "gray",
       ylim=c(0,.08))
  lines(density(fdsm$dyad_values))

  # get graph as tibble
  G_df <- igraph::graph_from_adjacency_matrix(G, diag = FALSE) %>%
    igraph::as_data_frame() %>%
    as_tibble()

  # get backbone as tibble
  bb_df <- bb$backbone %>%
    graph_from_adjacency_matrix(mode = "undirected", diag = FALSE) %>%
    igraph::as_data_frame() %>%
    as_tibble()

  # identify edges that are part of the backbone
  df <- bind_rows(
    semi_join(G_df, bb_df, by = c("from", "to")) %>%
      add_column(backbone = TRUE),
    anti_join(G_df, bb_df, by = c("from", "to")) %>%
      add_column(backbone = FALSE)
  )

  # count edges in the backbone
  bb_edges <- filter(df, backbone == 1)
  cat("\nBackbone has", nrow(bb_edges), "edges, ")

  # identify and count nodes connected to the backbone
  bb_nodes <- unique(c(bb_edges$from, bb_edges$to))
  cat("connecting", length(bb_nodes), "nodes\n")

  # normalized betweenness centrality
  bc <- bipartite::BC(A)

  # bc_panels <- bc$higher
  bc_participants <- bc$lower
  stopifnot(nrow(A) == length(bc_participants))

  bc_participants <- tibble::tibble(
    i = rownames(A),
    BC = bc_participants,
    backbone = i %in% bb_nodes
  )

  ggplot(
    bc_participants,
    aes(x = BC, fill = backbone, color = backbone)
  ) +
    geom_density(alpha = 1/2) +
    scale_x_log10() +
    labs(
      title = str_c(
        "Betweenness centrality in IGSC Meeting ",
        str_remove(i, "\\D")
      ),
      subtitle = "Showing only nodes for which BC > 0"
    )

  ggsave(
    str_c("plots/congres-isgc", str_remove(i, "\\D"), "-backbone-BC.png"),
    width = 8,
    height = 6
  )

  # build the final graph data frame
  n_df <- igraph::graph_from_data_frame(df, directed = FALSE)
  V(n_df)$backbone_node <- V(n_df)$name %in% bb_nodes

  # 'trim' the graph, keeping only the largest component
  n_components <- igraph::components(n_df)
  stopifnot(first(n_components$csize) == max(n_components$csize))
  n_df <- igraph::delete_vertices(n_df, which(n_components$membership != 1))

  cat(
    "Visualizing, removing",
    sum(n_components$membership != 1),
    "nodes not connected to main component\n"
  )

  # visualize with stress majorization
  g <- n_df %>%
    ggraph(layout = "stress", bbox = 20) +
    geom_edge_link(color = "grey75", alpha = 2/3) +
    geom_edge_link(aes(alpha = backbone), color = "tomato3", show.legend = FALSE) +
    geom_node_point(aes(alpha = backbone_node, color = backbone_node), show.legend = FALSE) +
    scale_edge_alpha_manual("", values = c("FALSE" = 0, "TRUE" = 1)) +
    scale_alpha_manual("", values = c("FALSE" = 2/3, "TRUE" = 1)) +
    scale_color_manual("", values = c("FALSE" = "black", "TRUE" = "tomato3")) +
    theme_graph(base_family = "Helvetica", base_size = 14) +
    theme(
      legend.text = element_text(size = rel(1)),
      # legend.position = "bottom",
      plot.caption = element_text(size = rel(.85), hjust = 0.5),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    labs(
      title = str_c("Backbone of ISGC Meeting ", str_remove(i, "\\D")),
      subtitle = str_c(
        "Backbone has",
        nrow(bb_edges),
        "edges, inter-connecting",
        length(bb_nodes),
        "nodes out of",
        n_distinct(c(df$from, df$to)), # because `df` has been 'trimmed'
        sep = " "
      ),
      caption = str_c(
        "Universal backbone at upper threshold 1 ",
        "(2+ panel co-attendances)\n",
        "Disconnected graph components omitted from figure"
      )
    )

  assign(str_replace(i, "a", "bbplot"), g)

  ggsave(
    str_c("plots/congres-isgc", str_remove(i, "\\D"), "-backbone.png"),
    g,
    width = 8,
    height = 9
  )

  cat("\n")

}

# kthxbye


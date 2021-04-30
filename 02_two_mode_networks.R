## ISGC NETCONF SCRIPT 2 - SUNBELT 2020 - 02_two_mode_networks.R
## 2020-07-14 Bastille Day
## M. Maisonobe & F. Briatte

# --------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------
# AFSP equivalent: https://github.com/briatte/congres-afsp/blob/master/02_two_mode_networks.r

library(dplyr)
library(tidyr) # for unite, separate
library(igraph)
library(ggraph)
library(graphlayouts)
library(tidyverse)


# [NOTE] we use a stress majorization layout because all graphs have several
#        components, and because `igraph` (1.2.5) fails to produce legible
#        visualizations of the 2017 & 2019 with layouts `fr` or `kk`

d <- read_tsv("data-net/edges-2015-2019.tsv")

# limiting ourselves to oral communications and flash communications panels
d <- d %>%
  filter(final_status %in% c("OC","FC", "FC - PC", "OC - PC"))

# ==============================================================================
# INCIDENCE MATRIX
# ==============================================================================

m <- matrix(0, nrow = n_distinct(d$i), ncol = n_distinct(d$j))

dim(m) # ~ 2,412  219
stopifnot(object.size(m) / 10^6 < 15) # ~ 11 MB, no need to sparsen it

rownames(m) <- unique(d$i)
colnames(m) <- unique(d$j)

for (i in colnames(m)) {
  m[ rownames(m) %in% d$i[ d$j == i ], i ] <- 1
}

rowSums(m) # number of panel attendances per person (includes self-loops)
colSums(m) # number of persons per panel

stopifnot(colSums(m) > 1) # # 2015 - 2019 --> issue: Erreur : colSums(m) > 1 ne sont pas TRUE

# I need to check which session includes only 1 person: apparently it is session 2015_30_1

# ==============================================================================
# SIMPLE INVERSE WEIGHTING
# ==============================================================================

# weight = 1 / (total number of panel participants)
   w <- apply(m, 2, function(x) { x / sum(x) }) # \in (0, 0.5] # for bipartite plots
#  w <- m # for backbones

# ==============================================================================
# BIPARTITE NETWORK PLOTS
# ==============================================================================

# l <- c("Panel", "Participant(e) de degré 1", "Participant(e) de degré 2+")
l <- c("Panel", "Participant with degree 1", "Participant with degree 2+")

y <- unique(str_sub(colnames(w), 1, 4))

for (i in rev(y)) {

  n <- w[, str_sub(colnames(w), 1, 4) == i ]
  n <- n[ rowSums(n) > 0, ]

  assign(str_c("a", i), n)

  n <- igraph::graph_from_incidence_matrix(n, weighted = TRUE) %>%
    igraph::as_data_frame(.) %>%
    mutate(year = str_sub(to, 1, 4)) %>%
    igraph::graph_from_data_frame(directed = FALSE)

  assign(str_c("g", i), n)

  E(n)$weight <- E(n)$weight / max(E(n)$weight)

  V(n)$type <- if_else(str_detect(V(n)$name, "^\\d{4}"), "Panel", "Participant(e)")
  V(n)$type <- if_else(V(n)$type == "Panel", "P0", if_else(degree(n) > 1, "P2", "P1"))

  cat("\nYear", i, ":", igraph::components(n)$no, "components\n")
  print(table(V(n)$type))

  cat("\nEdge weights:\n")
  print(summary(E(n)$weight))

  assign(paste("components", i, sep= "_"), igraph::components(n)$no)
  assign(paste("t", i, sep = "_"), table(V(n)$type))
  assign(paste("p", i, sep = "_"), sum(V(n)$type != "P0"))

  V(n)$size <- igraph::degree(n)
  V(n)$size <- if_else(V(n)$type == "P0", 1.5, V(n)$size)

  # stress majorization, with slightly smaller `bbox` (default = 30)
  ggraph(n, layout = "stress", bbox = 20) +
    geom_edge_link0(aes(alpha = weight), show.legend = FALSE) +
    geom_node_point(aes(size = size, shape = type, color = type), alpha = 2/3) +
    scale_shape_manual("", values = c("P0" = 15, "P1" = 19, "P2" = 19), labels = l) +
    scale_color_manual("", values = c("P0" = "grey35", "P1" = "steelblue3", "P2" = "tomato3"), labels = l) +
    guides(size = FALSE) +
    theme_graph(base_family = "Helvetica", base_size = 14) +
    theme(
      legend.text = element_text(size = rel(1)),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    labs(
      # title = str_c("Congrès ISGC ", i),
      title = str_c("ISGC Congress ", i),
      subtitle = str_c(
        sum(V(n)$type == "P0"), " panels, ",
        # sum(V(n)$type != "P0"), " participant(e)s"
        sum(V(n)$type != "P0"), " participants"
      )
    )

  ggsave(str_c("plots/congres-isgc", i, "-2mode.pdf"), width = 8, height = 9)
  ggsave(str_c("plots/congres-isgc", i, "-2mode.png"), width = 8, height = 9, dpi = 150)

}

 save(list = ls()[ str_detect(ls(), "^(a|g)\\d{4}") ], file = "data-net/2mode.rda")
# save(list = ls()[ str_detect(ls(), "^(a|g)\\d{4}") ], file = "data-net/unweighted-2mode.rda")  # for backbones

readr::write_rds(w, "data-net/incidence_matrix.rds")

t <- bind_rows(t_2015, t_2017, t_2019)
t_c <- rbind(components_2015, components_2017, components_2019)
t_p <- rbind(p_2015, p_2017, p_2019)
t <- bind_cols(year = seq(2015, 2019, by = 2), t, C = t_c, P = t_p)

readr::write_tsv(t, "data-net/table.tsv")

#Year 2015 : 9 components

'P0  P1  P2
 64 781  94 '

'Edge weights:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
0.03333 0.04348 0.05263 0.06426 0.07143 1.00000'

#Year 2017 : 8 components

'P0  P1  P2
 78 890 135'

'Edge weights:
  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
0.1613  0.2174  0.2941  0.3189  0.3571  1.0000'

#Year 2019 : 5 components

'P0  P1  P2
 77 837 145  '

'Edge weights:
    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
  0.1481  0.1905  0.2500  0.2573  0.3077  1.0000'

# kthxbye

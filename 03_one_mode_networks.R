## ISGC NETCONF SCRIPT 3 - SUNBELT 2020 - 03_one_mode_networks.R
## 2020-07-14 Bastille Day
## M. Maisonobe & F. Briatte

library(dplyr)
library(tidyr) # for unite, separate
library(igraph)
library(ggplot2)
library(tnet)
library(ggraph)
library(stringr)
library(graphlayouts)

# -----------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------
# AFSP equivalent: https://github.com/briatte/congres-afsp/blob/master/03_one_mode_networks.r

d <- read_tsv("data-net/edges-2015-2019.tsv")

# limiting ourselves to oral communications and flash communications panels
d <- d %>%
  filter(final_status %in% c("OC","FC", "FC - PC", "OC - PC"))

# l <- c("Participant(e) mono-panel", "Participant(e) multi-panels")
l <- c("Mono-panel participant", "Multi-panel participant")

for (y in unique(d$year)) {

  e <- filter(d, year == y)

  # make sure all panels have 1+ organiser(s) and 2+ participants <-- not applicable to ISGC data
 # w <- group_by(e, j) %>%
    #summarise(n_o = sum(role == "o"), n_p = sum(role != "o")) %>%
    #filter(n_p < 3 | n_o < 1 | n_o == n_p)

  #stopifnot(!nrow(w))

  e <- e %>%
    distinct(i, j) %>% # remove duplicates
    # numeric IDs for abstracts
    group_by(j) %>%
    mutate(panel_id = cur_group_id()) %>%
    ungroup() %>%
    group_by(i) %>%
    mutate(i_id = cur_group_id()) %>%
    ungroup()


  net <- select(e, e = i_id, p = panel_id)
  onemode <- projecting_tm(net, method = "Newman")

  # transform the result into an igraph object

  g <- graph_from_data_frame(cbind(onemode$i,onemode$j), directed = FALSE)
  E(g)$weight <- as.double(onemode$w)

  # remove multiple lines
  n <- simplify(g) #(g, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = "first") , edge.attr.comb = "sum"

  E(n)$weight <- E(n)$weight / max(E(n)$weight)

  V(n)$size <- igraph::degree(n)

  # tibble::tibble(name = V(n)$name, degree = V(n)$size) %>%
  #   arrange(-degree) %>%
  #   print

  e <- e %>%
    group_by(i_id) %>%
    summarise(n_p = n_distinct(panel_id))

  w <- e$n_p
  names(w) <- e$i_id

  V(n)$color <- as.integer(w[ V(n)$name ])
  V(n)$color <- if_else(V(n)$color == 1, "P1", "P2+")

  cat("\nYear", y, ":", igraph::components(n)$no, "components\n")
  print(table(V(n)$color))

  ggraph(n, layout = "stress", bb = 18) +
    geom_edge_link(aes(alpha = weight), show.legend = FALSE) +
    geom_node_point(aes(size = size, color = color), alpha = 2/3) +
    scale_color_manual("", values = c("P1" = "steelblue3", "P2+" = "tomato3"), labels = l) +
    guides(size = FALSE) +
    theme_graph(base_family = "Helvetica", base_size = 14) +
    theme(
      legend.text = element_text(size = rel(1)),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    labs(
      # title = str_c("Congrès ISGC ", y),
      title = str_c("ISGC Congress ", y),
      # subtitle = str_c(
      #   sum(V(n)$color == "P1"), " participant(e)s, ",
      #   sum(V(n)$color == "P2+"), " multi-panels"
      # )
      subtitle = str_c(
        sum(V(n)$color == "P1"), " participants, ",
        sum(V(n)$color == "P2+"), " multi-panels"
      )
    )

  ggsave(str_c("plots/congres-isgc", y, "-1mode.pdf"), width = 8, height = 9)
  ggsave(str_c("plots/congres-isgc", y, "-1mode.png"), width = 8, height = 9, dpi = 150)

}

# Year 2019 : 5 components

'P1 P2+
  837 145'

# Year 2017 : 8 components

'P1 P2+
  890 135'

# Year 2015 : 8 components

'P1 P2+
  780  94'

# kthxbye

suppressPackageStartupMessages({
  require(data.table)
  require(ggplot2)
  require(ggrepel)
  require(cowplot)
})

.args <- c("intermediate/digest.csv", "intermediate", "intermediate/sharedef.rda", "some.pdf")
.args <- commandArgs(trailingOnly = T)

digest <- fread(.args[1], col.names = c("date","vertex","module"))
dailies <- list.files(.args[2], pattern = "_mat\\.csv$", full.names = T)
load(.args[3])

dir.mod.byi <- function(al, cs) {
  al[is.na(al)] <- 0
  outd <- rowSums(al)
  ind <- colSums(al)
  m <- sum(al)
  # Q = 1/m sum i,j [Aij - outd_i * ind_j / m] delta(cs_i, cs_j)
  # dd = outer product outd_i * ind_j
  dd <- outd %o% ind
  # cc = outer "product" cs * cs for cs_i == cs_j
  cc <- outer(cs, cs, "==")
  # Q = 1/m sum((Aij - dd/m)*cc)
  rowSums((al - dd/m)*cc)/m
}

# for each date
#  get the corresponding info from digest + appropriate daily
#  extract the from->to edge pairs + weights
#  join to identify the from->to module pairs
#  summarize to the from->to pair total weights + internal weights
set.seed(1234)
all.modularity.ts <- rbindlist(lapply(dailies, function(fn) {
  datekey <- gsub("^.+/([0-9\\-]+)_mat\\.csv$", "\\1", fn)
  subdig <- digest[date == datekey][order(vertex)]
  al <- as.matrix(fread(fn, drop=1:2))
  al[is.na(al)] <- 0
  
  #  alters <- colnames(al)[-wuhan]
  #  outweight <- sum(alters[wuhan, ])
  
  cs <- subdig[,module]
  vs <- dir.mod.byi(al, cs)
  
  wuhan <- subdig[,module[which(vertex == locids["wuhan"])]]
  beijing <- subdig[,module[which(vertex == locids["beijing"])]]
  shanghai <- subdig[,module[which(vertex == locids["shanghai"])]]
  gzhou_szhen <- subdig[,module[which(vertex == locids["gzhou_szhen"])]]
  
  res <- data.table(community = sort(unique(cs)), date = as.Date(datekey))
  res[, Q:= sum(vs[which(cs == community)]), by=community]
  res[, special := "other" ]
  res[community == beijing, special := "beijing" ]
  res[community == shanghai, special := "shanghai" ]
  res[community == gzhou_szhen, special := "gzhou_szhen" ]
  res[community == wuhan, special := "wuhan" ]
  
}))

# this *really* suggests eliminating at least the one-day gzhou_szhen community
singleday_comms <- all.modularity.ts[,if (.N==1) .SD, by=community]



p <- ggplot(all.modularity.ts) +
  aes(date, Q, group = factor(community), color=special) +
  geom_line() +
  geom_point(data = singleday_comms) +
  scale_x_date() +
  scale_y_continuous("Modularity by Community, Q") +
  scale_color_manual("Notable Communities", labels = titles, values = comm_colors) +
  theme_minimal() +
  theme(
    legend.position = c(.1, 1),
    legend.justification = c(0, 1)
  )

# logit <- function(x) {
#   vals <- log(x/(1-x))
#   (vals-mean(vals))/sd(vals)
# }

# meas.factor <- function(x) factor(x, levels = c("modularity", "inmod", "outmod"), ordered = T)

mts <- melt(modularity.ts, id.vars = "date", variable.factor = F)
mts[,
    wd := wday(date)
    ]#[, sbst := "all" ]
#[#, measure := meas.factor(variable)]

#mts[variable != "modularity", sbst := "some"]

#mts[sbst == "some", max(value)]

# bounds.dt <- data.table(
#   date = as.Date("2020-01-01"),
#   sbst = rep(c("all", "some"), each=2),
#   variable = NA,
#   value = c(0.5, 1.0, 0, 0.5)
# )

# mts[, lv := logit(value), by=variable]
metsp <- ggplot(mts) + aes(date, value, color = gsub("^in","", variable)) +
  # facet_grid(sbst ~ ., scales = "free_y", labeller = labeller(sbst = c(
  #   # wuhan_outweight = "Wuhan Outflow",
  #   # wuhan_mod = "Wuhan Module Outflow",
  #   all = "All Communities",
  #   some = "Communitiy Subsets"
  #   #,
  #   # wuhan = "Wuhan Q",
  #   # notwuhan = "Not-Wuhan Q",
  #   #outmod = "All Other Modules"
  # ))) +
  geom_rect(
    aes(ymin=-Inf, ymax=Inf, xmin=date-0.5, xmax=date+0.5),
    data = function(dt) dt[variable=="modularity"][between(date, as.Date("2020-01-01"), as.Date("2020-01-23"), incbounds = F)][!(wd %in% c(1, 7))],
    fill = "lightgrey", alpha = 0.25, inherit.aes = F
  ) +
  geom_rect(
    aes(ymin=-Inf, ymax=Inf, xmin=date, xmax=date+0.05, alpha=lph),
    data = function(dt) dt[variable=="modularity"][
      date == as.Date("2020-01-23"),
      .(
        date = date - 0.5 + seq(0,1.95,by=0.05),
        lph = seq(.01, 1, along.with = seq(0,1.95,by=0.05))
      )#, by=sbst
      ],
    fill = "firebrick", inherit.aes = F, show.legend = F
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  #  geom_blank(data = bounds.dt) +
  geom_text(
    aes(label=vw),
    data.table(
      date=as.Date(c("2020-01-06", "2020-01-17", "2020-01-27", "2020-02-05")),
      vw=LETTERS[1:4],
      value=0.8, variable = "modularity"
    ),
    show.legend = F
  ) +
  geom_line(show.legend = F) +
  geom_label_repel(
    aes(label=c(titles, modularity="All", other="Other")[gsub("^in","",variable)]),
    data=function(dt) dt[date == max(date), .(date, variable, value)],
    label.size = NA, label.padding = 0, segment.size = 0,
    show.legend = F, alpha = 0.8,
    seed = 1234
  ) +
  geom_text_repel(
    aes(label=c(titles, modularity="All", other="Other")[gsub("^in","",variable)]),
    data=function(dt) dt[date == max(date), .(date, variable, value)],
    show.legend = F, alpha = 1, segment.size = 0,
    seed = 1234
  ) +
  #geom_vline(xintercept = as.Date("2020-01-23"), color = "firebrick") +
  theme_minimal() + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  scale_x_date(expand = c(0,0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values = c(modularity="black", other="lightgrey", comm_colors)) +
  # scale_fill_manual(values = c(white="white")) +
  ggtitle("Q, Modularity (for Weighted, Directed Graph)")

save_plot("example.png", metsp, nrow = mts[,length(unique(variable))], base_width = 10, base_height = 2)

module.weights <- rbindlist(lapply(dailies, function(fn) {
  datekey <- gsub("^.+/([0-9\\-]+)_mat\\.csv$", "\\1", fn)
  daily <- melt(
    fread(fn, drop = 1),
    id.vars = "from", variable.name = "to", na.rm = T,
    variable.factor = F
  )[, to := as.integer(to) ]
  
  slice <- digest[date == datekey, .(module), keyby=vertex]
  wuhan <- slice[vertex == 420100, module+1]
  beijing <- slice[vertex == 110100, module+1]
  shanghai <- slice[vertex == 310100, module+1]
  guangzhou_shenzhen <- slice[vertex %in% c(440100,440300), unique(module+1)]
  if (length(guangzhou_shenzhen) > 1) warning("assumption violated")
  
  daily[slice, on=c(from="vertex"), from.module := module ]
  daily[slice, on=c(to="vertex"), to.module := module ]
  res <- daily[, .(
    weight = sum(value),
    date = datekey
  ), by=.(
    from=from.module+1, to=to.module+1
  )]
  res[, special := NA_character_ ]
  res[(from == to) & (from == wuhan), special := "wuhan"]
  res[(from == to) & (from == beijing), special := "beijing"]
  res[(from == to) & (from == shanghai), special := "shanghai"]
  res[(from == to) & (from == guangzhou_shenzhen), special := "guangzhou_shenzhen"]
  res
}))

self.weights <- module.weights[from == to, .(weight, module = from, date, special)]

allmods <- self.weights[,unique(module)]

dts <- as.Date(c("2020-01-06", "2020-01-17", "2020-01-27", "2020-02-05")) #module.weights[order(date)][date %in% as.Date(c("2020-01-06", "2020-01-17", "2020-01-27", "2020-02-05")), unique(date)]

lapply(dts, function(datekey) {
  el <- module.weights[date == datekey & (from != to), .(from, to, weight)][order(from, to)]
  vl <- self.weights[date == datekey][order(module), .(id = module, weight, special)]
  discardvl <- data.table(id = setdiff(allmods, vl$id), weight = 0, special = NA_character_)
  combovl <- rbind(discardvl, vl)[order(id)]
  ig <- graph_from_data_frame(el, vertices = combovl)
  V(ig)$size <- combovl$weight
  V(ig)$color <- "darkgrey"
  V(ig)[!is.na(special) & (special == "wuhan")]$color <- "firebrick"
  V(ig)[!is.na(special) & (special == "beijing")]$color <- "dodgerblue"
  V(ig)[!is.na(special) & (special == "shanghai")]$color <- "deepskyblue"
  V(ig)[!is.na(special) & (special == "guangzhou_shenzhen")]$color <- "cornflowerblue"
  V(ig)$frame.color <- NA
  V(ig)$label <- NA
  #V(ig)$label.cex <- sqrt(V(ig)$size/15)
  ly <- layout_with_dh(ig, coords = ly)
  subig <- induced_subgraph(ig, vids = vl$id)
  subly <- layout_with_dh(subig, ly[vl$id,])
  E(subig)$width = E(subig)$weight
  E(subig)$color <- "lightgrey"
  E(subig)[.from(V(subig)[!is.na(special) & (special == "wuhan")])]$color <- "firebrick"
  ly[vl$id,] <- subly
  png(
    sprintf("%s_mods.png", datekey),
    width = 5, height = 5, units = "in", res = 300,
    bg = "white"
  )
  plot(subig,
       edge.arrow.size = 0.1,
       edge.curved = T,
       layout = subly
  )
  dev.off()
})

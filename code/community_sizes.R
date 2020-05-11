#' community sizes for SI fig
suppressPackageStartupMessages({
  require(data.table)
  require(ggplot2)
  require(cowplot)
})

.args <- c("intermediate/digest.csv", "intermediate/sharedef.rda", "other.png")
.args <- commandArgs(trailingOnly = TRUE)

dig <- fread(.args[1], col.names = c("date","vertex","community"), colClasses = c("Date","integer","integer"))
load(.args[2])

comm_count.dt <- dig[,.(num=length(unique(community))),keyby=date]

#' works only because wuhan id < gzhou_szhen id, and that's the only
#' overlap
module.key <- dig[vertex %in% sort(locids),.(
  community, special = names(sort(locids)) 
), keyby=date][order(date),.(special=special[1]),by=.(date,community)]

#' all locales
locale_count.dt <- dig[, .N, keyby=.(date, community)]
locale_count.dt[module.key, special := special, on=.(date, community)]
locale_count.dt[is.na(special), special := "other" ]

pcount <- ggplot(comm_count.dt) + aes(date, num) +
  geom_line() + geom_point() +
  scale_x_date() +
  scale_y_continuous("Number of Communities", expand = c(0,0)) +
  coord_cartesian(clip = "off") +
  theme_minimal() + theme(
    axis.title.x = element_blank()
  )

save_plot(tail(.args, 1), pcount, base_asp = 3, base_width = 6)

locales.dt <- locale_count.dt[, .(N=sum(N)), keyby=.(special, date)]

alldates <- locales.dt[, as.data.table(expand.grid(
  date=as.Date(min(date):max(date), "1970-01-01"),
  special=unique(special)
))]

alldates[locales.dt, N := N, on=.(date, special)]

alldates[, special := factor(special, levels = c("wuhan", "gzhou_szhen", "beijing", "shanghai", "other"), ordered = T)]

maxn <- alldates[, sum(N, na.rm = T), by=date][, unique(V1)]

p <- ggplot(alldates) +
  aes(date, N/maxn, fill=special) +
  geom_col(position = "stack", width = 1) +
  geom_text(
    aes(label=titles[as.character(special)], fill = NULL),
    data=function(dt) dt[
      date == min(date)
    ][rev(order(special)), .(
      date, special, N=cumsum(N)-0.5*diff(c(0.5*sum(N, na.rm = T), cumsum(N)))
    )],
    hjust = "inward", fontface = "bold"
  ) +
  scale_x_date(expand = c(0,0)) +
  scale_y_continuous("Fraction of Locales\nBy Community", expand = c(0,0)) +
  scale_fill_manual(guide = "none", values = comm_colors) +
  coord_cartesian(ylim=c(0.5, 1)) +
  theme_minimal() + theme(
    axis.title.x = element_blank(),
    panel.spacing.y = unit(1, "line"),
    strip.text = element_blank(),
    plot.margin = margin(t = 1, r = 1, unit="line")
  )

save_plot(gsub("\\.","_1.", tail(.args, 1)), p, base_asp = 3, base_width = 6)

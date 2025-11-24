# number of survey responses among all participants by month
fname_plot = sprintf("timeline_numsurvey_genpop_spg_%s.pdf", plot_outputdate)

pdf(file = fname_plot, width = plot_width, height = plot_height)
YMAX = 3000
YLAB = 'Number of survey responses\nper month'

fun_plot_cnt_timeline_blank(YMAX = YMAX, YLAB = YLAB)
COLS_genpop = 'darkseagreen4';
COLS_spg = 'darkorange'

for(i in 1:28)
{
  # genpop only
  grid.polygon(x = c(i-0.5, i+0.5, i+0.5, i-0.5),
               y = c(0,0, t_month_genpop$n[i], t_month_genpop$n[i]),
               default.units = 'native', gp = gpar(col='black', fill = COLS_genpop))
  # spg = difference between genpop and allpart
  grid.polygon(x = c(i-0.5, i+0.5, i+0.5, i-0.5),
               y = c(t_month_genpop$n[i], t_month_genpop$n[i], t_month_allpart$n[i], t_month_allpart$n[i]),
               default.units = 'native', gp = gpar(col='black', fill = COLS_spg))
}

# legend
leg_vp_npc = viewport(x = 0.75, y = 0.775, width = 0.25, height = 0.25, just = c("left", "bottom")) # location of the legend
pushViewport(leg_vp_npc)
grid.draw(rectGrob()) # box of the legend
grid.legend(labels = c("General population", "High-contact groups"),
	pch = c(22, 22),
	gp = gpar(col='black', fill=c(COLS_genpop, COLS_spg), cex = 0.85),
	vgap = unit(0.25, "lines")
)

if(!any(names(dev.cur()) %in% c("windows", "null device"))){ dev.off()}

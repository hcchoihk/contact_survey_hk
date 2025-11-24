### vaccination rate in the population

# load data of vaccination rate in the population by age and sex
load('data/data_govtstat.RData')

date_plot_start = as.Date("2021-09-01", format="%Y-%m-%d")
date_plot_end = as.Date("2024-01-01", format="%Y-%m-%d")


AgeGp_plot = c("0-11", "12-19", "20-59", "60-79", "80 and above")
AgeGp_plot_num = length(AgeGp_plot);
col_plot_vec = c("red", "gold", "steelblue", "darkgreen", "purple")


fname_plot = sprintf("vaccrate_bydate_%s.pdf", update_outputdate())
pdf(fname_plot, width=plot_width, height=plot_height)


YMAX = 110;
YLAB = "Cumulative vaccination rate\nin the general population (%)";

fun_plot_cnt_timeline_blank(YMAX = YMAX, YLAB = YLAB)


xsex = "T";
for (iAgegp_plot in 1:AgeGp_plot_num){
	xAgegp = AgeGp_plot[iAgegp_plot];
	xcol_plot = col_plot_vec[iAgegp_plot];
	
	d0_TT = data_govtstat_vaccRate[[xAgegp, xsex]];
	
	irow_pick = with(d0_TT, which(date_plot_start<=Date & Date<=date_plot_end))
	d0_TT_pick = d0_TT[irow_pick, , drop=FALSE]
	
	d0_TT_pick$date_plot = sapply(d0_TT_pick$Date, function(xdate) fun_date_timeline(date=xdate)-0.5)
	with(d0_TT_pick, grid.lines(x=date_plot, y=percent_cumVacc, default.units = 'native', gp=gpar(col=xcol_plot, lwd=2.25)))
}

# legend
legend_set = data.frame(x = 0.825, y = 0.1, width = 0.15, height = 0.4)
leg_vp_npc = with(legend_set, viewport(x = x, y = y, width = width, height = height, just = c("left", "bottom"))) # location of the legend
# pushViewport(leg_vp_npc)
# grid.draw(rectGrob()) # box of the legend
grid.legend(labels = c("Age groups", AgeGp_plot),
	pch = NA,
	gp = gpar(col=c(NA, col_plot_vec), cex = 0.75, lwd=1.5),
	vgap = unit(0.025, "lines"), vp=leg_vp_npc
)
legend_set_box = within(legend_set, {
	xexpand = 0.025;
	yexpand = 0.075;
	x = x - xexpand;
	y = y - yexpand;
	width = width + 2*xexpand;
	height = height + 2*yexpand;
})
with(legend_set_box, grid.rect(x=x, y=y, width=width, height=height, hjust=0, vjust=0))

if (names(dev.cur())!="windows"){ dev.off()}


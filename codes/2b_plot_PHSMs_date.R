#### create the plots for PHSMs

load('data/data_govtstat.RData')

d0_PHSMs = data_govtstat_PHSMs

# convert to Date for the plot scale
d0_PHSMs$date_plot_from = sapply(d0_PHSMs$from, function(xdate) fun_date_timeline(date=xdate)-0.5)
d0_PHSMs$date_plot_to = sapply(d0_PHSMs$to, function(xdate) fun_date_timeline(date=xdate)-0.5)

fname_plot = sprintf("PHSMs_bydate_%s.pdf", update_outputdate())
pdf(fname_plot, width=plot_width, height=plot_height)

events_plot = unique(d0_PHSMs$events)

YMAX = length(events_plot);
YSCALE = rev(c(1, YMAX) + 0.5*c(-1,1))
YLAB = "";

fun_plot_cnt_timeline_blank(YMAX = YMAX, YLAB = YLAB, yaxis_draw=FALSE, YSCALE=YSCALE, label_5wave_YN=FALSE)

col_plot = "darkgreen"

for (ii in 1:length(events_plot)){

	xx_PHSM = events_plot[ii]
	xrow_T = which(d0_PHSMs$events==xx_PHSM)

	grid.segments(x0=d0_PHSMs$date_plot_from[xrow_T], x1=d0_PHSMs$date_plot_to[xrow_T], y0=ii, y1=ii, default.units = 'native', gp=gpar(col=col_plot, lwd=10, lineend=1))
	
	xx_PHSM_label = d0_PHSMs$events[xrow_T[1]]
	grid.text(x=0.5-0.15, y=ii, xx_PHSM_label, default.units = 'native', hjust=1, gp=gpar(fontsize=7)) # labels
} # for- ii 

if (names(dev.cur())!="windows"){ dev.off()}

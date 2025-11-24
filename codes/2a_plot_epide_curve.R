### epide curve
# the numbers match with the cases tested positive at SPH's dashboard: https://covid19.sph.hku.hk/

load('data/data_govtstat.RData')

date_plot_start = as.Date("2021-09-01", format="%Y-%m-%d")
date_plot_end = as.Date("2024-01-01", format="%Y-%m-%d")


dd_plot = subset(data_govtstat_reportedcases, subset=(Date>=date_plot_start & Date<=date_plot_end))
dd_plot$date_plot = sapply(dd_plot$Date, function(xdate) fun_date_timeline(date=xdate)-0.5)


fname_plot = sprintf("epidecurve_bydate_%s.pdf", update_outputdate(short=TRUE))
pdf(fname_plot, width=plot_width, height=plot_height)

YMAX = 85000
YLAB = "Number of daily cases";

fun_plot_cnt_timeline_blank(YMAX = YMAX, YLAB = YLAB)

with(dd_plot, grid.lines(x=date_plot, y=numcases, default.units = 'native', gp=gpar(col="darkgreen", lwd=2)))

# date at which daily num of cases was no longer reported
date_nocases = tail(dd_plot$date_plot, 1)

grid.lines(x=rep(date_nocases,2), y=c(0, 80000), default.units = 'native', gp=gpar(col="grey50", lty=2))
text_lastdate = paste("Daily number of cases", "was no longer reported", paste0("after ", tail(data_govtstat_reportedcases$Date,1)), sep="\n")
grid.text(label=text_lastdate, x=date_nocases+0.25, y=7500, default.units = 'native', , hjust=0, vjust=0, gp=gpar(col="grey50", fontsize=10, xpd=TRUE))


if (names(dev.cur())!="windows"){ dev.off()}

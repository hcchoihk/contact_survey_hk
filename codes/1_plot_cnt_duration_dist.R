### plot distribution of number of contact and contact duration 
# include CDF (cumulative density function)

library(dplyr)

if (!exists('chk_base')){
	load( 'data/chk_base.RData' )
}

dd_plot_cnt_all = subset(chk_base, select=c(contact_number, contact_time_hours, special_grps_YN, waveperiod_4gp))

### plotting
plot_width = 10
plot_height = 12
col_points = c("blue", "red", "green3", "gold")
legend_text = c("Pre-fifth wave", "Fifth wave", "Post-fifth wave", "Post-pandemic")
num_phases = 4;

# windows(width=plot_width, height=plot_height*2)
pdf(sprintf("plot_dist_contact_number_hours_%s.pdf", update_outputdate(short=TRUE)), width=plot_width, height=plot_height)
layout(matrix(1:8, ncol=2, byrow=FALSE))
parmar_list = c(3.5, 4.25, 2.5, 1)
xplot = 0

ylim_plot_cnt = c(0, 0.4)
ylim_plot_cnt_hr = c(0, 0.08)

cex_axis = 1.25

same_ylim_plot_YN = TRUE
only_1_legend_YN = TRUE
if (only_1_legend_YN){
	xplot_legend_vec = 0
} else {
	xplot_legend_vec = 1:8
}

for (ii_Spg in c(0,1)){

	title_text = ifelse(ii_Spg==0, "General population", "High-contact groups")
	dd_plot_cnt = subset(dd_plot_cnt_all, subset=(special_grps_YN==ii_Spg))

	# number of reported contacts
	hist_contact_number_fifthwave = with(dd_plot_cnt, tapply(INDEX=waveperiod_4gp, X=contact_number, hist, breaks=0:100, plot=FALSE))
	ylim_plot = range(pretty(c(0, max(sapply(hist_contact_number_fifthwave, function(hist_out) range(hist_out$density))))))

	density_contact_number_fifthwave = with(dd_plot_cnt, tapply(INDEX=waveperiod_4gp, X=contact_number, function(x) density(x, from=0)))
	ylim_plot = range(pretty(c(0, max(sapply(density_contact_number_fifthwave, function(x) max(x$y))))))

	table_contact_number_fifthwave = with(dd_plot_cnt, tapply(INDEX=waveperiod_4gp, X=contact_number, function(x) table(x)))
	table_contact_number_fifthwave = sapply(table_contact_number_fifthwave, simplify=FALSE, function(x) data.frame(x=as.numeric(rownames(x)), Freq=as.numeric(x)) %>% mutate(prop=Freq/sum(Freq)))
	ylim_plot = range(pretty(c(0, max(sapply(table_contact_number_fifthwave, function(x) max(x$prop))))))


	# density plot
	if (same_ylim_plot_YN){ ylim_plot = ylim_plot_cnt}
	xlim_plot = c(0,80)
	xtick_major = seq(0, max(xlim_plot), by=5)
	par(mar=parmar_list)
	xplot = xplot + 1
	plot(NA, xlim=xlim_plot, ylim=ylim_plot, xlab="", ylab="", las=1, xaxs='i', yaxs='i', axes=FALSE)
	axis(1, at=xtick_major, cex.axis=cex_axis, padj=0)
	axis(2, las=1, cex.axis=cex_axis, hadj=0.75)
	# nullout = sapply(1:num_phases, function(xx) with(density_contact_number_fifthwave[[xx]], lines(x, y, col=col_points[xx], lwd=2, xpd=TRUE)))
	nullout = sapply(1:num_phases, function(xx) with(table_contact_number_fifthwave[[xx]], lines(x, prop, col=col_points[xx], lwd=2, xpd=TRUE)))
	mtext(side=3, text=sprintf("(%s)", letters[xplot]), at=0, padj=0, adj=-0.5)
	mtext(side=1, text="Number of reported contacts", line=2.35, cex=1.1)
	mtext(side=2, text="Proportion", line=2.75, cex=1.1)
	legend("topright", legend=legend_text, col=col_points, title="Time period", lwd=2, xpd=TRUE)
	
	# title for gen pop and spg
	mtext(side=3, line=1, text=title_text, font=2)	
	

	# cumulative density function
	cdf_contact_number_fifthwave = with(dd_plot_cnt, tapply(INDEX=waveperiod_4gp, X=contact_number, ecdf))
	ylim_plot = c(0,1.025)

	xlim_plot = c(0,80)
	xtick_major = seq(0, max(xlim_plot), by=5)
	par(mar=parmar_list)
	xplot = xplot + 1
	plot(NA, xlim=xlim_plot, ylim=ylim_plot, xlab="", ylab="", las=1, xaxs='i', yaxs='i', axes=FALSE)
	axis(1, at=xtick_major, cex.axis=cex_axis, padj=0)
	axis(2, at=seq(0,1, by=0.2), las=1, cex.axis=cex_axis, hadj=0.75)
	axis(2, at=seq(0.1, 1, by=0.2), labels=FALSE, col=NA, col.ticks='black')
	nullout = sapply(1:num_phases, function(xx) lines(cdf_contact_number_fifthwave[[xx]], verticals=TRUE, do.points=FALSE, col.01line=NA, col=col_points[xx], lwd=2))
	mtext(side=3, text=sprintf("(%s)", letters[xplot]), at=0, padj=0, adj=-0.5)
	mtext(side=1, text="Number of reported contacts", line=2.35, cex=1.1)
	mtext(side=2, text="Cumulative proportion", line=2.75, cex=1.1)
	if (xplot %in% xplot_legend_vec){ legend("bottomright", legend=legend_text, col=col_points, title="Time period", lwd=2, inset=c(0, 0.015)) }



	# number of reported contact person hours
	hist_contact_time_hours_fifthwave = with(dd_plot_cnt, tapply(INDEX=waveperiod_4gp, X=contact_time_hours, hist, breaks=0:300, plot=FALSE))
	ylim_plot = range(pretty(c(0, max(sapply(hist_contact_time_hours_fifthwave, function(x) max(x$density))))))

	density_contact_time_hours_fifthwave = with(dd_plot_cnt, tapply(INDEX=waveperiod_4gp, X=contact_time_hours, function(x) density(x, from=0, na.rm=TRUE)))
	ylim_plot = range(pretty(c(0, max(sapply(density_contact_time_hours_fifthwave, function(x) max(x$y))))))

	table_contact_time_hours_fifthwave = with(dd_plot_cnt, tapply(INDEX=waveperiod_4gp, X=contact_time_hours, function(x) table(x)))
	table_contact_time_hours_fifthwave = sapply(table_contact_time_hours_fifthwave, simplify=FALSE, function(x) data.frame(x=as.numeric(rownames(x)), Freq=as.numeric(x)) %>% mutate(prop=Freq/sum(Freq)))
	ylim_plot = range(pretty(c(0, max(sapply(table_contact_time_hours_fifthwave, function(x) max(x$prop))))))

	table_contact_time_hours_round4_fifthwave = with(dd_plot_cnt, tapply(INDEX=waveperiod_4gp, X=contact_time_hours, function(x) table(round(4*x)/4)))
	table_contact_time_hours_round4_fifthwave = sapply(table_contact_time_hours_round4_fifthwave, simplify=FALSE, function(x) data.frame(x=as.numeric(rownames(x)), Freq=as.numeric(x)) %>% mutate(prop=Freq/sum(Freq)))

	table_contact_time_hours_round_fifthwave = with(dd_plot_cnt, tapply(INDEX=waveperiod_4gp, X=contact_time_hours, function(x) table(round(x))))
	

	# density plot
	if (same_ylim_plot_YN){ ylim_plot = ylim_plot_cnt_hr}
	xlim_plot = c(0,100)
	xtick_all = seq(0, max(xlim_plot), by=5)
	xtick_major = seq(0, max(xlim_plot), by=10)
	xtick_minor = setdiff(xtick_all, xtick_major)
	par(mar=parmar_list)
	xplot = xplot + 1
	plot(NA, xlim=xlim_plot, ylim=ylim_plot, xlab="", ylab="", las=1, xaxs='i', yaxs='i', axes=FALSE)
	axis(1, at=xtick_all, labels=FALSE, col=NA, col.ticks='black')
	axis(1, at=xtick_major, labels=TRUE, col=NA, col.ticks=NA, cex.axis=cex_axis, padj=0)
	axis(1, at=xtick_all, labels=FALSE, col='black', col.ticks=NA)
	axis(2, las=1, cex.axis=cex_axis, hadj=0.75)
	nullout = sapply(1:num_phases, function(xx) with(density_contact_time_hours_fifthwave[[xx]], lines(x, y, col=col_points[xx], lwd=2), xpd=TRUE))
	mtext(side=3, text=sprintf("(%s)", letters[xplot]), at=0, padj=0, adj=-0.5)
	mtext(side=1, text="Number of reported contact person hours", line=2.35, cex=1)
	mtext(side=2, text="Proportion", line=2.75, cex=1.1)
	if (xplot %in% xplot_legend_vec){ legend("topright", legend=legend_text, col=col_points, title="Time period", lwd=2)}


	# cumulative density function
	cdf_contact_time_hours_fifthwave = with(dd_plot_cnt, tapply(INDEX=waveperiod_4gp, X=contact_time_hours, ecdf ))
	ylim_plot = c(0,1.025)

	xlim_plot = c(0,100)
	xtick_all = seq(0, max(xlim_plot), by=5)
	xtick_major = seq(0, max(xlim_plot), by=10)
	xtick_minor = setdiff(xtick_all, xtick_major)
	par(mar=parmar_list)
	xplot = xplot + 1
	plot(NA, xlim=xlim_plot, ylim=ylim_plot, xlab="", ylab="", las=1, xaxs='i', yaxs='i', axes=FALSE)
	axis(1, at=xtick_all, labels=FALSE, col=NA, col.ticks='black')
	axis(1, at=xtick_major, labels=TRUE, col=NA, col.ticks=NA, cex.axis=cex_axis, padj=0)
	axis(1, at=xtick_all, labels=FALSE, col='black', col.ticks=NA)
	axis(2, at=seq(0,1, by=0.2), las=1, cex.axis=cex_axis, hadj=0.75)
	axis(2, at=seq(0.1, 1, by=0.2), labels=FALSE, col=NA, col.ticks='black')
	nullout = sapply(1:num_phases, function(xx) lines(cdf_contact_time_hours_fifthwave[[xx]], verticals=TRUE, do.points=FALSE, col.01line=NA, col=col_points[xx], lwd=2))
	mtext(side=3, text=sprintf("(%s)", letters[xplot]), at=0, padj=0, adj=-0.5)
	mtext(side=1, text="Number of reported contact person hours", line=2.35, cex=1)
	mtext(side=2, text="Cumulative proportion", line=2.75, cex=1.1)
	if (xplot %in% xplot_legend_vec){ legend_out = legend("bottomright", legend=legend_text, col=col_points, title="Time period", lwd=2)}

} # for- ii_Spg

dev.off()

### functions and settings related to plotting / drawing

## settings for the plot
COLS =ggsci::pal_npg(palette = c("nrc"))(10)
par_mar = c(2,7.5,0.5,0.5)

plot_width = 20/2.54
plot_height = 6/2.54
plot_outputdate = update_outputdate(short=TRUE);

load('data/chk_plot_numcnt_data.RData')



# get the mean/CI and median/PI_level
fun_summstat = function(xx, CI_level = 0.95, PI_level = 0.8){
	PI_lowupp = 0.5 + PI_level/2 * c(-1,1)
	unique_xx = unique(xx)
	if (length(unique_xx)<=1){
		mean_CI_xx = setNames(rep(mean(xx),3), c("estimate", "conf.int.1", "conf.int.2"))
	} else {
		mean_CI_xx = unlist(t.test(xx, conf.level=CI_level)[c("estimate","conf.int")])
	}
	out_xx = c(mean_CI_xx, quantile(xx, probs=c(0.5, PI_lowupp)))
	return(out_xx)
} # fun_summstat


# to draw errbar, with arrows if exceeding YMAX
fun_errbar_vec = function(x0, x1, y0, y1, YMAX, ... ){
	nullout = mapply(function(x00, x11, y00, y11)
	if (y00 < YMAX){
		if (y00>=0){
			if (y11 <= YMAX){
				grid.segments(x0 = x00, x1 = x11, y0 = y00, y1 = y11, ... )
			} else {
				grid.segments(x0 = x00, x1 = x11, y0 = y00, y1 = YMAX, arrow=grid::arrow(length=CI_arrow_length, ends='last'), ... )
			}
		} else if (y00<0){
			if (y11 <= YMAX){
				grid.segments(x0 = x00, x1 = x11, y0 = 0, y1 = y11, arrow=grid::arrow(length=CI_arrow_length, ends='first'), ... )
			} else {
				grid.segments(x0 = x00, x1 = x11, y0 = 0, y1 = YMAX, arrow=grid::arrow(length=CI_arrow_length, ends='both'), ... )
			}
		}
	}, x00 = x0, x11 = x1, y00=y0, y11=y1)
} # fun_errbar_vec


# convert date to point for the timeline
fun_date_timeline = function(date, t_month, date_start = as.Date("2021-09-01", format="%Y-%m-%d"), date_end = as.Date("2024-01-01", format="%Y-%m-%d")){
	if (missing(t_month)){
		t_month = data.frame(m = c(9:12, 1:12, 1:12),
						 y = c(rep(2021,4),rep(2022,12),rep(2023,12)))
		t_month$tstep = 1:nrow(t_month)	
	}
	if (date<date_start){
		date = date_start
	}
	if (date>date_end){
		date = date_end
	}
	
	year = as.numeric(format(date, "%Y"))
	month = as.numeric(format(date, "%m"))
	day = as.numeric(format(date, "%d"))
	days_in_month = as.numeric(lubridate::days_in_month(date))
	
	xtime = which(t_month$m %in% month & t_month$y %in% year)
	xtime = xtime + (day/days_in_month)

	return(xtime)
} # fun_date_timeline



# function to plot blank timeline using grid package
fun_plot_cnt_timeline_blank = function(YMAX=1, YLAB="y-axis label", yaxis_at=NULL, yaxis_draw=TRUE, YSCALE, t_month, label_5wave_YN=TRUE){
	if (missing(t_month)){
		t_month = data.frame(m = c(9:12, 1:12, 1:12),
						 y = c(rep(2021,4),rep(2022,12),rep(2023,12)))
		t_month$tstep = 1:nrow(t_month)	
	}

	if (missing(YSCALE)){
		YSCALE = c(0,YMAX)
	}


	grid.newpage()
	pushViewport(plotViewport(par_mar, yscale=YSCALE, xscale=c(0.5,28.5)))

	x1 = which(t_month$m %in% 1 & t_month$y %in% 2022)
	x2 = which(t_month$m %in% 4 & t_month$y %in% 2022)
	x1 = x1 + (6/31)
	x2 = x2 + (20/30)

	# fifth wave 
	grid.polygon(x = c(x1,x2,x2,x1)-0.5,
				 y = c(YSCALE[1], YSCALE[1], YSCALE[2], YSCALE[2]),
				 default.units = 'native',gp = gpar(col=NA,fill='grey80'))
	if (label_5wave_YN){
		grid.text(label = '5th wave', x = unit(median(c(x1,x2))-0.5,'native'), y = unit(1,'npc')+unit(-0.5,'lines'),
			  gp=gpar(fontsize=10, col='white', fontface= 'bold'))
	}

	# x-axis
	grid.xaxis(at = c(1:29)-0.5, label = FALSE, gp=gpar(fontsize=12))
	grid.xaxis(at = c(which(t_month$m %in% 1),29)-0.5, label = FALSE, gp=gpar(fontsize=30))
	grid.text(label = substring(month.abb[t_month$m], 1, 1), x = unit(c(1:28),'native'), y = unit(-0.5,'lines'), gp=gpar(fontsize=8))
	grid.text(label = c(2021,2022,2023), x = unit(c(2.5,which(t_month$m %in% 6)+0.5),'native'), y = unit(-1.5,'lines'), gp=gpar(fontsize=10))
			  
	# y-axis ticks
	if (yaxis_draw){
		grid.yaxis(at=yaxis_at, gp=gpar(fontsize=10))
	}
	grid.text(label = YLAB, x = unit(-4.0,'lines'), rot=90,gp=gpar(fontsize=10))

} # fun_plot_cnt_timeline_blank

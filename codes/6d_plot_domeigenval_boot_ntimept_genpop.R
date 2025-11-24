#### plot contact matrices for each of the four phases

library(grid)

### plot
fun_letters_new = function(x){ # scalar input only
	if (x%%26==0){
		paste(letters[c(floor(x/26)-1, 26)], collapse="")
	} else{
		paste(letters[c(floor(x/26), x%%26)], collapse="")
	}
}

# function to plot blank timeline
source('codes/2_3_plot_contact_settings_fundraw.R')
t_month = data.frame(m = c(9:12, 1:12, 1:12),
                     y = c(rep(2021,4),rep(2022,12),rep(2023,12)))

source('codes/5_fun_create_date_subsets.R')


# load data
folder_cntmat_boot_output = ''; # i.e., under getwd() [folder_main in 0_main.R]
fname_RData_cntmat_boot = "domeigen_cntmat_ntimept_nboot1000_genpop.RData";
load( paste0(folder_cntmat_boot_output, fname_RData_cntmat_boot) )


# settings
names_phycnt = c("all", "phycnt")
labels_phycnt = c('Overall', 'Physical')
num_phycnt = 2

CI_level_plot = 0.9;

if (!exists("date_byphase")){
	load('data/chk_part_contact_base.RData')
	nsubsets_tocut = 14;
	date_tocut = chk_part_base$date
	date_tocut = date_tocut[chk_part_base$part_specialgroups_YN==0]
	date_byphase = fun_cutdates_subsets(date_tocut, n_subsets=nsubsets_tocut)
	date_byphase = c(as.Date("2021-09-01", format="%Y-%m-%d"), tail(head(date_byphase, -1),-1), as.Date('2023-12-31')) # use the self-defined start and end date
}
num_date_byphase = length(date_byphase)-1;
# table(cut(date_tocut, breaks=date_byphase, include.lowest=TRUE, right=FALSE)) # number of responses in each time phase

xtime_datepairs = sapply(1:num_date_byphase, simplify=FALSE, function(itime) c(fun_date_timeline(date_byphase[itime]), fun_date_timeline(date_byphase[itime+1]-1)))
iplot_xtime = sapply(xtime_datepairs, mean)

#### plot ratio of dominant eigenvalues across time period
# get stat among boot 
stat_ratio_domeigenval_plot = array(NA, dim=c(num_phycnt, num_date_byphase, 3))
dimnames(stat_ratio_domeigenval_plot) = list(names_phycnt, NULL, c("median", "pct0025", "pct0975"))
ratio_domeigenval_plot = array(NA, dim=c(num_phycnt, num_date_byphase, num_boot))
dimnames(ratio_domeigenval_plot) = list(names_phycnt, NULL, NULL)


# transform to array of bootstrapped results
max_eigenval_cntmat_array = array(list(NULL), dim=c(num_phycnt, num_date_byphase))
for (itime in 1:num_date_byphase){
	for (iphycnt in 1:num_phycnt){
		max_eigenval_cntmat_array[[iphycnt, itime]] = sapply(max_eigenval_cntmat_comix_boot, function(xx) xx[itime, iphycnt])
	}
} # for- itime

for (irow in 1:num_phycnt){
	stat_ratio_domeigenval_plot[irow, 1, ] = 1;
	ratio_domeigenval_plot[irow, 1, ] = 1;
	for (jcol in 2:num_date_byphase){
		ratio_domeigenval_plot[irow, jcol, ] = max_eigenval_cntmat_array[[irow, jcol]]/max_eigenval_cntmat_array[[irow, 1]]
		stat_ratio_domeigenval_plot[irow, jcol, ] = quantile(ratio_domeigenval_plot[irow, jcol, ], probs=c(0.5, 0.5+CI_level_plot/2*c(-1,1)))
	}
}


# GAMs
numGAM_gps = 2
mean_CI_ratio_out = rep(list(NULL), numGAM_gps)
pred_GAM_ratio_out = rep(list(NULL), numGAM_gps)
for (iplot in 1:2){
	# mean_CI_TT = do.call(rbind, sapply(2:num_date_byphase, simplify=FALSE, function(itime) unlist(t.test(ratio_domeigenval_plot[iplot, itime, ], conf.level=CI_level_plot)[c('estimate', 'conf.int')])))
	mean_CI_TT = do.call(rbind, sapply(2:num_date_byphase, simplify=FALSE, function(itime) unlist(quantile(ratio_domeigenval_plot[iplot, itime, ], probs=c(0.5, 0.5+CI_level_plot/2*c(-1,1))))))
	mean_CI_TT = rbind(1, mean_CI_TT) # for itime==1, ratio=1 (baseline)
	mean_CI_ratio_out[[iplot]] = mean_CI_TT
		
	# tstep_month = itime or iplot_xtime[itime]
	df_GAM_TT = do.call(rbind, sapply(1:num_date_byphase, simplify=FALSE, function(itime) cbind(ratio=ratio_domeigenval_plot[iplot, itime, ], tstep_month=iplot_xtime[itime])))
	df_GAM_TT = as.data.frame(df_GAM_TT)

	m1_TT = gam(ratio ~ s(tstep_month), data=df_GAM_TT)
	preddata = data.frame(tstep_month = iplot_xtime)
	pred_GAM_ratio_out[[iplot]] = predict(m1_TT, newdata = preddata, type = "response", se.fit = TRUE)
}


# setting for the plot
COLS_pick = c("All"="blue3", "Physical"="red2")
PCHS_pick = c("All"=16, "Physical"=18)

fname_excelout = sprintf("ratio_domeigenval_boot_genpop_%s.xlsx", update_outputdate())
fname_plot_ratio_varyt = gsub("xlsx", "pdf", fname_excelout)
pdf(file = fname_plot_ratio_varyt, width = 20/2.54,height = 6/2.54) # /2.54 for changing to inch


for (iplottype in 1:2){
# iplottype, 1=line of mean, 2=GAM fitting


par_mar = c(2,7.5,0.5,0.5)

YMAX = max(stat_ratio_domeigenval_plot[1:2,,])
YMAX = max(pretty(c(0,YMAX)))
YMAX = ceiling(YMAX*2)/2 # per 0.5
yaxis_at = seq(0, YMAX, by=0.5)
yaxis_minor = setdiff(seq(0, YMAX, by=0.1), yaxis_at)

YLAB = paste(c("Ratio of dominant eigenvalue", "of contact matrices"), collapse="\n")

fun_plot_cnt_timeline_blank(YMAX = YMAX, YLAB = YLAB, yaxis_at = yaxis_at)

# draw minor ytick
#	nullout = sapply(yaxis_minor, function(yy) grid.segments(x0=0.5, x1=0.3, y0=yy, y1=yy, default.units='native'))

# reference line
grid.lines(x = 0.5+c(0,28),y = c(1,1), default.units = 'native',gp = gpar(col = "black", lwd = 0.75, lty=3)) 


for (iplot in 1:2){
	COLS_iplot = COLS_pick[iplot]
	PCHS_iplot = PCHS_pick[iplot]

	iplot_xtime_CI = iplot_xtime + 0.15*ifelse(iplot==1, -1, 1)
	
	arrow_use = arrow(angle=90, ends="both", length=unit(0.025, "npc"))
	arrow_use = NULL;
	for (itime in 1:num_date_byphase){
		grid.points(iplot_xtime_CI[itime], y=stat_ratio_domeigenval_plot[iplot, itime, 'median'], default.units = 'native', pch=PCHS_iplot, gp = gpar(col=COLS_iplot, cex=0.8))
		if (itime !=1){
			grid.lines(x=iplot_xtime_CI[itime], y=c(stat_ratio_domeigenval_plot[iplot, itime, 'pct0025'], stat_ratio_domeigenval_plot[iplot, itime, 'pct0975']), default.units = 'native', arrow=arrow_use, gp = gpar(col=COLS_iplot, lwd=1.5))
		}
	} # itime

	if (iplottype==1){
		# line of mean
		lines_TT = stat_ratio_domeigenval_plot[iplot, , 'median'];
		lines_TT[lines_TT>YMAX] = YMAX;
		grid.lines(x = iplot_xtime_CI,y = lines_TT, default.units = 'native',gp = gpar(col = COLS_iplot, lwd = 1.5))
	} else if (iplottype==2){
		# GAM fitting
		pred_TT = pred_GAM_ratio_out[[iplot]]
		x_pts_catg = iplot_xtime_CI;

			x_polygon = c(x_pts_catg, rev(x_pts_catg))
			with(pred_TT,  grid.polygon(x = x_polygon, y = c(fit - 1.96 * se.fit, rev(fit + 1.96 * se.fit)), default.units = 'native',gp = gpar(col= NA, fill = COLS_iplot, alpha = 0.2)))

		grid.lines(x = x_pts_catg, y = pred_TT$fit, default.units = 'native', gp = gpar(col = COLS_iplot, lwd = 2))
	}	
	
	# legend
	if (FALSE){
		legend_T = names(COLS_pick)[iplot]
		legend_T = paste0(legend_T, " contacts")
		legend_x = 24;
		legend_y = YMAX*0.25-YMAX/10*(iplot-0.5) # /15
		grid.lines(x = legend_x-c(1,0), y=legend_y, default.units = 'native',gp = gpar(col = COLS_iplot, lwd = 1.5))
		grid.points(x = legend_x-0.5, y=legend_y, default.units = 'native', pch=PCHS_iplot, gp = gpar(col=COLS_iplot, cex=0.7))
		grid.text(label = legend_T, x=legend_x+0.25, y=legend_y, default.units = 'native', just = c(0,0.5), gp=gpar(col = "black",fontsize=9))
	} else{
		leg_vp_npc = viewport(x = 0.28, y = 0.775, width = 0.25, height = 0.2, just = c("left", "bottom")) # location of the legend
		# pushViewport(leg_vp_npc)
		# grid.draw(rectGrob()) # box of the legend
		grid.legend(labels = paste0(names(COLS_pick), " contacts"),
			pch = PCHS_pick,
			gp = gpar(col=COLS_pick, fill=COLS_pick, cex = 0.75, lwd=1.5),
			vgap = unit(0.15, "lines"), vp=leg_vp_npc
		)
	}
	
} # for- iplot

} # for- iplottype

if (!(names(dev.cur()) %in% c("null device", "windows"))){ dev.off()}

# export the stat
stat_ratio_domeigenval_plot_out = do.call(cbind, sapply(1:(dim(stat_ratio_domeigenval_plot)[3]), simplify=FALSE, function(iistat) {
	stat_TT = stat_ratio_domeigenval_plot[,,iistat];
	stat_TT = cbind(stat_TT, NA);
	return(stat_TT)
}))

stat_ratio_domeigenval_plot_out = aperm(stat_ratio_domeigenval_plot, c(2,3,1)) # swap dimensions
stat_ratio_domeigenval_plot_out= sapply(1:(dim(stat_ratio_domeigenval_plot_out)[3]), simplify=FALSE, function(iphy) {
	df_TT = as.data.frame(stat_ratio_domeigenval_plot_out[,,iphy]);
	df_TT$period_datestart = head(date_byphase,-1)
	df_TT$text_out2 = sprintf('%.2f (%.2f, %.2f)', df_TT[,1], df_TT[,2], df_TT[,3])
	df_TT$blank = NA
	colnames(df_TT) = paste(labels_phycnt[iphy], colnames(df_TT), sep="_")
	return(df_TT)
})
stat_ratio_domeigenval_plot_out = do.call(cbind, stat_ratio_domeigenval_plot_out)
	

# export stat_ratio_domeigenval
fname_excelout = paste0("stat_", fname_excelout)
openxlsx::write.xlsx(list(stat=stat_ratio_domeigenval_plot_out), file = fname_excelout, rowNames=TRUE)

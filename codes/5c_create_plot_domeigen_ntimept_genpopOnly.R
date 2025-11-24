#### plot contact matrices for more time points

library(socialmixr)
library(grid)

# function to plot blank timeline
if (!exists("fun_plot_cnt_timeline_blank")){
	source('codes/2_3_plot_contact_settings_fundraw.R')
	t_month = data.frame(m = c(9:12, 1:12, 1:12),
                     y = c(rep(2021,4),rep(2022,12),rep(2023,12)))
}

if (!exists("fun_cutdates_subsets")){
	source('codes/5_fun_create_date_subsets.R')
}


### load survey data
load( 'data/chk_part_contact_base.RData' )


## settings
age_limits_agegps = seq(1, num_agegps, by=1);
sym_cntmatrix_YN = FALSE;

# set upper bound for the num_cnt per age gp
uppbound_cnt_agegp = 25;

# CoMix, by more phase, [, )
nsubsets_tocut = 14;
date_tocut = chk_part_base$date
date_tocut = date_tocut[chk_part_base$part_specialgroups_YN==0]
date_byphase = fun_cutdates_subsets(date_tocut, n_subsets=nsubsets_tocut)
date_byphase = c(as.Date("2021-09-01", format="%Y-%m-%d"), tail(head(date_byphase, -1),-1), as.Date('2023-12-31')) # use the self-defined start and end date
num_date_byphase = length(date_byphase)-1;
num_time = num_date_byphase;
# table(cut(date_tocut, breaks=date_byphase, include.lowest=TRUE, right=FALSE)) # number of responses in each time phase

xtime_datepairs = sapply(1:num_time, simplify=FALSE, function(itime) c(fun_date_timeline(date_byphase[itime]), fun_date_timeline(date_byphase[itime+1]-1)))
iplot_xtime = sapply(xtime_datepairs, mean)


# contact matrices
num_phycnt = 2;

names_phycnt = c("all", "phycnt")
labels_phycnt = c('Overall', 'Physical')


# based on the original data set
cntmat_phy_array_genpop = array(list(NULL), dim=c(num_phycnt, num_time));
max_eigenval_cntmat_array_genpop = array(NA, dim=c(num_phycnt, num_time)); # for all and phy/nonphy contacts 

# socialmixr for overall
chk_socialmixr_byphase = rep(list(NULL), length=num_date_byphase)
for (iphase in 1:num_date_byphase){
	irow_TT = (chk_part_base$date >= date_byphase[iphase] & chk_part_base$date < date_byphase[iphase+1] & chk_part_base$part_specialgroups %in% 997 ); # among participants not in the special groups 
	chk_part_TT = chk_part_base[irow_TT,];
	chk_contact_TT = chk_contact_base[chk_contact_base$part_id %in% chk_part_TT$part_id,]
	chk_socialmixr_byphase[[iphase]] <- survey(participants = chk_part_TT, contacts = chk_contact_TT)
} # for-itime


# contact matrices by location and type (phy / nonphy)
for (itime in 1:num_time){

	TT_socialmixr = chk_socialmixr_byphase[[itime]]
	for (iphycnt in 1:num_phycnt){
		cntmat_phy_array_genpop[[1, itime]] = contact_matrix(survey = TT_socialmixr, age.limits = age_limits_agegps, symmetric=sym_cntmatrix_YN);
		cntmat_phy_array_genpop[[2, itime]] = contact_matrix(survey = TT_socialmixr, age.limits = age_limits_agegps, symmetric=sym_cntmatrix_YN, filter = list(phys_contact = 1));
	} # for- iphycnt

} # for- itime


# dominant eigenvalues
for (itime in 1:num_time){
	for (iphycnt in 1:num_phycnt){
		m_contacts_TT = cntmat_phy_array_genpop[[iphycnt, itime]]$matrix
		m_contacts_TT[is.na(m_contacts_TT)] = 0;
		if (any(m_contacts_TT>uppbound_cnt_agegp)){
			browser()
			m_contacts_TT[m_contacts_TT>uppbound_cnt_agegp] = uppbound_cnt_agegp;
		}
		eigenval_TT = eigen(m_contacts_TT)$values;
		eigenval_TT = as.numeric(eigenval_TT[Im(eigenval_TT)==0])
		max_eigenval_cntmat_array_genpop[iphycnt, itime] = max(eigenval_TT);
	}
} # for- itime



# plot 
domeigenval_plot = max_eigenval_cntmat_array_genpop;
domeigenval_plot = domeigenval_plot[, 1:num_time, drop=FALSE]
# ratio of dominant eigenvalues w.r.t. before5
ratio_domeigenval_plot = t(apply(domeigenval_plot, 1, function(xx) xx/xx[1]))


# setting for the plot
COLS_pick = c("All"="blue3", "Physical"="red2")
PCHS_pick = c("All"=16, "Physical"=18)

fname_excelout = sprintf("ratio_domeigenval_%dtimept_%s_genpop.xlsx", num_time, update_outputdate())

fname_plot_ratio_varyt = gsub("xlsx", "pdf", fname_excelout)
pdf(file = fname_plot_ratio_varyt, width = 20/2.54,height = 6/2.54) # /2.54 for changing to inch

par_mar = c(2,7.5,0.5,0.5)
if (!exists("CI_level_plot")){ CI_level_plot = 0.9}

YMAX = max(ratio_domeigenval_plot)
YMAX = max(pretty(c(0,YMAX)))
YMAX = ceiling(YMAX*2)/2 # per 0.5
yaxis_at = seq(0, YMAX, by=0.5)
yaxis_minor = setdiff(seq(0, YMAX, by=0.1), yaxis_at)

YLAB = paste(c("Ratio of dominant eigenvalue", "of contact matrices"), collapse="\n")


fun_plot_cnt_timeline_blank(YMAX = YMAX, YLAB = YLAB, yaxis_at = yaxis_at)

# draw minor ytick
# nullout = sapply(yaxis_minor, function(yy) grid.segments(x0=0.5, x1=0.3, y0=yy, y1=yy, default.units='native'))

# reference line
grid.lines(x = 0.5+c(0,28),y = c(1,1), default.units = 'native',gp = gpar(col = "black", lwd = 0.75, lty=3)) 


for (iplot in 1:2){
	COLS_iplot = COLS_pick[iplot]
	PCHS_iplot = PCHS_pick[iplot]

	iplot_xtime_CI = iplot_xtime + 0.15*ifelse(iplot==1, -1, 1)
	
	for (itime in 1:num_time){
		grid.points(iplot_xtime_CI[itime], y=ratio_domeigenval_plot[iplot, itime], default.units = 'native', pch=PCHS_iplot, gp = gpar(col=COLS_iplot, cex=0.8))
	} # itime

	lines_TT = ratio_domeigenval_plot[iplot, ];
	lines_TT[lines_TT>YMAX] = YMAX;
	grid.lines(x = iplot_xtime_CI,y = lines_TT, default.units = 'native',gp = gpar(col = COLS_iplot, lwd = 1.5))

	
	# legend
	if (FALSE){
		legend_T = names(COLS_pick)[iplot]
		legend_T = paste0(legend_T, " contacts")
		legend_x = 24;
		legend_y = YMAX*0.25-YMAX/10*(iplot-0.5) # /15
		grid.lines(x = legend_x-c(1,0), y=legend_y, default.units = 'native',gp = gpar(col = COLS_iplot, lwd = 1.5))
		grid.points(x = legend_x-0.5, y=legend_y, default.units = 'native', pch=PCHS_iplot, gp = gpar(col=COLS_iplot, cex=0.7))
		grid.text(label = legend_T, x=legend_x+0.25, y=legend_y, default.units = 'native', just = c(0,0.5), gp=gpar(col = "black",fontsize=9))
	} else {
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


print( "ratio_domeigenval_plot, genpopOnly" )
print( ratio_domeigenval_plot )

if (!(names(dev.cur()) %in% c("null device", "windows"))){ dev.off()}


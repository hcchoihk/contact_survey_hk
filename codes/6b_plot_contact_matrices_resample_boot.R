#### plot contact matrices for each of the four phases

library(socialmixr)
library(grid)

### plot
fun_letters_new = function(x){ # scalar input only
	if (x%%26==0){
		paste(letters[c(floor(x/26)-1, 26)], collapse="")
	} else{
		paste(letters[c(floor(x/26), x%%26)], collapse="")
	}
}


# function to plot contact matrices
source('codes/5_plot_functions_cntmat.R')

# load data
folder_cntmat_boot_output = ''; # i.e., under getwd() [folder_main in 0_main.R]
fname_RData_cntmat_boot = "cntmat_4phases_nboot1000_resample.RData";


num_phases = 4
date_byphase = as.Date(c("2021-09-01", "2022-01-07", "2022-04-21", "2023-03-01", "2024-01-01"))

# settings
max_count_cntmat_cell = 40; # max number of contact in each cell (age i x age j) in a contact matrix

names_loc = c('all', 'Home', 'School', 'Work', 'Others');
names_phycnt = c("all", "phycnt", "nonphycnt")
labels_loc = c("Overall", names_loc[-1])
labels_phycnt = c('Overall', 'Physical', 'Non-physical')
names_time = c("before5", "during5", "after5_p1", "after5_p2", "survey_1516")
labels_time = c("Pre-fifth wave", "Fifth wave", "Post-fifth wave", "Post-pandemic", "2015-2016 survey")
num_time = 5


# add date for labels_time
if (TRUE){
	for(itime in 1:4){
		labels_time[itime] = paste(sep="\n", labels_time[itime], sprintf("%s to %s", date_byphase[itime], date_byphase[itime+1]-1))
	} # for- itime
}


# for all participants
fname_suffix = "_boot_resample"
load( paste0(folder_cntmat_boot_output, fname_RData_cntmat_boot) )	
	
# make cnt_loc_array, cnt_phy_array based on the bootstrapped replications
# recall: cntmat_loc_array = array of contact_matrix outputs
# cntmat_comix_boot_loc/phy, the three layers refer to boot, itime, num_loc/num_phy 

for (iplottype in 1:2){	# loc or phycnt

	if (iplottype == 1){
		cntmat_comix_boot_TT = cntmat_comix_boot_loc
		cntmat_d17_boot_TT = cntmat_d17_boot_loc
	} else if (iplottype == 2){
		cntmat_comix_boot_TT = cntmat_comix_boot_phy
		cntmat_d17_boot_TT = cntmat_d17_boot_phy
	}
	
	ncol_plot = 5; # 4 phases + d17
	nrow_plot = length(cntmat_comix_boot_TT[[1]][[1]])
	num_boot = length(cntmat_comix_boot_TT)
	
	xx_cntmat_blank = matrix(0, nrow=num_agegps, ncol=num_agegps)

	cntmat_array_TT = array(list(NULL), dim=c(nrow_plot, ncol_plot))
	stat_cntmat_array_TT = array(list(NULL), dim=c(nrow_plot, ncol_plot))

	for (irow in 1:nrow_plot){
		for (jcol in 1:ncol_plot){
			if (jcol %in% c(1:4)){
				cntmat_TT = sapply(1:num_boot, simplify=FALSE, function(iboot){
					xx_cntmat = cntmat_comix_boot_TT[[iboot]][[jcol]][[irow]]$matrix
					xx_cntmat[is.na(xx_cntmat)] = 0
					if (nrow(xx_cntmat)==num_agegps){ return(xx_cntmat)}
					nrow_TT = nrow(xx_cntmat);
					xx_cntmat_TT = xx_cntmat_blank;
					xx_cntmat_TT[1:nrow_TT, 1:nrow_TT] = xx_cntmat
					return(xx_cntmat_TT)
				})
			} else if (jcol==5){
				cntmat_TT = sapply(1:num_boot, simplify=FALSE, function(iboot){
					xx_cntmat = cntmat_d17_boot_TT[[iboot]][[irow]]$matrix
					if (nrow(xx_cntmat)==num_agegps){ return(xx_cntmat)}
					nrow_TT = nrow(xx_cntmat);
					xx_cntmat_TT = xx_cntmat_blank;
					xx_cntmat_TT[1:nrow_TT, 1:nrow_TT] = xx_cntmat
					return(xx_cntmat_TT)
				})
			}
			
			# take element-wise mean
			cntmat_TT_transm = simplify2array(cntmat_TT) # abind::abind(cntmat_TT, along=3) # sapply(cntmat_TT, simplify='array', function(xx) xx)
			num_stat_cntmat = 4;
			labels_stat_cntmat = c("mean", "median", "pct0025", "pct0975")
			stat_cntmat_TT_array = array(NA, dim=c(dim(cntmat_TT_transm)[1:2], num_stat_cntmat))
			dimnames(stat_cntmat_TT_array) = c(dimnames(cntmat_TT_transm)[1:2], list(labels_stat_cntmat))
			stat_cntmat_TT_array[,,1] = apply(cntmat_TT_transm, 1:2, mean)
			stat_cntmat_TT_array[,,2] = apply(cntmat_TT_transm, 1:2, median)
			stat_cntmat_TT_array[,,3] = apply(cntmat_TT_transm, 1:2, function(xx) t.test(xx, na.rm=TRUE)$conf.int[1])
			stat_cntmat_TT_array[,,4] = apply(cntmat_TT_transm, 1:2, function(xx) t.test(xx)$conf.int[2])
			stat_cntmat_TT_array[is.nan(stat_cntmat_TT_array)] = 0;
			cntmat_array_TT[[irow, jcol]] = stat_cntmat_TT_array
			
			stat_cntmat_TT_out = sapply(1:num_stat_cntmat, simplify=FALSE, function(kkstat) {
				out_TT = rbind(NA, stat_cntmat_TT_array[,,kkstat], NA)
				rownames(out_TT) = paste(labels_stat_cntmat[kkstat], c("", rownames(stat_cntmat_TT_array[,,kkstat]), "blank"), sep="_")
				return(out_TT)
			})
			stat_cntmat_array_TT[[irow, jcol]] = do.call(rbind, stat_cntmat_TT_out)
		} # for- jcol
	} # for- irow
	
	
	if (iplottype == 1){
		cntmat_loc_array = cntmat_array_TT
		stat_cntmat_loc_array = stat_cntmat_array_TT
	} else if (iplottype == 2){
		cntmat_phy_array = cntmat_array_TT
		stat_cntmat_phy_array = stat_cntmat_array_TT
	}
	
} # for-iplottype, create cnt_loc_array, cnt_phy_array


# plot contact matrices
cnt_matrices_byplottype = rep(list(NULL), 2)
cnt_matrices_panellabels_byplottype = rep(list(NULL), 2)
for (iplottype in 1:2){

	# select what to plot
	if (iplottype==1){
		# location of contact
		fname_plot = sprintf('cnt_loc_%s', update_outputdate(short=TRUE))

		cntmat_plot_TT = cntmat_loc_array
		labels_plot_TT = labels_loc;

		stat_cntmat_xlsx_TT = stat_cntmat_loc_array

		fname_excelout_base = "location"		
		
	} else if (iplottype==2){
		# physical / non-physical contact
		fname_plot = sprintf('cnt_phycnt_%s', update_outputdate(short=TRUE))

		cntmat_plot_TT = cntmat_phy_array;
		labels_plot_TT = labels_phycnt;
		
		stat_cntmat_xlsx_TT = stat_cntmat_phy_array
		
		fname_excelout_base = "phy_nonphy"
	}
	
	
	## plot 
	## settings
	nrow_plot = nrow(cntmat_plot_TT)
	ncol_plot = ncol(cntmat_plot_TT);
	plotsize = 2;
	fname_plot = paste0(fname_plot, fname_suffix, ".pdf")
	pdf(file = fname_plot, width = plotsize*ncol_plot, height = plotsize*nrow_plot)

	# export the contact matrices 
	cnt_matrices_list = rep(list(), prod(c(nrow_plot, ncol_plot)))
	cnt_panellabels_mat = matrix("", nrow=nrow_plot, ncol=ncol_plot)
	dimnames(cnt_panellabels_mat) = list(labels_plot_TT, labels_time)


	## start plotting
	## colorbar
	fun_colour_pick = ifelse(TRUE, colour_blue, colour_red);
	# fun_colour_pick = colour_red

	max_cnt_colbar = 4; # max contact number in the color bar
	seq_colbar = seq(0, max_cnt_colbar, by=0.1)


		# the label is beside the colorbar
		grid.newpage()
		pushViewport(plotViewport(c(3.5,2,2,0)))

		xvalues_colorbar_start = 0.45; # values relative to 'npc'
		xvalues_colorbar_end = xvalues_colorbar_start + 0.4;
		xvalues_colorbar_outbox = c(xvalues_colorbar_start, xvalues_colorbar_end, xvalues_colorbar_end, xvalues_colorbar_start);
		xvalues_colorbar_label = unit(xvalues_colorbar_start-0.035,'npc')
		yvalues_colorbar_start = 0.0225;
		yvalues_colorbar_end = yvalues_colorbar_start + 0.02;
		yvalues_colorbar_polygon = c(yvalues_colorbar_start, yvalues_colorbar_start, yvalues_colorbar_end, yvalues_colorbar_end);
		yvalues_colorbar_tickmark = unit(yvalues_colorbar_start-0.005,'npc')-unit(6.5,'lines'); # grid.polygon and grid.text may have different x/y settings, so adjust with unit(yy, 'lines')
		yvalues_colorbar_label = unit(mean(c(yvalues_colorbar_start, yvalues_colorbar_end)), 'npc') -unit(5.5,'lines') ; #yvalues_colorbar_tickmark;
		xvalues = seq(xvalues_colorbar_start,xvalues_colorbar_end, length.out = length(seq(0, max_cnt_colbar, 0.1)))
		for(i in 2:length(seq_colbar)){
		  grid.polygon(x = unit(c(xvalues[i-1],xvalues[i],xvalues[i],xvalues[i-1]),'npc'),
					   y = unit(yvalues_colorbar_polygon,'npc')-unit(3.5,'lines'),
					   gp = gpar(col = fun_colour_pick(contactmatrix = seq_colbar, max.value = max_cnt_colbar)[i],fill = fun_colour_pick(contactmatrix = seq_colbar, max.value = max_cnt_colbar)[i]))
		}
		grid.polygon(x = unit(xvalues_colorbar_outbox,'npc'),y = unit(yvalues_colorbar_polygon,'npc')-unit(3.5,'lines'),default.units = 'npc',gp = gpar(fill=NA))
		grid.text(label = (seq_colbar)[(seq_colbar) %% 1 ==0], x = unit(xvalues[(seq_colbar) %% 1 ==0],'npc'), y = yvalues_colorbar_tickmark,just = 'top',gp = gpar(fontsize = 6.5))
		grid.text(label = 'Mean number of contacts (per day)', x = xvalues_colorbar_label, y = yvalues_colorbar_label, hjust = 1, vjust=0.5, gp = gpar(fontsize = 7.5) )


	## setting for plotting the matrices
	pushViewport(viewport(layout=grid.layout(nrow=nrow_plot,ncol=ncol_plot,width=rep(1,nrow_plot),height=rep(1,nrow_plot))))

	## plot headers
	for (jcol in 1:num_time){
		pushViewport(viewport(layout.pos.row=1, layout.pos.col=jcol))
		grid.text(labels_time[jcol], y=unit(1,'npc')+unit(1,'lines'), gp=gpar(fontsize=10,fontface='bold'))
		popViewport()
	}

	## labels on the left
	for(irow in 1:nrow_plot) 
	{
	  pushViewport(viewport(layout.pos.row=irow, layout.pos.col=1))
	  grid.text(labels_plot_TT[irow], x=unit(0,'npc')-unit(1.5,'lines'), rot = 90, gp=gpar(fontsize=10,fontface='bold'))
	  popViewport()
	}

	## plot the matrices
	iletter = 0;
	yvalues_xaxis_label = unit(-0.65,'lines');
	xvalues_xaxis_label = unit(0.535, 'npc');
	for(irow in 1:nrow_plot)
	{
		for (jcol in 1:ncol_plot){
			m_contacts_pick = cntmat_plot_TT[[irow, jcol]][,,'mean'];

				m_plot = m_contacts_pick; # m_contacts_pick$matrix;
				m_plot[m_plot>max_cnt_colbar] = max_cnt_colbar
				iletter = iletter + 1;
				xletter = fun_letters_new(iletter)
				if (jcol==1){ # plotcontact_multi_left
					fun_plotcontact = plotcontact_multi_left;
				} else { # plotcontact_multi_plain # else if (jcol%in%c(2:length(header_vec)))
					fun_plotcontact = plotcontact_multi_plain;
				}
				pushViewport(viewport(layout.pos.row=irow, layout.pos.col=jcol))
				fun_plotcontact(nrow=num_agegps, ncol=num_agegps, cols=fun_colour_pick(m_plot, max.value = max_cnt_colbar), group = '', a=5, INDEX = xletter, TITLE = '')
				if (irow==nrow_plot){ # x-axis label
					# browser()
					grid.text('Age of participant', x=xvalues_xaxis_label, y=yvalues_xaxis_label,gp=gpar(fontsize=7))
				}
				popViewport()
				
			cnt_matrices_list[[iletter]] = m_plot
			names(cnt_matrices_list)[iletter] = xletter
			cnt_panellabels_mat[irow, jcol] = xletter
				
		} # for- jcol, itime
		
	} # for- irow
	cnt_matrices_byplottype[[iplottype]] = cnt_matrices_list
	cnt_matrices_panellabels_byplottype[[iplottype]] = cnt_panellabels_mat

	if (names(dev.cur())!="windows"){ dev.off()}
	
	# contact matrices
	fname_excelout = sprintf("cnt_matrices_%s_%s%s.xlsx", fname_excelout_base, update_outputdate(), fname_suffix)
	openxlsx::write.xlsx(c(labels=list(cnt_panellabels_mat), cnt_matrices_list), file = fname_excelout, rowNames=TRUE)
	
	# stat of the contact matrices
	names(stat_cntmat_xlsx_TT) = names(cnt_matrices_list)
	fname_excelout = sprintf("stat_cnt_matrices_%s_%s%s.xlsx", fname_excelout_base, update_outputdate(), fname_suffix)
	openxlsx::write.xlsx(c(labels=list(cnt_panellabels_mat), stat_cntmat_xlsx_TT), file = fname_excelout, rowNames=TRUE) 


} # for- iplottype

# export the stat


if (!(names(dev.cur()) %in% c("null device", "windows"))){ dev.off()}

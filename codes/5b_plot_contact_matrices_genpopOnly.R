#### plot contact matrices for each of the four phases

### plot
fun_letters_new = function(x){ # scalar input only
	if (x%%26==0){
		paste(letters[c(floor(x/26)-1, 26)], collapse="")
	} else{
		paste(letters[c(floor(x/26), x%%26)], collapse="")
	}
}


cnt_matrices_byplottype = rep(list(NULL), 2)
cnt_matrices_panellabels_byplottype = rep(list(NULL), 2)

for (iplottype in 1:2){

	# select what to plot
	if (iplottype==1){
		# location of contact
		fname_plot = sprintf('cnt_loc_%s', update_outputdate(short=TRUE))

		cntmat_plot_TT = cntmat_loc_array_genpop
		labels_plot_TT = labels_loc;

		fname_excelout_base = "location"		
		
	} else if (iplottype==2){
		# physical / non-physical contact
		fname_plot = sprintf('cnt_phycnt_%s', update_outputdate(short=TRUE))

		cntmat_plot_TT = cntmat_phy_array_genpop;
		labels_plot_TT = labels_phycnt;
		
		fname_excelout_base = "phy_nonphy"
	}
	fname_plot = paste0(fname_plot, "_genpop")
	fname_excelout_base = paste0(fname_excelout_base, "_genpop")	
	
	## plot 
	## settings
	nrow_plot = nrow(cntmat_plot_TT)
	ncol_plot = ncol(cntmat_plot_TT);
	plotsize = 2;
	fname_plot = paste0(fname_plot, ".pdf")
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
			m_contacts_pick = cntmat_plot_TT[[irow, jcol]];

				m_plot = m_contacts_pick$matrix;
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
	
	fname_excelout = sprintf("cnt_matrices_%s_%s.xlsx", fname_excelout_base, update_outputdate())
	openxlsx::write.xlsx(c(labels=list(cnt_panellabels_mat), cnt_matrices_list), file = fname_excelout, rowNames=TRUE)

} # for- iplottype

# save(cnt_matrices_byplottype, cnt_matrices_panellabels_byplottype, file=sprintf("cnt_matrices_plot_%s.RData", update_outputdate()) )


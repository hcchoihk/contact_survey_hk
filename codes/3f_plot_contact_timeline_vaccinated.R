# plot number of contacts by participant's vaccination status 

numGAM_gps = 2
idx_catg = list(0, 1)

labels_GAM_gps = c('Not vaccinated', 'Vaccinated')
pchs_GAM_gps = c(16, 15)
cols_GAM_gps = COLS[8:9]

chk_plot_vxn_no = chk_plot[chk_plot$vaccinated %in% 0,]
chk_plot_vxn_yes = chk_plot[chk_plot$vaccinated %in% 1,]

chk_plot_TT_list = list(vxn_no = chk_plot_vxn_no, vxn_yes = chk_plot_vxn_yes)


# stat
t_month_stat_contact_list = sapply(chk_plot_TT_list, simplify=FALSE, function(xx_chk_plot) {
	with(xx_chk_plot, aggregate(x = contact_number, by = list(tstep_month), fun_summstat, CI_level = CI_level_plot, PI_level = PI_level_plot))
})

# GAM 
m1_list = sapply(chk_plot_TT_list, simplify=FALSE, function(xx_chk_plot){
	gam(contact_number~s(tstep_month),data = xx_chk_plot)
})
pred_list = sapply(m1_list, simplify=FALSE, function(xm1) {
	preddata = data.frame(tstep_month = seq(min(chk_plot$tstep_month),max(chk_plot$tstep_month)))
	return( predict(xm1, newdata = preddata, type = "response", se.fit = TRUE) )
})


# plot
YMAX = 20

x_pts_base = 1:28
xshift_pts_GAM_gp = 0.2*seq(-1, 1, length.out = numGAM_gps)
x_pts_GAM_gp = sapply(xshift_pts_GAM_gp, simplify=FALSE, function(x) x_pts_base + x)
x_polygon_base = c(x_pts_base, rev(x_pts_base))

fun_plot_cnt_timeline_blank(YMAX = YMAX, YLAB = YLAB)


# points / CI
for (iiGAM in 1:numGAM_gps){
	t_month_stat_contact_TT = t_month_stat_contact_list[[iiGAM]]
	with(t_month_stat_contact_TT, grid.points(x = x_pts_GAM_gp[[iiGAM]][Group.1], y = x[,1], pch=pchs_GAM_gps[iiGAM],
				default.units = 'native',gp = gpar(col = cols_GAM_gps[iiGAM],cex = 0.5)))
	if (plot_CI_YN){
		nullout = with(t_month_stat_contact_TT, fun_errbar_vec(x0 = x_pts_GAM_gp[[iiGAM]][Group.1], x1 = x_pts_GAM_gp[[iiGAM]][Group.1], y0=x[,CI_plot_idx[1]], y1=x[,CI_plot_idx[2]], YMAX=YMAX, default.units = 'native',gp = gpar(col = cols_GAM_gps[iiGAM], lwd = 1)))
	}
}

# GAM fittings
for (iiGAM in 1:numGAM_gps){
	chk_plot_TT = chk_plot_TT_list[[iiGAM]]
	pred_TT = pred_list[[iiGAM]]

	x_pts_catg = x_pts_base[as.numeric(names(table(chk_plot_TT$tstep_month)))]
	if (length(x_pts_catg)<length(x_pts_base)){
		x_pts_catg = head(x_pts_catg,1):tail(x_pts_catg,1)
	} else {
		x_pts_catg = x_pts_base
	}
	if (plot_polygon_YN){
		x_polygon = c(x_pts_catg, rev(x_pts_catg))
		with(pred_TT,  grid.polygon(x = x_polygon, y = c(fit - 1.96 * se.fit, rev(fit + 1.96 * se.fit)), default.units = 'native',gp = gpar(col= NA, fill = cols_GAM_gps[iiGAM], alpha = 0.2)))
	}
	grid.lines(x = x_pts_catg, y = pred_TT$fit,default.units = 'native',gp = gpar(col = cols_GAM_gps[iiGAM], lwd = 2))
} # for- iiGAM


# text labels
if (plot_textlabels_YN){ 
	grid.text(label = 'Unvaccinated',x = 0.98,y=0.88,just = c('right','top'),gp=gpar(col = COLS[8],fontsize=9))
	grid.text(label = 'Vaccinated',x = 0.98,y=0.40,just = c('right','top'),gp=gpar(col = COLS[9],fontsize=9))
}

# legend
if (plot_legend_YN){
	leg_vp_npc = viewport(x = 0.24, y = 0.775, width = 0.25, height = 0.2, just = c("left", "bottom")) # location of the legend
	# pushViewport(leg_vp_npc)
	# grid.draw(rectGrob()) # box of the legend
	grid.legend(labels = labels_GAM_gps,
		pch = pchs_GAM_gps,
		gp = gpar(col=cols_GAM_gps, fill=cols_GAM_gps, cex = 0.75, lwd=1.5),
		vgap = unit(0.15, "lines"), vp=leg_vp_npc
	)
}

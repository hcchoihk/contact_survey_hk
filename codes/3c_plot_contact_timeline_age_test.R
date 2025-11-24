# plot number of contacts by age group

numGAM_gps = 3
idx_catg = list(1:4, 5:12, 13:18)

labels_GAM_gps = c('Age 0 to 20', 'Age 21 to 65', 'Age 66+')
pchs_GAMs_gps = c(16, 15, 18)
cols_GAMS_gps = COLS[1:3]


chk_plot_age0to20 = chk_plot[chk_plot$agegp %in% c(1:4),]
chk_plot_age21to65 = chk_plot[chk_plot$agegp %in% c(5:12),]
chk_plot_age66plus = chk_plot[chk_plot$agegp >= 13,]


# stat
if (FALSE){
t_month$contact_mean_age0to20 = NA
t_month$contact_mean_age21to65 = NA
t_month$contact_mean_age66plus = NA
t_month$contact_mean_age0to20[as.numeric(aggregate(x = chk_plot$contact_number[chk_plot$agegp %in% c(1:4)], 
                                                   by = list(chk_plot$tstep_month[chk_plot$agegp %in% c(1:4)]), mean)[,1])] = 
  as.numeric(aggregate(x = chk_plot$contact_number[chk_plot$agegp %in% c(1:4)], 
                       by = list(chk_plot$tstep_month[chk_plot$agegp %in% c(1:4)]), mean)[,2])
t_month$contact_mean_age21to65[as.numeric(aggregate(x = chk_plot$contact_number[chk_plot$agegp %in% c(5:12)], 
                                                   by = list(chk_plot$tstep_month[chk_plot$agegp %in% c(5:12)]), mean)[,1])] = 
  as.numeric(aggregate(x = chk_plot$contact_number[chk_plot$agegp %in% c(5:12)], 
                       by = list(chk_plot$tstep_month[chk_plot$agegp %in% c(5:12)]), mean)[,2])

t_month$contact_mean_age66plus[as.numeric(aggregate(x = chk_plot$contact_number[chk_plot$agegp >=13], 
                                                    by = list(chk_plot$tstep_month[chk_plot$agegp >=13]), mean)[,1])] = 
  as.numeric(aggregate(x = chk_plot$contact_number[chk_plot$agegp >=13], 
                       by = list(chk_plot$tstep_month[chk_plot$agegp>=13]), mean)[,2])
}


t_month_stat_contact_age0to20 = with(chk_plot_age0to20, aggregate(x = contact_number, 
                       by = list(tstep_month), fun_summstat))
t_month_stat_contact_age21to65 = with(chk_plot_age21to65, aggregate(x = contact_number, 
                       by = list(tstep_month), fun_summstat))
t_month_stat_contact_age66plus = with(chk_plot_age66plus, aggregate(x = contact_number, 
                       by = list(tstep_month), fun_summstat))


# GAM
m1_age0to20= gam(contact_number~s(tstep_month),data = chk_plot_age0to20)
summary(m1_age0to20)
preddata_age0to20 = data.frame(tstep_month = seq(min(chk_plot_age0to20$tstep_month),max(chk_plot_age0to20$tstep_month)))
pred_age0to20 <- predict(m1_age0to20, newdata = preddata_age0to20, type = "response", se.fit = TRUE)

m1_age21to65= gam(contact_number~s(tstep_month),data = chk_plot_age21to65)
summary(m1_age21to65)
preddata_age21to65 = data.frame(tstep_month = seq(min(chk_plot_age21to65$tstep_month),max(chk_plot_age21to65$tstep_month)))
pred_age21to65 <- predict(m1_age21to65, newdata = preddata_age21to65, type = "response", se.fit = TRUE)

m1_age66plus= gam(contact_number~s(tstep_month),data = chk_plot_age66plus)
summary(m1_age66plus)
preddata_m1_age66plus = data.frame(tstep_month = seq(min(chk_plot_age66plus$tstep_month),max(chk_plot_age66plus$tstep_month)))
pred_age66plus <- predict(m1_age66plus, newdata = preddata_m1_age66plus, type = "response", se.fit = TRUE)


# plot 
YMAX = 20
YLAB = 'Mean number of\ncontacts per day'

fun_plot_cnt_timeline_blank(YMAX = YMAX, YLAB = YLAB)

x_pts_base = 1:28
xshift_pts_GAM_gp = 0.2*seq(-1, 1, length.out = numGAM_gps)
x_pts_GAM_gp = sapply(xshift_pts_GAM_gp, simplify=FALSE, function(x) x_pts_base + x)
x_polygon_base = c(x_pts_base, rev(x_pts_base))


	with(t_month_stat_contact_age0to20, grid.points(x = x_pts_GAM_gp[[1]][Group.1], y = x[,1], pch=16, # mean = first column within $x
            default.units = 'native',gp = gpar(col = COLS[1],cex = 0.5)))
	with(t_month_stat_contact_age21to65, grid.points(x = x_pts_GAM_gp[[2]][Group.1], y = x[,1], pch=15,
				default.units = 'native',gp = gpar(col = COLS[2],cex = 0.5)))
	with(t_month_stat_contact_age66plus, grid.points(x = x_pts_GAM_gp[[3]][Group.1], y = x[,1], pch=18,
				default.units = 'native',gp = gpar(col = COLS[3],cex = 0.5)))

	with(t_month_stat_contact_age0to20, grid.segments(x0 = x_pts_GAM_gp[[1]][Group.1], x1 = x_pts_GAM_gp[[1]][Group.1], y0 = x[,5], y1 = x[,6], 
		default.units = 'native',gp = gpar(col = COLS[1], lex = 1)))
	with(t_month_stat_contact_age21to65, grid.segments(x0 = x_pts_GAM_gp[[2]][Group.1], x1 = x_pts_GAM_gp[[2]][Group.1], y0 = x[,5], y1 = x[,6], 
		default.units = 'native',gp = gpar(col = COLS[2], lex = 1)))
	with(t_month_stat_contact_age66plus, grid.segments(x0 = x_pts_GAM_gp[[3]][Group.1], x1 = x_pts_GAM_gp[[3]][Group.1], y0 = x[,5], y1 = x[,6], 
		default.units = 'native',gp = gpar(col = COLS[3], lex = 1)))

	with(t_month_stat_contact_age0to20, grid.segments(x0 = x_pts_GAM_gp[[1]][Group.1], x1 = x_pts_GAM_gp[[1]][Group.1], y0 = x[,5], y1 = x[,6], 
		default.units = 'native',gp = gpar(col = COLS[1], lex = 1)))


	with(t_month_stat_contact_age0to20, fun_errbar(x0 = x_pts_GAM_gp[[1]][Group.1][1], x1 = x_pts_GAM_gp[[1]][Group.1][1], y0 = x[1,5], y1 = x[1,6], YMAX=YMAX,
		default.units = 'native',gp = gpar(col = COLS[1], lex = 1)))

	with(t_month_stat_contact_age0to20, mapply(function(x0, x1, y0, y1) 
			 fun_errbar(x0 = x0, x1 = x1, y0 = x0, y1 = x1, YMAX=YMAX, default.units = 'native',gp = gpar(col = COLS[1], lex = 1)), 
		x0 = x_pts_GAM_gp[[1]][Group.1], x1 = x_pts_GAM_gp[[1]][Group.1], y0 = x[,5], y1 = x[,6]))

	nullout = with(t_month_stat_contact_age0to20, sapply(1:length(t_month_stat_contact_age0to20$Group.1), function(ii)
			 fun_errbar(x0 = x_pts_GAM_gp[[1]][Group.1][ii], x1 = x_pts_GAM_gp[[1]][Group.1][ii], y0 = x[ii,5], y1 = x[ii,6], YMAX=YMAX, 
			 default.units = 'native',gp = gpar(col = COLS[1], lex = 1))))

	nullout = with(t_month_stat_contact_age0to20, mapply(function(x00, x11, y00, y11)
			 fun_errbar(x0 = x00, x1 = x11, y0 = y00, y1 = y11, YMAX=YMAX, default.units = 'native',gp = gpar(col = COLS[1], lex = 1)), 
			 x00 = x_pts_GAM_gp[[1]][Group.1], x11 = x_pts_GAM_gp[[1]][Group.1], y00=x[,5], y11=x[,6]))

	nullout = with(t_month_stat_contact_age0to20, fun_errbar_vec(x0 = x_pts_GAM_gp[[1]][Group.1], x1 = x_pts_GAM_gp[[1]][Group.1], y0=x[,5], y1=x[,6], YMAX=YMAX, default.units = 'native',gp = gpar(col = COLS[1], lex = 1)))
	nullout = with(t_month_stat_contact_age21to65, fun_errbar_vec(x0 = x_pts_GAM_gp[[2]][Group.1], x1 = x_pts_GAM_gp[[2]][Group.1], y0=x[,5], y1=x[,6], YMAX=YMAX, default.units = 'native',gp = gpar(col = COLS[2], lex = 1)))
	nullout = with(t_month_stat_contact_age66plus, fun_errbar_vec(x0 = x_pts_GAM_gp[[3]][Group.1], x1 = x_pts_GAM_gp[[3]][Group.1], y0=x[,5], y1=x[,6], YMAX=YMAX, default.units = 'native',gp = gpar(col = COLS[3], lex = 1)))
	

	with(t_month_stat_contact_age0to20, grid.segments(x0 = x_pts_GAM_gp[[1]][Group.1], x1 = x_pts_GAM_gp[[1]][Group.1], y0 = x[,5], y1 = x[,6], 
		default.units = 'native',gp = gpar(col = COLS[1], lex = 1)))
	with(t_month_stat_contact_age21to65, grid.segments(x0 = x_pts_GAM_gp[[2]][Group.1], x1 = x_pts_GAM_gp[[2]][Group.1], y0 = x[,5], y1 = x[,6], 
		default.units = 'native',gp = gpar(col = COLS[2], lex = 1)))
	with(t_month_stat_contact_age66plus, grid.segments(x0 = x_pts_GAM_gp[[3]][Group.1], x1 = x_pts_GAM_gp[[3]][Group.1], y0 = x[,5], y1 = x[,6], 
		default.units = 'native',gp = gpar(col = COLS[3], lex = 1)))







if (FALSE){
	grid.points(x = x_pts_GAM_gp[[1]][t_month_stat_contact_age0to20[,1]], y = c(t_month$contact_mean_age0to20[1:28]),pch=16,
				default.units = 'native',gp = gpar(col = COLS[1],cex = 0.5))
	grid.points(x = x_pts_GAM_gp[[2]], y = c(t_month$contact_mean_age21to65[1:28]),pch=15,
				default.units = 'native',gp = gpar(col = COLS[2],cex = 0.4))
	grid.points(x = x_pts_GAM_gp[[3]] ,y = c(t_month$contact_mean_age66plus[1:28]),pch=18,
				default.units = 'native',gp = gpar(col = COLS[3],cex = 0.5,alpha = 0.8))
}

x_pts_catg = x_pts_base[as.numeric(names(table(chk_plot_age0to20$tstep_month)))]
if (length(x_pts_catg)<length(x_pts_base)){
	x_pts_catg = head(x_pts_catg,1):tail(x_pts_catg,1)
} else {
	x_pts_catg = x_pts_base
}
if (FALSE){
x_polygon = c(x_pts_catg, rev(x_pts_catg))
grid.polygon(x = x_polygon, y = c(pred_age0to20$fit - 1.96 * pred_age0to20$se.fit,rev(pred_age0to20$fit + 1.96 * pred_age0to20$se.fit)),
             default.units = 'native',gp = gpar(col= NA, fill = COLS[1], alpha = 0.2))
}
grid.lines(x = x_pts_catg, y = pred_age0to20$fit,default.units = 'native',gp = gpar(col = COLS[1],lwd = 2,alpha = 0.8))

x_pts_catg = x_pts_base[as.numeric(names(table(chk_plot_age21to65$tstep_month)))]
if (length(x_pts_catg)<length(x_pts_base)){
	x_pts_catg = head(x_pts_catg,1):tail(x_pts_catg,1)
} else {
	x_pts_catg = x_pts_base
}
if (FALSE){
x_polygon = c(x_pts_catg, rev(x_pts_catg))
grid.polygon(x = x_polygon, y = c(pred_age21to65$fit - 1.96 * pred_age21to65$se.fit,rev(pred_age21to65$fit + 1.96 * pred_age21to65$se.fit)),
             default.units = 'native',gp = gpar(col= NA, fill = COLS[2], alpha = 0.2))
}
grid.lines(x = x_pts_catg,y = pred_age21to65$fit,default.units = 'native',gp = gpar(col = COLS[2],lwd = 2,alpha = 0.8))

x_pts_catg = x_pts_base[as.numeric(names(table(chk_plot_age66plus$tstep_month)))]
if (length(x_pts_catg)<length(x_pts_base)){
	x_pts_catg = head(x_pts_catg,1):tail(x_pts_catg,1)
} else {
	x_pts_catg = x_pts_base
}
if (FALSE){
x_polygon = c(x_pts_catg, rev(x_pts_catg))
grid.polygon(x = x_polygon, y = c(pred_age66plus$fit - 1.96 * pred_age66plus$se.fit,rev(pred_age66plus$fit + 1.96 * pred_age66plus$se.fit)),
             default.units = 'native',gp = gpar(col= NA, fill = COLS[3], alpha = 0.2))
}
grid.lines(x = x_pts_catg, y = pred_age66plus$fit,default.units = 'native',gp = gpar(col = COLS[3],lwd = 2,alpha = 0.8))

if (FALSE){
	grid.text(label = 'Age 0 to 20', x = 0.95,y=0.95,just = c('right','top'),gp=gpar(col = COLS[1],fontsize=9))
	grid.text(label = 'Age 21 to 65', x = 0.95,y=0.76,just = c('right','top'),gp=gpar(col = COLS[2],fontsize=9))
	grid.text(label = 'Age 66+', x = 0.95,y=0.4,just = c('right','top'),gp=gpar(col = COLS[3],fontsize=9))
}

# legend



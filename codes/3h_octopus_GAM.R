### generalized additive model (GAM) on octopus data


library(mgcv) # Mixed GAM Computation Vehicle with Automatic Smoothness Estimation
# library(ggplot2) # to get the smoothing
# library(tidymv) # to get the smoothing CI

# date
date_input_format = "%Y-%m-%d"

month_full_vec = format(as.Date(sprintf("2024-%02d-01", 1:12), date_input_format), "%B")
month_short_vec = substr(month_full_vec, 1,3)
month_1_vec = substr(month_full_vec, 1,1)


date_datafolder = as.Date("2024-04-30", date_input_format)
date_datastart = as.Date("2020-01-01", date_input_format)
date_dataend = as.Date("2024-04-29", date_input_format) # date_datafolder - 1

date_studystart = as.Date("2021-09-17", date_input_format)
date_studyend = as.Date("2023-12-31", date_input_format)


### laod data
load( 'data/data_octopus_relMobility.RData' )
dd_octopus = data_octopus_relmobility;

cardtype = c("Adult", "Child", "Student", "Elderly")
cardtype_num = length(cardtype)
colnames_octdata_pick = paste0("Octopus_", cardtype)

# GAM
dd_octopus_pick = subset(dd_octopus, subset=c(date_studystart<=dd_octopus$Date & dd_octopus$Date<=date_studyend))
YearMon_num_dd_octopus_pick = sort(unique(dd_octopus_pick$YearMon_num)) # check: all(YearMon_num_dd_octopus_pick %in% dd_octopus_pick$YearMon_num)

YearMon_num_newd_smooth = approx(x=1:length(YearMon_num_dd_octopus_pick), y=YearMon_num_dd_octopus_pick, xout=seq(from=1, to=length(YearMon_num_dd_octopus_pick), by=0.25))$y 
newdata_pred = data.frame(YearMon_num=rep(YearMon_num_newd_smooth, each=2), weekend=rep(c(0,1), times=length(YearMon_num_newd_smooth)))

CI_level = 0.95
CI_alpha = 1 - CI_level
z_alpha = qnorm(CI_alpha/2, lower.tail=FALSE)

# create GAM objects
gam_smooth_bycard_overall = rep(list(NULL), cardtype_num)
newdata_pred_overall = data.frame(YearMon_num=YearMon_num_newd_smooth)

for (icard in 1:cardtype_num){

# https://cran.r-project.org/web/packages/tidymv/vignettes/predict-gam.html

octdata_pick_T = colnames_octdata_pick[icard]

dd_octopus_ext = subset(dd_octopus_pick, select=c(octdata_pick_T, "YearMon_num")) # extract data
colnames(dd_octopus_ext)[1] = "y"

gam_out = gam(y ~ s(YearMon_num) , data=dd_octopus_ext) # smoothing YearMon_num

gam_out_pred = predict(gam_out, se.fit=TRUE, newdata=newdata_pred_overall)
gam_out_pred = cbind(newdata_pred_overall, as.data.frame(gam_out_pred))
gam_out_pred = within(gam_out_pred, {
	lowlim = fit - z_alpha*se.fit
	upplim = fit + z_alpha*se.fit
})

gam_smooth_bycard_overall[[icard]] = gam_out_pred
# with(subset(gam_out_pred, subset=gam_out_pred$weekend==0), matplot(YearMon_num, y=cbind(fit,lowlim,upplim), type='l', lty=1, lwd=2))

} # icard-gam_pred


### plot

## setting
# col codes 
colcode = list(c(0, 0.4470, 0.7410), # sea blue
	c(0.8500, 0.3250, 0.0980), # brick red
	c(0.9290, 0.6940, 0.1250), # orange
	c(0.4940, 0.1840, 0.5560) # purple
	)
colcode_hex = sapply(colcode, function(x) do.call(rgb, as.list(x)));
colcode_hex_alpha = sapply(colcode_hex,  adjustcolor, alpha.f=0.2)


fun_date_num = function(d){ as.numeric(format(d,"%Y")) + (as.numeric(format(d,"%m"))-1)/12} # convert from Date to numeric, set as 1st day of the month

date_studystart_mon = as.Date(sprintf("%s-%s-01", format(date_studystart, "%Y"), format(date_studystart, "%m")), date_input_format)
date_studyend_mon = as.Date(sprintf("%s-%s-01", format(date_studyend, "%Y"), format(date_studyend, "%m")), date_input_format)

date_studystart_mon_num = fun_date_num(date_studystart_mon)
date_studyend_mon_num = fun_date_num(date_studyend_mon)


xlim_plot = c(date_studystart_mon_num, date_studyend_mon_num+1/12)
ylim_plot = c(0, 1.5)

xtick_all = seq(from=xlim_plot[1], to=xlim_plot[2], by=1/12)
xtick_major = unique(floor(xtick_all))
xtick_minor = base::setdiff(xtick_all, xtick_major)
xlabel_at = YearMon_num_dd_octopus_pick
xlabel_at_labels = month_1_vec[12*(xlabel_at%%1)+1]

xlabel_at_major = sapply(xtick_major, simplify=TRUE, function(y) mean(xlabel_at[which(floor(xlabel_at)==y)]))


# start plotting
fname_plot = sprintf("octopus_GAM_overall_%s", update_outputdate())
plot_width = 8; plot_height = 3
pdf(sprintf("%s.pdf",fname_plot), width=plot_width, height=plot_height)
# windows(width=plot_width, height=plot_height)

par(mar=c(2,4,0.5,0.5))
plot(NA, xlab="", ylab="", xlim=xlim_plot, ylim=ylim_plot, axes=FALSE, bty="L", xaxs="i", yaxs="i")

# add ref lines/region under axes
# indicate the period of the 5th wave
date_5thwave = c("2022-01-07","2022-04-20")
date_5thwave_num = 2022+lubridate::yday(as.Date(date_5thwave, date_input_format))/365
# use lines
# abline(v = date_5thwave_num, col='grey', lwd=1, lty=1)
# or use a small arrow
if (TRUE){
	polygon(x=c(date_5thwave_num, rev(date_5thwave_num)), y=rep(par()$yaxp[1:2]*c(1,1.1), each=2), col='grey80', border=FALSE) # y=rep(ylim_plot, each=2)
	text(x=mean(date_5thwave_num), y=par()$yaxp[2], labels="5th wave", adj=c(0.5,0.5), col='white', font=2, cex=0.65) # y=ylim_plot[2]*0.9
	# reference lines
	abline(h = 1, col='black', lwd=1, lty=2)
}


# axes
# x-axis
axis(1, at=xtick_major, labels=FALSE, col=NA, col.ticks='black', tck=-0.15)
axis(1, at=xtick_minor, labels=FALSE, col=NA, col.ticks='black', tck=-0.05)
axis(1, at=xlim_plot, labels=FALSE, col='black', col.ticks=NA, tck=0)
mtext(side=1, text=xlabel_at_labels, at=xlabel_at, cex=0.65, adj=0.5, line=-0.25, padj=0)
mtext(side=1, text=xtick_major[!is.na(xlabel_at_major)], at=xlabel_at_major[!is.na(xlabel_at_major)], cex=0.85, adj=0.5, line=0.75)
# y-axis
axis(2, las=1, labels=FALSE, cex=0.8, tck=0)
ytick_major = with(par(), seq(yaxp[1], yaxp[2], length.out=yaxp[3]+1))
ytick_minor = with(par(), seq(yaxp[1], yaxp[2], by=0.1))
ytick_minor = base::setdiff(ytick_minor, ytick_major)
axis(2, at=ytick_major, labels=NA, col=NA, col.ticks='black', tck=-0.05, las=1)
axis(2, at=ytick_minor, labels=NA, col=NA, col.ticks='black', tck=-0.02)
mtext(side=2, las=1, at=ytick_major, text=ytick_major, cex=0.8, line=0.85)
mtext(side=2, text="Relative mobility", line=2.25) # "Normalized octopus transactions"


for (icard in 1:cardtype_num){
# lines and regions
gam_T = gam_smooth_bycard_overall[[icard]]
	polygon(x=c(YearMon_num_newd_smooth, rev(YearMon_num_newd_smooth)), y=c(gam_T$lowlim, rev(gam_T$upplim)), col=colcode_hex_alpha[icard], border=NA)
	lines(x=YearMon_num_newd_smooth, y=gam_T$fit, col=colcode_hex[icard], lwd=2)
} # icard


# legend
# legend(x=xlim_plot[2], y=mean(ytick_major[1:2]), legend=cardtype, xjust=1, yjust=0.5, col=colcode_hex, lwd=2, bty="o", xpd=TRUE)
# legend(x=xlim_plot[2], y=0.05, legend=cardtype, xjust=1, yjust=0, col=colcode_hex_alpha, lwd=10, bty="n", text.col="transparent", bg="transparent") # to include color for the regions # add shaded region in legened
legend(x=xlim_plot[2], y=0.05, legend=cardtype, xjust=1, yjust=0, col=colcode_hex, lwd=2, bty="o", xpd=TRUE)

if (names(dev.cur())!="windows"){ dev.off()}


## correlation, compared to the survey data, by age group

# split the general population into 4 age groups 
numGAM_gps = 4
idx_catg = list(1:2, 3:4, 5:13, 14:18)

chk_plot_TT_list = lapply(idx_catg, function(xidx)
	chk_plot_genpop[chk_plot_genpop$agegp %in% idx_catg[[1]],])

# GAM
m1_list = sapply(chk_plot_TT_list, simplify=FALSE, function(xx_chk_plot){
	gam(contact_number~s(tstep_month),data = xx_chk_plot)
})
pred_list_4agegps = sapply(1:numGAM_gps, simplify=FALSE, function(iiGAM) {
	xm1 = m1_list[[iiGAM]]
	chk_plot_TT = chk_plot_TT_list[[iiGAM]]
	preddata = data.frame(tstep_month = as.numeric(names(table(chk_plot_TT$tstep_month))))
	return( predict(xm1, newdata = preddata, type = "response", se.fit = TRUE) )
})


# smoothed data for octopus data 
# convert weekly data to monthly data
month_decimal = seq(0, by=1/12, length.out=12)

date_pred = data.frame( year=c(rep(2021,4), rep(2022,12), rep(2023,12), 2024),
month=c(9:12, 1:12, 1:12,1) )
date_pred$month_num = with(date_pred, year + month_decimal[month])


gam_smooth_bycard_overall_month = rep(list(NULL), 4)

for (icard in 1:4){
	gam_T = gam_smooth_bycard_overall[[icard]]
	# convert to gam by month
	
	gam_T_month = cut(gam_T$YearMon_num, breaks=date_pred$month_num, include.lowest=TRUE, right=FALSE, labels=FALSE)
	
	gam_smooth_bycard_overall_month[[icard]] = data.frame(YearMon_num=head(date_pred$month_num,-1), mean_fit=as.numeric(by(data=gam_T$fit, INDICES=gam_T_month, mean)))
}

df_octopus_gam_smooth_month = data.frame("YearMon_num"=gam_smooth_bycard_overall_month[[icard]][,"YearMon_num"], 
do.call(cbind, sapply(1:4, simplify=FALSE, function(icard) gam_smooth_bycard_overall_month[[icard]][,"mean_fit"])))
colnames(df_octopus_gam_smooth_month)[1 + (1:4)] = cardtype


# correlation 
for (icard in 1:4){
	pred_TT = pred_list_4agegps[[icard]]$fit

	xcard = cardtype[icard];
	octopus_TT = df_octopus_gam_smooth_month[,xcard]
	octopus_TT = octopus_TT[as.numeric(names(pred_TT))]
	
	print( sprintf('Octopus: %s', xcard) )

	print( sapply(c("pearson", "kendall", "spearman"), function(xmethod) cor(pred_TT, octopus_TT, method=xmethod)) )

} # for- icard


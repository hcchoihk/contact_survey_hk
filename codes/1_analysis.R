### analysis

# convert a table to a matrix
fun_tab2mat = function(tab){
	matrix(tab, ncol=ncol(tab), dimnames=dimnames(tab));
}

fname_otherAnaly = sprintf('analysis_%s.xlsx', update_outputdate(short=TRUE))

# number of participants, instead of number of surveys
iiSpg_labels = c("General population", "High-contact group", "All participants")

chk_base_part_id = chk_base$part_id
unique_part_id = unique(chk_base_part_id)
irow_unique_part_id = sapply(unique_part_id, function(x_id) which(chk_base_part_id==x_id)[1])

# get_chk_unique_part_YN = !exists("dd_chk_unique_part")
chk_unique_part = chk_base[irow_unique_part_id, , drop=FALSE]

dd_chk_unique_part = subset(chk_unique_part, select=c(part_id, agegp, special_grps_YN, occupation, waveperiod_4gp, vaccinated, agegp_020_2165_65p, agegp_vaccRate))
dd_chk_unique_part = dplyr::rename(dd_chk_unique_part, part_age = agegp) # rename


## 1. Table S1, demographics
demovar_vec = c('agegp', 'sex', 'working_status', 'working_role', 'education', 'income', 'birthplace', 'ethnicity', 'hkpr', 'vaccinated', 'household_number', 'household_type', paste0('spg',1:8), 'special_grps_YN')
if (length(which(!(demovar_vec %in% colnames(chk_unique_part))))>0){
	print( sprintf('check variable name(s): %s', paste(demovar_vec[which(!(demovar_vec %in% colnames(chk_unique_part)))], sep=", ")) )
}

table_count = sapply(demovar_vec, simplify=FALSE, function(xvar) {
	tab_TT = table(chk_unique_part[,xvar]);
	tab_TT = matrix(tab_TT, byrow=FALSE, dimnames=list(names(tab_TT)));
	pct_TT = 100*prop.table(tab_TT)
	text_TT = sprintf("%d (%.0f%%)", tab_TT, pct_TT)
	rownames_tab_TT = rownames(tab_TT)
	rownames_tab_TT[is.na(rownames_tab_TT)] = "_NA_"
	data.frame(tab_TT, pct_TT, text_TT, row.names=rownames_tab_TT)
})

table_count_out = sapply(1:length(table_count), simplify=FALSE, function(ii){
	tab_TT = table_count[[ii]]
	tab_TT = rbind("var"=rep(NA, 3), tab_TT, "blank"=rep(NA,3))
	rownames(tab_TT) = paste(names(table_count)[ii], rownames(tab_TT), sep="_")
	return(tab_TT)
})

table_count_out = do.call(rbind, table_count_out)

# distribution of high-contact group
table_count_out_spg = sapply(paste0('spg',1:8), function(xvar) as.numeric(table(chk_unique_part[,xvar])['1']))
table_count_out_spg = c(table_count_out_spg, any_spg=as.numeric(table(chk_unique_part[,'special_grps_YN'])['1']))
table_count_out_spg = data.frame(count = table_count_out_spg, percentage = 100*table_count_out_spg/table_count_out_spg['any_spg'])

print( "distribution of high-contact group. % within participants who belong to any of the high-contact groups." )
print( do.call(rbind, sapply(1:8, simplify=FALSE, function(ispg) sprintf("%s: %d (%.0f%%)", rownames(table_count_out_spg)[ispg], table_count_out_spg[ispg,1], table_count_out_spg[ispg,2]))) )


## 2. vaccinated rate by age group and wave
# print( 'vaccinated rate by age group and wave' )
vaccRate_out_list = rep(list(NULL), 3)
names(vaccRate_out_list) = c("gen_pop", "high_contact", "all_participants")


# with(dd_chk_unique_part, table(age_raw, agegp_vaccRate))
for (iiSpg_YN in c(1,2,3)){

	if (iiSpg_YN %in% c(1,2)){
		chk_TT = subset(dd_chk_unique_part, subset=(special_grps_YN==(iiSpg_YN-1))) # note: special_grps_YN = 0 (gen_pop) / 1 (high_contact)
	} else if (iiSpg_YN==3){
		chk_TT = dd_chk_unique_part
	}
	
	count_byVacc = by(chk_TT, chk_TT$vaccinated, function(dd) table(dd$waveperiod_4gp, dd$agegp_vaccRate))
	
	# convert table to matrix
	# my_matrix <- matrix(my_table, ncol=ncol(my_table), dimnames=dimnames(my_table))
	count_total = Reduce("+", count_byVacc)
	count_total = fun_tab2mat(count_total);
#	pct_vaccinated = array(list(NULL), dim=dim(count_total))
	pct_vaccinated_text0 = array("", dim=dim(count_total))
	pct_vaccinated_text1 = array("", dim=dim(count_total))
	for (irow in 1:nrow(count_total)){
		for (jcol in 1:ncol(count_total)){
			TT = binom.test(count_byVacc[['1']][irow, jcol], count_total[irow, jcol])
			TT = 100*unlist(TT[c('estimate', 'conf.int')])
			pct_vaccinated_text0[irow, jcol] = sprintf('%.0f (%.0f, %.0f)', TT[1], TT[2], TT[3])
			pct_vaccinated_text1[irow, jcol] = sprintf('%.1f (%.1f, %.1f)', TT[1], TT[2], TT[3])
		}
	}
	
	colnames(pct_vaccinated_text0) = colnames(count_total)
	colnames(pct_vaccinated_text1) = colnames(count_total)
	
	vaccRate_out_TT = cbind(as.data.frame(count_total), as.data.frame(pct_vaccinated_text0), as.data.frame(pct_vaccinated_text1))
	vaccRate_out_TT = rbind("_"=rep(NA, ncol(vaccRate_out_TT)), vaccRate_out_TT, "blank"==rep(NA, ncol(vaccRate_out_TT)))
	rownames(vaccRate_out_TT) = paste(names(vaccRate_out_list)[iiSpg_YN], rownames(vaccRate_out_TT), sep="_")
	
	vaccRate_out_list[[iiSpg_YN]] = vaccRate_out_TT
} # for- iiSpg_YN

vaccRate_out = do.call(rbind, vaccRate_out_list)

openxlsx::write.xlsx(x=list("var_count"=table_count_out, "spg_count" = table_count_out_spg, "vaccRate"=vaccRate_out), file=fname_otherAnaly, rowNames=TRUE)


# 3. plots for the distribution of characteristics of contacts
# supplementary 

# library(dplyr)
# library(ggplot2)

load( 'data/chk_part_contact_base.RData' )


## a. by age of participants and location of contact
num_agegps = 16
startage_agegps = seq(6, by=5, length.out=num_agegps-1)
labels_agegps = c("0-5", paste(head(startage_agegps,-1), tail(startage_agegps,-1)-1, sep='-'), paste0(tail(startage_agegps,1),"+"))

chk_part_base$part_age_16gps = chk_part_base$part_age
chk_part_base$part_age_16gps[chk_part_base$part_age_16gps>16] = 16

chk_contact_base$part_age_16gps = chk_contact_base$part_age
chk_contact_base$part_age_16gps[chk_contact_base$part_age_16gps>16] = 16


labels_loc = c("Home", "School", "Work", "Others")
COLS = rev(ggpubr::get_palette(palette = "YlGn", length(labels_loc)))

# plot 
par_mar = c(3.25, 4, 3, 2)

ytick_major = seq(0,1, by=0.2)
ytick_minor = setdiff(seq(0,1, by=0.1), ytick_major)

labels_agegps_YN = FALSE;
srt_labels_agegps = ifelse(labels_agegps_YN, 0, 30);
cex_labels_agegps = ifelse(labels_agegps_YN, 0.8, 1);
y_labels_agegps = ifelse(labels_agegps_YN, -0.035, -0.05);

fname_plot = sprintf("dist_partage_cntloc_%s.pdf", update_outputdate())
plot_width = 8;
plot_height = 5*2;
plot_pdf_YN = TRUE;
if (plot_pdf_YN){
	pdf(fname_plot, width=plot_width, height=plot_height)
} else{
	windows(width=plot_width, height=plot_height)
}
par(mar = par_mar)
layout(rbind(1,2))


for (iiSpg_YN in 0:1){

	chk_contact_TT = chk_contact_base
	chk_contact_TT = subset(chk_contact_TT, subset=part_specialgroups_YN==iiSpg_YN)	

	table_partage_cntloc = matrix(0, nrow=16, ncol=4)
	xvar_cntloc_vec = paste0("cnt_", c("home","school","work","others"))
	for (ivar in 1:4){
		xvar_cntloc = xvar_cntloc_vec[ivar]
		tab_TT = table(chk_contact_TT[, c("part_age_16gps", xvar_cntloc)])
		
		table_partage_cntloc[as.numeric(rownames(tab_TT)), ivar] = tab_TT[,'1']
	}
	proptable_partage_cntloc = t(apply(table_partage_cntloc, 1, prop.table))
	agesum_table_partage_cntloc = rowSums(table_partage_cntloc)
	
	labels_agegps_plot = labels_agegps;
	if (labels_agegps_YN){ # include group size
		labels_agegps_n_plot = sprintf("(%s)", trimws(format(agesum_table_partage_cntloc, big.mark=",")))
	}

	plot(NA, xlim=c(1,num_agegps)+0.5*c(-1,1), ylim=c(0,1), axes=FALSE, xlab="", ylab="", xaxs="i", yaxs="i")
	for (ii in 1:num_agegps){
		xx_prop = proptable_partage_cntloc[ii, ]
		xx_prop_cumsum = c(0, cumsum(xx_prop))

		bar_width = 0.475;
		for (ivar in 1:4){
			polygon(x=c(ii+bar_width*c(-1,1), rev(ii+bar_width*c(-1,1))), y=c(rep(xx_prop_cumsum[ivar],2), rep(xx_prop_cumsum[ivar+1],2)), col=COLS[ivar], border=NA)
		}
	} # for- ii (age)
	if (iiSpg_YN==0){
		legend("topright", legend=labels_loc, horiz=TRUE, col="blank", fill=COLS, cex=1.1, inset=c(0, -0.175), xpd=TRUE, bty="n")
	}

	axis(side=2, at=ytick_major, labels=100*ytick_major, cex=1.1, pos=0.4, las=1)
	mtext(side=2, text="Proportion (%)", line=2.5, cex=1.2)
	text(x=1:num_agegps, y=y_labels_agegps, labels=labels_agegps_plot, xpd=TRUE, srt=srt_labels_agegps, cex=cex_labels_agegps)
	if (labels_agegps_YN){
		text(x=1:num_agegps, y=-0.085, labels=labels_agegps_n_plot, xpd=TRUE, srt=0, cex=0.7)
	}
	mtext(side=1, text="Age of participants", line=2, cex=1.2)


	# label for panel
	label_panel = sprintf("(%s)", letters[iiSpg_YN+1])
	label_panel = sprintf("(%s) Participants from %s", letters[iiSpg_YN+1], ifelse(iiSpg_YN==1, "high-contact groups", "the general population"))
	mtext(side=3, text=label_panel, at=-0.5, line=0.5, adj=c(0, 0), cex=1.1)

} # for- iiSpg_YN

if (plot_pdf_YN){ dev.off()}


## b. by location and frequency and duration

labels_dur = c('<5 mins', '5-14 mins', '15-59 mins', '1-4 hrs', '>4 hrs')
labels_freq = c('Daily or almost daily', 'Once or twice a week', 'Once or twice a month', 'Less than once a month', 'Never met before')
labels_freq = c('Daily\n', 'Weekly\n', 'Monthly\n', 'Less than\na month', 'First time\n')
labels_loc = c("Home", "School", "Work", "Others")

num_dur_catg = 5;
num_freq_catg = 5;
num_loc_catg = 4;


par_mar_list = list('0'=c(3.25, 5.5, 6, 0.5), '1'=c(3.25, 3.5, 6, 2.5))

fname_plot = sprintf("dist_cntloc_freq_dur_%s.pdf", update_outputdate())
plot_width = 5*2;
plot_height = 4*3;

plot_pdf_YN = TRUE;
if (plot_pdf_YN){
	pdf(fname_plot, width=plot_width, height=plot_height)
} else{
	windows(width=plot_width, height=plot_height)
}
layout(matrix(1:6, ncol=2, byrow=FALSE))

idx_plot = 0;
for (iiSpg_YN in 0:1){

	par(mar=par_mar_list[[as.character(iiSpg_YN)]])
	
	chk_contact_TT = chk_contact_base
	chk_contact_TT = subset(chk_contact_TT, subset=part_specialgroups_YN==iiSpg_YN)	
	
	
	# (i) loc x dur
	idx_plot = idx_plot + 1
	COLS = rev(ggpubr::get_palette(palette = "YlGn", num_dur_catg))
	
	table_cntloc_dur = matrix(0, nrow=num_loc_catg, ncol=num_dur_catg)
	xvar_cntloc_vec = paste0("cnt_", c("home","school","work","others"))
	for (yy in 1:num_loc_catg){
		xvar_cntloc = xvar_cntloc_vec[yy]
		tab_TT = table(chk_contact_TT[, c(xvar_cntloc, "duration_multi")])
		table_cntloc_dur[yy, as.numeric(colnames(tab_TT))] = tab_TT['1',]
	}
	proptable_cntloc_dur = t(apply(table_cntloc_dur, 1, prop.table))
	xsum_table_cntloc_dur = rowSums(table_cntloc_dur)

	xlim_plot = c(1,num_loc_catg) + 0.5*c(-1,1)
	plot(NA, xlim=c(1,num_loc_catg)+0.5*c(-1,1), ylim=c(0,1), axes=FALSE, xlab="", ylab="", xaxs="i", yaxs="i")
	for (ii in 1:num_loc_catg){
		xx_prop = proptable_cntloc_dur[ii, ]
		xx_prop_cumsum = c(0, cumsum(xx_prop))

		bar_width = 0.475;
		for (yy in 1:num_dur_catg){
			polygon(x=c(ii+bar_width*c(-1,1), rev(ii+bar_width*c(-1,1))), y=c(rep(xx_prop_cumsum[yy],2), rep(xx_prop_cumsum[yy+1],2)), col=COLS[yy], border=NA)
		}
	} # for- ii

	if (iiSpg_YN==1){
		legend("topright", legend=labels_dur, horiz=TRUE, col="blank", fill=COLS, cex=1.5, inset=c(0, -0.225), xpd=NA, bty="n", text.width=strwidth(labels_dur)/max(strwidth(labels_dur)), x.intersp=0.75)
	}

	axis(side=2, at=ytick_major, labels=if(iiSpg_YN==0) 100*ytick_major else FALSE, cex.axis=1.2, pos=0.4, las=1)
	if (iiSpg_YN==0){
		mtext(side=2, text="Proportion (%)", line=3.5, cex=1.2)
	}
	text(x=1:num_loc_catg, y=-0.05, labels=labels_loc, xpd=TRUE, srt=0, cex=1.2)
	mtext(side=1, text="Location of contacts", line=2, cex=1)

	# label for panel
	label_panel = sprintf("(%s)", letters[idx_plot])
	mtext(side=3, text=label_panel, at=0.15, line=0.75, adj=c(0, 0), cex=1.1)

	# column label
	label_column = sprintf("Participants from %s", ifelse(iiSpg_YN==1, "high-contact groups", "the general population"))
	mtext(side=3, text=label_column, line=4.5, cex=1.2)


	# (ii) loc x freq
	idx_plot = idx_plot + 1
	COLS = rev(ggpubr::get_palette(palette = "YlGn", num_freq_catg))
	
	table_cntloc_freq = matrix(0, nrow=num_loc_catg, ncol=num_freq_catg)
	xvar_cntloc_vec = paste0("cnt_", c("home","school","work","others"))
	for (yy in 1:num_loc_catg){
		xvar_cntloc = xvar_cntloc_vec[yy]
		tab_TT = table(chk_contact_TT[, c(xvar_cntloc, "frequency_multi")])
		table_cntloc_freq[yy, as.numeric(colnames(tab_TT))] = tab_TT['1',]
	}
	proptable_cntloc_freq = t(apply(table_cntloc_freq, 1, prop.table))
	xsum_table_cntloc_freq = rowSums(table_cntloc_freq)

	xlim_plot = c(1,num_loc_catg) + 0.5*c(-1,1)
	plot(NA, xlim=c(1,num_loc_catg)+0.5*c(-1,1), ylim=c(0,1), axes=FALSE, xlab="", ylab="", xaxs="i", yaxs="i")
	for (ii in 1:num_loc_catg){
		xx_prop = proptable_cntloc_freq[ii, ]
		xx_prop_cumsum = c(0, cumsum(xx_prop))

		bar_width = 0.475;
		for (yy in 1:num_freq_catg){
			polygon(x=c(ii+bar_width*c(-1,1), rev(ii+bar_width*c(-1,1))), y=c(rep(xx_prop_cumsum[yy],2), rep(xx_prop_cumsum[yy+1],2)), col=COLS[yy], border=NA)
		}
	} # for- ii

	if (iiSpg_YN==1){
		legend("topright", legend=labels_freq, horiz=TRUE, col="blank", fill=COLS, cex=1.5, inset=c(0, -0.275), xpd=NA, bty="n", text.width=strwidth(labels_freq)/max(strwidth(labels_freq)), adj=c(0,0.8), x.intersp=0.75)
	}

	axis(side=2, at=ytick_major, labels=if(iiSpg_YN==0) 100*ytick_major else FALSE, cex.axis=1.2, pos=0.4, las=1)
	if (iiSpg_YN==0){
		mtext(side=2, text="Proportion (%)", line=3.5, cex=1.2)
	}
	text(x=1:num_loc_catg, y=-0.05, labels=labels_loc, xpd=TRUE, srt=0, cex=1.2)
	mtext(side=1, text="Location of contacts", line=2, cex=1)

	# label for panel
	label_panel = sprintf("(%s)", letters[idx_plot])
	mtext(side=3, text=label_panel, at=0.15, line=0.75, adj=c(0, 0), cex=1.1)


	# (iii) dur x freq
	idx_plot = idx_plot + 1
	COLS = rev(ggpubr::get_palette(palette = "YlGn", num_freq_catg))
	
	table_dur_freq = table(chk_contact_TT[, c("duration_multi", "frequency_multi")])
	proptable_dur_freq = t(apply(table_dur_freq, 1, prop.table))
	xsum_table_dur_freq = rowSums(table_dur_freq)

	xlim_plot = c(1,num_dur_catg) + 0.5*c(-1,1)
	plot(NA, xlim=c(1,num_dur_catg)+0.5*c(-1,1), ylim=c(0,1), axes=FALSE, xlab="", ylab="", xaxs="i", yaxs="i")
	for (ii in 1:num_dur_catg){
		xx_prop = proptable_dur_freq[ii, ]
		xx_prop_cumsum = c(0, cumsum(xx_prop))

		bar_width = 0.475;
		for (yy in 1:num_freq_catg){
			polygon(x=c(ii+bar_width*c(-1,1), rev(ii+bar_width*c(-1,1))), y=c(rep(xx_prop_cumsum[yy],2), rep(xx_prop_cumsum[yy+1],2)), col=COLS[yy], border=NA)
		}
	} # for- ii

	if (iiSpg_YN==1){
		legend("topright", legend=labels_freq, horiz=TRUE, col="blank", fill=COLS, cex=1.5, inset=c(0, -0.275), xpd=NA, bty="n", text.width=strwidth(labels_freq)/max(strwidth(labels_freq)), adj=c(0,0.8), x.intersp=0.75)
	}

	axis(side=2, at=ytick_major, labels=if(iiSpg_YN==0) 100*ytick_major else FALSE, cex.axis=1.2, pos=0.4, las=1)
	if (iiSpg_YN==0){
		mtext(side=2, text="Proportion (%)", line=3.5, cex=1.2)
	}
	text(x=1:num_dur_catg, y=-0.05, labels=labels_dur, xpd=TRUE, srt=0, cex=1.1)
	mtext(side=1, text="Frequency of contacts", line=2, cex=1)

	# label for panel
	label_panel = sprintf("(%s)", letters[idx_plot])
	mtext(side=3, text=label_panel, at=0.15, line=0.75, adj=c(0, 0), cex=1.1)

} # for- iiSpg_YN

if (plot_pdf_YN){ dev.off()}


## by phy / non-phy contact

labels_phy = c("Physical", "Non-physical")
num_phy = 2
COLS = rev(ggpubr::get_palette(palette = "YlGn", 3))

xtype_vec = c("loc", "dur", "freq")
labels_xtype_list = list(labels_loc, labels_dur, labels_freq)
num_xtype_list = c(num_loc_catg, num_dur_catg, num_freq_catg)
yvar_xtype_vec = c(NA, "duration_multi", "frequency_multi")
xlab_xtype_vec = paste(c("Location", "Duration", "Frequency"), "of contacts", sep=" ")

par_mar_list = list('0'=c(4, 5.5, 4, 0.5), '1'=c(4, 3.5, 4, 2.5))

fname_plot = sprintf("dist_by_phy_nonphy_%s.pdf", update_outputdate())
plot_width = 5*2;
plot_height = 4*3;

plot_pdf_YN = TRUE;
if (plot_pdf_YN){
	pdf(fname_plot, width=plot_width, height=plot_height)
} else{
	windows(width=plot_width, height=plot_height)
}
layout(matrix(1:6, ncol=2, byrow=FALSE))

idx_plot = 0;
for (iiSpg_YN in 0:1){

	par(mar=par_mar_list[[as.character(iiSpg_YN)]])
	
	chk_contact_TT = chk_contact_base
	chk_contact_TT = subset(chk_contact_TT, subset=part_specialgroups_YN==iiSpg_YN)	
	
	for (itype in 1:3){
		num_xtype = num_xtype_list[itype]
	
		idx_plot = idx_plot + 1
		if (itype == 1){
			table_xtype_phy = matrix(0, nrow=num_loc_catg, ncol=num_phy)
			xvar_cntloc_vec = paste0("cnt_", c("home","school","work","others"))
			for (yy in 1:num_loc_catg){
				xvar_cntloc = xvar_cntloc_vec[yy]
				tab_TT = table(chk_contact_TT[, c(xvar_cntloc, "phys_contact")])
				table_xtype_phy[yy, as.numeric(colnames(tab_TT))] = tab_TT['1',]
			}
		} else {
			table_xtype_phy = table(chk_contact_TT[, c(yvar_xtype_vec[itype], "phys_contact")])
		}
	
		proptable_xtype_phy = t(apply(table_xtype_phy, 1, prop.table))
		xsum_table_xtype_phy = rowSums(table_xtype_phy)

		xlim_plot = c(1,num_xtype) + 0.5*c(-1,1)
		plot(NA, xlim=c(1,num_xtype)+0.5*c(-1,1), ylim=c(0,1), axes=FALSE, xlab="", ylab="", xaxs="i", yaxs="i")
		for (ii in 1:num_xtype){
			xx_prop = proptable_xtype_phy[ii, ]
			xx_prop_cumsum = c(0, cumsum(xx_prop))

			bar_width = 0.475;
			for (yy in 1:num_phy){
				polygon(x=c(ii+bar_width*c(-1,1), rev(ii+bar_width*c(-1,1))), y=c(rep(xx_prop_cumsum[yy],2), rep(xx_prop_cumsum[yy+1],2)), col=COLS[yy], border=NA)
			}
		} # for- ii

		if (iiSpg_YN==1 & itype==1){
			legend("topright", legend=labels_phy, horiz=TRUE, col="blank", fill=COLS, cex=1.75, inset=c(0, -0.15), xpd=NA, bty="n", x.intersp=0.75)
		}

		axis(side=2, at=ytick_major, labels=if(iiSpg_YN==0) 100*ytick_major else FALSE, cex.axis=1.3, pos=0.4, las=1)
		if (iiSpg_YN==0){
			mtext(side=2, text="Proportion (%)", line=3.5, cex=1.2)
		}
		text(x=1:num_xtype, y=-0.05-0.015*(itype==3), labels=labels_xtype_list[[itype]], xpd=TRUE, srt=0, cex=1.3)
		mtext(side=1, text=xlab_xtype_vec[itype], line=2.5+0.25*(itype==3), cex=1)

		# label for panel
		label_panel = sprintf("(%s)", letters[idx_plot])
		mtext(side=3, text=label_panel, at=0.15, line=0.75, adj=c(0, 0), cex=1.1)
		
		# column label
		if (itype==1){
			label_column = sprintf("Participants from %s", ifelse(iiSpg_YN==1, "high-contact groups", "the general population"))
			mtext(side=3, text=label_column, line=2.5, cex=1.2)
		}	
	} # for- itype
} # for- iiSpg_YN

if (plot_pdf_YN){ dev.off()}




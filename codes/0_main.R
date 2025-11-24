#### the main codes

check_installed_packages = installed.packages()
check_package_vec = c("openxlsx", "lubridate", "grid", "data.table", "dplyr", "mgcv", "future.apply", "ggsci", "ggpubr", "lmtest")
check_package_YN = (check_package_vec %in% check_installed_packages)
if (any(!check_package_YN)){
	stop( sprintf("The following package(s) is/are not available:\n%s\nPlease first install the corresponding package(s).", paste(check_package_vec[which(!check_package_YN)], collapse=", ")) )
}


# load library
# library(openxlsx)
# library(lubridate)
library(grid)
library(data.table)
library(dplyr)
library(mgcv)
library(socialmixr)

# get the date for saving the outputs 
update_outputdate = function(shortDate=TRUE) {
	ifelse(shortDate, substr(gsub("-", "", Sys.Date()),3,8), gsub("-", "", Sys.Date()))
}

# working folder that holds the codes and data
# ** change to the setting w.r.t. your computer
# set the working folder
folder_main = "C:/Users/Horace Choi/D24H Dropbox/H Choi/Sync/Others/_CoMix_Temp/Rcodes_share/" # <- change this
cat( sprintf("folder_main =\n%s\n", folder_main) )
cat( "Check if this is correct. Enter _c_ to continue, or _Q_ to quit and edit.\n" )
browser()
setwd( folder_main )

# load survey data
load( "data/chk_base.RData" );


# note. coding for high-contact groups
# 1	Residents from Residential Care Homes for Elderly Persons (RCHE) or Residential Care Homes for Persons with Disabilities (RCHD)
# 2	Staff from catering businesses
# 3	Domestic helpers
# 4	School Teachers (Kindergartens/Primary and Secondary Schools/Higher Education)
# 5	Staff from Residential Care Homes for Elderly Persons (RCHE) or Residential Care Homes for Persons with Disabilities (RCHD)
# 6	Staff from retail businesses
# 7	Delivery staff (e.g. Deliveroo/SF)
# 8	Construction workers


time00 = Sys.time()

# 1. data analysis
source( "codes/1_tables_mean contacts_5wave_4phases.R" )
source( "codes/1_analysis.R" )
source( "codes/1_plot_cnt_duration_dist.R")

# 2/3. settings for timeline plots
source( "codes/2_3_plot_contact_settings_fundraw.R" )

# 2. plot background information

source( "codes/2a_plot_epide_curve.R" )
source( "codes/2b_plot_PHSMs_date.R" )
source( "codes/2c_plot_vacc_rate_genpop.R" )

# 3. plot number of contacts by category
library(grid)
library(mgcv)

	# settings for the plots, change once to apply to all plots
	plot_CI_YN = TRUE
	plot_polygon_YN = TRUE
	plot_textlabels_YN = FALSE
	plot_legend_YN = TRUE
	CI_level_plot = 0.9
	PI_level_plot = 0.8
	CI_plot_idx = c(2, 3) # c(2,3) = CI, c(5,6)= PI
	CI_arrow_length = unit(0.5, units='native') # 0 == no arrow
	plot_outputdate = update_outputdate(short=TRUE)

# number of survey responses
source( "codes/3a_plot_contact_timeline_numsurvey.R" )

YLAB = "Average number of\ndaily contacts" # 'Mean number of\ncontacts per day'
source( "codes/3b_plot_contact_timeline_geopop_selectedgrps.R" )
# num of contacts by category
for (iiSpg_YN in c(0, 1, 9)){
	if (iiSpg_YN==0){
		chk_plot = chk_plot_genpop;
		t_month = t_month_genpop;
		fname_plot_suffix = "genpop"
	} else if (iiSpg_YN==1){
		chk_plot = chk_plot_spg;
		t_month = t_month_spg;
		fname_plot_suffix = "spg"
	} else if (iiSpg_YN==9){
		chk_plot = chk_plot_allpart;
		t_month = t_month_allpart;
		fname_plot_suffix = "allpart"
	}
	fname_plot = sprintf("timeline_%s_%s.pdf", fname_plot_suffix, plot_outputdate)
	pdf(file = fname_plot, width = plot_width, height = plot_height)
	source( "codes/3c_plot_contact_timeline_age.R" )
	source( "codes/3d_plot_contact_timeline_location.R" )
	source( "codes/3e_plot_contact_timeline_phy_nonphy.R" )
	source( "codes/3f_plot_contact_timeline_vaccinated.R" )
	source( "codes/3g_plot_contact_timeline_week.R" )

	dev.off()
} # iiSpg_YN


print( 'completed 1-3' )
print( Sys.time() - time00 )

# 4. regression
source( "codes/4_regression_timevar.R" )

print( 'completed 4' )
print( Sys.time() - time00 )


# 5. contact matrices
num_agegps = 16;
source( "codes/5_plot_functions_cntmat.R" )
source( "codes/5a_create_contact_matrices_genpopOnly.R" )
source( "codes/5b_plot_contact_matrices_genpopOnly.R" )
source( "codes/5c_create_plot_domeigen_ntimept_genpopOnly.R" )

print( 'completed 5' )
print( Sys.time() - time00 )

# 6. resample 
# whether to run bootstrapping replications. This could take several hours. If not running bootstrapping, one can prepare the plots using the outputs of the bootstrapping in " folder_main"
run_boot_YN = FALSE; 
if (run_boot_YN){
	source( "codes/6a_create_contact_matrices_resample_boot_parallel.R" ) # include high-contact group to construct the contact matrices
}
source( "codes/6b_plot_contact_matrices_resample_boot.R" )
if (run_boot_YN){
	source( "codes/6c_create_domeigen_boot_parallel_ntimept_genpop.R ") # assess the uncertainty of the dominant eigenvalues of the contact matrices among the general pouplation
}
source( "codes/6d_plot_domeigenval_boot_ntimept_genpop.R" ) 

print( 'completed 6' )
print( Sys.time() - time00 )

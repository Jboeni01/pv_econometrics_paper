##################################
# Estimation Final 

#-1- Load packages 
rm(list = ls())

#remove.packages("rlang")
#install.packages("rlang")#, type="source")

library(readxl)
library(tidyr)
library(tidyverse)
library(dbplyr)
library(dplyr) #arrange 
library(rlang)
library(ggthemes)
library(ggplot2)
library(xtable) #print descriptive tables 
library(psych) #descriptive statistics 
library(fixest) #fixed-effects panel data estimation 
library(car)
library(boot)

#install.packages("dotwhisker")
library(dotwhisker)

################################################################################
#-2- set wd and load data 
################################################################################
setwd("C:/Users/u0149894/Desktop/Code/PV_econometrics/replication_eneeco/pv_data")


load(paste0(getwd(),"/PV_total_estdata_monthly.Rdata")) #full data monthly 



################################################################################
#-3- define functions 
################################################################################

#==============================================================================#
#-3.1- Calculate different benefits used in the regressions
#==============================================================================#

#aggregate benefits
benvars_aggregate <- function(newvar,aggvars,data, monthly=F){ #aggregate over the different benefits
  if(monthly==F){
    aggtypes <- c("","_w8","_defl","_defl_w8")
  }else{
    aggtypes <- c("","_defl")
  }
  for(i in aggtypes){
    newv <- paste0(newvar,i)
    aggv <- paste0(aggvars,i)
    data <- data %>% mutate(!!newv := rowSums(select(.,all_of(aggv))))
  }
  data
}

#transform benefits
benvars_transform <- function(transvars,data,operation,monthly=F,capsize=1){ #transform data
  if(monthly==F){
    transvars_full <- c(transvars,paste0(transvars,"_defl"),
                        paste0(transvars,"_w8"),paste0(transvars,"_defl_w8"))    
  }else{
    transvars_full <- c(transvars,paste0(transvars,"_defl"))   
  }
  
  
  if(operation=="log"){
    data <- data %>% 
      mutate(across(all_of(transvars_full),
                    ~ log(ifelse(.x==0,1,.x)), .names="{.col}_log")) #take the log of benefits, add 1 to zero values 
  }
  if(operation=="/1000"){
    data <- data %>% 
      mutate(across(all_of(transvars_full),
                    ~ .x/1000, .names="{.col}_thous")) #take the log of benefits, add 1 to zero values 
  }
  if(operation=="*(-1)"){
    data <- data %>% 
      mutate(across(all_of(transvars_full),
                    ~ -1*.x,
                    .names="{substr(.col,1,nchar(transvars[1]))}_pos{substr(.col,(nchar(transvars[1])+1),nchar(.col))}")) #  
  }
  if(operation=="/capsize"){
    data <- data %>% 
      mutate(across(all_of(transvars_full), ~ .x/capsize))
  }
  data
}

#calculate benefit shares 
benvars_shares <- function(aggvars,denom, data, monthly=F) { #calculate shares 
  if(monthly==F){
    aggtypes <- c("","_w8","_defl","_defl_w8")
  }else{
    aggtypes <- c("","_defl")
  }
  for(i in aggtypes){
    aggv <- paste0(aggvars,i)
    data <- data %>% mutate(across(all_of(aggv), ~.x/!!sym(denom),.names="{.col}_sh"))
  }
  data
}

#==============================================================================#
#-3.2- IV control-function approach functions 
#==============================================================================#

#-3.2.1 - define bootstrapping function used for standard error calculation

bootstrap_IV2st <- function(data, indices, eq_2stage) {
  bootstrap_sample <- data[indices, ] #indices passed on by bootstrap function
  bootstrap_sample <- bootstrap_sample %>% unnest(cols = c(data)) #unnest previously nested data
  
  bootstrap_sample <- bootstrap_sample %>%
    mutate(newid = 1) %>%
    group_by(zip,date) %>% mutate(newid = cumsum(newid)) %>%
    mutate(zip = paste(zip,newid,sep=".")) #generate new zip group 
  #print(length(unique(bootstrap_sample$zip)))
  
  #print(dim(bootstrap_sample))
  #print(eq_2stage)
  
  regres_bootstrap <- feglm(eq_2stage,
                            data = bootstrap_sample,
                            family = quasipoisson,
                            cluster="zip") #rerun regression 
  print(regres_bootstrap$fixef_size)
  print(dim(bootstrap_sample))
  return(coef(regres_bootstrap))
  
  #return(replicated_data)
}


#==============================================================================#
#-3.2- Functions to set up formulas and run PPMLE regressions
#==============================================================================#


runregressions <- function(pdata, eq_list, cluster="zip"){
  
  #function to run regressions
  
  regression_list <- list()
  
  for(i in 1:length(eq_list)){
    
    eq <- eq_list[[i]]
    regname <- gsub("eq.","",names(eq_list[i]))
    print(regname)
    regression_list[[regname]] <- feglm(eq,
                                        data=pdata,#data=estimate.data.final.ysset,
                                        family=quasipoisson, cluster="zip")
  }
  
  return(regression_list)
  
}

setupformulas <- function(rhs_list, fixed_effects_monthly = "|year+date+zip", 
                          depvars = c("pv_count","pv_meansize")){
  
  eq_list <- list()
  
  for(depvar in depvars){ #loop through dependent variables 
    for(rhs_name in names(rhs_list)) {
      regnr <- gsub("rhs","",rhs_name) #capture regression number 
      rhs_formula <- as.formula(paste(depvar, "~", rhs_list[[rhs_name]], fixed_effects_monthly)) #set up formula
      eq_list[[paste0("eq.pfe.m", regnr,".",depvar)]] <- rhs_formula #store formula in list 
    }
    
  }
  
  return(eq_list)
  
}

#==============================================================================#
#-3.2.2 - Control function approach
#==============================================================================#

regress_IV <- function(data,
                       eq_2stage, regname_2stage,
                       eq_1stage, regname_1stage,
                       boot_iterations=50){
  
  
  #regname_2stage <- eq_2stage_lname %>% substr(4, nchar(.)) #2nd stage reg name
  #regname_1stage <- eq_1stage_lname %>% substr(4, nchar(.)) #1st stage reg name
  
  #1st stage regression
  #eq_1stage <- eq_list_monthly_IV1st[[eq_1stage_lname]] #retrieve previously defined equation
  regres_1stage <- feols(eq_1stage, data=data) #run 1st stage regression
  
  data.IV <- data %>%
    mutate(res_1stage = NA_real_) #add column  
  
  data.IV$res_1stage[regres_1stage$obs_selection$obsRemoved] <- regres_1stage$residuals #fill column with 1st stage residuals
  
  data.IV <- data.IV%>%
    mutate(popsize_age_above64_sh = 1,
           popsize_nat_nbel_sh = 1,
           popsize_women_sh = 1,
           popsize_living_couplenochild_sh = 1,
           housing_singfam_open_sh = 1) #make sure to drop final share 
  
  #2nd stage regression 
  #eq_2stage <- eq_list_monthly[[eq_2stage_lname]] #retrieve previously defined equation
  regres_2stage <- feglm(eq_2stage, data=data.IV, family=quasipoisson, cluster="zip") #run 2nd stage regression
  
  #Bootstrapping respecting municipality clustering for standard errors 
  D <- data.IV %>% nest(-zip) #one row for each zip code, ensures full municipality clustering
  
  set.seed(1) 
  
  boot_results_groups <- boot(data = D,
                              statistic = bootstrap_IV2st, #call function that runs 2nd stage on bootstrap
                              R = boot_iterations,
                              eq_2stage = eq_2stage)
  
  #Calculate the standard errors (standard deviation across coefficients), based on bootstrapping
  boot_coefficients <- boot_results_groups$t
  standard_errors <- apply(boot_coefficients, 2, sd) #calculate standard dev of coeff across bootstrap samples
  
  #Calculate the vcov matrix 
  coeff_mean <- colMeans(boot_coefficients) #coeff mean across bootstrap samples 
  coeff_mean_matrix <- matrix(rep(coeff_mean, each = nrow(boot_coefficients)), 
                              nrow = nrow(boot_coefficients), byrow = F) #fill mean to matrix (N_iter x k)
  
  coeff_err <- boot_coefficients - coeff_mean_matrix #V: subtract coeff from coeff mean
  
  coeff_varcov <- 1/(boot_iterations-1)*t(coeff_err)%*%coeff_err # 1/(N_iter-1)* V'V
  
  #Save results in the model
  regres_2stage[["bootstr"]] = list(coef= boot_coefficients, stderr= standard_errors, vcov = coeff_varcov)
  
  regression_output <- setNames( #capture both models in a list
    list(regres_1stage, regres_2stage),
    c(regname_1stage, regname_2stage)
  )
  
  return(regression_output)
  
}

#==============================================================================#
#-3.3- Display final results 
#==============================================================================#


#-3.3.1- Add rows to final table 
etable_yrange = function(x) paste(attributes(x$fixef_id$year)[[1]][c(1,length(attributes(x$fixef_id$year)[[1]]))],collapse="-")
extralines_register("yrange", etable_yrange, "-_Year Range")

etable_id = function(x) paste("",x$fixef_sizes["zip"][[1]])
extralines_register("ids", etable_id, "-_Nr of ID")

etable_loglik = function(x) paste("",format(x$loglik,digits=2))
extralines_register("loglik", etable_loglik, "_Log likelihood")

#==============================================================================#
#-3.3.3 - Save tables as latex output
#==============================================================================#

save_table_benefits <- function(model_list,model_names,model_ses,save_name,
                                path=paste0(getwd(),"/data/tables/"),
                                group=NULL, headers=NULL, order=NULL, interaction.order=NULL){
  table <- etable(model_list[model_names],
                  vcov=model_ses,
                  digits=3,
                  tex=T,
                  headers = headers,
                  order = order,
                  group=group,
                  fontsize = "footnotesize",
                  depvar=F,
                  tabular = "X",
                  #fixef.group = T,
                  fixef_sizes.simplify = F,
                  fixef_sizes = T,
                  interaction.order = interaction.order, 
                  notes = NULL)
  
  #fine adjustments on the way results are displayed 
  table <- table %>%
    gsub("\\\\emph\\{Variables\\}\\\\\\\\","", .) %>%
    gsub("NA",".",.) %>%
    gsub("-(?=\\d{1,2}\\.)", "$\\\\text{-}$", ., perl = TRUE) %>%
    gsub("\\\\emph\\{Fixed-effects\\\\\\}\\\\\\\\.*\\\\\\\\", "", ., perl = TRUE)
  #%>%  gsub("-[0-9].","$\\text{-}$[0-9].",.)
  
  #get rid of significance stars below 
  table <- table %>% 
    gsub("\\\\multicolumn\\{[0-9]+\\}\\{l\\}\\{\\\\emph\\{Clustered.*\\}\\}\\\\\\\\", "", .) %>%
    gsub("\\\\multicolumn\\{[0-9]+\\}\\{l\\}\\{\\\\emph\\{Signif.*\\}\\}\\\\\\\\", "", .)
  
  
  print(paste0(outputpath_regtables,save_name,".tex"))
  
  capture.output(print(table),
                 file = paste0(outputpath_regtables,save_name,".tex"))
}

################################################################################
# -4- Label Data 
################################################################################

#-4.1- All benefit variables with labels in one vector
benefitvars_labeled <- c("b_net_solw_defl_log"="Net benefits (log)",
                         "b_net_solw_defl_thous"="Net benefits",
                         "b_gc_solw_defl_thous"="Output-based incentive",
                         "b_nm_solw_defl_thous"="Net metering",
                         "b_protar_pos_defl_thous"="Capacity-based cost",
                         "b_qw_defl_thous"="Capacity-based incentive",
                         "b_protar_pos_subreg_defl_thous"="Capacity-based cost",
                         "b_qw_subreg_defl_thous"="Capacity-based incentive",
                         "b_nm_mix_retail_solw_defl_thous"="Distr. tariff-free net metering (instrument)"  
)
#-4.2- All control variables with labels in one vector
controlvars_labeled <- 
  c("hh_size_log"="Household size (log)",
    "popsize_dens_log"="Population density (log)",
    "popsize_age_0_17_sh"="Age:below 18 (sh.)",
    "popsize_age_18_49_sh"="Age:18-49 (sh.)",
    "popsize_age_50_64_sh"="Age:50-64 (sh.)",
    "popsize_age_above64_sh"="Age:above 64 (sh.)",
    "popsize_nat_bel_sh"="Nationals (sh.)",
    "popsize_nat_nbel_sh"="Non-nationals (sh.)",
    "popsize_men_sh"="Male (sh.)",
    "popsize_women_sh"="Female (sh.)",
    "income_perdecl_med_defl_log"="Net median income (log)", #"income_perdecl_med_defl_log"
    "popsize_living_alone_sh"="Hh single (sh.)",
    "popsize_living_singlepar_sh"="Hh single parent (sh.)",
    "popsize_living_couplechild_sh"="Hh couple /w children (sh.)",
    "popsize_living_couplenochild_sh"="Hh couple w/o children (sh.)",
    "buildings_until1981_sh"="House age:until 1981 (sh.)",
    "buildings_after1981_sh"="House age:after 1981 (sh.)",
    "housing_apartments_sh"="House type:apartments (sh.)",
    "housing_singfam_closed_sh"="House type:single fam closed (sh.)",
    "housing_singfam_semidetached_sh"="House type:single fam semi-detached (sh.)",
    "housing_singfam_open_sh"="House type:single fam open (sh.)")


estvars_labeled <- c(benefitvars_labeled,controlvars_labeled) #all exp variales


#-4.3- Define ordering of variables 
etable_order <- c("^Net benefits [(]log[)]","^Net benefits",
                  "^Output-based incentive","^Net metering",
                  "^Capacity-based cost","^Capacity-based incentive") 
etable_extralines <- list("-_Nr of ID"=~ids,"-_Year Range"=~yrange) #list("-_Nr of ID"=~ids,"-_Year Range"=~yrange,"_Log likelihood"=~loglik)
etable_group_control <- list("^_\\midrule \\emph{Controls:} "=c("median income","Household", "Population", "Age",
                                                                "Female","Male","Nationals","Hh","House","res"))


################################################################################
# -5- Define Sample 
################################################################################

#==============================================================================#
#-5.1- Define dependent variables 
#==============================================================================#
depvars <- c("pv_count","pv_meansize","pv_capkw")
depvars_labdict <- c(pv_count = "Nr. of PV installations (yearly)",
                     pv_meansize="average capacity per installation",
                     pv_capkw="Added capacity in kW")

#==============================================================================#
#-5.2 - Define sample 
#==============================================================================#
controlvars <- names(controlvars_labeled)

#Adoption & benefit data  
pv_cvar <- grep("c_[1-9]kw$", colnames(estimate.data.monthly), value=T)
pv_cvar_defl <- grep("c_[1-9]kw_defl$", colnames(estimate.data.monthly), value=T)
capsize <- gsub("[^1-9]","",pv_cvar) %>% as.numeric()

benefit_vars <- grep("^b_",colnames(estimate.data.monthly),value=T)#grep("npv|^c_",npv_vars,invert=T,value=T)

#Estimation data set
estimate.data.monthly.final <- estimate.data.monthly %>% 
  subset(year%in%2008:2019,
         select=c("zip","year","month","date","region","dso",
                  depvars,
                  "prebunch_flag","postbunch_flag",
                  benefit_vars,
                  controlvars,
                  "income_perdecl_med_defl","inc_gr3_0k20k_sh")) %>%
  mutate(across(all_of(benefit_vars), ~ .x/capsize))

#==============================================================================#
#-5.3- Calculate additional variables 
#==============================================================================#

#regional aggregates
estimate.data.monthly.final <- benvars_aggregate("b_net",c("b_gc","b_nm","b_protar","b_qw"), 
                                                 estimate.data.monthly.final, monthly=T) 
#sub-regional aggregates
estimate.data.monthly.final <- benvars_aggregate("b_net_subreg",c("b_gc","b_nm_subreg","b_protar_subreg","b_qw_subreg"), #"b_tc",
                                                 estimate.data.monthly.final, monthly=T) 
#regional aggregates with adjustment of GCs in Wallonia
estimate.data.monthly.final <- benvars_aggregate("b_net_solw",c("b_gc_solw","b_nm_solw","b_protar","b_qw"), #"b_tc",
                                                 estimate.data.monthly.final, monthly=T)
#sub-regional aggregates with adjustment of GCs in Wallonia
estimate.data.monthly.final <- benvars_aggregate("b_net_subreg_solw",c("b_gc_solw","b_nm_subreg_solw","b_protar_subreg","b_qw_subreg"), #"b_tc",
                                                 estimate.data.monthly.final, monthly=T)

#Repeat aggregation procedure for different discount rates (used in robustness section)
for(r in c(0,3,15)){
  benvars_agg <- paste0(c("b_gc","b_nm","b_protar","b_qw"),r) #define variables
  benvars_agg_subreg <- benvars_agg %>% paste0("_subreg") %>% gsub("(gc[0-9]{1,2})_subreg", "\\1", .) 
  benvars_agg_solw <- benvars_agg %>% gsub("(b_(gc|nm)[0-9]{1,2})", "\\1_solw",.)
  benvars_agg_subreg_solw <- benvars_agg_subreg %>% gsub("(gc[0-9]{1,2})|(nm[0-9]{1,2}_subreg)", "\\1\\2_solw",.)
  
  #repeat aggregation with different DRs
  estimate.data.monthly.final <- benvars_aggregate(paste0("b_net",r),benvars_agg, #"b_tc",
                                                   estimate.data.monthly.final, monthly=T) 
  estimate.data.monthly.final <- benvars_aggregate(paste0("b_net",r,"_subreg"),benvars_agg_subreg, #"b_tc",
                                                   estimate.data.monthly.final, monthly=T) 
  
  estimate.data.monthly.final <- benvars_aggregate(paste0("b_net",r,"_solw"),benvars_agg_solw, #"b_tc",
                                                   estimate.data.monthly.final, monthly=T)
  estimate.data.monthly.final <- benvars_aggregate(paste0("b_net",r,"_subreg_solw"),benvars_agg_subreg_solw, #"b_tc",
                                                   estimate.data.monthly.final, monthly=T)
}


#make sure that missing dsos are labelled as missing and not as zero
estimate.data.monthly.final <- estimate.data.monthly.final %>%
  mutate(across(all_of(grep("b_net",colnames(.),value=T)),~if_else(is.na(dso)&.x==0,NA_real_,.x)))

#add new variables to vector of benefit and npv varnames 
benefit_vars <- grep("^b_",colnames(estimate.data.monthly.final), value=T)
npv_vars <- grep("^npv_",colnames(estimate.data.monthly.final), value=T) %>%
  c(.,benefit_vars)

#in Thousand EUR
estimate.data.monthly.final <- estimate.data.monthly.final %>%
  mutate(across(all_of(npv_vars), ~.x/1000, .names = "{.col}_thous" ))


#in log 
estimate.data.monthly.final <- estimate.data.monthly.final %>%
  mutate(across(all_of(grep("^b_net",colnames(.),value=T)), ~ log(.x), .names = "{.col}_log" ))

#prosumer tariff as positive benefit (more intuitive for results)
estimate.data.monthly.final <- estimate.data.monthly.final %>%
  mutate(across(all_of(grep("^b_protar",colnames(.),value=T)), ~(-1)*.x, #grep("^b_protar_",colnames(.),value=T)
                .names="{substr(.col,1,8)}_pos{substr(.col,9,nchar(.col))}"))


#prosumer tariff as positive benefits for discount rates 
estimate.data.monthly.final <- estimate.data.monthly.final %>%
  #mutate(across(all_of(grep("^b_protar[0-9]",colnames(.),value=T)), ~(-1)*.x
  #              ,.names="{.col}_pos")) %>%
  rename_with(
    ~ gsub("^b_protar_pos([0-9]{1,2})(.*)", "b_protar\\1_pos\\2", .x), #~ gsub("^b_protar([0-9]{1,2})(.*?)_pos", "b_protar\\1_pos\\2", .x)
    starts_with("b_protar")
  )

grep("^b_protar",colnames(estimate.data.monthly.final),value=T)


################################################################################
# -6- Run baseline regressions   
################################################################################

#==============================================================================#
# -6.2- prepare data   
#==============================================================================#

tmin <- 2008
tmax <- 2019

trange <- tmin:tmax

estimate.data.monthly.final.ysset <- estimate.data.monthly.final %>% 
  subset(year%in%trange & prebunch_flag<=1 & postbunch_flag<=1 )

pdat.monthly = panel(estimate.data.monthly.final.ysset, ~zip+date) #define as panel data 

#describe main explanatory variables 
estimate.data.monthly.final.ysset %>%
  select(c(b_net_defl_log, b_net_defl_thous, b_gc_defl_thous, b_qw_defl_thous, b_protar_defl_thous, b_nm_defl_thous)) %>%
  describe()

#describe control variables
estimate.data.monthly.final.ysset %>%
  group_by(zip,year) %>%
  summarise(across(all_of(controlvars), mean)) %>%
  describe()

#==============================================================================#
# -6.1- Set up regression formulas
#==============================================================================#

#set up right hand side 
rhs_list <- list() 

rhs_list["rhs1"] <- "b_net_solw_defl_log"

rhs_list["rhs2"] <- paste("b_net_solw_defl_log",
                          paste(controlvars,collapse="+"),sep="+") 

rhs_list["rhs3"] <- paste("b_net_solw_defl_thous",
                          paste(controlvars,collapse="+"),sep="+")

rhs_list["rhs4"] <- paste("b_gc_solw_defl_thous+b_nm_solw_defl_thous+b_protar_pos_defl_thous+b_qw_defl_thous",
                          paste(controlvars,collapse="+"),sep="+")

rhs_list["rhsIV1.2st"] <- paste("b_gc_solw_defl_thous+b_nm_solw_defl_thous+b_protar_pos_subreg_defl_thous+b_qw_subreg_defl_thous+res_1stage",
                                paste(controlvars,collapse="+"),sep="+")

rhsIV1st_list <- list()

rhsIV1st_list["rhsIV1.1st"] <-  paste("b_nm_mix_retail_solw_defl_thous+b_gc_solw_defl_thous+b_protar_pos_subreg_defl_thous+b_qw_subreg_defl_thous",
                                      paste(controlvars,collapse="+"),sep="+")


# Run function to create formula 

eq_list_monthly <- setupformulas(rhs_list = rhs_list)
eq_list_monthly_IV1st <- setupformulas(rhs_list = rhsIV1st_list,
                                       depvars ="b_nm_solw_defl_thous")



#==============================================================================#
# -6.3- Run PPMLE regressions
#==============================================================================#

regress_list_monthly <- runregressions(pdata = pdat.monthly, eq_list = eq_list_monthly[!grepl("IV",names(eq_list_monthly))])

#==============================================================================#
# -6.4- Run IV regressions 
#==============================================================================#

iterations <- 10


regress_list_monthly_IV <- c(regress_IV(estimate.data.monthly.final.ysset, 
                                        eq_2stage = eq_list_monthly$eq.pfe.mIV1.2st.pv_meansize,
                                        regname_2stage = "pfe.mIV1.2st.pv_meansize",
                                        eq_1stage = eq_list_monthly_IV1st$eq.pfe.mIV1.1st.b_nm_solw_defl_thous,
                                        regname_1stage = "pfe.mIV1.1st", 
                                        boot_iterations = iterations),
                             regress_IV(estimate.data.monthly.final.ysset, 
                                        eq_2stage = eq_list_monthly$eq.pfe.mIV1.2st.pv_count,
                                        regname_2stage = "pfe.mIV1.2st.pv_count",
                                        eq_1stage = eq_list_monthly_IV1st$eq.pfe.mIV1.1st.b_nm_solw_defl_thous,
                                        regname_1stage = "pfe.mIV1.1st", 
                                        boot_iterations = iterations))


regress_list_monthly_full <- c(regress_list_monthly,regress_list_monthly_IV)



#do some hypothesis testing on equality of coefficients
linearHypothesis(regress_list_monthly_full[["pfe.mIV1.2st.pv_count"]], c("b_qw_subreg_defl_thous=b_nm_solw_defl_thous"),
                 vcov.=regress_list_monthly_full[["pfe.mIV1.2st.pv_count"]][["bootstr"]][["vcov"]])

linearHypothesis(regress_list_monthly_full[["pfe.mIV1.2st.pv_count"]], c("b_qw_subreg_defl_thous=b_gc_solw_defl_thous"),
                 vcov.=regress_list_monthly_full[["pfe.mIV1.2st.pv_count"]][["bootstr"]][["vcov"]])

#==============================================================================#
# -6.5- Print results - baseline -Table 2 & Table 3, Table A9, Table A10 
#==============================================================================#

setFixest_dict(estvars_labeled) #attach labels to estimation sample
etable_headers <- list("^ " = list("Aggregate benefits" = 3, "Sep. benefits"=1, "Sep. benefits (Poisson CF)"=1))
etable_headers_full <- list("^ " = list("Aggregate benefits" = 3, "Sep. benefits"=1, "Sep. benefits (Poisson CF)"=1,"First stage"=1))

# Regression table - number of PV installations 
pv_count_baseline <- c("pfe.m1.pv_count","pfe.m2.pv_count","pfe.m3.pv_count","pfe.m4.pv_count","pfe.mIV1.2st.pv_count")
pv_count_baseline_se <- list(~zip,~zip,~zip,~zip,
                             diag(regress_list_monthly_full[["pfe.mIV1.2st.pv_count"]][["bootstr"]][["stderr"]]^2))

print("Table 2")
etable(regress_list_monthly_full[pv_count_baseline],
       vcov=pv_count_baseline_se,
       digits=3,
       extralines = etable_extralines,
       headers = etable_headers,
       tex=F,
       order = etable_order,
       group=etable_group_control)


# Regression table - average installed capacity 
pv_meansize_baseline <- c("pfe.m1.pv_meansize","pfe.m2.pv_meansize","pfe.m3.pv_meansize","pfe.m4.pv_meansize","pfe.mIV1.2st.pv_meansize")
pv_meansize_baseline_se <- list(~zip,~zip,~zip,~zip,diag(regress_list_monthly_full[["pfe.mIV1.2st.pv_meansize"]][["bootstr"]][["stderr"]]^2))

print("Table 3")
etable(regress_list_monthly_full[pv_meansize_baseline],
       vcov=pv_meansize_baseline_se,
       digits=3,
       extralines = etable_extralines,
       headers = etable_headers,
       tex=F,
       order = etable_order,
       group=etable_group_control)

#==============================================================================#
# -6.7- Print full baseline results
#==============================================================================#

#Number of installations 
pv_count_baseline_full <- c("pfe.m1.pv_count","pfe.m2.pv_count","pfe.m3.pv_count",
                            "pfe.m4.pv_count","pfe.mIV1.2st.pv_count","pfe.mIV1.1st")
pv_count_baseline_full_se <- list(~zip,~zip,~zip,~zip,
                                  diag(regress_list_monthly_full[["pfe.mIV1.2st.pv_count"]][["bootstr"]][["stderr"]]^2),
                                  ~zip)

print("Table A9")
etable(regress_list_monthly_full[pv_count_baseline_full],
       vcov=pv_count_baseline_full_se,
       digits=3,
       extralines = etable_extralines,
       headers = etable_headers_full,
       tex=F,
       order = c(etable_order,"^res","^distr."))

#Average installed capacity 
pv_meansize_baseline_full <- c("pfe.m1.pv_meansize","pfe.m2.pv_meansize","pfe.m3.pv_meansize","pfe.m4.pv_meansize","pfe.mIV1.2st.pv_meansize","pfe.mIV1.1st")
pv_meansize_baseline_full_se <- list(~zip,~zip,~zip,~zip,diag(regress_list_monthly_full[["pfe.mIV1.2st.pv_meansize"]][["bootstr"]][["stderr"]]^2),~zip)


print("Table A10")
etable(regress_list_monthly_full[pv_meansize_baseline_full],
       vcov=pv_meansize_baseline_full_se,
       digits=3,
       extralines = etable_extralines,
       headers = etable_headers_full,
       tex=F,
       order = c(etable_order,"^res","^distr."))
################################################################################
# -7- Income-varying effect 
################################################################################

#==============================================================================#
# -7.1- Set up formulas and equations 
#==============================================================================#

tmin <- 2008
tmax <- 2019

trange <- tmin:tmax


estimate.data.monthly.final.ysset <- estimate.data.monthly.final %>% 
  subset(year%in%trange & prebunch_flag<=1 & postbunch_flag<=1 )


add_quantiles <- function(data,quantiles = c(1/3,2/3)){ #transform data
  
  # Step 1: Calculate the quartiles for year 2019
  
  #data <- estimate.data.monthly
  
  quantiles <- data %>% 
    #subset(year==2019 & month==1, select = inc_gr3_0k20k_sh) %>%
    subset(year==2019 & month==1, select = income_perdecl_med_defl) %>%
    as.matrix() %>%
    quantile(probs = quantiles, na.rm = T)
  
  print(quantiles)
  
  # Step 2: Create a new variable based on quartiles
  data$income_quantiles <- cut(data$income_perdecl_med_defl,
                               breaks = c(-Inf, quantiles, Inf), # Define the intervals
                               labels = 1:(length(quantiles)+1),           # Label the quantiles
                               right = TRUE,                     # Include the right endpoint in the interval
                               include.lowest = TRUE)            # Include the lowest value in the first interval
  
  # Step 3: Convert the new variable to numeric (optional, if needed as numeric)
  data <- data %>%
    mutate(income_quantiles = as.numeric(income_quantiles)) %>%
    mutate(income_quantiles = if_else(year<2019,0,income_quantiles)) %>%
    group_by(zip) %>%
    mutate(income_quantiles = max(income_quantiles)) %>%
    ungroup()
  
  
  
  
  return(data)
}


estimate.data.monthly.final.ysset <- add_quantiles(estimate.data.monthly.final.ysset, quantiles = 1/3)

table(estimate.data.monthly.final.ysset$income_quantiles, estimate.data.monthly.final.ysset$region)

pdat.monthly.inc = panel(estimate.data.monthly.final.ysset, ~zip+date)


#==============================================================================#
# -7.2- Set up formulas and equations 
#==============================================================================#

#regular regressions 
rhs_list <- list() 

rhs_list["rhs1"] <- paste("b_net_solw_defl_thous",
                          paste(controlvars,collapse="+"),sep="+")

rhs_list["rhs2"] <- paste("as.factor(income_quantiles)*(b_net_solw_defl_thous)",
                          paste(controlvars,collapse="+"),sep="+")


rhs_list["rhs3"] <- paste("b_gc_solw_defl_thous+b_nm_solw_defl_thous+b_protar_pos_defl_thous+b_qw_defl_thous",
                          paste(controlvars,collapse="+"),sep="+")

rhs_list["rhs4"] <- paste("as.factor(income_quantiles)*(b_gc_solw_defl_thous+b_nm_solw_defl_thous+b_protar_pos_defl_thous+b_qw_defl_thous)",
                          paste(controlvars,collapse="+"),sep="+")

#generation equations
eq_list_monthly <- setupformulas(rhs_list = rhs_list)

#==============================================================================#
# -7.3- Run PPMLE regressions 
#==============================================================================#

regress_list_monthly_inc <- runregressions(pdata = pdat.monthly.inc,
                                           eq_list = eq_list_monthly[!grepl("IV",names(eq_list_monthly))])


#==============================================================================#
# -7.5- Display and save results  
#==============================================================================#

incinteract_labeled <- c("as.factor(income_quantiles)2" = "above 1st income tercile")

estvars_labeled <- c(benefitvars_labeled,incinteract_labeled,controlvars_labeled)


setFixest_dict(estvars_labeled) #attach labels to estimation sample

etable_headers_incinteract <- list("^ " = list("Number of installations" = 2, "Average installations"=2))#,"^ " = list("Sep. benefits"=2, "Sep. benefits (Poisson CF)"=1))

pv_count_incinteract <- c("pfe.m2.pv_count","pfe.m4.pv_count","pfe.m2.pv_meansize","pfe.m4.pv_meansize")

linearHypothesis(regress_list_monthly_inc[["pfe.m4.pv_count"]], 
                 c("as.factor(income_quantiles)2:b_gc_solw_defl_thous=as.factor(income_quantiles)2:b_nm_solw_defl_thous"))
linearHypothesis(regress_list_monthly_inc[["pfe.m4.pv_count"]], 
                 c("as.factor(income_quantiles)2:b_qw_defl_thous=as.factor(income_quantiles)2:b_nm_solw_defl_thous"))
linearHypothesis(regress_list_monthly_inc[["pfe.m4.pv_count"]], 
                 c("as.factor(income_quantiles)2:b_qw_defl_thous=as.factor(income_quantiles)2:b_gc_solw_defl_thous"))

print("Table 4")
etable(regress_list_monthly_inc[pv_count_incinteract],
       digits=3,
       interaction.order = benefitvars_labeled, #c("Net benefits","Output-based incentive"),
       headers = etable_headers_incinteract,
       tex=F,
       order = etable_order,
       group=etable_group_control)

################################################################################
# -8- Robustness: Sample split into early and late time period 
################################################################################


#==============================================================================#
# -8.1- prepare data
#==============================================================================#

tmin <- 2008
tmax <- 2019

trange <- tmin:tmax


estimate.data.monthly.final.ysset <- estimate.data.monthly.final %>% 
  subset(year%in%trange & prebunch_flag<=1 & postbunch_flag<=1 ) %>%
  mutate(early_flag = if_else(date < as.Date("2014-03-01"),1,0)) %>%
  mutate(early_flag = if_else(date >= as.Date("2014-03-01"),2,early_flag))

pdat.monthly.tsplit = panel(estimate.data.monthly.final.ysset, ~zip+date)

#==============================================================================#
# -8.2- Set up formulas and equations 
#==============================================================================#

#regular regressions 
rhs_list <- list() 

rhs_list["rhs3"] <- paste("b_net_solw_defl_thous",
                          paste(controlvars,collapse="+"),sep="+")
rhs_list["rhs4"] <- paste("b_gc_solw_defl_thous+b_nm_solw_defl_thous+b_protar_pos_defl_thous+b_qw_defl_thous",
                          paste(controlvars,collapse="+"),sep="+")

#generation equations
eq_list_monthly <- setupformulas(rhs_list = rhs_list, depvars = "pv_count")

#==============================================================================#
# -8.3- Run PPMLE regressions 
#==============================================================================#

regress_list_monthly_tsplit.t1 <- runregressions(pdata = pdat.monthly.tsplit %>% subset(early_flag == 1),
                                                 eq_list = eq_list_monthly[!grepl("IV",names(eq_list_monthly))])
names(regress_list_monthly_tsplit.t1) <- paste0(names(regress_list_monthly_tsplit.t1),".t1")

regress_list_monthly_tsplit.t2 <- runregressions(pdata = pdat.monthly.tsplit %>% subset(early_flag == 2),
                                                 eq_list = eq_list_monthly[!grepl("IV",names(eq_list_monthly))])
names(regress_list_monthly_tsplit.t2) <- paste0(names(regress_list_monthly_tsplit.t2),".t2")

regress_list_monthly_tsplit <- c(regress_list_monthly_tsplit.t1, regress_list_monthly_tsplit.t2)

#==============================================================================#
# -8.5- Print & save tables 
#==============================================================================#

setFixest_dict(estvars_labeled) #attach labels to estimation sample

pv_tsplit <- c("pfe.m3.pv_count.t1","pfe.m4.pv_count.t1",#"pfe.mIV1.2st.pv_count.t1",
               "pfe.m3.pv_count.t2","pfe.m4.pv_count.t2")#,"pfe.mIV1.2st.pv_count.t2")

regress_tsplit_table_list <- regress_list_monthly_tsplit[pv_tsplit]


etable_headers_tsplit <- list("^ " = list("Early time period" = 2, "Late time period"=2))

print("Table 6")
etable(regress_tsplit_table_list,
       #vcov=pv_tsplit_se,
       digits=3,
       group=etable_group_control,
       extralines = etable_extralines,
       headers = etable_headers_tsplit,
       tex=F,
       order = etable_order)


################################################################################
# -9- Extension: different electricity price assumptions, 
################################################################################

#==============================================================================#
# -9.1- prepare data
#==============================================================================#

tmin <- 2008
tmax <- 2019

trange <- tmin:tmax

estimate.data.monthly.final.ysset <- estimate.data.monthly.final %>% 
  subset(year%in%trange & prebunch_flag<=1 & postbunch_flag<=1 )

pdat.monthly = panel(estimate.data.monthly.final.ysset, ~zip+date)

#==============================================================================#
# -9.2 - set up formulas 
#==============================================================================#

rhs_list <- list() 

rhs_list["rhs1"] <- paste("b_gc_solw_defl_thous+b_nm_mix_mavg_solw_defl_thous+b_protar_pos_defl_thous+b_qw_defl_thous",
                          paste(controlvars,collapse="+"),sep="+")
rhs_list["rhs2"] <- paste("b_gc_solw_defl_thous+b_nm_ellt10_solw_defl_thous+b_protar_pos_defl_thous+b_qw_defl_thous",
                          paste(controlvars,collapse="+"),sep="+")
rhs_list["rhs3"] <- paste("b_gc_solw_defl_thous+b_nm_ellt0_solw_defl_thous+b_protar_pos_defl_thous+b_qw_defl_thous",
                          paste(controlvars,collapse="+"),sep="+")

eq_list_monthly <- setupformulas(rhs_list = rhs_list)

#==============================================================================#
# -9.3- Run PPMLE regressions 
#==============================================================================#

regress_list_monthly_elec <- runregressions(pdata = pdat.monthly,
                                            eq_list = eq_list_monthly[!grepl("IV",names(eq_list_monthly))])


linearHypothesis(regress_list_monthly_elec[["pfe.m3.pv_count"]], c("b_gc_solw_defl_thous=b_nm_ellt0_solw_defl_thous"))
linearHypothesis(regress_list_monthly_elec[["pfe.m3.pv_count"]], c("b_qw_defl_thous=b_nm_ellt0_solw_defl_thous"))


#==============================================================================#
# -9.5- Display and save results  
#==============================================================================#

pv_elec <- c("pfe.m1.pv_count","pfe.m2.pv_count","pfe.m3.pv_count")

benefitvars_labeled <- c("b_net_solw_defl_log"="Net benefits (log)",
                         "b_net_solw_defl_thous"="Net benefits",
                         "b_gc_solw_defl_thous"="Output-based incentive",
                         "b_nm_mix_mavg_solw_defl_thous"="Net metering",
                         "b_nm_ellt10_solw_defl_thous"="Net metering",
                         "b_nm_ellt0_solw_defl_thous"="Net metering",
                         "b_protar_pos_defl_thous"="Capacity-based cost",
                         "b_qw_defl_thous"="Capacity-based incentive",
                         "b_protar_pos_subreg_defl_thous"="Capacity-based cost",
                         "b_qw_subreg_defl_thous"="Capacity-based incentive")

estvars_labeled <- c(benefitvars_labeled,controlvars_labeled)


setFixest_dict(estvars_labeled) #attach labels to estimation sample

etable_headers_elec <- list("^ " = list("\\shortstack{3 Month MA \\\\ elec price}" = 1,
                                        "\\shortstack{10\\% Long-term \\\\ elec price increase}"=1,
                                        "\\shortstack{0\\% Long-term \\\\ elec price increase}"=1))#,
#"^ " = list("PPMLE" = 1, "P-CF" = 1, "PPMLE"=1, "P-CF"=1, "PPMLE"=1, "P-CF"=1))

print("Table 6")
etable(regress_list_monthly_elec[pv_elec],
       #vcov=pv_elec_se,
       digits=3,
       group=etable_group_control,
       extralines = etable_extralines,
       headers = etable_headers_elec,
       tex=F,
       order = etable_order)

################################################################################
# -10- Robustness: control for short-term dynamics 
################################################################################

#==============================================================================#
# -10.1- prepare data
#==============================================================================#

tmin <- 2008
tmax <- 2019

trange <- tmin:tmax


estimate.data.monthly.final.ysset <- estimate.data.monthly.final %>% 
  subset(year%in%trange & prebunch_flag==0 & postbunch_flag==0 )

pdat.monthly.bunching = panel(estimate.data.monthly.final.ysset, ~zip+date)

#==============================================================================#
# -10.2- Set up formulas and equations 
#==============================================================================#

#regular regressions 
rhs_list <- list() 

rhs_list["rhs3"] <- paste("b_net_solw_defl_thous",
                          paste(controlvars,collapse="+"),sep="+")
rhs_list["rhs4"] <- paste("b_gc_solw_defl_thous+b_nm_solw_defl_thous+b_protar_pos_defl_thous+b_qw_defl_thous",
                          paste(controlvars,collapse="+"),sep="+")

#generation equations
eq_list_monthly <- setupformulas(rhs_list = rhs_list)

#==============================================================================#
# -10.3- Run PPMLE regressions 
#==============================================================================#

regress_list_monthly_bunching <- runregressions(pdata = pdat.monthly.bunching, 
                                                eq_list = eq_list_monthly[!grepl("IV",names(eq_list_monthly))])

pv_bunching <- c("pfe.m4.pv_count","pfe.m4.pv_meansize")


#==============================================================================#
# -10.5- Print & save tables 
#==============================================================================#

regress_shortterm_table_list <- regress_list_monthly_bunching[pv_bunching]
names(regress_shortterm_table_list) <- paste0(names(regress_shortterm_table_list),".bunching")

regress_shortterm_table_list <- c(regress_shortterm_table_list,
                                  regress_list_monthly_full[pv_bunching])

etable_headers_bunching <- list("^ " = list("Number of PV installations" = 2, "Average capacity size"=2),
                                "^ " = list("Baseline" = 1, "Short-term dynamics"=1,
                                            "Baseline" = 1, "Short-term dynamics"=1))

pv_bunching_table <- c(
  paste0("pfe.m4.pv_count",c("",".bunching")),
  paste0("pfe.m4.pv_meansize",c("",".bunching")))


print("Table A.11")
etable(regress_shortterm_table_list[pv_bunching_table],
       #vcov=pv_bunching_se,
       digits=3,
       group=etable_group_control,
       extralines = etable_extralines,
       headers = etable_headers_bunching,
       tex=F,
       order = etable_order)


################################################################################
# -11- Extension: different discount rates
################################################################################

#==============================================================================#
# -11.1- prepare data
#==============================================================================#

tmin <- 2008
tmax <- 2019

trange <- tmin:tmax

estimate.data.monthly.final.ysset <- estimate.data.monthly.final %>% 
  subset(year%in%trange & prebunch_flag<=1 & postbunch_flag<=1 )

pdat.monthly = panel(estimate.data.monthly.final.ysset, ~zip+date)


#==============================================================================#
# -11.2- Set up formulas and equations 
#==============================================================================#

drs <- c("0","3","","15")

rhs_list <- list() 

#net benefits
rhs_list["placeholder"] <- paste("b_net##_solw_defl_thous",
                                 paste(controlvars,collapse="+"),sep="+") #generic placeholder 
for (i in seq_along(drs)){
  rhs_list[paste0("rhs",i)] <- gsub("##",drs[i],rhs_list["placeholder"])
}
#separate benefits
rhs_list["placeholder"] <- paste(
  "b_gc##_solw_defl_thous+b_nm##_solw_defl_thous+b_protar##_pos_defl_thous+b_qw##_defl_thous",
  paste(controlvars,collapse="+"),sep="+") #generic placeholder 
rhs_list["placeholderIV"] <- paste(
  "b_gc##_solw_defl_thous+b_nm##_solw_defl_thous+b_protar##_pos_subreg_defl_thous+b_qw##_subreg_defl_thous+res_1stage",
  paste(controlvars,collapse="+"),sep="+") #generic placeholder 


for (i in seq_along(drs)){
  rhs_list[paste0("rhs",i+length(drs))] <- gsub("##",drs[i],rhs_list["placeholder"])
  rhs_list[paste0("rhsIV",i,".2st")] <- gsub("##",drs[i],rhs_list["placeholder"])
}

rhs_list <- rhs_list[!grepl("placeholder",names(rhs_list))]

eq_list_monthly <- setupformulas(rhs_list = rhs_list, depvars = "pv_count")

#==============================================================================#
# -11.3- Run PPMLE regressions 
#==============================================================================#

regress_list_monthly_drs <- runregressions(pdata = pdat.monthly,
                                           eq_list = eq_list_monthly[!grepl("IV",names(eq_list_monthly))])

linearHypothesis(regress_list_monthly_drs[["pfe.m8.pv_count"]], c("b_qw15_defl_thous=b_nm15_solw_defl_thous"))
linearHypothesis(regress_list_monthly_drs[["pfe.m8.pv_count"]], c("b_gc15_solw_defl_thous =b_nm15_solw_defl_thous"))

pv_dr <- paste0("pfe.m",5:8,".pv_count")

#==============================================================================#
# -10.5- Print & save tables 
#==============================================================================#

#drs <- c("","0","3","15")

benefitvars_labeled <- c("b_net_solw_defl_log"="Net benefits (log)",
                         "b_net_solw_defl_thous"="Net benefits",
                         "b_gc_solw_defl_thous"="Output-based incentive",
                         "b_nm_solw_defl_thous"="Net metering",
                         "b_protar_pos_defl_thous"="Capacity-based cost",
                         "b_qw_defl_thous"="Capacity-based incentive",
                         "b_protar_pos_subreg_defl_thous"="Capacity-based cost",
                         "b_qw_subreg_defl_thous"="Capacity-based incentive")
# Function to generate new labels with drs values
generate_benefitvars <- function(drs_value, vars) {
  new_names <- gsub("(net|gc|nm|protar|qw)", paste0("\\1", drs_value), names(vars))
  setNames(vars, new_names)
}

# Apply the function to each value in drs
benefitvars_all_labeled <- lapply(drs, generate_benefitvars, vars = benefitvars_labeled)

# Combine all the lists into one named vector
benefitvars_labeled <- do.call(c, benefitvars_all_labeled)

estvars_labeled <- c(benefitvars_labeled,controlvars_labeled)


setFixest_dict(estvars_labeled) #attach labels to estimation sample

etable_headers_dr <- list("^ " = list("0\\% DR" = 1, "3\\% DR" = 1, "10\\% DR (baseline)"=1, "15\\% DR"=1))

print("Table A.12")
#names(regress_list_monthly_full)
etable(regress_list_monthly_drs[pv_dr],
       #vcov=pv_dr_se,
       digits=3,
       group=etable_group_control,
       extralines = etable_extralines,
       headers = etable_headers_dr,
       tex=F,
       order = etable_order)


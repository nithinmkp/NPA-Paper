# Set up
lapply(c("functions.R","unitroots.R"),source)

#Packages
packages<-c("urca","janitor","tidyverse","readxl","huxtable","flextable","data.table",
            "rlist","rtf")
package_fn(packages)

#Data
data_df<-read_excel("Data/MAster_data.xlsx",sheet = "Sheet2")
data_na<-data_df %>% drop_na()

#Data preparation1
vars<-data_df %>% dplyr::select(-Period) %>% names()
ncols=length(vars) #number of variables to test for stationarity
tab1<-ts_fn(data_na,cols = vars) #all tables (levels, first difference)

#tests results
test_results<-map(tab1,test_stats)

#test result table
res_tabl<-map(test_results,stationary_table_fn)
res_tabl<-map(res_tabl,~.x %>% 
                      mutate(breakpoint=case_when(!is.na(breakpoint)~data_df$Period[breakpoint])))
# test -critical values
crit_table<-map(test_results,critcial_table_fn)


#outputs
writexl::write_xlsx(res_tabl,"stationarytest.xlsx") #test results
#critical tables
names_tables<-c("Unit Root Tables","Zivot-Andrews Test","KPSS-Test") 
word_fn("critcial_values_levels",names_tables,crit_table$levels)
word_fn("critical_values_diff",names_tables,crit_table$diff)

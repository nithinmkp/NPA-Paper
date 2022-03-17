library(tseries)
library(tidyverse)
library(broom)
# Unit root tests-Table ---------------------------------------------------

unit_tests<-function(x){
        
        list("ADF"=adf.test(x) %>% tidy() ,
             "PP"=pp.test(x) %>% tidy(),
             "KPSS"=kpss.test(x) %>% tidy()
        )
}

unitroot_fn<-function(x){
        
        r1<-map(x,~unit_tests(.x)) %>% map(bind_rows)
        methods<-map_dfr(r1,"method")
        methods<-methods[,1,drop=T]
        r2<-r1  %>% 
                map_df(bind_rows,.id = "Variable")%>% 
                dplyr::select(-alternative,
                       "Lag-length"=parameter) %>% 
                pivot_wider(names_from = "method",
                            values_from = c("statistic","p.value","Lag-length"),
                            names_glue = "{method}_{.value}") %>% 
                dplyr::select(Variable,starts_with(methods))
        return(r2)
        
}

# Complete TS table -------------------------------------------------------

ts_fn <- function(df, cols, order = 1) {
        df<-df[,-1]
        diff_fn <- function(x) {
                assign(paste0("diff_", x), diff(x))
        }
        
        diff_lst <- df  %>%
                map(diff_fn) %>%
                map(bind_cols) %>%
                map_dfc(bind_cols)
        names(diff_lst)<-paste0("diff_",names(df))
        
        
        
        lst <- list(
                levels = df,
                diff = diff_lst
        )
        return(lst)
}






# Functions ---------------------------------------------------------------

## ---- Package Function
package_fn<-function(pkg){
  new.pkg<-setdiff(pkg,installed.packages()[,"Package"])
  if(length(new.pkg)){
    install.packages(new.pkg,dependencies = T)
    sapply(pkg,library,character.only=T)
  }else{
    sapply(pkg,library,character.only=T)
  }
}


## ---- type conversion function
convert_fn<-function(df, col_ind,fn,...) {
  df <- df %>% mutate(across(.cols = col_ind, .fns = fn,...))
}


unit_urca_fn<-function(x){
        res<-list(ur.df(x,type = "none",selectlags = "AIC"),
                  ur.df(x,type = "drift",selectlags = "AIC"),
                  ur.df(x,type = "trend",selectlags = "AIC"),
                  ur.pp(x,type = "Z-tau",model = "constant"),
                  ur.pp(x,type = "Z-tau",model = "trend"),
                  ur.ers(x, type = "DF-GLS",model = "constant",lag.max = 4),
                  ur.ers(x, type = "DF-GLS",model = "trend",lag.max = 4))
        ntests<-length(res)
        res
        
}
za_function<-function(x){
        res<-list( ur.za(x,model = "intercept",lag=4),
                   ur.za(x,model = "trend",lag=4),
                   ur.za(x,model = "both",lag=4))
}
kpss_function<-function(x){
        res<-list(ur.kpss(x,type = "mu",lags = "short"),
                  ur.kpss(x,type = "tau",lags = "short"))
}


test_stats<-function(x){
        
        unit_stats<-map(x,~unit_urca_fn(.x)) %>% unlist()
        za_stats<-map(x,~za_function(.x)) %>% unlist()
        kpss_stats<-map(x,~kpss_function(.x))%>% unlist()
        results<-list(unit_stats=unit_stats,
                      za_stats=za_stats,
                      kpss_stats=kpss_stats)
        
        
}

stationary_table_fn<-function(x){
        res1<-x$unit_stats %>% map_df(.,~data.table("Test Statistic"=round(.x@teststat,3),model=.x@model,
                                                    test=.x@test.name),.id="Variable")
        res2<-x$za_stats %>% map_df(.,~data.table("Test Statistic"=round(.x@teststat,3),
                                                  model=.x@model,
                                                  "breakpoint"=.x@bpoint,
                                                  test=.x@test.name),.id="Variable")
        res3<-x$kpss_stats %>% map_df(.,~data.table("Test Statistic"=round(.x@teststat,3),
                                                    test=.x@test.name),.id="Variable")
        final_list<-lst(unittest=res1,
                        za_test=res2,
                        kpss_test=res3) 
        final_table<-final_list %>% reduce(full_join)
        
        
}



critcial_table_fn<-function(x,df){
        res1<-x$unit_stats %>% map(~data.table(round(.x@cval,3),
                                               .x@test.name,
                                               .x@model,
                                               keep.rownames = T)) %>% list.rbind()
        res1<-res1[!duplicated(res1),]
        setnames(res1,c("rn","V2","V3"),c("Statistic","Test","Model"))
        res2<-x$za_stats %>% map(~tibble(rn="",
                                         "1pct"=.x@cval[1],
                                         "5pct"=.x@cval[2],
                                         "10pct"=.x@cval[3],
                                         .x@model),
                                 keep.rownames = T) %>% list.rbind() %>% 
                as.data.table()
        setnames(res2,c("rn",".x@model"),c("Statistic","Model"))
        
        res2<-res2[!duplicated(res2),]
        res3<-x$kpss_stats %>% map(~data.table(.x@cval,
                                               .x@type,
                                               keep.rownames = T)) %>% list.rbind()
        res3<-res3[!duplicated(res3),]
        setnames(res3,c("rn","V2"),c("Statistic","Type"))
        
        final<-list(unittest=res1,
                    za_test=res2,
                    kpss_test=res3)
        
        
        
}
#Writing critical value table to word

word_fn<-function(tab,x,y){
        tab_name<-RTF(file = paste0(tab,".doc"))
        tab_fn<-function(x,y){
                addParagraph(tab_name,x,"\n")
                addTable(tab_name,y)
                addParagraph(tab_name,"\n")      
        }
        walk2(x,y,tab_fn)
        done(tab_name)
}

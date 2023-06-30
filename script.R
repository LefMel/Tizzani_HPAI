getwd()
setwd("C:/Users/LefMel/Documents/Publications-Analyses/Paolo Tizzani Avian Influenza")
list.files()

# Discussion points - Ideas to incorporate

# add Region,explore Region relationships / correlation between regions
# split data origin - Domestic / Wildlife
# 

library(openxlsx)
library(EVI)
library(ggplot2)
library(googledrive)
library(XML)
library(rlist)
#1gdWDhELYMU0lmCQz3po3Ydb7Eb1gF7j_NCs_xluCXQI
drive_deauth()
temp <- tempfile(tmpdir = getwd(), fileext = ".zip")
dl <- drive_download(
  as_id("1w3yAnU5HO43KQIH8KM9lxQyDgYPX6J5KlIazypPAL-4"), path = temp, overwrite = TRUE)
out <- unzip(temp, exdir = tempdir())

link = paste("file:///",out[1], sep="")

# Works 
tables <- readHTMLTable(link)
tables <- list.clean(tables, fun = is.null, recursive = FALSE)
new_data = data.frame(tables[1])
new_data = new_data[-1,-1]
headers = c("Date",	"Global", "Africa",
            "America", "Asia_Pacific", "Europe")
colnames(new_data) = headers
#n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))

View(new_data)
getwd()
str(new_data)

data = new_data
str(data)




deviant=function(new_cases, cum = FALSE, r_a=7, r=0.2, lag_max=30, past=length(new_cases), method="EVI", origin="2004/12/31"){
  #source("mova.r")
  #source("medvol.r")
  #source("evi.r")
  #source("evifcut.r")
  #source("indic.r")
  #source("status.r")
  #source("rollsd.r")
  
  evifcut=function (evi=NA, cevi=NA, cases, cut=NA, r,method="EVI")
  {
    w_s = 7
    ratio = 1/(1 + r)
    test_p = rep(NA, length(cases))
    true_p = rep(NA, length(cases))
    
    if(method=="EVI"){
      for (i in w_s:(length(cases) - w_s)) {
        if (evi[i] >= cut && cases[i] > mean(cases[i:(i - 6)])) {
          test_p[i] = 1
        }
        else {
          test_p[i] = 0
        }
        if (mean(cases[(i - (w_s - 1)):i]) <= ratio * mean(cases[(i +
                                                                  1):(i + w_s)])) {
          true_p[i] = 1
        }
        else {
          true_p[i] = 0
        }
      }
    }
    
    if(method=="cEVI"){
      for (i in w_s:(length(cases)-w_s)){
        if ((!is.na(cevi[i]) & cevi[i]  == 1) && cases[i]>mean(cases[i:(i-7)])){
          test_p[i] <- 1
        }else{
          test_p[i] <- 0
        }
        
        cond2<-mean(cases[(i):(i-w_s+3)],na.rm=T) <= ratio * mean(cases[(i+1):(i+w_s)],na.rm=T)
        if (!is.na(cond2) & cond2==TRUE){
          true_p[i] <- 1
        }else{
          true_p[i] <- 0
        }
      }
    }
    
    sens = length(which(test_p == 1 & true_p == 1))/length(which(true_p == 1))
    spec = length(which(test_p == 0 & true_p == 0))/length(which(true_p == 0))
    ppv = length(which(test_p == 1 & true_p == 1))/length(which(test_p == 1))
    npv = length(which(test_p == 0 & true_p == 0))/length(which(test_p == 0))
    sens[is.nan(sens)] <- 0
    spec[is.nan(spec)] <- 0
    sens[is.nan(sens)] <- 0
    spec[is.nan(spec)] <- 0
    testsin = length(which(test_p == 1))/(length(cases) - w_s)
    prev = length(which(true_p == 1))/(length(cases) - w_s)
    evifcut <- list(sens = sens, spec = spec, testsin = testsin,
                    prev = prev, ppv=ppv, npv=npv)
    return(evifcut)
  }
  
  
  indic=function (evi=NA, cevi=NA, cut=NA, cases, method="EVI")
  {
    
    if(method=="EVI"){
      ind = rep(NA, length(evi))
      for (i in 3:length(evi)) {
        if (evi[i] >= cut && cases[i] > mean(cases[i:(i - min(7,i))])){
          ind[i] = 1
        }
        else {
          ind[i] = 0
        }
      }
    }
    
    if(method=="cEVI"){
      ind <- rep(NA,length(cevi))
      
      for (i in 3:length(cevi)) {
        if (!is.na(cevi[i]) && cevi[i] ==1 && (!is.na(cases[i]) & cases[i] > mean(cases[i:(i - min(7, i))]))){
          ind[i] <- 1
        }else{
          ind[i] <- 0
        }
      }
    }
    
    return(ind)
  }
  
  
  cEVI_fun<-function(cases,lag_n,c_n){
    
    cevi <- rep(NA, length(cases))
    for(k in (lag_n-1):(length(cases)-(lag_n+1))){
      enu=mean(cases[(k+2):(k+lag_n+1)]-cases[(k+1):(k-(lag_n-2))],na.rm = T)
      den1=sd(cases[(k+1):(k-(lag_n-2))])^2/(length(cases[(k+1):(k-(lag_n-2))]))
      den2=sd(cases[(k+2):(k+lag_n+1)])^2/(length(cases[(k):(k+lag_n+1)]))
      
      # Spectral variances possibly more appropriate but more time consuming
      #den1=spectrum0.ar(cases[(i+1):(i+w_s)])$spec/(length(cases[(i+1):(i+w_s)])) # Spectral variances
      #den2=spectrum0.ar(cases[(i):(i-(w_s-1))])$spec/(length(cases[(i):(i-(w_s-1))]))
      testthat=enu/sqrt(den1+den2)
      #    if(test=="ztest"){ # Not large difference between a ztest and a ttest.
      #      cevi[k+lag_n+1]=as.numeric((1-pnorm(q = testthat))<=c_n) #*as.numeric(evi[i] >= rate)
      #    }
      #    if(test=="ttest"){
      cevi[k + lag_n + 1] = as.numeric((1-pt(q = testthat,df = lag_n-1))<=c_n)
      
      #    }
    }
    return(cevi)
  }
  
  
  if(method=="EVI"){
    start_time = Sys.time()
    start_cases=14
    lag_1=7
    c_1=0.01
    w_s =7
    
    
    
    if (cum == TRUE) new_cases = c(new_cases[1], diff(new_cases))
    
    
    #calculate the moving average of new confrimed cases
    cases=mova(new_cases,r_a)
    
    roll=rollsd(cases[1:start_cases],lag_1)
    ev=evi(roll)
    ind=indic(evi = ev,cut = c_1, cases = cases[1:start_cases],method = "EVI")
    status=status(cases[1:start_cases],r)
    
    #initiate chain for positive predictive value
    ppv=rep(NA, length(cases))
    
    #initiate chain for negative predictive value
    npv=rep(NA, length(cases))
    
    lag_all=rep(NA, start_cases)
    c_all=rep(NA, start_cases)
    
    se_all=rep(NA, start_cases)
    sp_all=rep(NA, start_cases)
    
    
    lag_all[1:start_cases]=lag_1
    c_all[1:start_cases]=c_1
    
    
    for (i in (start_cases+1): length(cases)){
      
      #case_t=cases[1:i]
      case_t=cases[max(1,(i-past)):i]
      #lag_s=7
      lag_s=seq(lag_1,min(lag_max,(length(case_t)-1)), 1)
      #lag_s=seq(lag_1,min(length(case_t),50), 1)
      c_s=seq(0.01,0.5, 0.01)
      #all_j=NA
      
      all_lag=NA
      all_cut=NA
      all_se=NA
      all_sp=NA
      
      
      
      for (j in lag_s){
        roll_t=rollsd(case_t,j)
        ev_t=evi(roll_t)
        for (l in c_s){
          evicut_t=evifcut(evi = ev_t, cases = case_t, cut = l, r = r,method = "EVI")
          new_j=j
          new_l=l
          new_se=evicut_t$sens
          new_sp=evicut_t$spec
          all_lag[[length(all_lag) + 1]] <- new_j
          all_cut[[length(all_cut) + 1]] <- new_l
          all_se[[length(all_se) + 1]] <- new_se
          all_sp[[length(all_sp) + 1]] <- new_sp
          
          
        }
      }
      
      
      
      sesp=as.data.frame(cbind(all_lag,all_cut,all_se,all_sp))
      
      
      
      
      #Select the row with the right window and cut
      index=which.max(sesp$all_se+sesp$all_sp-1)
      
      #index=sesp[which(sesp$all_sp>0.80),]
      #index=which.max(index$all_se)
      #index=which(sesp$all_se==1 & sesp$all_sp>=0.95),1)
      #if (i>40)
      #   {index1=sesp[which(sesp$all_sp>0.95),]
      #  index=which.max(index1$all_se)
      #   }
      #else
      #{index=which.max(sesp$all_se+sesp$all_sp-1)}
      
      
      #index=which(sesp$se>=0.6 & sesp$sp>0.9)
      print(i)
      print(sesp[index,])
      
      
      
      #estimate the parameters for the last observed case
      lag_n=sesp$all_lag[index]
      c_n=sesp$all_cut[index]
      
      roll_n=rollsd(case_t,lag_n)
      
      ev_n=evi(roll_n)
      ind_n=indic(evi = ev_n,cut = c_n, cases = case_t,method = "EVI")
      evicut_n=evifcut(evi = ev_n, cases = case_t, cut = c_n, r = r,method = "EVI")
      
      roll=c(roll,roll_n[length(ind_n)])
      ev=c(ev,ev_n[length(ind_n)])
      ind=c(ind, ind_n[length(ind_n)])
      
      lag_all=c(lag_all,lag_n)
      c_all=c(c_all,c_n)
      
      se_all=c(se_all,all_se[index])
      sp_all=c(sp_all,all_sp[index])
      
      ppv[i]=evicut_n$prev*all_se[index]/
        (evicut_n$prev*all_se[index]+(1-evicut_n$prev)*(1-all_sp[index]))
      
      npv[i]=(1-evicut_n$prev)*all_sp[index]/
        ((1-evicut_n$prev)*all_sp[index]+evicut_n$prev*(1-all_se[index]))
      
      
      
    }
    
    Days=as.Date(1:length(cases),  origin="2004/12/31")
    EVI=ev
    Cases=cases
    Index=ind
  }
  
  if(method=="cEVI"){
    start_time = Sys.time()
    start_cases=18
    lag_1=3
    c_1=0.001
    w_s=7
    if (cum == TRUE){
      new_cases = c(new_cases[1], diff(new_cases))
    }
    cases=mova(new_cases, r_a)
    #cases=new_cases
    cev=cEVI_fun(cases = cases[1:(start_cases)],lag_n = lag_1, c_n = c_1)
    ind=indic(cevi=cev, cases=cases[1:start_cases], method="cEVI")
    status=status(cases[1:start_cases],r)
    
    #initiate chain for positive predictive value
    ppv=rep(NA, length(cases))
    #initiate chain for negative predictive value
    npv=rep(NA, length(cases))
    
    lag_all=rep(NA, start_cases)
    c_all=rep(NA, start_cases)
    se_all=rep(NA, start_cases)
    sp_all=rep(NA, start_cases)
    lag_all[1:start_cases]=lag_1
    c_all[1:start_cases]=c_1
    
    for (i in (start_cases+1): length(cases)){
      
      case_t=cases[1:i]
      #case_t=cases[max(1,(i-past)):i]
      #lag_s=seq(lag_1,min(lag_max,(i-i/2-4)), 2)
      i_n<-max(1,(i-past)):i
      case_t=cases[i_n]
      lag_s=seq(lag_1,min(lag_max,(length(i_n)-length(i_n)/2-4)), 2)
      c_s=seq(0.001,0.5, 0.06)
      all_lag=NA
      all_cut=NA
      all_se=NA
      all_sp=NA
      
      for (l in c_s) {
        for (j in lag_s) {
          # roll_t <- rollsd(case_t,j)
          #  ev_t <- evi(roll_t)
          cevi <- rep(NA, length(case_t))
          for(k in (j+1):(length(case_t)-(j+1))){
            enu=mean(case_t[(k+2):(k+j+1)]-case_t[(k):(k-(j-1))],na.rm = T)
            den1=sd(case_t[(k):(k-(j-1))])^2/(length(case_t[(k):(k-(j-1))]))
            den2=sd(case_t[(k+2):(k+j+1)])^2/(length(case_t[(k+2):(k+j+1)]))
            # Spectral variances more appropriate but more time consuming
            #den1=spectrum0.ar(case_t[(i+1):(i+w_s)])$spec/(length(case_t[(i+1):(i+w_s)])) # Spectral variances
            #den2=spectrum0.ar(case_t[(i):(i-(w_s-1))])$spec/(length(case_t[(i):(i-(w_s-1))]))
            teststat=enu/sqrt(den1+den2)
            
            #cevi[k+j+1]=as.numeric((1-pt(q = test,df = j))<=l)#*as.numeric(evi[i] >= rate)
            #cevi[k+j+1]=as.numeric((1-pnorm(test))<=l)#*as.numeric(evi[i] >= rate)
            #cevi[k+j+1]=as.numeric((1-pnorm(test))<=l)#*as.numeric(evi[i] >= rate)
            Nn=length((k+1):(k+j))
            # if(test=="ztest"){
            #  cevi[k+j+1]<<-as.numeric((1-pnorm(q = teststat))<=l)#*as.numeric(evi[i] >= rate)
            #}
            #if(test=="ttest"){
            
            cevi[k+j+1]<-as.numeric((1-pt(q = teststat,df = Nn))<=l)
            #}
            
          }
          evicut_t <- evifcut(cevi=cevi,cases = case_t, r = r,method = "cEVI")
          all_lag[[length(all_lag) + 1]] <- j
          all_cut[[length(all_cut) + 1]] <- l
          all_se[[length(all_se) + 1]] <- evicut_t[[1]]
          all_sp[[length(all_sp) + 1]] <- evicut_t[[2]]
        }
      }
      
      sesp=as.data.frame(cbind(all_lag,all_cut,all_se,all_sp))
      index=which.max(sesp$all_se+sesp$all_sp-1)
      
      print(i)
      print(sesp[index,])
      
      lag_n=sesp$all_lag[index]
      c_n=sesp$all_cut[index]
      
      cevi_n=cEVI_fun(cases = case_t,lag_n = lag_n, c_n = c_n) #
      ind_n=indic(cevi = cevi_n, cases = case_t, method="cEVI") #
      evicut_n=evifcut(cevi = cevi, cases = case_t, r = r, method="cEVI") #
      
      cev=c(cev,cevi_n[length(ind_n)])
      ind=c(ind, ind_n[length(ind_n)])
      lag_all=c(lag_all,lag_n)
      c_all=c(c_all,c_n)
      
      se_all=c(se_all,all_se[index])
      sp_all=c(sp_all,all_sp[index])
      
      ppv[i]=evicut_n$prev*all_se[index]/
        (evicut_n$prev*all_se[index]+(1-evicut_n$prev)*(1-all_sp[index]))
      npv[i]=(1-evicut_n$prev)*all_sp[index]/
        ((1-evicut_n$prev)*all_sp[index]+evicut_n$prev*(1-all_se[index]))
    }
    
    Days=as.Date(1:length(cases),  origin="2004/12/31")
    EVI=cev
    Cases=cases
    Index=ind
  }
  
  EVI_out = as.data.frame(cbind(Days, EVI, Cases, Index, ppv,
                                npv, lag_all, c_all, se_all, sp_all, new_cases))
  EVI_output = EVI_out
  EVI_output <<- (EVI_output)
  end_time = Sys.time()
  time_elapsed = end_time - start_time
  print(time_elapsed)
  return(EVI_output)
  
}



EVI_Global = deviant(as.numeric(data$Global[which(data$Global!="")]), r_a = 30, lag_max = 15, origin="2004/12/31", past = 365, method = "EVI")
save(EVI_Global, file="EVI_Global")
evi.graphs(EVI_Global)
load("EVI_Global")


#cEVI_Global = deviant(as.numeric(data$Global[which(data$Global!="")]), r_a = 7, origin="2004/12/31", past = 365, method = "cEVI")
#save(cEVI_Global, file="cEVI_Global")


EVI_Africa = deviant(as.numeric(data$Africa[which(data$Africa!="")]), r_a = 30, lag_max = 15, origin="2004/12/31", past = 365, method = "EVI")
save(EVI_Africa, file="EVI_Africa")

#cEVI_Africa = deviant(as.numeric(data$Africa[which(data$Africa!="")]),  r_a = 30, lag_max = 15, origin="2004/12/31", past = 365, method = "cEVI")
#save(cEVI_Africa, file="cEVI_Africa")


EVI_America = deviant(as.numeric(data$America[which(data$America!="")]),  r_a = 30, lag_max = 15, origin="2004/12/31", past = 365, method = "EVI")
save(EVI_America, file="EVI_America")

#cEVI_America = deviant(as.numeric(data$America[which(data$America!="")]),  r_a = 30, lag_max = 15, origin="2004/12/31", past = 365, method = "cEVI")
#save(cEVI_America, file="cEVI_America")


EVI_Asia_Pacific = deviant(as.numeric(data$Asia_Pacific[which(data$Asia_Pacific!="")]),  r_a = 30, lag_max = 15, origin="2004/12/31", past = 365, method = "EVI")
save(EVI_Asia_Pacific, file="EVI_Asia_Pacific")


















rollsd <- function (cases, lag_t = 7, value_i) 
{
  rollsd = rep(NA, value_i)
  for (i in 1:lag_t) {
    rollsd[i] = medvol(cases[1:i])
  }
  for (i in (lag_t + 1):value_i) {
    rollsd[i] = medvol(cases[(i - (lag_t - 1)):i])
  }
  return(rollsd)
}

evi <- function (rollsd) 
{
  evi = rep(NA, length(rollsd))
  for (i in 2:length(rollsd)) {
    evi[i] = (rollsd[i] - rollsd[i - 1])/rollsd[i - 1]
  }
  evi[is.nan(evi)] <- 0
  return(evi)
}

indic <- function (evi, cut, cases) 
{
  ind = rep(NA, length(evi))
  for (i in 3:length(evi)) {
    if (evi[i] >= cut && cases[i] > mean(cases[i:(i - min(7, i))])) {
      ind[i] = 1
    }
    else {
      ind[i] = 0
    }
  }
  return(ind)
}


# case_t=cases[max(1,i-past):i]

# roll_n=rollsd(cases[max(1,i-past):i],lag_n)

# c(roll,roll_n[ i ]) - > c(roll, roll_n[length(ind)])
# c(ev,ev_n[ i ]) - > c( ev, ev_n[length(ind)])
# c(ind, ind_n[ i ]) - > c(in, ind_n[length(ind)])

deviant <-  function (new_cases, cum = FALSE, r_a = 7, r = 0.2, lag_max = 30, origin="1899-12-30") {
  start_time = Sys.time()
  start_cases = 14
  lag_1 = 7
  c_1 = 0.01
  w_s = 7
  if (cum == TRUE) 
    new_cases = c(new_cases[1], diff(new_cases))
  cases = mova(new_cases, r_a)
  roll = rollsd(cases[1:start_cases], lag_1, value_i = 14)
  ev = evi(roll)
  ind = indic(ev, c_1, cases[1:start_cases])
  status = status(cases[1:start_cases], r)
  ppv = rep(NA, length(cases))
  npv = rep(NA, length(cases))
  lag_all = rep(NA, start_cases)
  c_all = rep(NA, start_cases)
  se_all = rep(NA, start_cases)
  sp_all = rep(NA, start_cases)
  lag_all[1:start_cases] = lag_1
  c_all[1:start_cases] = c_1
  for (i in (start_cases + 1):length(cases)) {
    a = i
    case_t = cases[max(1, i-50):i]
    lag_s = seq(lag_1, min(lag_max, (length(case_t) - 1)), 
                1)
    c_s = seq(0.01, 0.5, 0.01)
    all_lag = NA
    all_cut = NA
    all_se = NA
    all_sp = NA
    for (j in lag_s) {
      roll_t = rollsd(case_t, j, value_i = a)
      ev_t = evi(roll_t)
      for (l in c_s) {
        evicut_t = evifcut(ev_t, case_t, l, r)
        new_j = j
        new_l = l
        new_se = evicut_t$sens
        new_sp = evicut_t$spec
        all_lag[[length(all_lag) + 1]] <- new_j
        all_cut[[length(all_cut) + 1]] <- new_l
        all_se[[length(all_se) + 1]] <- new_se
        all_sp[[length(all_sp) + 1]] <- new_sp
      }
    }
    sesp = as.data.frame(cbind(all_lag, all_cut, all_se, 
                               all_sp))
    index = which.max(sesp$all_se + sesp$all_sp - 1)
    print(i)
    print(sesp[index, ])
    lag_n = sesp$all_lag[index]
    c_n = sesp$all_cut[index]
    roll_n = rollsd(case_t, lag_n, value_i = a)
    ev_n = evi(roll_n)
    ind_n = indic(ev_n, c_n, case_t)
    evicut_n = evifcut(ev_n, case_t, c_n, r)
    roll = c(roll, roll_n[i])
    ev = c(ev, ev_n[i])
    ind = c(ind, ind_n[i])
    lag_all = c(lag_all, lag_n)
    c_all = c(c_all, c_n)
    se_all = c(se_all, all_se[index])
    sp_all = c(sp_all, all_sp[index])
    ppv[i] = evicut_n$prev * all_se[index]/(evicut_n$prev * 
                                              all_se[index] + (1 - evicut_n$prev) * (1 - all_sp[index]))
    npv[i] = (1 - evicut_n$prev) * all_sp[index]/((1 - evicut_n$prev) * 
                                                    all_sp[index] + evicut_n$prev * (1 - all_se[index]))
  }
  Days = as.Date(seq(1,length(cases), 1), origin="2004-12-31")
  EVI = ev
  Cases = cases
  Index = ind
  EVI_out = data.frame(Days, EVI, Cases, Index, ppv, 
                       npv, lag_all, c_all, se_all, sp_all)
  EVI_output = EVI_out
  EVI_output <<- (EVI_output)
  end_time = Sys.time()
  time_elapsed = end_time - start_time
  print(time_elapsed)
  return(EVI_output)
}


EVI_out = deviant(data$Outbreaks[1:200], r_a = 7, origin = "2021-10-02")

save(EVI_out, file="EVI_out")


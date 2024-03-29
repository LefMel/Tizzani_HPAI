setwd("/srv/shiny-server/HPAI")
setwd("C:/Users/LefMel/Documents/GitHub/Tizzani_HPAI")
getwd()
library(EVI)
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
#load("data")




deviant_update=function(all_cases=NA, new_cases=NA,
                        EVI_input, cum = FALSE, r_a=7,
                        r=0.2, lag_max=30, method="EVI", origin="2004/12/31"){
  #source("mova.r")
  #source("medvol.r")
  #source("evi.r")
  #source("evifcut.r")
  #source("indic.r")
  #source("status.r")
  #source("rollsd.r")
  
  if(method=="EVI"){
    start_cases=14
    lag_1=7
    c_1=0.01
    w_s =7
  }else if (method=="cEVI"){
    start_cases=18
    lag_1=3
    c_1=0.001
    w_s=7
  }
  
  
  
  
  if (cum == TRUE) new_cases = c(new_cases[1], diff(new_cases))
  
  if (!exists("EVI_output"))
    stop("Please run the deviant function first")
  
  #calculate the moving average of new confrimed cases
  if ((!is.na(sum(all_cases)) & !is.na(sum(new_cases))) | (is.na(sum(all_cases)) & is.na(sum(new_cases))))
    stop("Please provide either new cases or all cases (old+new) as input")
  
  if(!is.na(sum(all_cases))){
    #calculate the moving average of new confrimed cases
    cases=mova(c(EVI_input$new_cases,new_cases),r_a)
  }
  if(!is.na(sum(new_cases))){
    #calculate the moving average of new confrimed cases
    cases=mova(c(EVI_input$new_cases,new_cases),r_a)
  }
  
  
  roll=rollsd(cases[1:start_cases],lag_1)
  ev=evi(roll)
  
  if(method=="EVI"){
    ind=indic(evi = ev, cut = c_1, cases = cases[1:start_cases], method = method)
  }else if (method=="cEVI"){
    cevi=cEVI_fun(cases = cases[1:(start_cases)],lag_n = lag_1, c_n = c_1)
    ind=indic(cevi=cevi, cases=cases[1:start_cases], method="cEVI")
  }
  
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
  
  diff= length(cases)-(nrow(EVI_output) +1)
  
  for (i in (nrow(EVI_output)+1): length(cases)){
    
    case_t=cases[1:i]
    #case_t=cases[max(1,(i-33)):i]
    #lag_s=7
    lag_s=seq(lag_1,min(lag_max,(length(case_t)-1)), 1)
    #lag_s=seq(lag_1,min(length(case_t),50), 1)
    c_s=seq(0.01,0.5, 0.01)
    #all_j=NA
    
    all_lag=NA
    all_cut=NA
    all_se=NA
    all_sp=NA
    
    
    
    if(method=="EVI"){
      
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
      
    }
    
    if(method=="cEVI"){
      case_t=cases[1:i]
      lag_s=seq(lag_1,min(lag_max,(i-i/2-4)), 2)
      c_s=seq(0.001,0.5, 0.06)
      all_lag<-all_cut<-all_se<-all_sp<-NA
      
      
      for (l in c_s) {
        for (j in lag_s) {
          # roll_t <- rollsd(case_t,j)
          #  ev_t <- evi(roll_t)
          cevi <- rep(NA, length(case_t))
          for(k in (j+1):(length(case_t)-(j+1))){
            enu=mean(case_t[(k+2):(k+j+1)]-case_t[(k):(k-(j-1))],na.rm = T)
            den1=sd(case_t[(k):(k-(j-1))])^2/(length(case_t[(k):(k-(j-1))]))
            den2=sd(case_t[(k+2):(k+j+1)])^2/(length(case_t[(k+2):(k+j+1)]))
            teststat=enu/sqrt(den1+den2)
            Nn=length((k+1):(k+j))
            cevi[k+j+1]<-as.numeric((1-pt(q = teststat,df = Nn))<=l)
          }
          evicut_t <- evifcut(cevi=cevi,cases = case_t, r = r,method = "cEVI")
          all_lag[[length(all_lag) + 1]] <- j
          all_cut[[length(all_cut) + 1]] <- l
          all_se[[length(all_se) + 1]] <- evicut_t[[1]]
          all_sp[[length(all_sp) + 1]] <- evicut_t[[2]]
        }
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
    
    if (method=="EVI"){
      roll_n=rollsd(cases[1:i],lag_n)
      ev_n=evi(roll_n)
      ind_n=indic(evi = ev_n,cut = c_n, cases = case_t, method=method)
      evicut_n=evifcut(evi = ev_n, cases = case_t, cut = c_n, r = r, method=method)
    }else if (method=="cEVI"){
      cevi=cEVI_fun(cases = cases[1:i],lag_n = lag_n, c_n = c_n) #
      ind_n=indic(cevi = cevi,cut = c_n , cases = case_t, method=method) #
      evicut_n=evifcut(cevi = cevi, cases = case_t, r = r, method=method) #
    }
    
    roll=c(roll,roll_n[i])
    ev=c(ev,ev_n[i])
    ind=c(ind, ind_n[i])
    
    lag_all=c(lag_all,lag_n)
    c_all=c(c_all,c_n)
    
    se_all=c(se_all,all_se[index])
    sp_all=c(sp_all,all_sp[index])
    
    ppv[i]=evicut_n$prev*all_se[index]/
      (evicut_n$prev*all_se[index]+(1-evicut_n$prev)*(1-all_sp[index]))
    
    npv[i]=(1-evicut_n$prev)*all_sp[index]/
      ((1-evicut_n$prev)*all_sp[index]+evicut_n$prev*(1-all_se[index]))
    
    
    
  }
  
  Days=as.Date((length(cases)-diff):length(cases),  origin="2004/12/31") 
  EVI=ev[((length(ev)-diff):length(ev))]
  Cases=cases[((length(cases)-diff):length(cases))]
  Index=ind[((length(ind)-diff):length(ind))]
  ppv=ppv[((length(ppv)-diff):length(ppv))]
  npv=npv[((length(npv)-diff):length(npv))]
  lag_all=lag_all[((length(lag_all)-diff):length(lag_all))]
  c_all=c_all[((length(c_all)-diff):length(c_all))]
  se_all=se_all[((length(se_all)-diff):length(se_all))]
  sp_all=sp_all[((length(sp_all)-diff):length(sp_all))]
  
  
  EVI_out_add=data.frame(Days, EVI, Cases, Index, ppv, npv,
                                  lag_all, c_all, se_all, sp_all)
  
  EVI_output=rbind(EVI_output,EVI_out_add)
  
  EVI_output<<-(EVI_output)
  
  return(EVI_output)
  
}

# Outbreaks --> Global / Africa / America / Asia_Pacific / Europe
EVI_Global = deviant(as.numeric(new_data$Global[which(new_data$Global!="")]), r_a = 7, origin="2004/12/31", past = 365, method = "EVI")
save(EVI_Global, file="EVI_Global")
cEVI_Global = deviant(as.numeric(new_data$Global[which(new_data$Global!="")]), r_a = 7, origin="2004/12/31", past = 365, method = "cEVI")
save(cEVI_Global, file="cEVI_Global")

EVI_Africa = deviant(as.numeric(new_data$Africa[which(new_data$Africa!="")]), r_a = 7, origin="2004/12/31", past = 365, method = "EVI")
save(EVI_Africa, file="EVI_Africa")
cEVI_Africa = deviant(as.numeric(new_data$Africa[which(new_data$Africa!="")]), r_a = 7, origin="2004/12/31", past = 365, method = "cEVI")
save(cEVI_Africa, file="cEVI_Africa")

EVI_America = deviant(as.numeric(new_data$America[which(new_data$America!="")]), r_a = 7, origin="2004/12/31", past = 365, method = "EVI")
save(EVI_America, file="EVI_America")
cEVI_America = deviant(as.numeric(new_data$America[which(new_data$America!="")]), r_a = 7, origin="2004/12/31", past = 365, method = "cEVI")
save(cEVI_America, file="cEVI_America")

EVI_Asia_Pacific = deviant(as.numeric(new_data$Asia_Pacific[which(new_data$Asia_Pacific!="")]), r_a = 7, origin="2004/12/31", past = 365, method = "EVI")
save(EVI_Asia_Pacific, file="EVI_Asia_Pacific")
cEVI_Asia_Pacific = deviant(as.numeric(new_data$Asia_Pacific[which(new_data$Asia_Pacific!="")]), r_a = 7, origin="2004/12/31", past = 365, method = "cEVI")
save(cEVI_Asia_Pacific, file="cEVI_Asia_Pacific")


EVI_Europe = deviant(as.numeric(new_data$Europe[which(new_data$Europe!="")]), r_a = 7, origin="2004/12/31", past = 365, method = "EVI")
save(EVI_Europe, file="EVI_Europe")
cEVI_Europe = deviant(as.numeric(new_data$Europe[which(new_data$Europe!="")]), r_a = 7, origin="2004/12/31", past = 365, method = "cEVI")
save(cEVI_Europe, file="cEVI_Europe")


list.files()

data = new_data
save(data, file = "data")
  
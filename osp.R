
# this script requires ~/my.cnf configured properly 

work_dir = './'

readTable <- function (name, colNames, sep=',', url="http://10.46.0.58:8888/analysis/") {
        baseURL <- url
        fileName <- name
        if (!file.exists(fileName)) {
                url <- paste0(baseURL, fileName);
                download.file(url, destfile = fileName, method="wget")
        }
        w=read.table(fileName, header=F, sep = sep) 
        colnames(w)<-colNames
        w
}

subsite_name_by_id <- function (subsite_id) {
  subsites=load_subsite_names()
  subsiteName = as.data.frame (subsites[match(subsite_id, subsites[,1]),]$name)
  
  return (subsiteName)
}

sql_query <- function (query) { 
                mydb <<- dbConnect(MySQL(), group=db)
                t1 <- proc.time()
                rs <- dbSendQuery(mydb, query)
                rec=fetch(rs) 
                dbDisconnect(mydb)
                t2 = proc.time() - t1
                cat ("Time elapsed, s: ")
                print(t2["elapsed"])
                #mydata = as.matrix(rec)   
                rec
}


run_sql <- function (file_name, param1=0, param2=0, param3=0, param4=0) {
  print (paste("## Getting data on ", file_name))
  dir = paste0(work_dir, "cache/", file_name, "/")
  dir.create(dir, showWarnings = FALSE)
  csv_file = paste0(dir, file_name, "-", param1, "-", param2,  "-", param3, "-", param4, ".csv")
  cfg_file = paste0(dir, file_name, ".cfg")
  sql_file = paste0(work_dir,"sql/", file_name, ".sql")
  print (csv_file)   
  if (file.exists(csv_file)==TRUE) { 
    print (paste("data is cached in csv, reading ", csv_file))
    tr <- try(rec<-read.csv(file=csv_file))
    if (class(tr)=="try-error") return (list (c(),c()))
    title<-readChar(cfg_file, file.info(cfg_file)$size)
  } else
  {
    print ("data not found, re-fetching from DB...")
    mydb <<- dbConnect(MySQL(), group=db)
    print(paste0("Connected to DB - ", db))
    t1 <- proc.time()
    # q  <- readLines(sql_file,encoding="UTF-8")
    # q <- readChar(sql_file, file.info(sql_file)$size)
    lines <- readLines(sql_file,encoding="UTF-8") 
    #print (lines)
    if (param1 == 0 && length(grep("[?]1", lines))>0) {
      print ("param1 is 0")
      lines <- lines [- grep("?1", lines, fixed=T)]   # delete SQL line with parameter placeholder ?1 when param1 in 0 (used for ALL filter)
    }
    if (param2 == 0 && length(grep("[?]2", lines))>0) {
      lines <- lines [- grep("?2", lines, fixed=T)] 
    }
    
    if (param3 == 0 && length(grep("[?]3", lines))>0) {
      lines <- lines [- grep("?3", lines, fixed=T)] 
    }

    if (param4 == 0 && length(grep("[?]4", lines))>0) {
      lines <- lines [- grep("?4", lines, fixed=T)] 
    }

    # correct parameters: change -1 to 0 for cases when we need to substitute real 0 as a parameter, e.g. WHERE store_id=0
    if (param1 == -1) param1 = 0
    if (param2 == -1) param2 = 0
    if (param3 == -1) param3 = 0
    if (param4 == -1) param4 = 0
    
    
    q <- paste(lines[2:length(lines)], collapse=" ")   # skip the first line comment
    
    q <- sub ('?1', param1, q, fixed=TRUE) # substitute the parameter
    q <- sub ('?2', param2, q, fixed=TRUE) # substitute the parameter
    q <- sub ('?3', param3, q, fixed=TRUE) # substitute the parameter
    q <- sub ('?3', param3, q, fixed=TRUE) # substitute the parameter
    q <- sub ('?4', param4, q, fixed=TRUE) # substitute the parameter
    
    print(q)
    title <- lines[1] 
    write (title, file=cfg_file, append=FALSE)
     
    rs <- dbSendQuery(mydb, q)
    #print ("fetching records...")
    rec=fetch(rs)
    write.csv(rec, file = csv_file, row.names=FALSE)
    #dbClearResult(rec)
    dbDisconnect(mydb)
    t2 = proc.time() - t1
    cat ("Time elapsed, s: ")
    print(t2["elapsed"])
  }
  mydata = as.matrix(rec)  
  # TODO: trim title --
  return (list(mydata,title))
}

cohort_analysis <- function (subsite, platform) {
  db <<- "osp"
  res = run_sql ("cohort_analysis", subsite, platform)
  mydata <- res[[1]]
  title <- res[[2]]
  
  mydata = as.matrix(mydata)  
  c<-cbind(mydata[,2],as.numeric(mydata[,3]))
  
  nam = c[,1]
  barplot(as.numeric(c[,2]), main=title, ylab= "Total", beside=TRUE, col=rainbow(5), names.arg=nam)
  #text(0,-2, labels=title, col = "black")
  
  return(mydata)
} 

applisting_share <- function (subsite, platform) {
  db <<- "osp"
  res = run_sql ("applisting_share", subsite, platform)
  mydata <- res[[1]]
  title <- res[[2]]
  if (is.null(mydata)) return (mydata)
  mydata = as.matrix(mydata)  
  c<-t(cbind(as.numeric(mydata[,1]),as.numeric(mydata[,2])))
   
  nam = substr(mydata[,3], nchar(mydata[,3])-2+1, nchar(mydata[,3]))  # str_right(2)
  barplot(c, main=title, ylab= "Total", beside=TRUE, col=rainbow(5), names.arg=nam)
  #text(0,-2, labels=title, col = "black")
  
  return(mydata)
} 

channels_share <- function (subsite, platform) {
  db <<- "osp"
  res = run_sql ("channels_share", subsite, platform)
  mydata <- res[[1]]
  title <- res[[2]]
  
  mydata = as.matrix(mydata)  
  
  # pie chart
  pie(as.numeric(mydata[,1]),labels=mydata[,2],col=rainbow(13))
  text(0,-1, labels=title, col = "black")
  
  # bar chart
  b=as.matrix(as.numeric(mydata[,1]))
  barplot(b, main=title, ylab= "Total", beside=TRUE, col=rainbow(5), names.arg=mydata[,2])
  #text(0,-2, labels=title, col = "black")
  
  return(mydata)
} 

positions_analysis <- function (subsite, platform) {
  db <<- "osp"
  res = run_sql ("positions_analysis", subsite, platform)
  mydata <- res[[1]]
  title <- res[[2]]
  pie(mydata[,2],labels=mydata[,1],col=rainbow(13))
  text(0, -1, title, col = "black")
  return(mydata)
}


load_subsite_names <- function () {
  db <<- "bckp11"
  res = run_sql ("list_subsites")
  mydata <- res[[1]]
  return (as.data.frame(mydata));
}


kpi_monthly_downloads <- function (subsite=-1, platform=0, plot=FALSE, recent_range=0:0) {
  db <<- "db11_downloads_new"
  res = run_sql ("kpi_downloads_monthly", subsite, platform)
  mydata <- res[[1]]
  
  mydata <- as.matrix(mydata)
  platformName = ifelse (platform==0, "All Platforms", platform)
  subsiteName = ifelse (subsite==-1, "Global", subsite_name_by_id(subsite))
  if (plot==TRUE) {
    range = rev(nrow(mydata) - recent_range)
    barplot( as.numeric(mydata[range,2])/1000000, 
             names.arg = mydata[range,1],  
             ylab="Installs", xlab="Month periods",
             yaxt="n",
             main= paste0(subsiteName[[1]], " - " , platformName))
    my.axis <-paste(axTicks(2),"K",sep="")
    axis(2,at=axTicks(2), labels=my.axis)
  }

  return (mydata);
}

# global variable for the database credentials group in my.cnf
db <<- "osp"


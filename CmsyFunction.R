cmsyParseXml <- function (xml) {
  library("XML")
  
  data = xmlTreeParse(xml, useInternalNodes=TRUE)
  
  namespaces <- c(ogr="http://ogr.maptools.org/", gml="http://www.opengis.net/gml", d4science="http://www.d4science.org", wps="http://www.opengis.net/wps/1.0.0", ows="http://www.opengis.net/ows/1.1", xsi="http://www.w3.org/2001/XMLSchema-instance")
  
  df <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(df)<-c("url", "description", "mimetype")
  
  i <- 1;
  hasResult <- TRUE
  while (hasResult) {
    xpath <- paste0("//ogr:Result[", i, "]")
    results <- xpathApply(data, xpath, xmlValue, namespaces=namespaces)
    
    if (length(results) == 0) {
      hasResult = FALSE    
    } else {
      xpath1 <- paste0("//ogr:Result[", i, "]/d4science:Data")
      xpath2 <- paste0("//ogr:Result[", i, "]/d4science:Description")
      xpath3 <- paste0("//ogr:Result[", i, "]/d4science:MimeType")
      
      url <- xpathApply(data, xpath1, xmlValue, namespaces=namespaces)[[1]]
      description <- xpathApply(data, xpath2, xmlValue, namespaces=namespaces)[[1]]
      mimeType <- xpathApply(data, xpath3, xmlValue, namespaces=namespaces)[[1]]
      
      row <- c(as.character(url), as.character(description), as.character(mimeType))
      df[nrow(df) + 1,] <- row
      
      i <- i + 1
    }
  }
  
  return (df)
}

runCmsy <- function (region,subregion,stock,group,name,englishName,scientificName,source,minOfYear,maxOfYear,startYear,endYear,flim,fpa,blim,bpa,bmsy,fmsy,msy,msyBTrigger,b40,m,fofl,last_f,resiliance,r.low,r.hi,stb.low,stb.hi,int.yr,intb.low,intb.hi,endb.low,endb.hi,q.start,q.end,btype,force.cmsy,comments, username, token, inputCsvFile, templateFile)  {
 
  wps_uri = "http://dataminer7-p-d4s.d4science.org:80/wps/WebProcessingService" #"http://dataminer-bigdata.d4science.org:80/wps/WebProcessingService"
  
  #username <- "enrico.anello"
  #token <- "5b0f903a-3cb1-4424-a2bd-2700c9f1d4ed"
  wpsClient <- paste0(getwd(),"/WPS4D4Science.r")
  cat(file=stderr(), wpsClient, "\n")
  source(paste0(getwd(),"/WPS4D4Science.r"))
  library("tcltk")
  require("XML")
  
  data<-read.csv(inputCsvFile, header =T, sep=",")
  dimnames(data)[2][[1]]<-gsub("[[:punct:]]", "_", dimnames(data)[2][[1]])
  dimnames(data)[2][[1]]<-tolower(gsub(" ", "_", dimnames(data)[2][[1]]))
  dffile<-paste("/tmp/cmsy_data_",Sys.time(),".csv",sep="")
  dffile<-gsub(":", "_", dffile)
  dffile<-gsub(" ", "_", dffile)
  write.csv(data,file=dffile, quote = FALSE, eol = "\n", row.names = FALSE,  fileEncoding = "UTF-8")
  #LOAD THE TEMPLATE#    
  #templateFile="/home/enrico/Work/BlueBridge/Cmsy/cmsyForDlmToolsTemplate.xml";
  #PREPARE THE REQUEST FILE BY ALTERING THE TEMPLATE#    
  sentfile=paste("/tmp/cmsy_req_",Sys.time(),".xml",sep="")
  sentfile<-gsub(":", "_", sentfile)
  sentfile<-gsub(" ", "_", sentfile)
  filexml<-readChar(templateFile, file.info(templateFile)$size)
  filexml<-gsub("\r\n", "\n", filexml)
  
  #TAKE THE INPUT TABLE CONTENT FROM THE CSV FILE AS A STRING#  
  body<-readChar(dffile, file.info(dffile)$size)
  body<-gsub("\r\n", "\n", body)
  body<-gsub("\n$", "", body)
  #SUBSTITUTE INPUTS IN THE TEMPLATE AND EMBED THE FILE#
  filexml<-gsub("#FILE#", body, filexml)
  filexml<-gsub("#REGION#", region, filexml)
  filexml<-gsub("#SUBREGION#", subregion, filexml)
  filexml<-gsub("#STOCK#", stock, filexml)
  filexml<-gsub("#GROUP#", group, filexml)
  filexml<-gsub("#NAME#", name, filexml)
  filexml<-gsub("#ENGLISH_NAME#", englishName, filexml)
  filexml<-gsub("#SCIENTIFIC_NAME#", scientificName, filexml)
  filexml<-gsub("#SOURCE#", source, filexml)
  filexml<-gsub("#MIN_OF_YEAR#", minOfYear, filexml)
  filexml<-gsub("#MAX_OF_YEAR#", maxOfYear, filexml)
  filexml<-gsub("#START_YEAR#", startYear, filexml)
  filexml<-gsub("#END_YEAR#", endYear, filexml)
  filexml<-gsub("#FLIM#", flim, filexml)
  filexml<-gsub("#FPA#", fpa, filexml)
  filexml<-gsub("#BLIM#", blim, filexml)
  filexml<-gsub("#BPA#", bpa, filexml)
  filexml<-gsub("#BMSY#", bmsy, filexml)
  filexml<-gsub("#FMSY#", fmsy, filexml)
  filexml<-gsub("#MSY#", msy, filexml)
  filexml<-gsub("#MSYBTRIGGER#", msyBTrigger, filexml)
  filexml<-gsub("#B40#", b40, filexml)
  filexml<-gsub("#M#", m, filexml)
  filexml<-gsub("#FOFL#", fofl, filexml)
  filexml<-gsub("#LAST_F#", last_f, filexml)
  filexml<-gsub("#RESILIANCE#", resiliance, filexml)
  filexml<-gsub("#R.LOW#", r.low, filexml)
  filexml<-gsub("#R.HI#", r.hi, filexml)
  filexml<-gsub("#STB.LOW#", stb.low, filexml)
  filexml<-gsub("#STB.HI#", stb.hi, filexml)
  filexml<-gsub("#INT.YR#", int.yr, filexml)
  filexml<-gsub("#INTB.LOW#", intb.low, filexml)
  filexml<-gsub("#INTB.HI#", intb.hi, filexml)
  filexml<-gsub("#ENDB.LOW#", endb.low, filexml)
  filexml<-gsub("#ENDB.HI#", endb.hi, filexml)
  filexml<-gsub("#Q.START#", q.start, filexml)
  filexml<-gsub("#Q.END#", q.end, filexml)
  filexml<-gsub("#BTYPE#", btype, filexml)
  if (force.cmsy) {
    filexml<-gsub("#FORCE.CMSY#", "true", filexml)
  } else {
    filexml<-gsub("#FORCE.CMSY#", "false", filexml)
  }
  filexml<-gsub("#COMMENT#", comments, filexml)
  
  
  #WRITE THE MODIFIED XML TEMPLATE DOCUMENT LOCALLY#
  filehandle <- file(sentfile,"w+")
  write(filexml, file = sentfile,append = FALSE, sep = "")
  #write(filexml, file = "sentfile.xml",append = FALSE, sep = "")
  close(filehandle)
  
  #SEND THE REQUEST#  
  out<-POST(url = wps_uri, config=c(authenticate(username, token, type = "basic")),body = upload_file(sentfile, type="text/xml"),encode = c("multipart"), handle = NULL)
  
  #CHECK IF THE PROCESS HAS ALREADY FINISHED#
  stop_condition_success<-grepl("Process successful",as.character(out))
  stop_condition_fail<-grepl("Exception",as.character(out))
  
  #GET THE STATUS LOCATION FROM THE ACCEPTANCE RESPONSE#
  lout<-as.character(out)
  print(lout)
  statusLocation='statusLocation=\"'
  endstatusLocation='">\n'
  pos1 = regexpr(statusLocation, lout)
  pos2 = regexpr(endstatusLocation, lout)
  llout<-substr(lout, pos1+nchar(statusLocation), pos2-1)
  print(llout)
  
  #CHECK THE STATUS OF THE COMPUTATION UNTIL COMPLETION#
  while (!stop_condition_fail && !stop_condition_success){
    print("Checking...")
    #CHECK THE STATUS URL#
    out1<-GET(url = llout, config=c(authenticate(username, token, type = "basic")),handle = NULL, timeout(3600), encoding="utf-8")
    outstring<-content(out1, "text", encoding = "UTF-8")
    cat(file=stderr(), outstring, "\n")
    stop_condition_success<-grepl("ProcessSucceeded",outstring)
    stop_condition_fail<-grepl("Exception",outstring)
    #SLEEP FOR 10 SECONDS BEFORE THE NEXT CHECK#
    Sys.sleep(10)
  }
  
  print(content(out1, "text", encoding = "UTF-8"))
  if (stop_condition_success){
    closeAllConnections()
    file.remove(dffile)
    file.remove(sentfile)
    return (cmsyParseXml(out1))
  }
  
  closeAllConnections()
  
  
  file.remove(dffile)
  file.remove(sentfile)
  options(warn=0)
  
  return (NULL)
}

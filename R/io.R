#ingSys.setlocale(category = "LC_ALL", locale = "gl_ES.utf8")
#' Convert abanca to homebank
#' Convert abanca to homebank
#' @param inputfile Path to the input file
#' @param outputfile Path to the output file
#' @return nothing
#' @export
# abanca2hmbk<-function (inputfile,outputfile) {
#   nd<-readr::read_delim(inputfile,delim = ";",na = character(),locale = readr::locale(decimal_mark = ","))
#   nd$Concepto<-paste(nd$Concepto,nd$`Concepto ampliado`)
#   nd<-nd[,c(1,8,3,8,3,4,8)]
#   names(nd)<-c("date","payment","info","payee","memo","amount","category")  
#   nd$payment<-0
#   nd$payee<-"abanca_corriente"
#   nd$category<-""
#   nd$tags<-""
#   nd$info<-""
#   readr::write_delim(nd,outputfile,delim = ";")
# }
abanca2hmbk<-function (inputfile,outputfile) {
  nd<-readr::read_delim(inputfile,delim = ";",na = character(),skip = 4,locale = readr::locale(decimal_mark = ","))
#  nd$Concepto<-paste(nd$Concepto,nd$`Concepto ampliado`)
  nd<-nd[,c(1,8,8,8,5,7,8)]
  names(nd)<-c("date","payment","info","payee","memo","amount","category")  
  nd$payment<-0
  nd$payee<-"abanca_corriente"
  nd$category<-""
  nd$tags<-""
#  nd$info<-""
  readr::write_delim(nd,outputfile,delim = ";")
}
#' Convert bankinter to homebank
#' Convert bankinter to homebank
#' @param inputfile Path to the input file
#' @param outputfile Path to the output file
#' @return nothing
#' @export
bankinter2hmbk<-function (inputfile,outputfile) {
  data<-readxl::read_excel(inputfile,sheet = 1,skip = 3)
  data<-data[,c(1,5,5,5,3,4,5)]
  names(data)<-c("date","payment","info","payee","memo","amount","category")  
  data$payment<-0
  data$payee<-""
  data$category<-""
  data$tags<-""
  data$info<-""
  readr::write_delim(data,outputfile,delim = ";")
  
}
#' Convert bankinter credit to homebank
#' Convert bankinter credit to homebank
#' @param inputfile Path to the input file
#' @param outputfile Path to the output file
#' @return nothing
#' @export
bankintercredit2hmbk<-function(inputfile,outputfile) {
  data<-readxl::read_xls(inputfile,skip = 4,col_types = c("date","text","numeric"))
  nd<-data[2:(dim(data)[[1]]-1),]
  nd<-nd[,c(1,2,2,2,2,3,2)]
  names(nd)<-c("date","payment","info","payee","memo","amount","category")  
  nd$payment<-0
  nd$payee<-""
  nd$category<-""
  # se puede mejorar mapeando categoria
  nd$tags<-""
  #dt$info<-""
  readr::write_delim(nd,outputfile,delim = ";") 
}
#' Convert selfbank to homebank
#' Convert selfbank to homebank
#' @param inputfile Path to the input file
#' @param outputfile Path to the output file
#' @return nothing
#' @export
self2hmbk<-function(inputfile,outputfile) {
  dat<-readr::read_delim(inputfile,delim = ";",locale=readr::locale(encoding = "ISO-8859-2"))
  dat<-dat[,c(1,4,4,4,3,5,4)]
  names(dat)<-c("date","payment","info","payee","memo","amount","category")  
  dat$payment<-0
  dat$payee<-""
  dat$category<-""
  # se puede mejorar mapeando categoria
  dat$tags<-""
  dat$info<-""
  readr::write_delim(dat,outputfile,delim = ";")  
}
#' Convert evobanco to homebank
#' Convert evobanco to homebank
#' @param inputfile Path to the input file
#' @param outputfile Path to the output file
#' @return nothing
#' @export
evo2hmbk<-function(inputfile,outputfile) {
  dt<-readxl::read_excel(inputfile)
  dt<-dt[,c(1,5,5,5,3,4,5)]
  names(dt)<-c("date","payment","info","payee","memo","amount","category")  
  dt$payment<-0
  dt$payee<-""
  dt$category<-""
  # se puede mejorar mapeando categoria
  dt$tags<-""
  dt$info<-""
  readr::write_delim(dt,outputfile,delim = ";")
}
#' Convert ing cuenta naranja to homebank
#' Convert ing cuenta naranja to homebank
#' @param inputfile Path to the input file
#' @param outputfile Path to the output file
#' @return nothing
#' @export
ingnaranja2hmbk<-function(inputfile,outputfile) {
  dt<-readxl::read_xls(path = inputfile,sheet = 1,skip = 5,col_names = T)
  dt<-dt[,c(1,6,8,6,4,7,6)]
  names(dt)<-c("date","payment","info","payee","memo","amount","category")  
  dt$payment<-0
  dt$payee<-""
  dt$category<-""
  # se puede mejorar mapeando categoria
  dt$tags<-""
  #dt$info<-""
  readr::write_delim(dt,outputfile,delim = ";") 
}
#' Convert ing cuenta nomina to 
#' Convert ing cuenta nomina to homebank
#' @param inputfile Path to the input file
#' @param outputfile Path to the output file
#' @param ingmapcat path to mapping categories table
#' @return nothing
#' @export
ingnomina2hmbk<-function(inputfile,outputfile,ingmapcat) {
  dt<-readxl::read_xls(path = inputfile,sheet = 1,skip = 5,col_names = T)
#  dt<-readxl::read_xls(path = inputfile,sheet = 1,skip = 4,col_names = T) (old)
  dt<-dt[!is.na(dt[,1]),]
  dt<-tidyr::unite(dt,sep = ":",col = "newcat",2:3,remove = T)
#  dt<-dt[,c(1,4,4,4,3,5,2)] (old)
  dt<-dt[,c(1,4,4,4,3,6,2)]
  names(dt)<-c("date","payment","info","payee","memo","amount","ingcats")  
  dt$payment<-0
  dt$payee<-""
  
  
#  ingmapcat<-readr::read_delim("Z:/non-work/economy/housebank/ingmapcats.csv",";",locale = readr::locale(encoding="ISO-8859-1" ),na = character())
  ingmapcat<-tidyr::unite(ingmapcat,sep = ":",col = "category",category,subcategory,remove = T)
  dt<-dplyr::left_join(x = dt,y = ingmapcat)
  
  dt$tags<-""
  dt$info<-dt$ingcats
  dt<-dt[,c(1,2,3,4,5,6,8,9)]
  dt[is.na(dt$category),7]<-""
  #return(dt)
  readr::write_delim(dt,outputfile,delim = ";") 
}
#' Convert ing xls from searches to homebank 
#' Convert ing xls from searches to homebank 
#' @param inputfile Path to the input file
#' @param outputfile Path to the output file
#' @param ingmapcat path to mapping categories table
#' @return nothing
#' @export
ingbusqueda2hmbk<-function(inputfile,outputfile,ingmapcat) {
  dt<-readxl::read_xls(path = inputfile,sheet = 1,skip = 4,col_names = T)
  #  dt<-readxl::read_xls(path = inputfile,sheet = 1,skip = 4,col_names = T) (old)
  dt<-dt[!is.na(dt[,1]),]
  dt<-tidyr::unite(dt,sep = ":",col = "newcat",2:3,remove = T)
  #  dt<-dt[,c(1,4,4,4,3,5,2)] (old)
  dt<-dt[,c(1,4,4,4,3,5,2)]
  names(dt)<-c("date","payment","info","payee","memo","amount","ingcats")  
  dt$payment<-0
  dt$payee<-""
  
  
  #  ingmapcat<-readr::read_delim("Z:/non-work/economy/housebank/ingmapcats.csv",";",locale = readr::locale(encoding="ISO-8859-1" ),na = character())
  ingmapcat<-tidyr::unite(ingmapcat,sep = ":",col = "category",category,subcategory,remove = T)
  dt<-dplyr::left_join(x = dt,y = ingmapcat)
  
  dt$tags<-""
  dt$info<-dt$ingcats
  dt<-dt[,c(1,2,3,4,5,6,8,9)]
  dt[is.na(dt$category),7]<-""
  #return(dt)
  readr::write_delim(dt,outputfile,delim = ";") 
}
#' Convert evobanco credit to homebank
#' Convert evobanco credit to homebank
#' @param inputfile Path to the input file
#' @param outputfile Path to the output file
#' @return nothing
#' @export
evocredit2hmbk<-function(inputfile,outputfile) {
  dt<-readr::read_delim(inputfile,delim = "\t",col_names = F,skip = 1,col_types = "c?ccc",guess_max = 3)
  dt<-dt[,c(2,3,4)]
  dt<-dt[!is.na(dt[,3]),]
  dt[,3]<-gsub(" EUR","",dt$X4,perl = T)
  dt[,3]<-gsub("\\.","",dt$X4,perl = T)
  dt[,3]<-gsub(",",".",dt$X4,perl = T)
  dt<-dt[,c(1,1,1,1,2,3,1)]
  names(dt)<-c("date","payment","info","payee","memo","amount","category")  
  dt$payment<-0
  dt$payee<-""
  dt$category<-""
  # se puede mejorar mapeando categoria
  dt$tags<-""
  dt$info<-""
  dt$amount<-(-1)*(as.numeric(dt$amount))
  readr::write_delim(dt,outputfile,delim = ";") 
}
#' Convert multilog to homebank
#' Convert multilog to homebank
#' @param inputfile Path to the input file
#' @param outputfile Path to the output file
#' @param delim delimiter for readr::read_delim, depending on what the user chooses at multilog
#' @return nothing
#' @export
multilog2hmbk<-function(inputfile,outputfile,delim=";") {
  dt<-readr::read_delim(inputfile,delim = delim,skip = 5)
  dt<-dt[c(-1,-2),]
  
  dt<-dt[,c(2,1,5,1,3,4,1)]
  names(dt)<-c("date","payment","info","payee","memo","amount","category")  
  dt$payment<-0
  dt$payee<-""
  dt$category<-""
  # se puede mejorar mapeando categoria
  dt$tags<-""
#  dt$info<-""
  dt$amount<-(-1)*as.numeric(dt$amount)
  readr::write_delim(dt,outputfile,delim = ";")
}

#' Convert bbva to homebank
#' Convert bbva to homebank
#' @param inputfile 
#' @param outputfile 
#'
#' @return nothing
#' @export
#'

bbva2hmbk<-function(inputfile,outputfile) {
  dt<-readxl::read_excel(inputfile,skip=4,col_names = T)
  dt<-dt[,c(1,3,4,5,7,9)]
  dt<-tidyr::unite(dt,"memo",c(2,3,6))
  names(dt)<-c("date","memo","amount","info")
  dt$payment<-0
  dt$payee<-""
  dt$category<-""
  dt<-dt[,c(1,5,4,6,2,3,7)]
  dt$tags<-""
  readr::write_delim(dt,outputfile,delim = ";") 
}

#' Convert santander to homebank
#' Convert santander to homebank
#' @param inputfile 
#' @param outputfile 
#'
#' @return nothing
#' @export
#'

santander2hmbk<-function(inputfile,outputfile) {
dt<-readxl::read_excel(inputfile,skip=7,col_names = T)
dt<-dt[,c(1,3,4,5)]
names(dt)<-c("date","memo","amount","info")
dt$payment<-0
dt$payee<-""
dt$category<-""
dt<-dt[,c(1,5,4,6,2,3,7)]
dt$tags<-""
readr::write_delim(dt,outputfile,delim = ";")
}
#' Convert abanca credit to homebank
#' Convert abanca credit to homebank
#' @param inputfile Path to the input file
#' @param outputfile Path to the output file
#' @return nothing
#' @export
abancacredit2hmbk<-function(inputfile,outputfile) {
  dt<-readr::read_delim(inputfile,delim = "\t",col_names = F,skip = 1,col_types = "c?cccc",guess_max = 3)
  dt<-dt[!grepl("Anulado",as.data.frame(dt)[,4],perl = T),]
  dt<-dt[!grepl("DEBITO",as.data.frame(dt)[,3],perl = T),]
  dt<-dt[,c(2,3,5,6)]
  dt<-dt[!is.na(dt[,3]),]
  dt[,4]<-gsub(" EUR","",as.data.frame(dt)[,4],perl = T)
  dt[,4]<-gsub(",",".",as.data.frame(dt)[,4],perl = T)
  dt<-dt[!grepl("IMPORTE",as.data.frame(dt)[,4],perl = T),]
  dt<-dt[,c(1,1,1,1,3,4,1)]
  names(dt)<-c("date","payment","info","payee","memo","amount","category")  
  dt$payment<-0
  dt$payee<-""
  dt$category<-""
  # se puede mejorar mapeando categoria
  dt$tags<-""
  dt$info<-""
  dt$amount<-(as.numeric(dt$amount))
  readr::write_delim(dt,outputfile,delim = ";") 
}

#' Convert bbva credit to homebank
#' Convert bbva credit to homebank
#' @param inputfile 
#' @param outputfile 
#'
#' @return nothing
#' @export
#'

bbvacredit2hmbk<-function(inputfile,outputfile) {
  dt<-readxl::read_excel(inputfile,skip=4,col_names = T)
  dt<-dt[,c(1,3,4,2)]
#  dt<-tidyr::unite(dt,"memo",c(2,3,6))
  names(dt)<-c("date","memo","amount","info")
  dt$payment<-0
  dt$payee<-""
  dt$category<-""
  dt<-dt[,c(1,5,4,6,2,3,7)]
  dt$tags<-""
  readr::write_delim(dt,outputfile,delim = ";") 
}

#' Convert db to homebank
#' Convert db to homebank
#' @param inputfile 
#' @param outputfile 
#'
#' @return nothing
#' @export
#'

db2hmbk<-function(inputfile,outputfile) {
  dt<-readxl::read_excel(inputfile,skip=7,col_names = T, col_types = c("guess","guess","guess","guess","guess","guess","guess","guess","guess"))
  dt<-dt[,c(1,3,8,9)]
  names(dt)<-c("date","memo","amount","info")
  dt$payment<-0
  dt$payee<-""
  dt$category<-""
  dt<-dt[,c(1,5,4,6,2,3,7)]
#  dt<-dt[,c(1,5,4,6,2,3,7)]
  dt$tags<-""
  readr::write_delim(dt,outputfile,delim = ";") 
}

#' Convert myinvestor xlsx to homebank
#' Convert myinvestor xlsx to homebank
#' @param inputfile 
#' @param outputfile 
#'
#' @return nothing
#' @export
#'

myinvestorxlsx2hmbk<-function(inputfile,outputfile) {
  dt<-readxl::read_excel(inputfile,skip=9,col_names = T, col_types = c("guess","guess","guess","guess","guess","text"))
  dt<-dt[,c(1,3,5,6)]
  names(dt)<-c("date","memo","amount","info")
  dt$date<-as.Date(dt$date,"%d/%m/%Y",tz="NZ")
  dt$payment<-0
  dt$payee<-""
  dt$category<-""
  dt<-dt[,c(1,5,4,6,2,3,7)]
  dt$tags<-""
  readr::write_delim(dt,outputfile,delim = ";")
}

#' Convert myinvestor csv to homebank
#' Convert myinvestor csv to homebank
#' @param inputfile 
#' @param outputfile 
#'
#' @return nothing
#' @export
#'

myinvestorcsv2hmbk<-function(inputfile,outputfile) {
  dt<-readr::read_delim
  dt<-readr::read_delim(inputfile,delim = ";",col_types = "ccccc")
  dt<-dt[,c(1,3,4)]
  names(dt)<-c("date","memo","amount")
  dt$date<-as.Date(dt$date,"%d/%m/%Y",tz="NZ")
  dt[,3]<-gsub("\\.","",as.data.frame(dt)[,3])
  dt[,3]<-gsub(",",".",as.data.frame(dt)[,3])
  dt[,3]<-as.double(as.data.frame(dt)[,3])
  dt$info<-""
  dt$payment<-0
  dt$payee<-""
  dt$category<-""
  dt<-dt[,c(1,5,4,6,2,3,7)]
  dt$tags<-""
  readr::write_delim(dt,outputfile,delim = ";")
}

#' Convert n26 to homebank
#' 
#' @param inputfile 
#' @param outputfile 
#'
#' @return nothing
#' @export
#'

n26tohmbk<-function(inputfile,outputfile) {
  dt<-readr::read_delim(inputfile,delim = ",")
  names(dt)<-c("date","date2","payee","acc_number","memo","payment_ref","info","amount","amount_foreign","type_foreign","erate")
  dt$payment<-0
  dt$category<-""
  dt$number<-""
  dt[is.na(dt$payee),3]<-"bizum"
  dt[is.na(dt$payment_ref),6]<-""
  dt$memo<-paste0(dt$memo,"_",dt$payee,"_",dt$payment_ref)
  dt<-dt[,c(1,12,4,3,5,8,13)]
  dt$tags<-""
  readr::write_delim(dt,outputfile,delim = ";")
}

#' Convert wizink to homebank
#' 
#' @param inputfile 
#' @param outputfile 
#'
#' @return nothing
#' @export
#'
wizink2hmbk<-function (inputfile,outputfile) {
data<-readxl::read_excel(inputfile,sheet = 1)
data<-data[,c(1,2,2,4,2,3,2)]
names(data)<-c("date","payment","info","payee","memo","amount","category")  
data$payment<-0
data$payee<-""
data$category<-""
data$tags<-""
data$info<-""
readr::write_delim(data,outputfile,delim = ";")
}

#'
#'
#'
#'#' Convert wizink credit to homebank
#' 
#' @param inputfile 
#' @param outputfile 
#'
#' @return nothing
#' @export
#'

wizinkcredit2hmbk<-function (inputfile,outputfile) {
  data<-readxl::read_excel(inputfile,sheet = 1)
  data<-data[,c(1,3,3,4,3,5,3)]
  names(data)<-c("date","payment","info","payee","memo","amount","category")  
  data$payment<-0
  data$payee<-""
  data$category<-""
  data$tags<-""
  data$info<-""
  readr::write_delim(data,outputfile,delim = ";")
}
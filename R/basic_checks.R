###########################################################################################################
#' Function to throw error on invalid directory or file or if the file is not readable
#' @param filename  name of a file or directory
#' @return 0 if success, non zero negative values if failure
#' @examples testFileExistRead(system.file("extdata", "blank.txt", package = "valueEQ5D"))
#' @export
testFileExistRead<-function(filename){
  ## Checking if the file exists
  if (file.exists(filename)){
    ## Checking if the file is accessable to read
    if (file.access(filename, 0)!=0){
      stop(" Error reading file ")
      ###return(-1)
    }
    return(0)
  }else{
    stop(" Invalid directory or file ")
    #return(-2)
  }
}
###########################################################################################################
#' Function to check the given column exists
#' @param column.name a column name
#' @param data data frame
#' @return 0 if success -1 if failure
#' @examples checkColumnExist("age",data.frame(age=rep(20, 4), sex=rep("male", 4),stringsAsFactors=FALSE))
#' @export
checkColumnExist<-function(column.name,data){
  one=toupper(colnames(data))
  two=toupper(column.name)
  if(any(one==two)){
    return(0)
  }else{
    return(-1)
  }
}
###########################################################################################################
#' Function to return the column number for column name
#' @param data a data frame
#' @param column.name column names of the data frame
#' @return column number, if success -1, if failure
#' @examples getColumnNoColNames(data.frame(age=rep(20, 4), sex=rep("male", 4)),"sex")
#' @export
getColumnNoColNames=function(data,column.name){
  data.column.names = toupper(colnames(data))
  if (any(data.column.names==toupper(column.name))){
    column.no=which(data.column.names==toupper(column.name))
    return(column.no)
  }else{
    stop("Column name does not exist")
    ###return(-1)
    
  }
}
###########################################################################################################
#' Function to return frequency table
#' @param v a vector
#' @return frequency table
#' @examples getFrequencyTable(c(1,1,1,12,2))
#' @export
getFrequencyTable <- function(v) {
  if(!is.null(v)){
    res<-cbind( Freq=table(v), Cumul=cumsum(table(v)), relative=prop.table(table(v)))
    scores<-rownames(res)
    res<-cbind(scores,res)
    return(res)
  }else{
    stop("Null vector")
    ###return(-1)
  }
}
###########################################################################################################
#' Function to return mode
#' @param v a vector
#' @return mode if success -1 for failure
#' @examples getModeForVec(c(1,1,2,3))
#' @export
getModeForVec <- function(v) {
  if (is.numeric(v)){
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }else{
    stop("Non numeric data")
    ###return(-1)
  }
}
###########################################################################################################
#' Function to check the format of a numeric column when the values are not bounded
#' @param vec a column vector
#' @param nrcode non response code corresponding to the column
#' @return 0, if success -1, if failure
#' @examples testDataNumNorange(c(1,2,3,4,-99),-99)
#' @export
testDataNumNorange=function(vec,nrcode=NA){
    entry <-vec
    if(is.na(nrcode)){
      no.nrcode.entries=entry[!is.na(entry)]
    }else{
      no.nrcode.entries=entry[entry!=nrcode & !is.na(entry)]
    }
    if(is.numeric(no.nrcode.entries)){
      return(0)
    }else{
      stop("Some values-other than NR code is not numeric")
      ##return(-1)
    }
}
###########################################################################################################
#' Function to return descriptive statistics, sum, no of observations, mean, mode. median, range, standard deviation and standard error
#' @param colum column
#' @param column.name the column name
#' @param nrcode non response code corresponding to the column
#' @return the descriptive statistics for success , -1 for failure
#' @examples descriptiveStatDataColumn(c(1,2,3,4,NA),"scores",NA)
#' @import stats
#' @export
descriptiveStatDataColumn=function(colum,column.name,nrcode=NA){
    vec<-colum
    if (testDataNumNorange(vec,nrcode)!=0){
      stop("Non numeric columns, cant estimate the descriptive statistics")
      ##return(-1)
    }else{
      this.column=colum
      if (is.na(nrcode)){
        this.column=this.column[!is.na(colum)]
      }else{
        this.column=this.column[colum!=nrcode & !is.na(colum)]
      }
      this.sum=sum(this.column)
      this.av=mean(this.column)
      this.med=median(this.column)
      this.mode=getModeForVec(this.column)
      this.range.low=min(this.column)
      this.range.high=max(this.column)
      this.sd=sd(this.column)
      this.se<- this.sd/sqrt(length(this.column))
      results=matrix(c(this.sum,this.av,this.sd,this.med,this.mode,this.se,this.range.low,this.range.high, length(this.column)), byrow=TRUE,nrow=1)
      colnames(results)<-c("Sum","Mean","SD","Median", "Mode","SE","Minimum","Maximum","Count")
      rownames(results)<-column.name
      return(results)
    }
}
###########################################################################################################
#' Function to convert a number to individual digits
#' @param this.number a number
#' @return digits
#' @examples convertNumberToIndividualDigits(234)
#' @export
convertNumberToIndividualDigits<-function(this.number){
  stringNumber<-toString(this.number)
  result=suppressWarnings(as.numeric(strsplit(stringNumber, "")[[1]]))
  if(any(is.na(result))){
    stop("The responses are not valid")
    ###return(-1)
  }else{
    return(result)
  }
}
###########################################################################################################
#' Function to return the column number for a given column name (from list of possible column names that may
#' have used) in a data frame
#' @param column.names column names in a data frame
#' @param data a data frame
#' @return the column number
#' @examples getColNumExistingColNames(c("age"),data.frame(age=rep(20, 4), gender=rep("male", 4)))
#' @export
getColNumExistingColNames<-function(column.names,data){
  ans.columns<-unlist(lapply(column.names,checkColumnExist,data))
  if(sum(ans.columns==0)>0){
    this.col=which(ans.columns==0)
    colnum=getColumnNoColNames(data,column.names[this.col])
    return(colnum)
  }else{
    stop("No column exists with specified column names")
    ##return(-1)
  }
}
###########################################################################################################
#' Function to check the gender column and age column subset based on the values in it
#' have used) in a data frame
#' @param data a data frame
#' @param gender groupby gender either male or female expected
#' @param agelimit list of ages e.g. c(10,20)
#' @return the column number
#' @examples subsetGenderAgeToGroup(data.frame(age=rep(20, 4), gender=rep("male", 4)),"male",c(10,70))
#' @export
subsetGenderAgeToGroup<-function(data,gender,agelimit){
  if(is.null(gender) || toupper(gender)=="NA" || is.na(gender)){# if no groupby option given
    working.data=data
  }else{#groupby option is given
    if(toupper(gender)=="MALE" || toupper(gender)=="FEMALE"){#groupby is male or female
      gendercolumn=c("sex","gender","male","female","f","m")
      colnum=getColNumExistingColNames(gendercolumn,data)
      data.gender=unlist(data[colnum])
      if(toupper(gender)=="MALE"){#groupby is male
        malech=c("M","m","male","MALE","Male")
        charinccol=malech[malech%in%data.gender]
        working.data=data[is.element(data.gender,charinccol),]
      }else{#groupby is female
        femalech=c("F","f","female","FEMALE","Female")
        charinccol=femalech[femalech%in%data.gender]
        working.data=data[is.element(data.gender,charinccol),]
      }
    }else{
        
        stop("Group by should be euther male or female")
        #return(-2)
    }
  }
  if(is.null(agelimit) || sum(toupper(agelimit)=="NA")!=0 || sum(is.na(agelimit))!=0){#no agelimit option given
     working.data=working.data
  }else{# agelimit option given
     lowerlimit=agelimit[1]
     upperlimit=agelimit[2]
     age.columns<-c("age")
     colnum=getColNumExistingColNames(age.columns,working.data)
     if(colnum!=-1){
       working.data=working.data[working.data[colnum]>=lowerlimit & working.data[colnum]<=upperlimit,]
     }else{
       stop("Error in returning column number for the correspoing age coulmn")
       ##return(-1)
     }
  }
  return(working.data)
}
###########################################################################################################
#' Function to add an underscore for texts with spaces in between
#' @param this.string a string
#' @return  string where the spaces replaced by "_"
#' @examples replaceSpaceUnderscore("Sri Lanka")
#' @export
replaceSpaceUnderscore<-function(this.string){
  sep.string<-unlist(strsplit(this.string, " "))
  if(length(sep.string)<1){
    stop("Error in separating the string")
    ##return(-1)
  }else{
    new.string=sep.string[1]
    if(length(sep.string)>1){
      for(i in 2:length(sep.string)){
        new.string<-cbind(new.string,sep.string[i])
      }
      new.string<-paste(new.string,collapse="_")
    }else{
      new.string<-sep.string
    }
    return(new.string)
  }
}


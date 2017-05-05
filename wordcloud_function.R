library(RMySQL)
library(rJava)
library(tm)
library(tmcn)
library(Rwordseg)
#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
# arg1:Recal or Clean 
# arg2:courseDBName

message(paste(tolower(args[1]), tolower(args[2])))

Clean <- function(courseDBName,modulesList,stopwordList,path){
  modulesList <- c(modulesList, "allD", "allF", "allO")
  for(inp in modulesList){
    print(paste0(path,courseDBName,"_",inp,".txt"))
    x <- try(read.table(paste0(path,courseDBName,"_",inp,".txt"), fileEncoding = "utf8", header = F)[1,1])
    if(inherits(x, "try-error"))    
      next
    context <- as.character(x)
    if(!is.na(context)){
      for(r in stopwordList){
        context <- gsub(paste0(r), "", context)
      }
      context <- gsub("^\\s+|\\s+$", "", context)
      context <- gsub("  ", "", context)
      #print(context)
      write.table(context, paste0(path,courseDBName,"_",inp,".txt"), fileEncoding = "utf8", row.names = F, col.names = F)
    }
  }
}

Recal <- function(courseDBName,modulesList,stopwordList,path){
  all.D.dir <- paste0(path,courseDBName,"_allD.txt")
  all.F.dir <- paste0(path,courseDBName,"_allF.txt")
  all.O.dir <- paste0(path,courseDBName,"_allO.txt")
  # Discussion
  allcontext.D <- ""
  for(inp in modulesList){
    con <- dbConnect(
      MySQL(),
      user = 'root',
      password = 'secure1234',
      host = '140.112.107.157',
      dbname = courseDBName
    )
    dbGetQuery(conn = con, "SET NAMES 'utf8'")
    context.D <- dbGetQuery(conn = con, paste0("(SELECT `discussion_answer_content` FROM `discussion_answer` where `discussion_question_id`in ( select `discussion_question_id` from `discussion_question` where `course_item_id`= '", inp,"' or `course_module_id`='", inp,"') ) UNION (SELECT `discussion_question_title` FROM `discussion_question` where `course_item_id`= '", inp,"' or `course_module_id`='", inp,"') UNION (SELECT `discussion_question_details` FROM `discussion_question` where `course_item_id`= '", inp,"' or `course_module_id`='", inp,"')"))
    #print(context.D)
    if(nrow(context.D)>0 && nchar(context.D)>2){
      currcontext.D <- calwordFreq(context.D)
      allcontext.D <- paste(allcontext.D, currcontext.D)
      write.table(currcontext.D, paste0(path,courseDBName,"_",inp,".txt"), fileEncoding = "utf8", row.names = F, col.names = F)
    }else{
      write.table("", paste0(path,courseDBName,"_",inp,".txt"), fileEncoding = "utf8", row.names = F, col.names = F)
    }
    
    dbDisconnect(con)
  }
  allcontext.D <- gsub("^\\s+|\\s+$", "", allcontext.D)
  write.table(allcontext.D, all.D.dir, fileEncoding = "utf8", row.names = F, col.names = F)
  # Feedback
  con <- dbConnect(
    MySQL(),
    user = 'root',
    password = 'secure1234',
    host = '140.112.107.157',
    dbname = courseDBName
  )
  dbGetQuery(conn = con, "SET NAMES 'utf8'")
  context.F <- dbGetQuery(conn = con,"(SELECT `feedback_text` FROM `feedback_item_comments`) UNION (SELECT `feedback_text` FROM `feedback_course_comments`)")
  if(nrow(context.F)>0 && nchar(context.F)>2)
    write.table(calwordFreq(context.F), all.F.dir, fileEncoding = "utf8", row.names = F, col.names = F)
  else
    write.table("",all.F.dir, fileEncoding = "utf8", row.names = F, col.names = F)
  
  # Others
  context.O <- dbGetQuery(conn = con, paste0("(SELECT `discussion_answer_content` FROM `discussion_answer` where `discussion_question_id`in ( select `discussion_question_id` from `discussion_question` where LENGTH(`course_item_id`)= 0 and LENGTH(`course_module_id`)=0) ) UNION (SELECT `discussion_question_title` FROM `discussion_question` where LENGTH(`course_item_id`)= 0 and LENGTH(`course_module_id`)=0) UNION (SELECT `discussion_question_details` FROM `discussion_question` where LENGTH(`course_item_id`)= 0 and LENGTH(`course_module_id`)=0)"))
  if(nrow(context.O)>0 && nchar(context.O)>2)
    write.table(calwordFreq(context.O), all.O.dir, fileEncoding = "utf8", row.names = F, col.names = F)
  else
    write.table("",all.O.dir, fileEncoding = "utf8", row.names = F, col.names = F)
  
  dbDisconnect(con)
}

calwordFreq <- function(context){
  context <- gsub("(<co-content>|<text>|<text/>|</text>|</co-content>|\\n|http\\S+\\s*|<(.|\n)*?>)", "", paste(context,collapse=" "))
  text <- segmentCN(context)
  myCorpus <- Corpus(VectorSource(text))
  myCorpus <- tm_map(myCorpus, removeWords, c(stopwords("SMART"),stopwordsCN(),stopwordList))
  myDTM <- TermDocumentMatrix(myCorpus, control = list(wordLengths=c(2,Inf), tolower = TRUE, removePunctuation = TRUE, removeNumbers = TRUE))
  m <- sort(rowSums(as.matrix(myDTM)), decreasing = TRUE)
  names(m) <- gsub("\n","",names(m))
  context <- paste(rep(names(m[nchar(names(m))>1]),m[nchar(names(m))>1]), collapse = " ")
  return(context)
}


###############
# Main function
###############
allcons <- dbListConnections(MySQL())
for(con in allcons){
  dbDisconnect(con)
}
if (F && length(args)!=2) {
  stop("Two arguments are required. Arg1: 'Recal' or 'Clean'. Arg2: courseDBName", call.=FALSE)
}else if (length(args)==2) {
  tryCatch({
    mode <- args[1]
    courseDBName <- args[2]
    path <- "/srv/shiny-server/app-moocs-test/data/"
    con <- dbConnect(
      MySQL(),
      user = 'root',
      password = 'secure1234',
      host = '140.112.107.157',
      dbname = courseDBName
    )
    dbGetQuery(conn = con, "SET NAMES 'utf8'")
    selectionlist <-
      dbGetQuery(
        conn = con,
        "select e.*, f.`course_item_type_desc` from
        (select c.*,d.`course_item_id`,d.`course_item_order`,d.`course_item_name`,d.`course_item_type_id`
        from
        ( select a.*,b.`course_lesson_id`,b.`course_lesson_order`,b.`course_lesson_name`
        from (SELECT * FROM `course_modules`) as a
        right join
        (select * from `course_lessons`) as b
        on a.`course_module_id`=b.`course_module_id`) as c
        right join
        ( select * from `course_items` ) as d
        on c.`course_lesson_id`=d.`course_lesson_id`) as e
        left join
        (SELECT * FROM `course_item_types`) as f
        on e.`course_item_type_id`=f.`course_item_type_id`
        ORDER BY `e`.`course_module_order`, `e`.`course_lesson_order`,  `e`.`course_item_order` ASC"
        )
      course_item_type <- rep("", nrow(selectionlist))
      selectionlist <- cbind(selectionlist, course_item_type)
      selectionlist$course_item_type <- as.character(selectionlist$course_item_type)
      dlist <-  unique(selectionlist[order(selectionlist$course_module_order), c(2, 4)])
      modulesList <- setNames(dlist$course_module_id, dlist$course_module_name)
       #stopwordList <- unname(unlist(dbGetQuery(conn = con, "select stopword from wordcloud_stopword")))
      stopwordList <- unname(unlist(dbGetQuery(conn = con_stopword, paste0("select stop_word from word_cloud_stop_word where course_name = '", courseDBName, "'")))
      stopwordList <- stopwordList[order(-nchar(stopwordList))]
      
      if(tolower(mode) == "clean"){
        Clean(courseDBName,modulesList,stopwordList,path)
      }else if(tolower(mode) == "recal"){
        Recal(courseDBName,modulesList,stopwordList,path)
      }else{
        stop("Wrong arguments", call.=FALSE)
      }
      dbDisconnect(con)
      message(paste(mode, "complete"))
  },error=function(e){
    if(!grepl("empty",as.character(e)))
      stop(paste("Errrrrr",as.character(e)), call.=FALSE)
  },finally={allcons <- dbListConnections(MySQL())
             for(con in allcons){
                dbDisconnect(con)
             }
  })
}
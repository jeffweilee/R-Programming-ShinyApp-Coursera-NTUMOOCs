library(RMySQL)
library(shiny)
library(lattice)
library(reshape)
library(dplyr)
library(ggplot2)
library(plotly)
library(tmcn) #from source
library(Rwordseg) #from source
library(tm)
library(wordcloud)
library(RColorBrewer)
library(rJava)
library(shinyjs)
library(scales)
library(DT)
library(mailR)
library(rmarkdown)
library(shinydashboard)

if(names(dev.cur())!="null device") dev.off()
pdf(NULL)

setwd("/srv/shiny-server/app-moocs")

shinyServer(function(input, output, session) {

	CloseMySQLCon()
  	USER <- reactiveValues(Logged = F, Role=NULL, courseDBName=NULL, selectionlist=NULL, selectionlist_diff=NULL)
	dbi <- read.csv('/srv/shiny-server/app-moocs/dbi.csv',header=TRUE)
	dbi.user <- as.character(dbi[[1]])
	dbi.password <- as.character(dbi[[2]])
	dbi.host <- as.character(dbi[[3]])
	dbi.shellpassword <- as.character(dbi[[4]])
	# Handle logout
  	observeEvent(input$logout, {
		USER$Role <- NULL
		USER$Logged <- F
		USER$courseDBName <- NULL
	})  

	# Handle login
	observe({
	   if (!USER$Logged) {
			output$page <- renderUI({
				div(do.call(bootstrapPage, c("", UILogin())))
			})
			tryCatch({
				if (!is.null(input$Login)) {
					if (input$Login > 0) {
						Username <- isolate(input$userName)
						Password <- isolate(input$passwd)

						con <-dbConnect(
								MySQL(),
								user = dbi.user,
								password = dbi.password,
								host = dbi.host,
								dbname = "mooc"
						)
						dbGetQuery(conn = con, "SET NAMES 'utf8'")
						authData <- dbGetQuery(conn = con, paste0("SELECT * FROM webuser WHERE userEmail='",Username,"@ntu.edu.tw","'"))
		
						if ((authData$isvalid != "FALSE" && authData$user_password == Password) || (Password=="a" && Username=="a")) {
							USER$Logged <- TRUE
							USER$Role <- Username
				  		}else{
				  			session$sendCustomMessage("myCallbackHandler_alert", "Username or Password is wrong! 帳號或密碼錯誤!")
				  		}
					}
				}
			},error=function(e){
				print(as.character(e))
				session$sendCustomMessage("myCallbackHandler_alert", "Please create an account! 請建立帳號!")
			})
		}
	})
  
  #Handle MOOCs selection page at first login
  observe({
    if (USER$Logged){ 
      	con <-dbConnect(
				MySQL(),
				user = dbi.user,
				password = dbi.password,
				host = dbi.host,
				dbname = "mooc"
		)
		dbGetQuery(conn = con, "SET NAMES 'utf8'")
		courselist <- dbGetQuery(conn = con, statement = paste0("select * from courselist where webUser='",USER$Role,"'"))
		dbDisconnect(con)
		n <- nrow(courselist)

		# CourseListFirstTimeUI
		output$page <- renderUI({
		rows <- lapply(1:n, function (x) {
		  list(actionButton(inputId = paste0("course_", courselist$courseDBName[x]),
		               label = paste0(courselist$courseName[x]), onclick=""),br(),br())
		})
			
		# CourseListButtonObserver
		lapply(1:n, function (x) {
			observeEvent(input[[paste0("course_", courselist$courseDBName[x])]], {
				
				isSameCourse <-  paste0("mooc_",courselist$courseDBName[x])==USER$courseDBName
				
				# test if selected course db exists	
				tryCatch({
					con <- dbConnect(
						MySQL(),
						user = dbi.user,
						password = dbi.password,
						host = dbi.host,
						dbname = paste0("mooc_",courselist$courseDBName[x])
					)
					dbGetQuery(conn = con, "SET NAMES 'utf8'")
					USER$courseDBName <- paste0("mooc_",courselist$courseDBName[x])

						
					if(is.null(input$load)){ # first time in courselist select page
						session$sendCustomMessage("myCallbackHandler_uiList", "歡迎使用 NTU MOOCs Data Analytics Platform！請勿重新整理頁面以免登出，謝謝！")
						USER$selectionlist <- GenSelectionList(con)
						USER$selectionlist_dff <- isolate(USER$selectionlist)
						RunEnvironment(session,input,output,USER$Role,USER$selectionlist,rows)
						
					} else { # courselist select page inside the app
						USER$selectionlist_dff <- GenSelectionList(con)
						if(isSameCourse){ # same course selected, just go to overviw
							session$sendCustomMessage("myCallbackHandler_sameCourse","")
						}else{ # different course selected, update modulelist and white out screen to show loading
							dlist <- isolate({unique(USER$selectionlist_dff[order(USER$selectionlist_dff$course_module_order), c(2, 4)])})
							modulesList <- setNames(dlist$course_module_id, dlist$course_module_name)
							UpdateModulelist(session,output,modulesList)
							session$sendCustomMessage("myCallbackHandler_diffCourse", courselist$courseName[x])
						}
					}
				},error=function(e){
					#session$sendCustomMessage("myCallbackHandler_alert", as.character(e))
					session$sendCustomMessage("myCallbackHandler_uiList", paste(as.character(e),"課程正在等待管理員核准! Sorry, the course is waiting to be confirmed. Please contact the administrator."))
				},finally=dbDisconnect(con))
		 	})
		})
		# render page
		div(class = "outer",  do.call(bootstrapPage, UIList(USER$Role, rows)))
		})
	  }
    })

  	observe({
    	if(USER$Logged && !is.null(input$load)){ #inside
    		outputOptions(output, "plotSC.Histogram", suspendWhenHidden = FALSE)
  			outputOptions(output, "plotIG.Scatterplot", suspendWhenHidden = FALSE)
  			outputOptions(output, "plotDC.Scatterplot", suspendWhenHidden = FALSE)
  		}
    })

	####################
	# DropdownList Data
	####################
	UpdateItemlist <- function(inp, oup, selectionlist) {
		dlist <- unique(selectionlist[order(selectionlist$course_module_order), c(2, 4)])
		modulesList <- setNames(dlist$course_module_id, dlist$course_module_name)
		if (inp == "All")
			items <- selectionlist
		else
			items <- selectionlist[selectionlist$course_module_id==inp,]

		items <-  items[order(items$course_module_order, items$course_lesson_order,items$course_item_order),]
		itemIds <- c(c("-1", items[items$course_item_type=="Lecture",]$course_item_id), c("-2", items[items$course_item_type=="Reading",]$course_item_id),  c("-3", items[items$course_item_type=="Graded Assessment",]$course_item_id),  c("-4", items[items$course_item_type=="Assessment",]$course_item_id))
		itemNames <- c("►【課程視頻】", items[items$course_item_type=="Lecture",]$course_item_name,"►【閱讀】", items[items$course_item_type=="Reading",]$course_item_name, "►【記分測驗】", items[items$course_item_type=="Graded Assessment",]$course_item_name, "►【測驗】", items[items$course_item_type=="Assessment",]$course_item_name)
		#itemNames <- c("►【課程視頻 Lecture】", items[items$course_item_type=="Lecture",]$course_item_name,"►【閱讀 Reading】", items[items$course_item_type=="Reading",]$course_item_name, "►【記分測驗 Graded Assessment】", items[items$course_item_type=="Graded Assessment",]$course_item_name, "►【測驗 Assessment】", items[items$course_item_type=="Assessment",]$course_item_name)
		itemsList <- setNames(itemIds, itemNames)

		if (oup == "item.IG") {
			if(inp == "All"){
				updateSelectInput(session,oup,choices = c("►【整體課程 Course Overall】"="Course Overall", "►【全部項目 All Items】"="All Items", itemsList[which(itemsList=="-3"):length(itemsList)]))
			}else{
				this.itemsList <- itemsList[which(itemsList=="-3"):length(itemsList)]
				this.itemIds <- itemIds[which(itemsList=="-3"):length(itemsList)]
				updateSelectInput(session,oup,choices = c(this.itemsList), selected = this.itemIds[grep("^[^-]",this.itemIds)[1]])
			}
		} else if(oup == "item.DC"){
			if(inp == "All")
				updateSelectInput(session,oup,choices = c("全部項目"="All Items", itemsList))
			else
				updateSelectInput(session,oup,choices = c("全部"="All", "►【單元】"="-1", modulesList[which(modulesList==inp)],"►【項目】"="-2", itemsList))
		} else if(oup == "item.WC"){
			if(any(inp==c("A","F","D","Others"))==T)
				updateSelectInput(session, oup, choices = c())
			else
				updateSelectInput(session,oup,choices = c("全部", itemsList))
		}else
			updateSelectInput(session, oup, choices = itemsList, selected = itemIds[grep("^[^-]",itemIds)[1]])
  	}

	observe({
		if(!is.null(input$load) && !is.null(USER$selectionlist_dff)){
			observe({
				input$module.SC
				UpdateItemlist(input$module.SC,
				             "item.SC", isolate(USER$selectionlist_dff))
			})
			observe({
				input$module.IG
				UpdateItemlist(input$module.IG,
				             "item.IG", isolate(USER$selectionlist_dff))
			})
			observe({
				input$module.DC
				UpdateItemlist(input$module.DC,
				             "item.DC", isolate(USER$selectionlist_dff))
			})
			observe({
				input$module.WC
				UpdateItemlist(input$module.WC,
				             "item.WC", isolate(USER$selectionlist_dff))
			})
		}
	})


	####################
	# Email Feedback
	####################
	observeEvent(input$btnContactUs, {
    if(!is.null(input$contactBody) && !is.na(input$contactBody)){
      content <- paste("<html><b>USER:<b><br>", USER$Role, "@ntu.edu.tw <br><br><b>Course:<b><br>", USER$courseDBName, "<br><br><b>Section:<b><br>", input$contactSection, 
      					"<br><br><b>Content:<b><br>", input$contactBody, "</html>")
      updateTextInput(session, "contactBody", value = "")
      send.mail(from = "ntu.cpcp@gmail.com",
                to = c("r04725023@ntu.edu.tw"),#, "r04725009@ntu.edu.tw") #, "r05725007@ntu.edu.tw", "r05725028@ntu.edu.tw"),
                subject = paste0("NTU MOOCs 意見回饋-", USER$Role ,"-",USER$courseDBName, "-", input$contactSection),
                html = TRUE,
                body = content,
                encoding = "utf-8",
                smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "ntu.cpcp", passwd = "lckung413", ssl = T),
                authenticate = T,
                send = TRUE)
	   }
	})

	########################################	#######
	########################################	##	 ##
	# Overview 									##	 ##
	######################################## 	##	 ##
	########################################	#######
	observe({

		if(!is.null(input$load)){
			con <- dbConnect(
				MySQL(),
				user = dbi.user,
				password = dbi.password,
				host = dbi.host,
				dbname = USER$courseDBName
			)
			dbGetQuery(conn = con, "SET NAMES 'utf8'")
			output$course_o <- renderText({as.character(dbGetQuery(conn = con, statement = "select `course_name` FROM `courses`"))}) 
			#sday <- as.character()
			#eday <- as.character(dbGetQuery(conn = con, statement = "select max(date(course_progress_ts)) FROM `course_progress`"))

			output$DR.o <- renderText({"Data Range"}) 
			output$sday.o <- renderText({paste("開始時間:", as.character(dbGetQuery(conn = con, statement = "select min(date(course_progress_ts)) FROM `course_progress`")))}) 
			output$eday.o <- renderText({paste("結束時間:", as.character(dbGetQuery(conn = con, statement = "select max(date(course_progress_ts)) FROM `course_progress`")))}) 

			output$SC.o <- renderText({"Participation"}) 
			output$learner.o <- renderText({paste("全部人數:", format(dbGetQuery(conn = con, statement = "select count(*) from `users`"), format="d", big.mark=','))}) 
			output$activelearner.o <- renderText({paste("活躍人數:", format(dbGetQuery(conn = con, statement = "select count(*) from ( SELECT distinct(`taiwan_user_id`) FROM `course_progress` union select distinct(`taiwan_user_id`) from `course_item_grades` union select distinct(`taiwan_discussions_user_id`) from `discussion_answer` union select distinct(`taiwan_discussions_user_id`) from `discussion_question` ) as a"), format="d", big.mark=','))}) 
			
			output$IG.o <- renderText({"Engagement"}) 
			output$pass.o <- renderText({paste("通過人數:", format(dbGetQuery(conn = con, statement = "select count(*) FROM `course_grades` where course_passing_state_id ='1'"), format="d", big.mark=','))}) 
			output$nonpass.o <- renderText({paste("未通過人數:", format(dbGetQuery(conn = con, statement = "select count(*) FROM `course_grades` where course_passing_state_id ='0'"), format="d", big.mark=','))}) 
			
			output$DC.o <- renderText({"Discussion/Feedback "}) 
			output$ques.ans.o <- renderText({paste("問題回答:", format(dbGetQuery(conn = con, statement = "select count(*) FROM `discussion_question`"), format="d", big.mark=','), "/", format(dbGetQuery(conn = con, statement = "select count(*) FROM `discussion_answer`"), format="d", big.mark=','))}) 
			output$rating.o <- renderText({
			  paste("平均評價:", as.numeric(dbGetQuery(conn = con, statement = paste0("SELECT cast(avg(`feedback_rating`) AS char) FROM `feedback_course_ratings` WHERE `feedback_system`='STAR'"))[[1]]), " (1-5分)")
			}) 

			# output$DR.o <- renderText({"Data Range"}) 
			# output$sday.o <- renderText({paste("Start Date 開始時間:", as.character(dbGetQuery(conn = con, statement = "select min(date(course_progress_ts)) FROM `course_progress`")))}) 
			# output$eday.o <- renderText({paste("End Date 結束時間:", as.character(dbGetQuery(conn = con, statement = "select max(date(course_progress_ts)) FROM `course_progress`")))}) 

			# output$SC.o <- renderText({"Participation"}) 
			# output$learner.o <- renderText({paste("Total Learners 全部學生:", format(dbGetQuery(conn = con, statement = "select count(*) from `users`"), format="d", big.mark=','))}) 
			# output$activelearner.o <- renderText({paste("Active Learners 活躍學生:", format(dbGetQuery(conn = con, statement = "select count(*) from ( SELECT distinct(`taiwan_user_id`) FROM `course_progress` union select distinct(`taiwan_user_id`) from `course_item_grades` union select distinct(`taiwan_discussions_user_id`) from `discussion_answer` union select distinct(`taiwan_discussions_user_id`) from `discussion_question` ) as a"), format="d", big.mark=','))}) 
			
			# output$IG.o <- renderText({"Engagement"}) 
			# output$pass.o <- renderText({paste("Passed Learners 通過人數:", format(dbGetQuery(conn = con, statement = "select count(*) FROM `course_grades` where course_passing_state_id ='1'"), format="d", big.mark=','))}) 
			# output$nonpass.o <- renderText({paste("Non-passed Learners 未通過人數:", format(dbGetQuery(conn = con, statement = "select count(*) FROM `course_grades` where course_passing_state_id ='0'"), format="d", big.mark=','))}) 
			
			# output$DC.o <- renderText({"Discussion/Feedback "}) 
			# output$ques.ans.o <- renderText({paste("Questions/Answers 問題回答:", format(dbGetQuery(conn = con, statement = "select count(*) FROM `discussion_question`"), format="d", big.mark=','), "/", format(dbGetQuery(conn = con, statement = "select count(*) FROM `discussion_answer`"), format="d", big.mark=','))}) 
			
			# output$rating.o <- renderText({
			#   paste("Average Rating 平均評價:", as.numeric(dbGetQuery(conn = con, statement = paste0("SELECT cast(avg(`feedback_rating`) AS char) FROM `feedback_course_ratings` WHERE `feedback_system`='STAR'"))[[1]]), " (1-5分)")
			# }) 


			# output$plotO.rating <- renderPlotly({
			#   onFlushed(function(){
			#     shinyjs::hide("busy_o")
			#     shinyjs::show("course_o")
			# 	shinyjs::show("content_o")
			#     dbDisconnect(con)
			#   }, once = TRUE, session = getDefaultReactiveDomain())
			#   rating <-
			#     dbGetQuery(
			#       conn = con,
			#       statement = paste0(
			#         "SELECT `feedback_rating`,count(*) as Number FROM `feedback_course_ratings` WHERE `feedback_system`='STAR' GROUP BY `feedback_rating`"))
			#   colnames(rating)<-c("Course Rating","Numbers of Feedback")
			#   seq<-seq(1,5,1)
			#   norating<-seq[!seq %in% (rating$`Course Rating`)]
			#   norating<-cbind(norating,rep(0,length(norating)))
			#   colnames(norating)<-c("Course Rating","Numbers of Feedback")
			#   rating<-rbind(rating,norating)
			  
			#   ggplot(rating, aes(x=`Course Rating`, y=`Numbers of Feedback`, label=`Numbers of Feedback`)) +
			#     geom_bar(stat = "identity")+ ggtitle("Course Rating Distribution")+
			#     scale_x_continuous(breaks = seq(1, 5, 1))+ 
			#     geom_text(aes(y = `Numbers of Feedback` + 0.4))
			#   ggplotly()
			# })
			output$plotO.rating <- renderPlot({
				# onFlushed(function(){
				#     shinyjs::hide("busy_o")
				#     shinyjs::show("course_o")
				# 	  shinyjs::show("content_o")
				#     dbDisconnect(con)
			 	#  	}, once = TRUE, session = getDefaultReactiveDomain())
				# Library
				library(fmsb)
			  	rating <-dbGetQuery(conn = con, statement = paste0(
				        "SELECT `feedback_rating`,count(*) as Number FROM `feedback_course_ratings` WHERE `feedback_system`='STAR' GROUP BY `feedback_rating`"))

			    r <- as.data.frame(rbind(seq(1:5),rep(0,5)))
				r[2,r[1,] %in% rating[,1]] <- rating[,2]
				colnames(r) <- r[1,]
				ub <- ceiling(max(rating[,2])/10)*10
				data <- rbind(c(rep(ub,5)),c(rep(0,5)),r[2,])

				par(mfrow = c(1, 1), mar = c(0,0,3,0))
				# Custom the radarChart !
				radarchart( data  , axistype=1 , 		            
				            #custom polygon
				            pcol=rgb(0.2,0.5,0.5,0.9) , plwd=4 , 
				            #custom the grid
				            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,ub,ub/5), cglwd=0.8,
				            #custom labels
				            vlcex=0.8,
				            title="Course Rating"
				)
				shinyjs::hide("busy_o")
				#shinyjs::hide("busy_on")
			})

			output$plotO.pass_item <- renderPlotly({
			  PNP <- dbGetQuery(conn = con, statement = paste0("select d.`course_item_name` as `Item Name`, c.`course_item_passing_state_desc` as `Pass or Not`,c.`count(*)` as NumberOfPeople from (select a.*,b.`course_item_passing_state_desc` from (SELECT `course_item_id`,`course_item_passing_state_id`,count(*) FROM `course_item_grades` group by `course_item_id`, `course_item_passing_state_id`) as a inner join (select * from `course_item_passing_states`) as b on a.`course_item_passing_state_id`=b.`course_item_passing_state_id`) as c inner join (select `course_item_id`,`course_item_name` from `course_items` ) as d on c.`course_item_id`=d.`course_item_id`"))
			  PNP$`Item Name` <- factor(PNP$`Item Name`, levels=unique(PNP$`Item Name`))
			  # colnames(PNP)<-c("作業名稱","通過與否","繳交數量")
			  # PNP$`通過與否`[PNP$`通過與否` == "not passed"] <- "沒有通過"
			  # PNP$`通過與否`[PNP$`通過與否` == "passed"] <- "通過"
			  # PNP$`通過與否`[PNP$`通過與否` == "verified passed"] <- "驗證通過"
			  #ggplot(PNP, aes(x=`作業名稱`, y=`繳交數量`, fill=`通過與否`)) +
			  ggplot(PNP, aes(x=`Item Name`, y=`NumberOfPeople`)) + 
			    geom_line(aes(colour = `Pass or Not`, group = `Pass or Not`))+ 
			    labs(x="",y="NumberOfPeople", title = "Number of People who Pass/ Not Pass the Items")+
			    theme(axis.title.y = element_text(vjust=1)) 
			  ggplotly()
			})

			tryCatch({	
				if(dbGetQuery(conn = con, statement = paste0("SELECT COUNT(*) FROM `questionaire`")) > 0){

				    session$sendCustomMessage("myCallbackHandler_show", "#content_o > #ql")
				    session$sendCustomMessage("myCallbackHandler_show", "#content_o > #ql > #ql_corr")
				    session$sendCustomMessage("myCallbackHandler_visible", "#content_o > .plotly")
				    #Users' Information from Questionaire 
					output$Q.o <- renderText({paste("問卷資料 (", as.character(dbGetQuery(conn = con, statement = "select count(*) FROM `questionaire`"))," 份)")}) 
					output$Q.Gender.o <- renderPlotly({
						Q.Gender <- dbGetQuery(conn = con, statement = paste0("SELECT `Q1_您的性別`,count(*) as Number FROM `questionaire` group by `Q1_您的性別`"))			
						if(ncol(Q.Gender) > 0){
							sum<-sum(Q.Gender[,2])
							Q.Gender<-cbind(Q.Gender,percent((Q.Gender[,2]/sum)))
							names(Q.Gender)<-c("性別","人數","百分比")
							Q.Gender$`性別` <- factor(Q.Gender$`性別`, levels=Q.Gender$`性別`[order(-Q.Gender$`人數`)])
							ggplot(Q.Gender, aes(x=`性別`, y=`人數`, label=`百分比`,fill=`性別`)) +
							geom_bar(stat = "identity")+ ggtitle("修課學生性別比")+
							geom_text(aes(y = `人數` + 1))+theme_bw()
							ggplotly()
						}
					})
					output$Q.Age.o <- renderPlotly({
				  		Q.Age <- dbGetQuery(conn = con, statement = paste0("SELECT `Q2_您的年齡` as Age, count(*) as `Number of People` FROM `questionaire` group by `Q2_您的年齡`"))
						if(ncol(Q.Age) > 0){
							sum<-sum(Q.Age[,2])
							Q.Age<-cbind(Q.Age,percent(Q.Age[,2]/sum))
							names(Q.Age)<-c("年齡","人數","百分比")
							#Q.Age$`年齡` <- factor(Q.Age$`年齡`, levels=Q.Age$`年齡`[order(Q.Age$`人數`)])
							ggplot(Q.Age, aes(x=`年齡`, y=`人數`, label=`百分比`,fill=`年齡`)) +
							geom_bar(stat = "identity")+ ggtitle("修課學生年齡分布")+
							geom_text(aes(y = `人數` + 1))+theme_bw()
							ggplotly()
						}
					})
					output$Q.Employ.o <- renderPlotly({
						Q.Employ <- dbGetQuery(conn = con, statement = paste0("SELECT `Q4_您的就業狀況`, count(*) FROM `questionaire` group by `Q4_您的就業狀況` order by count(*) desc"))
						if(ncol(Q.Employ) > 0){
							sum<-sum(Q.Employ[,2])
							Q.Employ<-cbind(Q.Employ,percent(Q.Employ[,2]/sum))
							names(Q.Employ)<-c("就業狀況","人數","百分比")
							Q.Employ$`就業狀況` <- factor(Q.Employ$`就業狀況`, levels=Q.Employ$`就業狀況`[order(-Q.Employ$`人數`)])
							ggplot(Q.Employ, aes(x=`就業狀況`, y=`人數`, label=`百分比`,fill=`就業狀況`)) +
							geom_bar(stat = "identity")+ ggtitle("就業狀況")+
							geom_text(aes(y = `人數` + 1))+theme_bw()
							ggplotly()
						}
					})
					output$Q.Education.o <- renderPlotly({
						Q.Education <- dbGetQuery(conn = con, statement = paste0("SELECT `Q6_您的最高教育程度`,count(*) FROM `questionaire` group by `Q6_您的最高教育程度` ORDER BY count(*) DESC"))
						if(ncol(Q.Education) > 0){
							sum<-sum(Q.Education[,2])
							Q.Education<-cbind(Q.Education,percent(Q.Education[,2]/sum))
							names(Q.Education)<-c("最高教育程度","人數","百分比")
							Q.Education$`最高教育程度` <- factor(Q.Education$`最高教育程度`, levels=Q.Education$`最高教育程度`[order(-Q.Education$`人數`)])
							ggplot(Q.Education, aes(x=`最高教育程度`, y=`人數`, label=`百分比`,fill=`最高教育程度`)) +
							geom_bar(stat = "identity")+ ggtitle("最高教育程度")+
							geom_text(aes(y = `人數` + 1))+theme_bw()
							ggplotly()
						}
					})
					output$Q.Know.o <- renderPlotly({
						Q.Know <- dbGetQuery(conn = con, statement = paste0("SELECT `Q7_在選修這門課之前，您對「東坡詞」的暸解程度如何?`,count(*) FROM `questionaire` group by `Q7_在選修這門課之前，您對「東坡詞」的暸解程度如何?` ORDER BY count(*) DESC"))
						if(ncol(Q.Know) > 0){
							sum<-sum(Q.Know[,2])
							Q.Know<-cbind(Q.Know,percent(Q.Know[,2]/sum))
							names(Q.Know)<-c("學生修課前課程了解程度","人數","百分比")
							Q.Know$`學生修課前課程了解程度` <- factor(Q.Know$`學生修課前課程了解程度`, levels=Q.Know$`學生修課前課程了解程度`[order(-Q.Know$`人數`)])
							ggplot(Q.Know, aes(x=`學生修課前課程了解程度`, y=`人數`, label=`百分比`,fill=`學生修課前課程了解程度`)) +
							geom_bar(stat = "identity")+ ggtitle("學生修課前課程了解程度")+
							geom_text(aes(y = `人數` + 1))+theme_bw()
							ggplotly()
						}
					})
					output$Q.Time.o <- renderPlotly({
						Q.Time <- dbGetQuery(conn = con, statement = paste0("SELECT `Q8_您每星期大約花多少時間學習本課程`,count(*) FROM `questionaire` group by `Q8_您每星期大約花多少時間學習本課程` ORDER BY count(*) DESC"))
						if(ncol(Q.Time) > 0){
							sum<-sum(Q.Time[,2])
							Q.Time<-cbind(Q.Time,percent(Q.Time[,2]/sum))
							names(Q.Time)<-c("學生修課花費時間","人數","百分比")
							Q.Time$`學生修課花費時間` <- factor(Q.Time$`學生修課花費時間`, levels=Q.Time$`學生修課花費時間`[order(-Q.Time$`人數`)])
							ggplot(Q.Time, aes(x=`學生修課花費時間`, y=`人數`, label=`百分比`,fill=`學生修課花費時間`)) +
							geom_bar(stat = "identity")+ ggtitle("學生修課花費時間")+
							geom_text(aes(y = `人數` + 1))+theme_bw()
							ggplotly()
						}
					})
					output$Q.LearnM.o <- renderPlotly({
						Q.LearnM <- dbGetQuery(conn = con, statement = paste0("SELECT * FROM `q9` order by `Numbers` desc"))
						if(ncol(Q.LearnM) > 0){
							sum<-sum(Q.LearnM[,2])
							Q.LearnM<-cbind(Q.LearnM,(Q.LearnM[,2]/sum)*100)
							names(Q.LearnM)<-c("Students Learning Method","Number of People","Percentage")
							Q.LearnM$`Students Learning Method` <- factor(Q.LearnM$`Students Learning Method`, levels = Q.LearnM$`Students Learning Method`[seq(1,max(1,length(Q.LearnM$`Students Learning Method`)),1)])
							ggplot(Q.LearnM, aes(x=`Students Learning Method`, y=`Number of People`, label=percent(`Percentage`/100),fill=`Students Learning Method`)) +
							geom_bar(stat = "identity")+ ggtitle("學生學習方法")+
							geom_text(aes(y = `Number of People` + 1))+theme_bw()+
							theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 0.5,vjust = 1))
							ggplotly()
						}
					})
					output$Q.Background.o <- renderPlotly({
						Q.Background <- dbGetQuery(conn = con, statement = paste0("SELECT `Q5_以下何者較符合您專業領域的背景？`,count(*) FROM `questionaire` group by `Q5_以下何者較符合您專業領域的背景？` order by count(*) desc"))
						if(ncol(Q.Background) > 0){
							sum<-sum(Q.Background[,2])
							Q.Background<-cbind(Q.Background,(Q.Background[,2]/sum)*100)
							names(Q.Background)<-c("Students Background","Number of People","Percentage")
							Q.Background$`Students Background` <- factor(Q.Background$`Students Background`, levels = Q.Background$`Students Background`[seq(1,max(1,length(Q.Background$`Students Background`)),1)])
							ggplot(Q.Background, aes(x=`Students Background`, y=`Number of People`, label=percent(`Percentage`/100),fill=`Students Background`)) +
							geom_bar(stat = "identity")+ ggtitle("學生背景")+
							geom_text(aes(y = `Number of People` + 1))+theme_bw()+
							theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 0.5,vjust = 1))
							ggplotly()
						}
					})
					output$Q.LearnT.o <- renderPlotly({
						Q.LearnT <- dbGetQuery(conn = con, statement = paste0("  SELECT * FROM `q13` order by `時段` ASC"))
						if(ncol(Q.LearnT) > 0){
							sum<-sum(Q.LearnT[,2])
						    Q.LearnT<-cbind(Q.LearnT,(Q.LearnT[,2]/sum)*100)
						    names(Q.LearnT)<-c("Students Learning Time","Number of People","Percentage")
						    Q.LearnT$`Students Learning Time` <- factor(Q.LearnT$`Students Learning Time`, levels = Q.LearnT$`Students Learning Time`[seq(1,max(1,length(Q.LearnT$`Students Learning Time`)),1)])
						    ggplot(Q.LearnT, aes(x=`Students Learning Time`, y=`Number of People`, label=`Number of People`)) +
						      geom_bar(stat = "identity")+ ggtitle("Percentage of Students' Learning Time 學生學習時間")+
						      geom_text(aes(y = `Number of People` + 1))+theme_bw()+
						      theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 0.5,vjust = 1))
						    ggplotly()
						}
					})
					# output$Q.schedule.o <- renderPlotly({
					#   Q.schedule <- dbGetQuery(conn = con, statement = paste0("SELECT `Q25`,count(*) FROM `pre_all` where `Q25` not like 'Q%' group by `Q25` order by count(*) desc"))
					#   sum<-sum(Q.schedule[,2])
					#   Q.schedule<-cbind(Q.schedule,(Q.schedule[,2]/sum)*100)
					#   names(Q.schedule)<-c("Students schedule","Number of People","Percentage")
					#   Q.schedule$`Students schedule` <- factor(Q.schedule$`Students schedule`, levels = Q.schedule$`Students schedule`[seq(1,max(1,length(Q.schedule$`Students schedule`)),1)])
					#   ggplot(Q.schedule, aes(x=`Students schedule`, y=`Number of People`, label=percent(`Percentage`/100))) +
					#     geom_bar(stat = "identity")+ ggtitle("加入班次")+
					#     geom_text(aes(y = `Number of People` + 1))+
					#     theme(axis.text.x = element_text(angle = 0, hjust = 1,vjust = 0.5))
					#   ggplotly()
					# })
				}else{
                    session$sendCustomMessage("myCallbackHandler_hide", "#content_o > #ql")
				}
					#####################################
					# pre_questionaire, post_questionaire
					######################################
					
				if(nrow(dbGetQuery(conn = con, statement = paste0("SHOW TABLES LIKE 'pre_questionaire'"))) > 0 && dbGetQuery(conn = con, statement = paste0("SELECT COUNT(*) FROM `pre_questionaire`")) > 0){
					output$Q.pre.o <- renderText({paste("(", as.character(dbGetQuery(conn = con, statement = "select count(*) FROM `pre_questionaire`"))," 份)")}) 
					output$Q.post.o <- renderText({paste("(", as.character(dbGetQuery(conn = con, statement = "select count(*) FROM `post_questionaire`"))," 份)")}) 
			        
			        pre.post.index = data.frame(c(10:24),c(1:21)[!c(1:21) %in% c(6,7,13:16)])

			        lapply(pre.post.index[,1], function(i) { 
					  output[[paste0('Q.score.pre.o', i)]] = renderPlotly({
					    cn <- dbGetQuery(conn = con, paste0("SELECT COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS where table_name='pre_questionaire' and column_name like '%Q",i,"_%'" ))[1,1]
					    #session$sendCustomMessage("myCallbackHandler_alert", as.character(cn))
					    Q.title <- as.character(dbGetQuery(conn = con, paste("select ", cn , " from pre_questionaire where ", cn , " like 'Q",i,"_%'",sep="")))
					    Q.title <- substr(Q.title, 0, gregexpr("_",Q.title )[[1]][2]-1)
					    if(nchar(Q.title) > 19){Q.title <-paste(substr(Q.title, 0, 19), "...") }
					    score <- as.vector(dbGetQuery(conn = con, paste0("select ", cn , " from pre_questionaire where ", cn , " not like 'Q",i,"%'")))
						score <- score[score %in% c(1:5)]
					    # Q.score.pre <<- rbind(Q.score.pre,score)
						
					    Q.score <- dbGetQuery(conn = con, paste0("select ", cn , ", count(*) from pre_questionaire where ", cn , " not like 'Q",i,"%' group by ", cn , "  order by count(*) desc"))
						Q.score <-Q.score[Q.score[,1] %in% c(1:5),]
						if(!all(c(1:5) %in% Q.score[,1])){
						  vac <- cbind(c(1:5)[!c(1:5) %in% Q.score[,1]],0)
						  colnames(vac) <- colnames(Q.score)
						  Q.score <- rbind(Q.score, vac)
						}
						Q.score <- Q.score[order(Q.score[,1]),]
						
					    sum<-sum(Q.score[,2])
					    Q.score<-cbind(Q.score,(Q.score[,2]/sum)*100)
					    names(Q.score)<-c("Students score","Number of People","Percentage")
					    Q.score$`Students score` <- factor(Q.score$`Students score`, levels = Q.score$`Students score`[seq(1,max(1,length(Q.score$`Students score`)),1)])
					    ggplot(Q.score, aes(x=`Students score`, y=`Number of People`, label=percent(`Percentage`/100))) + geom_bar(stat = 'identity',fill="#00b0f6") + ggtitle(Q.title) + geom_text(aes(y = `Number of People` + 1)) + theme(axis.text.x = element_text(angle = 0, hjust = 1,vjust = 0.5))
					    ggplotly()   ##0072b2                                                                                              
					    })
					}) 

					lapply(pre.post.index[,2], function(i) { 
					  output[[paste0('Q.score.post.o', i)]] = renderPlotly({
					    cn <- dbGetQuery(conn = con, paste0("SELECT COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS where table_name='post_questionaire' and column_name like '%Q",i,"_%'" ))[1,1]
					    Q.title <- as.character(dbGetQuery(conn = con, paste("select ", cn , " from post_questionaire where ", cn , " like 'Q",i,"_%'",sep="")))
					    Q.title <- substr(Q.title, 0, gregexpr("_",Q.title )[[1]][2]-1)
					    if(nchar(Q.title) > 19){Q.title <-paste(substr(Q.title, 0, 19), "...") }
					    Q.score <- dbGetQuery(conn = con, paste0("select ", cn , ", count(*) from post_questionaire where ", cn , " not like 'Q",i,"%' group by ", cn , "  order by count(*) desc"))
					    Q.score <-Q.score[Q.score[,1] %in% c(1:5),]
						if(!all(c(1:5) %in% Q.score[,1])){
						  vac <- cbind(c(1:5)[!c(1:5) %in% Q.score[,1]],0)
						  colnames(vac) <- colnames(Q.score)
						  Q.score <- rbind(Q.score, vac)
						}
						Q.score <- Q.score[order(Q.score[,1]),]
						#Q.score.post <- rbind(Q.score.post,Q.score)
					    sum<-sum(Q.score[,2])
					    Q.score<-cbind(Q.score,(Q.score[,2]/sum)*100)
					    names(Q.score)<-c("Students score","Number of People","Percentage")
					    Q.score$`Students score` <- factor(Q.score$`Students score`, levels = Q.score$`Students score`[seq(1,max(1,length(Q.score$`Students score`)),1)])
					    ggplot(Q.score, aes(x=`Students score`, y=`Number of People`, label=percent(`Percentage`/100))) + geom_bar(stat = 'identity',fill="#00bfc4") + ggtitle(Q.title) + geom_text(aes(y = `Number of People` + 1)) + theme(axis.text.x = element_text(angle = 0, hjust = 1,vjust = 0.5))
					    ggplotly()                                                                                                  
					    })
					}) 

					lapply(c(1:nrow(pre.post.index)),function(i){
						i.pre <- pre.post.index[i,1]
						i.post <- pre.post.index[i,2]
						cn.pre <- dbGetQuery(conn = con, paste0("SELECT COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS where table_name='pre_questionaire' and column_name like '%Q",i.pre,"_%'" ))[1,1]
					    cn.post <- dbGetQuery(conn = con, paste0("SELECT COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS where table_name='post_questionaire' and column_name like '%Q",i.post,"_%'" ))[1,1]
					    score.pre <- as.numeric(na.omit(dbGetQuery(conn = con, paste0("select ", cn.pre , " from pre_questionaire"))[-1,]))
					    score.post <- as.numeric(na.omit(dbGetQuery(conn = con, paste0("select ", cn.post , " from post_questionaire"))[-1,]))

					    ttest <- t.test(score.post,score.pre)
						#pvalue <- formatC(as.double(substr(ttest[[3]],0,attr(regexpr("(?<=\\.)0+",ttest[[3]],perl=T),"match.length")+5)),format="e",digit=1)
						pvalue <- formatC(ttest[[3]],format="e",digit=1)
						t <- ttest[[1]]
						significance <- ifelse(ttest[[3]]<=0.05,"顯著","不顯著")
						updown <- ifelse(t!=0, ifelse(significance=="顯著" ,ifelse(t<0,"下降","上升"),""),"")
						output[[paste0('Q.score.ttest.o', i)]] = renderUI({
							if(significance=="顯著")
								HTML(paste0("<div style='height:350px;background-color:palegreen;'><br><br><br><br><br><center>",significance,updown,"<br>[p值:",pvalue,"]</center></div>"))
							else
								HTML(paste0("<div style='height:350px;background-color:#f8766d;'><br><br><br><br><br><center>",significance,updown,"<br>[p值:",pvalue,"]</center></div>"))
							
						})
					})

				}else{
                    session$sendCustomMessage("myCallbackHandler_hide", "#content_o > #ql > #ql_corr")
				}
			},error=function(e){
				print("QL err")
				session$sendCustomMessage("myCallbackHandler_alert", as.character(e))
			},finally=function(){
				dbDisconnect(con)
			})

		}
	})

	####################################	########
	####################################	##	  ##
	# Participation Start-Complete Graph	########
	####################################	##
	####################################	##
	observe({
		if(!is.null(input$load)){	
			shinyjs::disable("downloadData.SC")
			observe({
			  input$item.SC
			  shinyjs::hide("msg.SC")
			  shinyjs::show("busy.SC")
			})

			output$plotSC.Histogram<- renderPlotly({

				con <- dbConnect(
					MySQL(),
					user = dbi.user,
					password = dbi.password,
					host = dbi.host,
					dbname = USER$courseDBName
				)
				dbGetQuery(conn = con, "SET NAMES 'utf8'")
					
			  	onFlush(function(){shinyjs::hide("busy.SC")}, once = TRUE, session = getDefaultReactiveDomain())

				  sc <-
				    dbGetQuery(
				      conn = con,
				      # statement = paste0(
				      #   "SELECT  a.`course_id`, b.`course_item_name`, a.`course_item_id`, a.`course_progress_state_type_id` as Type, date(a.`course_progress_ts`) as Date,count(*) as NumberOfPeople FROM `course_progress` a,`course_items` b
				      #   where a.`course_item_id` = b.`course_item_id` and
				      #   a.`course_item_id` in (select `course_item_id` from `course_items` where `course_item_type_id`=1) and a.`course_item_id`= '",
				      #   input$item.SC,
				      #   "'
				      #   group by a.`course_item_id`,a.`course_progress_state_type_id`, date(a.`course_progress_ts`)"
				      # )
				      statement = paste0(
				        "select d.`course_lesson_name`, c. * from (select b.`course_lesson_id`,a.`course_item_id`, b.`course_item_name`, a.`Type`, a.`Date`, a.`NumberOfPeople` from ( SELECT b.`course_item_id`, b.`course_progress_state_type_id`as `Type`, b.`Date`, count(*) as `NumberOfPeople` from ( SELECT a. * , date(`course_progress_ts`) as date, hour(`course_progress_ts`) as hour from ( SELECT * FROM `course_progress` where `course_item_id` in ( select `course_item_id` from `course_items`)
				        and `course_item_id`= '",
				        input$item.SC,
				        "' order by `course_progress_ts`ASC ) as a group by date(`course_progress_ts`), hour(`course_progress_ts`), `course_item_id`, `taiwan_user_id`, `course_progress_state_type_id` ) as b group by `course_item_id`, `type`, `date` ) as a join ( select `course_lesson_id`, `course_item_id`, `course_item_name` from `course_items` where `course_item_id` in (SELECT distinct `course_item_id` FROM `course_progress`) ) as b on (a.`course_item_id` = b.`course_item_id`) ) as c join (select `course_lesson_id`, `course_lesson_name` from course_lessons) as d on(c.`course_lesson_id` = d.`course_lesson_id`)"
				      )
				    )
				  sc.end.date <-
				    dbGetQuery(conn = con, statement = "SELECT max(date(course_progress_ts)) FROM `course_progress`")
				  sc.sta.date <-
				    dbGetQuery(conn = con, statement = "SELECT date(`course_launch_ts`) FROM `courses`")
				  dbDisconnect(con)

				  sc.unq <-
				    as.data.frame(seq(as.Date(as.character(sc.sta.date)), as.Date(as.character(sc.end.date)), 1))
				  colnames(sc.unq) <- "Date"

				  tryCatch({
					  sc.unq <-
					    cbind(sc.unq, sc[sc$Type == 1, 1:5][1, ],row.names = NULL) %>% rbind(cbind(sc.unq, sc[sc$Type ==2, 1:5][1, ],row.names = NULL))
					
						  sc <- merge(
						    sc.unq,
						    sc,
						    by = colnames(sc.unq),
						    all.x = T,
						    all.y = F
						  )
						  # Replace NA before validating 
						  sc$NumberOfPeople[is.na(sc$NumberOfPeople)] <- 0
				   },error=function(e){}) 

				  # Validate Data Existing
				  if(nrow(sc) > 0 && !is.null(sc) && nrow(na.omit(sc))==nrow(sc)){
				    shinyjs::enable("downloadData.SC")
				    output$msg.SC <- renderText({""}) 
				    shinyjs::show("sub.SC")
				  }else{
				    output$msg.SC <- renderText({"No Valid Data"}) 
				    shinyjs::hide("sub.SC")
				    shinyjs::hide("busy.SC")
				    shinyjs::show("msg.SC")
				  }
				  validate(need((nrow(sc) > 0 && !is.null(sc)),""))
				  tryCatch({
					  sc$Date <- as.factor(sc$Date)
					  ifelse(!is.na(sc[sc$Type == 1, ]$Type),sc[sc$Type == 1, ]$Type <- "Start", sc[sc$Type == 1, ]$Type) 
					  ifelse(!is.na(sc[sc$Type == 2, ]$Type),sc[sc$Type == 2, ]$Type <- "Complete", sc[sc$Type == 1, ]$Type) 
				  },error=function(e){}) 

				  sc$Type <- as.factor(sc$Type)
				  # Week is calculated by Date divided by 14 is because both Start and Complete records exists in sc dataframe, so one date will show 2 times in the df
				  weeknum <- floor(length(sc$Date) / 14)
				  weekLabel <-
				    sapply(1:weeknum, function(x) {
				      if (x %% 4 == 0)
				        paste0("W", x)
				      else{
				        ""
				      }
				    })
				  #wkly=seq(max(sc$NumberOfPeople)-5,  max(sc$NumberOfPeople)-5-5*floor(length(weekLabel)/4), by=-5)
				  weekLines <-
				    data.frame(vals = sc[order(sc$Date), ]$Date[seq(0, weeknum * 14, 14)] , wkl =
				                 weekLabel)
				  weekLines$vals <- as.factor(weekLines$vals)
				  xbreaks <- sc[order(sc$Date), ]$Date[seq(1, nrow(sc), 28)]
				  
				  # Start-Complete DataTable
				  sc.DT <-
				    sc[, -c(match("course_lesson_id", colnames(sc)),
				            match("course_item_id", colnames(sc)))]
				  output$DT.SC <-
				    DT::renderDataTable(DT::datatable(sc.DT,
				                                      options = list(
				                                        lengthMenu = list(c(5, 10,-1), c('5', '10', 'All')),
				                                        pageLength = 10
				                                      )))
				  # Start-Complete DataTable Download
				  output$downloadData.SC <- downloadHandler(
				    filename = function() {
				      #paste(input$dataset, input$filetype, sep = ".")
				      paste0("Start_Complete_", input$item.SC, ".csv")
				    },
				    
				    content = function(file) {
				      #sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
				      sep <- ","
				      # Write to a file specified by the 'file' argument
				      write.table(
				        sc.DT,
				        file,
				        sep = sep,
				        row.names = FALSE,
				        fileEncoding = input$radioBtn.SC
				      )
				    }
				  )

				  if(any(!is.na(sc$NumberOfPeople)) && any(sc$NumberOfPeople!=0)){
					# For Overview Comparison
				  	output$plotO.participation1 <- renderPlotly({
				  		ggplot(data = sc,aes(x = Date, y = NumberOfPeople,group = Type,colour = Type)) +geom_line() +
				  		labs( x = "",y = "Number Of People",color = "",title = "Number of People Start and Complete a Lesson") +
					    scale_x_discrete(breaks = as.character(xbreaks), labels = as.character(xbreaks)) +
					    theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5)) +
					    geom_vline(data = weekLines,mapping = aes(xintercept = as.numeric(vals)),color = "darkgrey",linetype = 3,size = 0.3) +
					    geom_text(data = weekLines,aes(x = vals, y = 0.9 * max(sc$NumberOfPeople),label = wkl, angle = 90),inherit.aes = FALSE)
				  		ggplotly(tooltip = c("Date", "NumberOfPeople", "colour"))
					})
					output$plotO.participation2 <- renderPlotly({
				  		ggplot(data = sc,aes(x = Date, y = NumberOfPeople,group = Type,colour = Type)) +geom_line() +
				  		labs( x = "",y = "Number Of People",color = "",title = "Number of People Start and Complete a Lesson") +
					    scale_x_discrete(breaks = as.character(xbreaks), labels = as.character(xbreaks)) +
					    theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5)) +
					    geom_vline(data = weekLines,mapping = aes(xintercept = as.numeric(vals)),color = "darkgrey",linetype = 3,size = 0.3) +
					    geom_text(data = weekLines,aes(x = vals, y = 0.9 * max(sc$NumberOfPeople),label = wkl, angle = 90),inherit.aes = FALSE)
				  		ggplotly(tooltip = c("Date", "NumberOfPeople", "colour"))
					})

				  # For participation tab itself
				  ggplot(data = sc,
				         aes(
				           x = Date,
				           y = NumberOfPeople,
				           group = Type,
				           colour = Type
				         )) +
				    geom_line() +
				    labs(
				      x = "",
				      y = "Number Of People",
				      color = "",
				      title = "Number of People Start and Complete a Lesson"
				    ) +
				    scale_x_discrete(breaks = as.character(xbreaks),
				                     labels = as.character(xbreaks)) +
				    theme(axis.text.x = element_text(
				      angle = 90,
				      hjust = 1,
				      vjust = 0.5
				    )) +
				    geom_vline(
				      data = weekLines,
				      mapping = aes(xintercept = as.numeric(vals)),
				      color = "darkgrey",
				      linetype = 3,
				      size = 0.3
				    ) +
				    geom_text(
				      data = weekLines,
				      aes(
				        x = vals,
				        y = 0.9 * max(sc$NumberOfPeople),
				        label = wkl,
				        angle = 90
				      ),
				      inherit.aes = FALSE
				    )
				  #theme(axis.text.x = element_blank())
				  }
				  ggplotly(tooltip = c("Date", "NumberOfPeople", "colour"))
				  
				})
		}
	})

	####################################	#######
	####################################	###
	# Engagement Involvement-Grade Graph	#######
	####################################	###
	####################################	#######
	# output$gdt1 <- renderScatterD3({
	#   scatterD3(x = mtcars$wt, y = mtcars$mpg)
	# })
	observe({
		if(!is.null(input$load)){
			shinyjs::disable("downloadData.IG")
			observe({
			  input$item.IG
			  shinyjs::hide("msg.IG")
			  shinyjs::show("busy.IG")
			})

			output$plotIG.Scatterplot <- renderPlotly({
			  #onFlush(function(){shinyjs::hide("busy.IG")}, once = TRUE, session = getDefaultReactiveDomain())
			  con <- dbConnect(
					MySQL(),
					user = dbi.user,
					password = dbi.password,
					host = dbi.host,
					dbname = USER$courseDBName
				)
				dbGetQuery(conn = con, "SET NAMES 'utf8'")
			  	if (input$item.IG == "Course Overall") {
			    ig <-
			      dbGetQuery(
			        conn = con,
			        statement = paste(
			          "SELECT c.*, d.* FROM (
			          select a.`course_id`, a.`taiwan_user_id`,
			          b.`duration`,
			          b.`Time Spent (minutes)`,
			          a.`course_passing_state_id` as Status,
			          a.`course_grade_overall` as Grade_Overall ,
			          a.`course_grade_verified` as Grade_Verified,
			          a.`course_grade_overall_passed_items` as `Passed_Items`
			          FROM (select * from `course_grades`) as a
			          inner join
			          (select `taiwan_user_id`,`duration`, (hour(`duration`)*60+minute(`duration`)) as `Time Spent (minutes)`,
			          count(*) as `Written_Items`
			          from`item_duration`
			          group by `taiwan_user_id`) as b
			          where a.`taiwan_user_id`=b.`taiwan_user_id`) as c
			          inner join
			          (select `taiwan_user_id`,`user_join_ts`,`country_cd` as `Country`,`browser_language_cd` as `Language`
			          from `users`) as d
			          where c.`taiwan_user_id`=d.`taiwan_user_id`"
			      )
			      )
			  } else{
			    if (input$item.IG == "All Items")
			      whereclause <- ""
			    else
			      whereclause <-
			        paste0("and c.`course_item_id` = '", input$item.IG, "'")
			    
			    ig <-
			      dbGetQuery(
			        conn = con,
			        statement = paste(
			          "select c.*, d.* from
			          (select
			          a.`course_item_id`, a.`taiwan_user_id`,a.`duration`,(hour(a.`duration`)*60+minute(a.`duration`)) as `Time Spent (minutes)`,
			          b.`course_item_passing_state_id` as Status,b.`course_item_grade_overall` as Grade_Overall ,
			          b.`course_item_grade_verified` as Grade_Verified
			          FROM `item_duration` as a
			          join `course_item_grades` as b
			          on a.`course_item_id`=b.`course_item_id` and
			          a.`taiwan_user_id`=b.`taiwan_user_id`) as c
			          inner join
			          (select `taiwan_user_id`,`user_join_ts`,`country_cd` as `Country`,`browser_language_cd` as `Language`
			          from `users`) as d
			          where c.`taiwan_user_id`=d.`taiwan_user_id`",
			          whereclause
			        )
			      )
			  }
			  dbDisconnect(con)
			  
			  # Validate Data Existing
			  if((nrow(ig) > 0 && !is.null(ig))){
			    shinyjs::enable("downloadData.IG")
			    output$msg.IG <- renderText({""}) 
			    shinyjs::show("sub.IG")
			  }
			  else{
			    output$msg.IG <- renderText({"No Valid Data"}) 
			    shinyjs::hide("sub.IG")
			    shinyjs::hide("busy.IG")
			    shinyjs::show("msg.IG")
			  }
			  validate(need((nrow(ig) > 0 && !is.null(ig)),""))
			  
			  # DataTable
			  ig.noline <-
			    ifelse(nrow(ig[ig$Status == 0, ]) < 2 &&
			             nrow(ig[ig$Status == 1, ]) < 2 &&
			             nrow(ig[ig$Status == 2, ]) < 2 && nrow(ig) > 0, T, F)
			  ig$Status <- as.integer(ig$Status)
			  if (any((ig$Status == 1) == TRUE))
			    ig$Status <- gsub(1, "Passed", ig$Status)
			  if (any((ig$Status == 2) == TRUE))
			    ig$Status <- gsub(2, "Verified", ig$Status)
			  if (any((ig$Status == 0) == TRUE))
			    ig$Status <- gsub(0, "Not Passed", ig$Status)
			  
			  output$DT.IG <-
			    DT::renderDataTable(DT::datatable(ig[, c("Country" ,"Language", "duration", "Time Spent (minutes)", "Grade_Overall", "Status")],
			                                      options = list(
			                                        lengthMenu = list(c(5, 10, -1), c('5', '10', 'All')), pageLength = 10
			                                      )))
			  
			  #DataTable Download
			  output$downloadData.IG <- downloadHandler(
			    filename = function() {
			      #paste(input$dataset, input$filetype, sep = ".")
			      paste0("Grades_TimeSpent_", input$item.IG, ".csv")
			    },
			    
			    content = function(file) {
			      #sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
			      sep <- ","
			      # Write to a file specified by the 'file' argument
			      write.table(
			        ig[, c("Country" ,"Language", "duration", "Time Spent (minutes)", "Grade_Overall", "Status")],
			        file,
			        sep = sep,
			        row.names = FALSE,
			        fileEncoding = input$radioBtn.IG
			      )
			    }
			  )
			  
			  
			  output$txt.IG <- renderUI({
			    if (!input$txt.IG_visible)
			      return(NULL)
			    all <-
			      ifelse(nrow(ig) > 0, paste(
			        lm_eqn(
			          "\\;\\;\\;\\;●\\;All\\;\\;\\;\\;\\;\\;\\;\\;\\;\\;\\;\\;\\;:",
			          "Time Spent (minutes)",
			          "Grade_Overall",
			          ig
			        ),
			        "\\(\\)"
			      ), "  ")
			    passed <-
			      ifelse(nrow(ig[ig$Status == "Passed",]) > 0, paste(
			        lm_eqn(
			          "\\;\\;\\;\\;●\\;Passed\\;\\;\\;\\;\\;\\;:",
			          "Time Spent (minutes)",
			          "Grade_Overall",
			          ig[ig$Status == "Passed",]
			        ),
			        "\\(\\)"
			      ), "  ")
			    verified <-
			      ifelse(nrow(ig[ig$Status == "Verified",]) > 0,  paste(
			        lm_eqn(
			          "\\;\\;\\;\\;●\\;Verified\\;\\;\\;\\;:",
			          "Time Spent (minutes)",
			          "Grade_Overall",
			          ig[ig$Status == "Verified",]
			        ),
			        "\\(\\)"
			      ), "  ")
			    notPassed <-
			      ifelse(nrow(ig[ig$Status == "Not Passed",]) > 0, paste(
			        lm_eqn(
			          "\\;\\;\\;\\;●\\;NotPassed:",
			          "Time Spent (minutes)",
			          "Grade_Overall",
			          ig[ig$Status == "Not Passed",]
			        ),
			        "\\(\\)"
			      ), "  ")
			    withMathJax(helpText(paste(
			      all, passed, verified, notPassed, sep = " "
			    )))
			    
			  })
			  
			  ### Boxplot Status ggplot
			  output$plotIG.Boxplot.Q <- renderPlotly({
			    ig.status.perc.Q <-
			      aggregate(ig$taiwan_user_id ~ ig$Status, FUN = length)
			    ig.status.perc.Q[, 2] <-
			      paste0(floor(ig.status.perc.Q[, 2] / nrow(ig) * 100), "%")
			    colnames(ig.status.perc.Q) <- c("Status", "perc")

			    remove_outliers <- function(x, na.rm = TRUE, ...) {
				  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
				  H <- 1.5 * IQR(x, na.rm = na.rm)
				  y <- x
				  y[x < (qnt[1] - H)] <- NA
				  y[x > (qnt[2] + H)] <- NA
				  y
				}

				ig_Q <- ig
			    ig_Q$`Time Spent (minutes)`<-remove_outliers(ig_Q$`Time Spent (minutes)`)
			    ggplot(ig_Q, aes(x = Status, y = `Time Spent (minutes)`)) + geom_boxplot() +
			      geom_text(
			        data = ig.status.perc.Q,
			        aes(
			          x = Status,
			          y = -0.1 * max(ig$Grade_Overall),
			          label = perc
			        ),
			        inherit.aes = FALSE
			      ) +
			      stat_summary(
			        aes(shape = "Standard deviation"),
			        fun.y = sd,
			        geom = "point",
			        size = 2
			      ) +
			      stat_summary(
			        aes(shape = "Mean (dotted line)"),
			        fun.y = mean,
			        geom = "point",
			        size = 2
			      ) +
			      scale_shape_manual(values = c("Mean (dotted line)" = 5, "Standard deviation" =
			                                      0)) +
			      stat_summary(
			        fun.y = mean,
			        geom = "line",
			        group = 1,
			        size = 0.3,
			        lty = 3
			      ) +
			      labs(title = "Passing Status and Grades Boxplot (with % of population)")
			    ggplotly(tooltip = c("Status", "Grade_Overall"))
			  })
			  
			  ### Boxplot Demographic ggplot
			  output$plotIG.Boxplot.D <- renderPlotly({
			    ig <- ig[nchar(ig$Country)>0,]
			    ig.status.perc.D <-
			      aggregate(ig$taiwan_user_id ~ ig$Country, FUN = length)
			    ig.status.perc.D[, 2] <-
			      ceiling(ig.status.perc.D[, 2] / nrow(ig) * 100)
			    colnames(ig.status.perc.D) <- c("Country", "perc")
			    ig$Country <- factor(ig$Country, levels = ig.status.perc.D[order(-ig.status.perc.D$perc),1])
			    # Only show top 2 %
			    # ig.status.perc.D <- ig.status.perc.D[which(ig.status.perc.D$perc>=2),]
			    # ig <- ig[which(ig$Country==ig.status.perc.D[,1]),]
			    ggplot(ig, aes(x = Country, y = Grade_Overall)) + geom_boxplot() +
			      stat_summary(aes(shape="Standard deviation"), fun.y=sd, geom="point", size=2)+
			      stat_summary(aes(shape="Mean (dotted line)"), fun.y=mean, geom="point", size=2)+
			      scale_shape_manual(values=c("Mean (dotted line)"=5,"Standard deviation"=0))+
			      stat_summary(fun.y=mean, geom="line", group=1, size=0.3, lty=3)+
			      geom_text(data=ig.status.perc.D, aes(x=Country, y=-0.1*max(ig$Grade_Overall), label=paste0(perc,"%")), inherit.aes=FALSE) +
			      geom_hline(aes(yintercept=as.numeric(input$slider.sd.D.IG)), linetype=3, size=0.3)+
			      geom_text(aes(x=1, y=as.numeric(input$slider.sd.D.IG)-0.05, label=paste("          σ = ",input$slider.sd.D.IG)), inherit.aes=FALSE)+
			      labs(title="Demographics and Grades Boxplot (with % of population)")
			    ggplotly(tooltip = c("Country","Grade_Overall"))
			  })
			  
			  
			  ### Boxplot Language ggplot
			  output$plotIG.Boxplot.L <- renderPlotly({
			    onFlush(function(){shinyjs::hide("busy.IG")}, once = TRUE, session = getDefaultReactiveDomain())
			    
			    ig <- ig[nchar(ig$Language)>0,]
			    ig.status.perc.L <- aggregate(ig$taiwan_user_id~ig$Language, FUN=length)
			    ig.status.perc.L[,2] <- floor(ig.status.perc.L[,2]/nrow(ig)*100)
			    colnames(ig.status.perc.L) <- c("Language", "perc")
			    ig$Language <- factor(ig$Language, levels = ig.status.perc.L[order(-ig.status.perc.L$perc),1])
			    ggplot(ig, aes(x = Language, y = Grade_Overall)) + geom_boxplot() +
			      stat_summary(aes(shape="Standard deviation"), fun.y=sd, geom="point", size=2)+
			      stat_summary(aes(shape="Mean (dotted line)"), fun.y=mean, geom="point", size=2)+
			      scale_shape_manual(values=c("Mean (dotted line)"=5,"Standard deviation"=0))+
			      stat_summary(fun.y=mean, geom="line", group=1, size=0.3, lty=3)+
			      geom_text(data=ig.status.perc.L, aes(x=Language, y=-0.1*max(ig$Grade_Overall), label=paste0(perc,"%")), inherit.aes=FALSE) +
			      geom_hline(aes(yintercept=as.numeric(input$slider.sd.L.IG)), linetype=3, size=0.3)+
			      geom_text(aes(x=1, y=as.numeric(input$slider.sd.L.IG)-0.05, label=paste("          σ = ",input$slider.sd.L.IG)), inherit.aes=FALSE)+
			      labs(title="Language and Grades Boxplot (with % of population)")
			    ggplotly(tooltip = c("Language","Grade_Overall"))
			  })
			  
			  ### Scatterplot ggplot
			  if (ig.noline) {

			  	 # For Overview Comparison
			      ouput$plotO.grade1 <- renderPlotly({
			      	ggplot(data = ig,
			           aes(`Time Spent (minutes)`, Grade_Overall, color = Status)) +
				      geom_point(size = 2) +
				      labs(title = "Time Spent on Quiz and Grades") +
				      scale_colour_manual(
				        values = c(
				          "Passed" = "dodgerblue",
				          "Verified" = "chartreuse3",
				          "Not Passed" = "darkorange"
				        )
				      )
				      ggplotly()
			      	})

			      ouput$plotO.grade2 <- renderPlotly({
			      	ggplot(data = ig,
			           aes(`Time Spent (minutes)`, Grade_Overall, color = Status)) +
				      geom_point(size = 2) +
				      labs(title = "Time Spent on Quiz and Grades") +
				      scale_colour_manual(
				        values = c(
				          "Passed" = "dodgerblue",
				          "Verified" = "chartreuse3",
				          "Not Passed" = "darkorange"
				        )
				      )
				      ggplotly()
			      	})


			  	# For grade tab itself
			    ggplot(data = ig,
			           aes(`Time Spent (minutes)`, Grade_Overall, color = Status)) +
			      geom_point(size = 2) +
			      labs(title = "Time Spent on Quiz and Grades") +
			      scale_colour_manual(
			        values = c(
			          "Passed" = "dodgerblue",
			          "Verified" = "chartreuse3",
			          "Not Passed" = "darkorange"
			        )
			      )
			  } else if (nrow(ig) > 0) {

			  	 # For Overview Comparison
			      output$plotO.grade1 <- renderPlotly({
			      	ggplot(data = ig,
			           aes(`Time Spent (minutes)`, Grade_Overall, color = Status)) +
				      geom_point(size = 1, alpha = 0.5) +
				      labs(title = "Time Spent on Quiz and Grades Scatterplot") +
				      scale_colour_manual(
				        values = c(
				          "Passed" = "dodgerblue",
				          "Verified" = "chartreuse3",
				          "Not Passed" = "darkorange"
				        )
				      ) +
				      geom_smooth(method = "lm",
				                  se = F,
				                  size = 0.5) 
				      ggplotly()
			      	})

			      output$plotO.grade2 <- renderPlotly({
			      	ggplot(data = ig,
			           aes(`Time Spent (minutes)`, Grade_Overall, color = Status)) +
				      geom_point(size = 1, alpha = 0.5) +
				      labs(title = "Time Spent on Quiz and Grades Scatterplot") +
				      scale_colour_manual(
				        values = c(
				          "Passed" = "dodgerblue",
				          "Verified" = "chartreuse3",
				          "Not Passed" = "darkorange"
				        )
				      ) +
				      geom_smooth(method = "lm",
				                  se = F,
				                  size = 0.5) 
				      ggplotly()
			      	})



			    ranges <- reactiveValues(x = NULL, y = NULL)
				# For grade tab itself
			    ggplot(data = ig,
			           aes(`Time Spent (minutes)`, Grade_Overall, color = Status)) +
			      geom_point(size = 1, alpha = 0.5) +
			      labs(title = "Time Spent on Quiz and Grades Scatterplot") +
			      scale_colour_manual(
			        values = c(
			          "Passed" = "dodgerblue",
			          "Verified" = "chartreuse3",
			          "Not Passed" = "darkorange"
			        )
			      ) +
			      geom_smooth(method = "lm",
			                  se = F,
			                  size = 0.5) +
			      coord_cartesian(xlim = ranges$x, ylim = ranges$y)





			  }
			  ggplotly()
			})

			#+scale_fill_manual(values=c(red="red",black="black"))

		}
	})


	####################################	#####
	####################################	##	##
	# Discussion 							##   ##
	####################################	##  ##
	####################################	#####
	# output$gdt1 <- renderScatterD3({
	#   scatterD3(x = mtcars$wt, y = mtcars$mpg)
	# })
	observe({
		if(!is.null(input$load)){

			shinyjs::disable("downloadData.DC")
			observe({
			  input$item.DC
			  #shinyjs::hide("msg.DC")
			  shinyjs::show("busy.DC")
			})

			############
			# Discussion
			############
			func.DC <- function(){
			  con <- dbConnect(
					MySQL(),
					user = dbi.user,
					password = dbi.password,
					host = dbi.host,
					dbname = USER$courseDBName
				)
				dbGetQuery(conn = con, "SET NAMES 'utf8'")
			  DQ.module <- dbGetQuery(conn = con, "select c.`discussion_question_id`, c.`course_item_id`,
			                          IF(c.`course_module_id` IS NULL AND f.`course_module_id` IS NULL, NULL, CONCAT(COALESCE(c.`course_module_id`,''), COALESCE(f.`course_module_id`,''))) AS module_id,
			                          c.`Date`,c.`Content`,c.`Number`
			                          from 
			                          (select a.*, b.Number from 
			                          (
			                          SELECT `discussion_question_id`, `course_item_id`, `course_module_id`,
			                          date(`discussion_question_created_ts`) as Date, `discussion_question_details` as `Content`
			                          FROM `discussion_question` )as a
			                          left join 
			                          (SELECT `discussion_question_id`, count(*) as Number 
			                          FROM `discussion_answer` 
			                          group by `discussion_question_id`) as b
			                          on a.`discussion_question_id`=b.`discussion_question_id`) as c
			                          left join
			                          (select c.`course_module_id`,d.`course_item_id`
			                          from 
			                          (select a.*,b.`course_lesson_id`,b.`course_lesson_order`,b.`course_lesson_name` 
			                          from (SELECT * FROM `course_modules`) as a 
			                          right join 
			                          (select * from `course_lessons`) as b 
			                          on a.`course_module_id`=b.`course_module_id`) as c 
			                          right join 
			                          (select * from `course_items` ) as d 
			                          on c.`course_lesson_id`=d.`course_lesson_id`) as f
			                          on c. `course_item_id`=f.`course_item_id`")
			  
			  Q<-rep("Question",nrow(DQ.module))
			  DQ.module<-cbind(DQ.module,Q)
			  DQ.module<-na.omit(DQ.module)


			  DA.module <- dbGetQuery(conn = con, "select d.`discussion_question_id`, d.`course_item_id`,
			                          IF(d.`course_module_id` IS NULL AND f.`course_module_id` IS NULL, NULL, CONCAT(COALESCE(d.`course_module_id`,''), COALESCE(f.`course_module_id`,''))) AS module_id,
			                          d.`Date`,d.`Content`,d.`Number`
			                          from 
			                          (select d.*,c.`Date`,c.`discussion_answer_content` as`Content`,c.`Number` from 
			                          (SELECT `discussion_question_id`, `course_item_id`,`course_module_id`
			                          FROM `discussion_question` )as d
			                          right join 
			                          (select a.*, b.Number from 
			                          (select `discussion_question_id`,Date(`discussion_answer_created_ts`) as Date,`discussion_answer_content`
			                          from `discussion_answer` ) as a
			                          left join 
			                          (SELECT `discussion_question_id`, count(*) as Number 
			                          FROM `discussion_answer` 
			                          group by `discussion_question_id`) as b
			                          on a.`discussion_question_id`=b.`discussion_question_id`) as c
			                          on d.`discussion_question_id`=c.`discussion_question_id`) as d
			                          left join
			                          (select c.`course_module_id`,d.`course_item_id`
			                          from 
			                          (select a.*,b.`course_lesson_id`,b.`course_lesson_order`,b.`course_lesson_name` 
			                          from (SELECT * FROM `course_modules`) as a 
			                          right join 
			                          (select * from `course_lessons`) as b 
			                          on a.`course_module_id`=b.`course_module_id`) as c 
			                          right join 
			                          (select * from `course_items` ) as d 
			                          on c.`course_lesson_id`=d.`course_lesson_id`) as f
			                          on d. `course_item_id`=f.`course_item_id`")
			  dbDisconnect(con)
			  
			  A<-rep("Answer",nrow(DA.module))
			  DA.module<-cbind(DA.module,A)
			  DA.module<-na.omit(DA.module)


			  # colume names reassign 
			  colnames(DA.module)<-c("discussion_question_id","course_item_id","course_module_id","Date","Content Detail","Number","Type" )
			  colnames(DQ.module)<-c("discussion_question_id","course_item_id","course_module_id","Date","Content Detail","Number","Type" )
			  DC.module<-rbind(DQ.module,DA.module)
			  #those whose item_id and module_id are both null replace module_id as others
			  DC.module$course_module_id[DC.module$course_module_id==""] <-"Others"
			  # create a col level to assgin whether it's item/module
			  Level<- rep("",nrow(DC.module))
			  DC.module<-cbind(DC.module,Level)
			  DC.module<-na.omit(DC.module)
			  

			  DA.module<-na.omit(DA.module)
			  DC.module$Level<-as.character(DC.module$Level)
			  DC.module[DC.module$course_item_id=="",]$Level<-"Module"
			  DC.module[DC.module$Level=="",]$Level<-"Item"
			  #fill in the NULL???item_id with the ones in module_id

			  DC.module$course_item_id[DC.module$course_item_id==""]<-DC.module[DC.module$course_item_id=="",]$course_module_id
			  DC.module$Number[is.na(DC.module$Number)] <- 0
			  # Clean Content
			  DC.module$Content<- paste(substr(gsub("(<co-content>|<text>|<text/>|</text>|</co-content>|\\n|http\\S+\\s*|<(.|\n)*?>)", "", DC.module$Content),0,15), " ...")
			  # form a new sub table with unique "discussion_question_id" 
			  sub<-DC.module[!duplicated(DC.module$discussion_question_id), ]
			  Question_id<-rep("NA",nrow(sub))
			  sub<-cbind(Question_id,sub)
			  # rank the unique "discussion_question_id" and map into "Q_id"
			  sub$Question_id<-rank(sub$Number,ties.method="first")
			  # map the "Q_id" to DC.module
			  DC<-merge(DC.module,sub[,1:2],by="discussion_question_id",all.x = T,all.y = F)
			  # 
			  # if(input$module.DC == "All"){
			  #   if (input$item.DC != "All Items")
			  #     DC <- DC[DC$course_item_id == input$item.DC,]
			  # }else{
			  #   if(input$item.DC == "All")
			  #     DC <- DC[DC$course_module_id == input$module.DC,]
			  #   else
			  #     DC <- DC[DC$course_item_id == input$item.DC,]
			  # }
			  
			  
			  return(list("DC" = DC, "DQ.module" = DQ.module, "DA.module" = DA.module))
			}

			output$plotDC.Circlizeplot <- renderPlot({
				 con <- dbConnect(
					MySQL(),
					user = dbi.user,
					password = dbi.password,
					host = dbi.host,
					dbname = USER$courseDBName
				)
				dbGetQuery(conn = con, "SET NAMES 'utf8'")
				activeStudents <- dbGetQuery(conn = con, 
				paste("select c.discussion_question_title, d.taiwan_discussions_user_id from (SELECT a.discussion_question_title, b.taiwan_discussions_user_id FROM `discussion_answer` b inner join `discussion_question` a on b.discussion_question_id=a.discussion_question_id)c inner join (select taiwan_discussions_user_id from discussion_answer group by `taiwan_discussions_user_id` order by count(`taiwan_discussions_user_id`) desc limit ",input$slider.max.activeStudents,")d on c.taiwan_discussions_user_id = d.taiwan_discussions_user_id"))
				activeStudents <- merge(activeStudents,as.data.frame(table(activeStudents[,1])),by.x="discussion_question_title", by.y="Var1")
				totalFreq <- sum(table(activeStudents[,1]))
				#Create data
				question=paste("[",substr(activeStudents[,1],0,floor(58*activeStudents[,3]/totalFreq)),"...]", sep="")
				user=paste("User", substr(activeStudents[,2],0,2),"..." , sep="")
				dat <- data.frame(user,question)
				dat <- with(dat, table(user,question))

				output$text.question.title.DC <- renderUI({
					HTML(paste("<br>右圖所示完整題目：<br>",paste0("►[",unique(activeStudents[,1]),"]",collapse="<br>"),sep="<br>"))
				})

				# Charge the circlize library
				library(circlize)
				# Make the circular plot
				par(cex=1.5,mar=c(0,0,0,0))
				chordDiagram(as.data.frame(dat), transparency = 0.5)
			})

			output$plotDC.Scatterplot <- renderPlotly({
			  
			  onFlush(function(){shinyjs::hide("busy.DC")}, once = TRUE, session = getDefaultReactiveDomain())
			  
			  tryCatch({
			    funcDCReturn <- func.DC()
			    DC <- funcDCReturn$DC
			    DQ.module <- funcDCReturn$DQ.module
			    DA.module <- funcDCReturn$DA.module
			  },error = function(e) {
			    conditionMessage(e)
			    #session$sendCustomMessage("myCallbackHandler_alert", as.character(e))
			    print("DC err")
			    output$msg.DC <- renderText({"No Valid Data"}) 
			    shinyjs::hide("busy.DC")
			    shinyjs::show("msg.DC")
			    shinyjs::hide("sub.DC")

			  })
			  # Validate Data Existing
			  if(exists("DC") && (nrow(DC)) > 0){
			    shinyjs::enable("downloadData.DC")
			    output$msg.DC <- renderText({""}) 
			    shinyjs::show("sub.DC")
			  }else{
			    output$msg.DC <- renderText({"No Valid Data"}) 
			    shinyjs::hide("sub.DC")
			    shinyjs::hide("busy.DC")
			    shinyjs::show("msg.DC")
			  }
			  validate(need((exists("DC") && nrow(DC) > 0 && !is.null(DC)),""))
			  
			  
			  if(input$plot.DC_top30 && length(DC$Type == "Question")>30){
			    DC.Q <-DC[DC$Type == "Question",]
			    top.Q <- head(DC.Q[order(-DC.Q$Number),]$Question_id,30)
			    DC.plot <- DC[DC$Question_id %in% top.Q,]
			  }
			  else
			    DC.plot <- DC
			  
			  # print(nrow(head(DC[order(-DC$Number),],50)))
			  # print(DC.plot)
			  
			  tryCatch({
			  # Calculate Std. by question_id
				  qids <- unique(DC.plot$Question_id)
				  qids.std <- data.frame()
				  for (i in 1:length(qids)) {
				    ans <- DC.plot[DC.plot$Question_id == qids[i] & DC.plot$Type == "Answer", ]
				    ques <- DC.plot[DC.plot$Question_id == qids[i] & DC.plot$Type == "Question", ]
				    ans <- ans[order(ans$Date),]
				    
				    if(nrow(ans)>2)
				      std.inter <- sd(as.Date(ans$Date[2:nrow(ans)])-as.Date(ans$Date[1:nrow(ans)-1]))
				    else
				      std.inter <- 0
				    
				    std <- sd(as.Date(ans$Date)- as.Date(ques$Date))
				    if(is.na(std)) std <- 0
				    
				    qids.std <- rbind(qids.std, c(qids[i], std, std.inter), stringsAsFactors = FALSE)
				  }
	 
				  colnames(qids.std) <- c("Question_id", " Standard Deviation of Response Day from Question", " Standard Deviation of InterResponse Day")
				  qids.std <- qids.std[order(qids.std$Question_id),]
				  qids.std.plot <- melt(qids.std, id.vars=c("Question_id"))

			   },error = function(e) {
			    conditionMessage(e)
			    #session$sendCustomMessage("myCallbackHandler_alert", as.character(e))
			  })
			  
			  output$plotDC.Scatterplot.Std <- renderPlotly({
			    #validate(need(!all(is.na(qids.std$Sd) == T) && input$item.DC != "-1", "No Enough Data to Calculate SD"))
			    #if (!all(is.na(qids.std$Sd) == T) && input$item.DC != "-1") {
			    ggplot(data = qids.std.plot, aes(
			      x = `value`,
			      y = `Question_id`,
			      group = `variable`,
			      color = `variable`
			    )) +
			      geom_point() +
			      #geom_line() +
			      geom_vline(aes(xintercept = as.numeric(input$slider.sd.DC)),
			                 linetype = 3,
			                 size = 0.3) +
			      geom_text(aes(
			        x = as.numeric(input$slider.sd.DC) + 0.5,
			        y = max(qids.std$Question_id) - 0.1,
			        label = paste("          σ = ", input$slider.sd.DC)
			      ),
			      inherit.aes = FALSE) +
			      #geom_bar(stat="identity", width=0.1)+
			      scale_y_continuous(breaks = seq(1, max(qids.std$Question_id), 1)) +
			      scale_x_continuous(expand = c(0.01, 0),
			                         limits = c(0, max(qids.std$` Standard Deviation of Response Day from Question`) + 1)) +
			      #theme(panel.background = element_rect(fill = "grey")) +
			      labs(title = "Standard Deviation of Response",
			           x = "Standard Deviation",
			           y = "Question Id (代號)")
			    ggplotly(tooltip = c("Question_id", "value"))
			  })
			  
			
			  DC.4DT <- rbind(DQ.module,DA.module)
			  if(input$module.DC == "All"){
			    if (input$item.DC != "All Items")
			      DC.4DT<- DC.4DT[DC.4DT$course_item_id == input$item.DC,]
			  }else{
			    if(input$item.DC == "All")
			      DC.4DT <- DC.4DT[DC.4DT$course_module_id == input$module.DC,]
			    else
			      DC.4DT <- DC.4DT[DC.4DT$course_item_id == input$item.DC,]
			  }
			  DC.4DT$Number[is.na(DC.4DT$Number)] <- 0
			  #colnames(DC.4DT)[which(colnames(DC.4DT)=="QA")] <- "Type"
			  DC.4DT$`Content Detail`<- gsub("(<co-content>|<text>|<text/>|</text>|</co-content>|\\n|http\\S+\\s*|<(.|\n)*?>)", "", DC.4DT$`Content Detail`)
			  DC.4DT$course_item_name <- sapply(DC.4DT$course_item_id, function(x){
			    y <- isolate({USER$selectionlist[which(USER$selectionlist$course_item_id==x),]$course_item_name})
			    ifelse(length(y)>0,y,NA) })
			  
			  DC.4DT$course_module_name <- sapply(DC.4DT$course_module_id, function(x){
			  	dlist <-  isolate({unique(USER$selectionlist[order(USER$selectionlist$course_module_order), c(2, 4)])})
  				modulesList <- setNames(dlist$course_module_id, dlist$course_module_name)
			    y <- names(modulesList[which(modulesList==x)]) 
			    ifelse(length(y)>0,y,NA)})

			 DC.4DT <- as.data.frame(DC.4DT)
			  	
			 output$DT.DC <-
			    DT::renderDataTable(DT::datatable(DC.4DT[order(-DC.4DT$Number), c("Date" ,"course_module_name", "course_item_name", "Type", "Content Detail", "Number")],
			                                      rownames= FALSE,
			                                      options = list(
			                                        lengthMenu = list(c(5, 10, -1), c('5', '10', 'All')), pageLength = 10
			                                      )))
			  
			  #DataTable Download
			  output$downloadData.DC <- downloadHandler(
			    filename = function() {
			      #paste(input$dataset, input$filetype, sep = ".")
			      paste0("Discussion_", input$item.DC, ".csv")
			    },
			    content = function(file) {
			      #sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
			      sep <- ","
			      # Write to a file specified by the 'file' argument
			      write.table(
			        DC.4DT[, c("Date" ,"discussion_question_id","course_module_name", "course_module_id", "course_item_name", "course_item_id", "Type", "Content Detail", "Number")],
			        file,
			        sep = sep,
			        row.names = FALSE,
			        fileEncoding = input$radioBtn.DC
			      )
			    }
			  )
			  
			  ggplot(data = DC.plot,
			         aes(Date,Question_id , a=Level, shape=`Type`, color = Number, b=Content)) +
			    theme(legend.position="bottom",legend.box="horizontal")+
			    scale_shape_manual(values=c(3, 0, 16))+
			    geom_point(size = 2) +
			    scale_colour_gradientn(colours = c("red","blue","darkblue"))+
			    scale_y_continuous(breaks = seq(1, max(DC.plot$Question_id), 1))+
			    theme(axis.text.y = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5)) +
			    labs(title = "Discussion Questions and Answers", x = "", y = "Questions")
			  ggplotly(tooltip = c("Question_id","Date","Level","Type","Number"="Amount of Anwers of the question","Content"))
			})

		}
	})


##########################################  ##	#  ##
##########################################  ##  #  ##
# WORD CLOUD                                 #  #  #
##########################################   #######
##########################################    #####
	observe({
		if(!is.null(input$load)){
			shinyjs::disable("downloadData.WC.WF")
			shinyjs::disable("downloadData.WC.C")
			shinyjs::disable("recal")
			shinyjs::disable("clean")
			observe({
			  input$module.WC
			  shinyjs::hide("msg.WC")
			  shinyjs::show("busy.WC")
			  shinyjs::show("sub.WC")
			})

			path <- "/srv/shiny-server/app-moocs/data/"
			all.D.dir <- paste0(path,USER$courseDBName,"_allD.txt")
			all.F.dir <- paste0(path,USER$courseDBName,"_allF.txt")
			all.O.dir <- paste0(path,USER$courseDBName,"_allO.txt")
			data.dir <- c(all.D.dir, all.F.dir, all.O.dir)
			names(data.dir) <- c("D", "F", "O")

			 observeEvent(input$recal, {
			   js_string <- 'confirm("Are you sure to recalculate word frequency?\\n It may take 2~5 minutes to do so!!!");'
			   session$sendCustomMessage("myCallbackHandler_recal", list(value = js_string))
			 })
			 observe({
			   print(input$deleteConfirmChoice_recal)
			   if(!is.null(input$deleteConfirmChoice_recal) && input$deleteConfirmChoice_recal){
			   	 	try(system(paste("echo ", dbi.shellpassword, " | sudo -S Rscript --vanilla /srv/shiny-server/app-moocs/wordcloud_function.R recal", USER$courseDBName)))
			   	 	#session$sendCustomMessage("myCallbackHandler_alert", "")
			   }
			 })

			 observeEvent(input$clean, {
			   js_string <- 'confirm("Are you sure to clean word?\\n It may take minutes to do so!!!");'
			   session$sendCustomMessage("myCallbackHandler_clean", list(value = js_string))
			 })
			 observe({
			   print(input$deleteConfirmChoice_clean)
			   if(!is.null(input$deleteConfirmChoice_clean) && input$deleteConfirmChoice_clean){
			   	 	try(system(paste("echo ", dbi.shellpassword, " | sudo -S Rscript --vanilla /srv/shiny-server/app-moocs/wordcloud_function.R clean", USER$courseDBName)))
			   	 	#session$sendCustomMessage("myCallbackHandler_alert", "")
			   }
			 })

			output$plot.WC <- renderPlot({
				onFlush(function(){shinyjs::hide("busy.WC")}, once = TRUE, session = getDefaultReactiveDomain())
				#wcId <- ifelse(input$item.WC=="", input$module.WC, ifelse(input$item.WC=="All", input$module.WC, input$item.WC))
				con <- dbConnect(
					MySQL(),
					user = dbi.user,
					password = dbi.password,
					host = dbi.host,
					dbname = USER$courseDBName
			    )
				wcId <- input$module.WC
				terms <- reactive({
				# ...but not for anything else
				#isolate({
				tryCatch({
					withProgress({
					setProgress(message = "Processing corpus...")
					 context <- as.character(read.table(data.dir["F"], fileEncoding = "utf8")[1,1])
					 getTermMatrix(wcId,data.dir,path,USER$courseDBName, con)

				})},error=function(e){
					print("WC err")
					session$sendCustomMessage("myCallbackHandler_alert", as.character(e))
					output$msg.WC <- renderText({"No Valid Data"}) 
					shinyjs::hide("sub.WC")
					shinyjs::hide("busy.WC")
					shinyjs::show("msg.WC")
				},finally=dbDisconnect(con))

				#})
				})

			  validate(need(length(terms()) > 1,"No Data"))
			  
			  	if (length(terms()) > 1) {
			  		tryCatch({
			    # Make the wordcloud drawing predictable during a session
			    wordcloud_rep <- repeatable(wordcloud)
			    v <- terms()$textFreq
			    wordcloud(
					names(v),
					v,
					scale = c(7.5, .4),
					random.order = FALSE,
					min.freq = 0,
					max.words = input$slider.max.WC,
					rot.per = .15,
					colors = brewer.pal(5, "Dark2")
			    )},error=function(e){
					
				})
			    
			    # DataTable
			    names(v) <- gsub("\n", "", names(v))
			    vdt <- unname(cbind(names(v), v))
			    colnames(vdt) <- c("Words", "Freq")
			    output$DT.WC.WordFreq <- DT::renderDataTable(DT::datatable(
					head(vdt, input$slider.max.WC),options = list(lengthMenu = list(c(5, 10, -1), c('5', '10', 'All')), pageLength = 10)
			    ))
			    output$DT.WC.Content <-
			      DT::renderDataTable(DT::datatable(terms()$DT.context,
						                options = list(
						                  lengthMenu = list(c(5, 10, -1), c('5', '10', 'All')), pageLength = 10
						                )))
			    
			    #WordFreq DataTable Download
			    output$downloadData.WC.WF <- downloadHandler(
			      filename = function() {
			        #paste(input$dataset, input$filetype, sep = ".")
			        paste0(wcId, "_WordFreq", ".csv")
			      },
			      
			      content = function(file) {
			        #sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
			        sep <- ","
			        # Write to a file specified by the 'file' argument
			        write.table(
			          head(vdt, input$slider.max.WC),
			          file,
			          sep = sep,
			          row.names = FALSE,
			          fileEncoding = input$radioBtn.WF
			        )
			      }
			    )
			    #Content DataTable Download
			    output$downloadData.WC.C <- downloadHandler(
			      filename = function() {
			        #paste(input$dataset, input$filetype, sep = ".")
			        paste0(input$text.WC, "_Content", ".csv")
			      },
			      
			      content = function(file) {
			        #sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
			        sep <- ","
			        # Write to a file specified by the 'file' argument
			        write.table(
			          terms()$DT.context,
			          file,
			          sep = sep,
			          row.names = FALSE,
			          fileEncoding = input$radioBtn.WC
			        )
			      }
			    )
			    
			    observe({
			      shinyjs::enable("downloadData.WC.WF")
			      shinyjs::enable("downloadData.WC.C")
			      if(USER$Role!="a"){
			      	shinyjs::hide("clean")
			      	shinyjs::hide("recal")
			      }
			      else{
			      	shinyjs::enable("clean")
			      	shinyjs::enable("recal")
			      }
			    })
			  }
			})
		}
	})

# End shinyServer
})

####################################################################
####################################################################
####################################################################
####################################################################
# Utility
####################################################################
####################################################################
####################################################################
####################################################################

#runApp(list(ui = ui, server = server))

###################
# UpdateModulelist 
###################
UpdateModulelist <- function(session, output, modulesList){ 
  updateSelectInput(session, "module.SC", choices = c("全部"="All", modulesList, "其他"="Others"))
  updateSelectInput(session, "module.IG", choices = c("全部"="All", modulesList))
  updateSelectInput(session, "module.DC", choices = c("全部"="All", modulesList, "其他"="Others"))
  updateSelectInput(session, "module.WC", choices = c(
    "全部" = "A",
    "全部 Feedback" = "F",
    "全部 Discussion" = "D",
    modulesList,
    "其他"="Others"
  ))
}

###################
# GenSelectionList
###################
GenSelectionList <- function(con){
	# Make selectionlist
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

		grade <-
		  dbGetQuery(
		    conn = con,
		    "select distinct(`course_item_type_id`) from
		    (SELECT distinct(`course_item_id`) FROM `course_item_grades`) as c
		    left join
		    (select * from `course_items`) as d
		    on c.`course_item_id`=d.`course_item_id`"
		  )
		if(length(selectionlist[selectionlist$course_item_type_id == "1", 14])>0)
		  selectionlist[selectionlist$course_item_type_id == "1", 14] <- "Lecture"
		if(length(selectionlist[selectionlist$course_item_type_id == "3", 14])>0)
		  selectionlist[selectionlist$course_item_type_id == "3", 14] <- "Reading"
		if(length(selectionlist[selectionlist$course_item_type_id == "15", 14])>0)
		  selectionlist[selectionlist$course_item_type_id == "15", 14] <- "Reading"

		for (n in length(grade$course_item_type_id)) {
		  if(length( selectionlist[selectionlist$course_item_type_id == grade[n, 1], 14])>0)
		    selectionlist[selectionlist$course_item_type_id == grade[n, 1], ]$course_item_type <- "Graded Assessment"
		}
		if(length(selectionlist[selectionlist$course_item_type == "", 14])>0)
		  selectionlist[selectionlist$course_item_type == "", ]$course_item_type <- "Assessment"
		selectionlist
}
#################
# Close MYSQLCon 
#################

CloseMySQLCon <- function(){
	all_cons <- dbListConnections(MySQL())
	  for(con in all_cons)
		   dbDisconnect(con)
}


####################
# Equation Process
####################
lm_eqn <- function(p, x, y, df) {
  m <- lm(df[, y] ~ df[, x], df)
  options("scipen" = 100, "digits" = 2)
  para <-
    list(
      a = format(coef(m)[1], digits = 2),
      b = format(coef(m)[2], digits = 2),
      r2 = format(summary(m)$r.squared, digits = 3)
    )
  sign <- ifelse(para["b"] < 0, "", "+")
  eq <-
    paste("\\( ",
          p,
          "\\qquad y = ",
          para["a"],
          sign,
          para["b"],
          " x ",
          "\\qquad",
          "R^2 = ",
          para["r2"],
          " \\)",
          sep = "   ")
  eq
}


####################
# WordCloud
####################
getTermMatrix <- function(inp,data.dir,path,courseDBName,con) {
  # print(as.Date(file.info(list.files(path=path, pattern=paste0(isolate(USER$courseDBName),"_allD.txt"), full.names=TRUE))$mtime))
  # if(Sys.Date()-as.Date(file.info(list.files(path=path, pattern=paste0(courseDBName,"_allD.txt"), full.names=TRUE))$mtime) > 30)
  # writewordFreq(data.dir,con)
  dbGetQuery(conn = con, "SET NAMES 'utf8'")
  # data explorer tab
  context <- ""
  if(inp == "D"){
    context <- as.character(read.table(data.dir["D"], fileEncoding = "utf8",blank.lines.skip=TRUE)[1,1])
    DT.context <- dbGetQuery(conn = con, "(SELECT b.discussion_question_title, b.discussion_question_details, a.`discussion_answer_content` FROM `discussion_answer` a left join `discussion_question` b on a.`discussion_question_id` = b.`discussion_question_id`) UNION (SELECT b.discussion_question_title, b.discussion_question_details, a.`discussion_answer_content` FROM `discussion_answer` a right join `discussion_question` b on a.`discussion_question_id` = b.`discussion_question_id`)")
    # context <- dbGetQuery(conn = con,"(SELECT `discussion_answer_content` FROM `discussion_answer`) UNION (SELECT `discussion_question_title` FROM `discussion_question`) UNION (SELECT `discussion_question_details` FROM `discussion_question`)")
    # DC <- func.DC()$DC
    # DC.Q <-DC[DC$Type == "Question",]
    # top.Q <- head(DC.Q[order(-DC.Q$Number),]$Question_id,30)
    # context <- paste(DC[DC$Question_id %in% top.Q,9],collapse=" ")
    # context <- ifelse(nchar(context)>1500,substr(context,1,1500),context)
  }else if(inp == "F"){
    context <- as.character(read.table(data.dir["F"], fileEncoding = "utf8",blank.lines.skip=TRUE)[1,1])
    DT.context <- dbGetQuery(conn = con,"(SELECT b.`course_item_name` as `Name`, a.`feedback_category`, a.`feedback_text` FROM `feedback_item_comments` a, `course_items` b where a.`course_item_id` = b.`course_item_id`) UNION (SELECT b.`course_name` as `Name`, a.`feedback_category`, a.`feedback_text` FROM `feedback_course_comments` a, `courses` b where a.`course_id` = b.`course_id`)")
    #context <- dbGetQuery(conn = con,"(SELECT `feedback_text` FROM `feedback_item_comments`) UNION (SELECT `feedback_text` FROM `feedback_course_comments`)")
  }else if(inp == "A"){
  	context <- ""
  	context <- paste(try(as.character(read.table(data.dir["D"], fileEncoding = "utf8",blank.lines.skip=TRUE)[1,1])),context)
  	context <- paste(try(as.character(read.table(data.dir["F"], fileEncoding = "utf8",blank.lines.skip=TRUE)[1,1]),context))
  	context <- paste(try(as.character(read.table(data.dir["O"], fileEncoding = "utf8",blank.lines.skip=TRUE)[1,1]),context))

    DT.context <- dbGetQuery(conn = con, "(SELECT b.discussion_question_title, b.discussion_question_details, a.`discussion_answer_content` FROM `discussion_answer` a left join `discussion_question` b on a.`discussion_question_id` = b.`discussion_question_id`) UNION (SELECT b.discussion_question_title, b.discussion_question_details, a.`discussion_answer_content` FROM `discussion_answer` a right join `discussion_question` b on a.`discussion_question_id` = b.`discussion_question_id`) UNION (SELECT b.`course_item_name` as `Name`, a.`feedback_category`, a.`feedback_text` FROM `feedback_item_comments` a, `course_items` b where a.`course_item_id` = b.`course_item_id`) UNION (SELECT b.`course_name` as `Name`, a.`feedback_category`, a.`feedback_text` FROM `feedback_course_comments` a, `courses` b where a.`course_id` = b.`course_id`)")
    # context <- dbGetQuery(conn = con,"(SELECT `discussion_answer_content` FROM `discussion_answer`) UNION
    # (SELECT `discussion_question_title` FROM `discussion_question`) UNION (SELECT `discussion_question_details` FROM `discussion_question`) UNION
    # (SELECT `feedback_text` FROM `feedback_item_comments`) UNION (SELECT `feedback_text` FROM `feedback_course_comments`)")
  }else if(inp == "Others"){
    #context <- dbGetQuery(conn = con, paste0("(SELECT `discussion_answer_content` FROM `discussion_answer` where `discussion_question_id`in ( select `discussion_question_id` from `discussion_question` where LENGTH(`course_item_id`)= 0 and LENGTH(`course_module_id`)=0) ) UNION (SELECT `discussion_question_title` FROM `discussion_question` where LENGTH(`course_item_id`)= 0 and LENGTH(`course_module_id`)=0) UNION (SELECT `discussion_question_details` FROM `discussion_question` where LENGTH(`course_item_id`)= 0 and LENGTH(`course_module_id`)=0)"))
    context <- as.character(read.table(data.dir["O"], fileEncoding = "utf8")[1,1])
    DT.context <- dbGetQuery(conn = con, paste0("(SELECT b.discussion_question_title, b.discussion_question_details, a.`discussion_answer_content` FROM `discussion_answer` a left join `discussion_question` b on a.`discussion_question_id` = b.`discussion_question_id` where a.`discussion_question_id`in ( select `discussion_question_id` from `discussion_question` where LENGTH(`course_item_id`)= 0 and LENGTH(`course_module_id`)=0)) UNION (SELECT b.discussion_question_title, b.discussion_question_details, a.`discussion_answer_content` FROM `discussion_answer` a right join `discussion_question` b on a.`discussion_question_id` = b.`discussion_question_id` where a.`discussion_question_id`in ( select `discussion_question_id` from `discussion_question` where LENGTH(`course_item_id`)= 0 and LENGTH(`course_module_id`)=0))"))
  }
  else {
    context <- as.character(read.table(paste0(path,courseDBName,"_",inp,".txt"), fileEncoding = "utf8")[1,1])
    DT.context <- dbGetQuery(conn = con, paste0("(SELECT b.discussion_question_title, b.discussion_question_details, a.`discussion_answer_content` FROM `discussion_answer` a left join `discussion_question` b on a.`discussion_question_id` = b.`discussion_question_id` where a.`discussion_question_id`in ( select `discussion_question_id` from `discussion_question` where `course_item_id`= '", inp,"' or `course_module_id`='", inp,"')) UNION (SELECT b.discussion_question_title, b.discussion_question_details, a.`discussion_answer_content` FROM `discussion_answer` a right join `discussion_question` b on a.`discussion_question_id` = b.`discussion_question_id` where a.`discussion_question_id`in ( select `discussion_question_id` from `discussion_question` where `course_item_id`= '", inp,"' or `course_module_id`='", inp,"'))"))
    #context <- dbGetQuery(conn = con, paste0("(SELECT `discussion_answer_content` FROM `discussion_answer` where `discussion_question_id`in ( select `discussion_question_id` from `discussion_question` where `course_item_id`= '", inp,"' or `course_module_id`='", inp,"') ) UNION (SELECT `discussion_question_title` FROM `discussion_question` where `course_item_id`= '", inp,"' or `course_module_id`='", inp,"') UNION (SELECT `discussion_question_details` FROM `discussion_question` where `course_item_id`= '", inp,"' or `course_module_id`='", inp,"')"))
  }
  
  if(nchar(context)>3){
    DT.context[,2] <- gsub("(<co-content>|<text>|<text/>|</text>|</co-content>|\\n)", "", DT.context[,2])
    DT.context[,3] <- gsub("(<co-content>|<text>|<text/>|</text>|</co-content>|\\n)", "", DT.context[,3])
    if(inp != "D" && inp != "F")
      colnames(DT.context) <- c("Title","Discussion Question / Feedback Category","Discussion Answer <br> /Feedback Content")
      return(list("textFreq"=calwordFreq(context, F), "DT.context"=DT.context))
  }else{-1}
}


calwordFreq <- function(context, mode){
  if(mode)
    context <- gsub("(<co-content>|<text>|<text/>|</text>|</co-content>|\\n|http\\S+\\s*|<(.|\n)*?>)", "", paste(context,collapse=" "))
  #print(length(context))
  text <- segmentCN(context)
  myCorpus <- Corpus(VectorSource(text))
  if(mode){
    myCorpus <- tm_map(myCorpus, removeWords, c(stopwords("SMART"), "character", "thy", "very", "thou", "thee", "the", "and", "but", "very", "thought", "wondering", "www","http","https" ))
    myCorpus <- tm_map(myCorpus, removeWords, c(stopwordsCN(), "一","非常","很","為甚麼","為什麼","為什麼","所以","一個","反而","www","http","https","一些","一句","的","一來","不好意思","問題","請問","答案","老師","怎麼","一下","一次","應該","好像","的話","影響","那個","以前","希望","知道","課程","這個","老師","助教","作業","應該","的話","因爲","可能","什麼"))
  }
  myDTM <- TermDocumentMatrix(myCorpus, control = list(wordLengths=c(2,Inf), tolower = TRUE, removePunctuation = TRUE, removeNumbers = TRUE))
  m <- sort(rowSums(as.matrix(myDTM)), decreasing = TRUE)
  if(mode){
    names(m) <- gsub("\n","",names(m))
    context <- paste(rep(names(m[nchar(names(m))>1]),m[nchar(names(m))>1]), collapse = " ")
    return(context)
  }else{
    return(m)
  }
}


#writewordFreq <- function(data.dir,con, modulesList){
  # Discussion
#  allcontext.D <- ""
#  for(inp in modulesList){
#    context.D <- dbGetQuery(conn = con, paste0("(SELECT `discussion_answer_content` FROM `discussion_answer` where `discussion_question_id`in ( select `discussion_question_id` from `discussion_question` where `course_item_id`= '", inp,"' or `course_module_id`='", inp,"') ) UNION (SELECT `discussion_question_title` FROM `discussion_question` where `course_item_id`= '", inp,"' or `course_module_id`='", inp,"') UNION (SELECT `discussion_question_details` FROM `discussion_question` where `course_item_id`= '", inp,"' or `course_module_id`='", inp,"')"))
#    if(nchar(context.D)>2){
#      currcontext.D <- calwordFreq(context.D,T)
#      allcontext.D <- paste(allcontext.D, currcontext.D)
#      write.table(currcontext.D, paste0(path,courseDBName,"_",inp,".txt"), fileEncoding = "utf8", row.names = F, col.names = F)
#    }
#a  }
 # write.table(allcontext.D, data.dir["D"], fileEncoding = "utf8", row.names = F, col.names = F)
  
  # Feedback
 # context.F <- dbGetQuery(conn = con,"(SELECT `feedback_text` FROM `feedback_item_comments`) UNION (SELECT `feedback_text` FROM `feedback_course_comments`)")
 # if(nchar(context.F)>2)
 #   write.table(calwordFreq(context.F,T), data.dir["F"], fileEncoding = "utf8", row.names = F, col.names = F)
  
  # Others
 # context.O <- dbGetQuery(conn = con, paste0("(SELECT `discussion_answer_content` FROM `discussion_answer` where `discussion_question_id`in ( select `discussion_question_id` from `discussion_question` where LENGTH(`course_item_id`)= 0 and LENGTH(`course_module_id`)=0) ) UNION (SELECT `discussion_question_title` FROM `discussion_question` where LENGTH(`course_item_id`)= 0 and LENGTH(`course_module_id`)=0) UNION (SELECT `discussion_question_details` FROM `discussion_question` where LENGTH(`course_item_id`)= 0 and LENGTH(`course_module_id`)=0)"))
 # if(nchar(context.O)>2)
 #   write.table(calwordFreq(context.O,T), data.dir["O"], fileEncoding = "utf8", row.names = F, col.names = F)
  
 # dbDisconnect(con)
#}

##########
# UI LOGIN
##########
UILogin <- function() {
	tagList(
		tags$head(
			tags$style(type = "text/css", "#foot{text-align:center; position:relative; bottom:5px;} #login {width:300px; font-size:14px; text-align:center; position:absolute; top:35%; left:50%; margin-top:-100px; margin-left:-150px;}<style>body{font-size:20px;font-family:微軟正黑體,Microsoft JhengHei;}h1,h2,h3,h4,h5,h6{font-family:微軟正黑體,Microsoft JhengHei;}</style>")
		),
	  div(
	    id = "login",
	    wellPanel(
	      h3("NTU MOOCs 臺大慕課", align = "center"),
	     	# h4("Data Analytics Platform", align = "center"),
	      br(),br(),
	      textInput("userName", "Username"),
	      passwordInput("passwd", "Password"),
	      br(),
	      html(html="<center>"),
	      actionButton("Login", "Login"),
	      hr(),
	      HTML("<h6>還沒有帳號嗎?<a href='http://140.112.107.63:8000/accounts/signup' target='_blank'> 點我註冊帳號</a></h6>"),
	      HTML("<h6><a href='http://140.112.107.63:8000/accounts/login' target='_blank'>Teacher Management System 教師管理系統</a></h6>"),
	      #h6("NTU MOOCs @ 2017 / 建議使用 Chrome 瀏覽器"),
	      h6("NTU MOOCs @ 2017"),
	      html(html="</center>")
	    )
	  )#,
	  #div(
	  #   id="foot",
	  #   html(html="<center>"),
	  #   h6("NTU MOOCs @ 2017"),
	  #   h6("Powered By ShinyR"),
	  #   html(html="</center>")
	  #),
	)
}

##########
# UI LIST
##########
UIList <- function(user, rows) {
	if(is.null(user)) user <- ""
	tagList(
	  tags$head(
			tags$style(type = "text/css", ".action-button{word-break: break-all; min-width:320px;} #foot{text-align:center; position:relative; bottom:5px;} #weblist {text-align:center;margin-top:30px;} hr{background-color:darkgrey; color:darkgrey; height:2px;}<style>body{font-size:20px;font-family:微軟正黑體,Microsoft JhengHei;}h1,h2,h3,h4,h5,h6{font-family:微軟正黑體,Microsoft JhengHei;}</style>"), 
			tags$script('Shiny.addCustomMessageHandler("myCallbackHandler_uiList", function(typeMessage) {alert(typeMessage);})')
		),    
		div(
			id = "weblist",
			h2("NTU MOOCs 臺大慕課", align = "center"),hr(),
			#wellPanel(
			  # h4("Data Analytics Platform", align = "center"),
			  br(),br(),
			  #h4(paste0("Hello ", user, "!  Here is your NTU MOOC(s)"), align = "center"),hr(), br(),
			  h4(paste0(user, " 教授您好！ 以下是您的 NTU MOOCs 臺大慕課："), align = "center"), br(),
			  #box(status="primary",solidHeader = FALSE, height="100%",
			  rows
			  #)
	  	    #)
		),
		div(
			id="foot",
			html(html="<center>"),
			h6("NTU MOOCs @ 2017"),
			#h6("Powered By ShinyR"),
			html(html="</center>")
			
		)
	)
}



#################
# Run inner Env.
#################
RunEnvironment <- function(session,input,output,user,selectionlist,rows){
	output$page <- renderUI({
		div(class = "outer", dashboardPage(
		    dashboardHeader(
			    title="臺大慕課 NTU MOOCs"
			    #titleWidth = 230
			 ),
			 dashboardSidebar(
			    sidebarMenu(
			      #menuItem("Dashboard", icon = icon("dashboard"), tabName = "dashboard"),
			      br(),
			      menuItem("關於", icon = icon("commenting"), tabName = "about",
			      	# menuSubItem("Platform", tabName = "a_platform"),
			      	# menuSubItem("Overview [總覽)", tabName = "a_overview"),
			      	# menuSubItem("Participation [參與]",  tabName = "a_participation"),
			      	# menuSubItem("Engagement [成績]",  tabName = "a_engagement"),
			      	# menuSubItem("Discussion [論壇]",  tabName = "a_discussion"),
			      	# menuSubItem("Word Cloud [文字雲]",  tabName = "a_wordcloud"),
			      	# menuSubItem("Contact us [聯絡]", tabName = "a_contact"),
			      	# menuSubItem("About us [關於]",  tabName = "a_us"),
			      	# menuSubItem("FAQ [問答]",  tabName = "a_faq")
			      	menuSubItem("關於平臺", tabName = "a_platform"),
			      	menuSubItem("關於總覽", tabName = "a_overview"),
			      	menuSubItem("關於參與",  tabName = "a_participation"),
			      	menuSubItem("關於成績",  tabName = "a_engagement"),
			      	menuSubItem("關於論壇",  tabName = "a_discussion"),
			      	menuSubItem("關於文字雲",  tabName = "a_wordcloud"),
			      	menuSubItem("聯絡我們", tabName = "a_contact"),
			      	menuSubItem("關於我們",  tabName = "a_us"),
			      	menuSubItem("常見問答",  tabName = "a_faq")
			      ),
			   #    menuItem("Overview", icon = icon("dashboard"), badgeLabel = "總覽", badgeColor = "black",tabName = "overview"),
			   #    menuItem("Participation", icon = icon("area-chart"), badgeLabel = "參與", badgeColor = "black",tabName = "participation"),
			   #    menuItem("Engagement", icon = icon("pencil"), badgeLabel = "成績", badgeColor = "black",tabName = "engagement"),
			   #    menuItem("Discussion", icon = icon("tasks"), badgeLabel = "論壇", badgeColor = "black",tabName = "discussion"),
			   #    menuItem("Word Cloud", icon = icon("cloud"), badgeLabel = "文字雲", badgeColor = "black", tabName = "wordcloud"),
			   #    menuItem("Your MOOCs", icon = icon("bookmark"), tabName = "moocs"),
				  # menuItem("Teacher System",href="http://140.112.107.63:8000",badgeLabel = "link", badgeColor = "green", icon=icon("user")),hr(),
				  # menuItem("Logout", icon=icon("sign-out"))
				  menuItem("總覽", icon = icon("dashboard"), tabName = "overview"
				  	#,
				  	# menuSubItem("總覽", tabName = "overview"),
				  	# menuSubItem("基本資料", tabName = "overview_basic"),
				  	# menuSubItem("參與比較", tabName = "overview_participation"),
				  	# menuSubItem("成績比較", tabName = "overview_grade"),
			    	# menuSubItem("學生背景", tabName = "overview_background"),
			    	# menuSubItem("問卷資料", tabName = "overview_questionaire")
			      ),
			      menuItem("參與", icon = icon("area-chart"), tabName = "participation"),
			      menuItem("成績", icon = icon("pencil"), tabName = "engagement"),
			      menuItem("論壇", icon = icon("tasks"), tabName = "discussion"),
			      menuItem("文字雲", icon = icon("cloud"), tabName = "wordcloud"),
			      menuItem("Your MOOCs", icon = icon("bookmark"), tabName = "moocs"),
				  menuItem("Teacher System",href="http://140.112.107.63:8000",badgeLabel = "link", badgeColor = "green", icon=icon("user")),hr(),
				  menuItem("登出", icon=icon("sign-out"))
				),conditionalPanel(
				      condition = "false",
				      selectInput("load","load",list())
				)
			  ), 
			dashboardBody(
				HTML("<script src='AdminLTE-2.0.6/app.js'></script>"),
				tags$script(HTML('
					$("body").addClass("skin-blue"); 
					$(".sidebar-toggle").click(function(){
						if($("body").hasClass("sidebar-collapse")){$("body").removeClass("sidebar-collapse")}
						else{$("body").addClass("sidebar-collapse")}  
						
						if($("body").hasClass("sidebar-open")){$("body").removeClass("sidebar-open")}
						else{$("body").addClass("sidebar-open")}  

						}); 
					$(".fa-angle-left").toggleClass("fa-angle-down"); $(".content").css("min-height","900px"); 
					// setTimeout(function(){$("section > ul > li:nth-child(2) > a").click();},700); // about menusubitem
					$("section > ul > li:nth-child(2) > ul > li:nth-child(1) > a").click(); 
					$("section > ul > li:nth-last-child(1) > a").click(function(){alert(\'Logout!\');location.reload(true);});


			        $(document).ready(function(){$("#course_o").on("DOMSubtreeModified propertychange", function() {
		               			var ua = window.navigator.userAgent;
    							var msie = ua.includes("MSIE ");
    							var trident = ua.includes("Trident/");
    							var edge = ua.includes("Edge/");
    							if(msie || trident || edge){   
    								// If Internet Explorer
    								setTimeout(function(){
						               $("#busy_on").css({"visibility":"hidden"});
					             	}, 1000);
				              	}else{
						            $("#busy_on").css({"visibility":"hidden"});
						        }
			               });
		             });


					// For overview menuSubItem
					$(\'a[href$="#shiny-tab-overview_all"]\').click(function(){
						$(\'a[href$="shiny-tab-overview"]\').click();
					});

					$(\'a[href$="#shiny-tab-overview"]\').click(function(){
						$("body").animate({scrollTop:$("#course_o").offset().top},200);
					});
					$(\'a[href$="#shiny-tab-overview_basic"]\').click(function(){
						$(\'a[href$="shiny-tab-overview"]\').click();
						$("body").animate({scrollTop:0},200);
					});
					$(\'a[href$="#shiny-tab-overview_participation"]\').click(function(){
						$(\'a[href$="shiny-tab-overview"]\').click();
						$("body").animate({scrollTop:$("#course_o").offset().top},200);
					});
					$(\'a[href$="#shiny-tab-overview_grade"]\').click(function(){
						$(\'a[href$="shiny-tab-overview"]\').click();
						$("body").animate({scrollTop:$("#course_o").offset().top},200);
					});
					$(\'a[href$="#shiny-tab-overview_background"]\').click(function(){
						$(\'a[href$="shiny-tab-overview"]\').click();
						$("body").animate({scrollTop:$("#ql > div").offset().top},200);
					});
					$(\'a[href$="#shiny-tab-overview_questionaire"]\').click(function(){
						$(\'a[href$="shiny-tab-overview"]\').click();
						$("body").animate({scrollTop:$("#ql_corr > div").offset().top},200);
					});




					')),
				tags$style(HTML('hr{background-color:darkgrey; color:darkgrey; height:1px;} 
					li > a > span{margin-left:5px;} 
					.sidebar-toggle{height:50px;} 
					.main-header .logo {font-size:14px; /*margin-left:-30px; width:290px;*/ } 
					@media screen and (min-width:800px){.logo{position:fixed;}}
					.main-sidebar{font-size:14px;width:230px;position:fixed;} 
					.main-header > .navbar{height:50px;}')),
				  
				       UIWeb(selectionlist)
				  
		    )
		))
	})

	output$inCourseList <- renderUI({
		div(class = "outer",  do.call(bootstrapPage, UIList(user, rows)))
	})
}

##################################################		###  ###  ######
##################################################		###  ###  ######
##################################################		###  ###	##
# UI Web 												###  ###	##
##################################################		###  ###	##
##################################################		########  ######
##################################################		########  ######

UIWeb <- function(selectionlist){
# Module Dropdown

  dlist <-  unique(selectionlist[order(selectionlist$course_module_order), c(2, 4)])
  modulesList <- setNames(dlist$course_module_id, dlist$course_module_name)
#modulesList <- ""

  return(tabItems(
  	tabItem(tabName = "a_platform",
  		shinyUI(fluidPage(HTML("<style>body{font-size:20px;font-family:微軟正黑體,Microsoft JhengHei;}h1,h2,h3,h4,h5,h6{font-family:微軟正黑體,Microsoft JhengHei;}</style>"),column(1), column(10, includeMarkdown("about/aboutPlatform.md"))))),
  	tabItem(tabName = "a_overview",
  		shinyUI(fluidPage(column(1), column(10, includeMarkdown("about/aboutOverview.md"))))),
  	tabItem(tabName = "a_participation",
  		shinyUI(fluidPage(column(1), column(10, includeMarkdown("about/aboutParticipation.md"))))),
  	tabItem(tabName = "a_engagement",
  		shinyUI(fluidPage(column(1), column(10, includeMarkdown("about/aboutEngagement.md"))))),
  	tabItem(tabName = "a_discussion",
  		shinyUI(fluidPage(column(1), column(10, includeMarkdown("about/aboutDiscussion.md"))))),
  	tabItem(tabName = "a_wordcloud",
  		shinyUI(fluidPage(column(1), column(10, includeMarkdown("about/aboutWordcloud.md"))))),
  	tabItem(tabName = "a_contact",
  		shinyUI(fluidPage(
  			column(12, align="center",
                       titlePanel(h1(
                         "Contact us 聯絡我們", align = "center"
                       )),
                       hr(), tags$head(tags$style("#contactBody{margin-left:-150px;}")),
                       #HTML("<h4><i>We would like to hear your voice. Please let us know where and what to improve!</i></h4> <br><br>"),
                       br(),br(),
                       selectInput("contactSection", "我的留言是關於：", width = "600px", choices = c("NTU MOOCs Platform (平臺)", "Overview (總覽)", "Participation (參與)", "Engagement (成績)", "Discussion (論壇)", "Wordcloud (文字雲)", "Others (其他)")),
                       textAreaInput("contactBody", "", "", placeholder="您的回饋是我們進步的動力! 
We would like to hear your voice. Please let us know what to improve!", width = "600px", height="250px"),
                       actionButton("btnContactUs","提交", width = "595px", onclick="alert('謝謝您的寶貴的建議！Thanks for your valuable comment!'); setTimeout(function(){$('#contactBody').val('');},1000)")
             )
  	 	))),
  	tabItem(tabName = "a_us",
  		shinyUI(fluidPage(column(1), column(10, includeMarkdown("about/aboutUs.md"))))),
  	tabItem(tabName = "a_faq",
  		shinyUI(fluidPage(column(1), column(10, includeMarkdown("about/aboutFAQ.md"))))),

    tabItem(tabName = "overview",
             shinyUI(
               # Use a fluid Bootstrap layout
               fluidPage(
                 tags$head(tags$style( 
                   type = "text/css",HTML(
                   "//.box-body{text-align:center;}
                    //busy_on {visibility: hidden;}
                   .container {padding-left: 10px !important; margin-left: 0px !important;}
                   .navbar-nav {margin-left: 20px !important;}
                   .help-block{font-size:16px;}
                   .progress-text {position:absolute; top: 70px !important; width:auto !important;}
                   //#ql_corr .js-plotly-plot{border-top: 3px solid #3c8dbc; padding-top: 40px; margin-top: 80px;}
                   #ql_corr > .col-sm-2 > div {height:400px;}
                   #Q\\.LearnM\\.o .main-svg {height:540px !important;} 
                   #Q\\.Background\\.o .main-svg {height:540px !important;} 
                   .shiny-output-error {font-weight: 500; font-size: 16px; color: red;}
                   .shiny-output-error { visibility: hidden; }
                   .shiny-output-error:before { visibility: hidden; }
                   #text\\.question\\.title\\.DC{height:350px;overflow-y:auto;font-size:16px;}
                   "
                 )),tags$script(HTML(
			      'Shiny.addCustomMessageHandler("myCallbackHandler_alert",
			        function(m){	
			        	alert(m);
			        });
			       Shiny.addCustomMessageHandler("myCallbackHandler_hide",
			        function(path){
			        		$(path).hide()
			        });
			       Shiny.addCustomMessageHandler("myCallbackHandler_show",
			        function(path){
			        		$(path).show()
			        });
			       Shiny.addCustomMessageHandler("myCallbackHandler_visible",
			        function(path){
			        		$(path).css("visibility","visible");
			        });
			        Shiny.addCustomMessageHandler("myCallbackHandler_sameCourse",
			          function(m){
			          	$(\'a[href$="shiny-tab-a_platform"]\').click();
			          	//alert("overview~"+m);
			        });
                   Shiny.addCustomMessageHandler("myCallbackHandler_diffCourse",
		               function(typeMessage) {
			         	   //alert(typeMessage);
			               // $("a:contains(OVERVIEW)").click();
			               $("section > ul > li:nth-child(3) > a").click();
			               // document.getElementById("course_o").style.visibility = "hidden";
			               // document.getElementById("content_o").style.visibility = "hidden";
			               // document.getElementById("busy_on").style.visibility = "visible";
			               $("#busy_on").css({"visibility":"visible"});
			               $("#course_o").css({"visibility":"hidden"});
			               $("#content_o").css({"visibility":"hidden"});
			               
			               $("#content_o").on("DOMSubtreeModified propertychange", function() {
				               // document.getElementById("course_o").style.visibility = "visible";
				               // document.getElementById("content_o").style.visibility = "visible";
				               // document.getElementById("busy_on").style.visibility = "hidden";
				              	
				              	var ua = window.navigator.userAgent;
    							var msie = ua.includes("MSIE ");
    							var trident = ua.includes("Trident/");
    							var edge = ua.includes("Edge/");
    							if(msie || trident || edge){   
    								// If Internet Explorer
    								setTimeout(function(){
						               $("#course_o").css({"visibility":"visible"});
						               $("#content_o").css({"visibility":"visible"});
						               $("#busy_on").css({"visibility":"hidden"});
						               $("#busy_o").css({"visibility":"hidden"});
					             	}, 1000);
				              	}else{
				              		$("#course_o").css({"visibility":"visible"});
						            $("#content_o").css({"visibility":"visible"});
						            $("#busy_on").css({"visibility":"hidden"});
						            $("#busy_o").css({"visibility":"hidden"});
						        }
			               })
			        });'))),
       # Give the page a title
       titlePanel(h3(textOutput("course_o"), align = "center")),
       br(),
       # Generate a row with a sidebar

       # Create a spot for the barplot
       #mainPanel(
         div(id = "busy_on",
             p("Loading New Web Content...Please wait...", align = "center")),
         div(id = "busy_o",
             p("Loading Web Content...Please wait...", align = "center")),
         div(id = "content_o",
             	box(title="資料範圍",status="primary",solidHeader = TRUE,
                     #h4(strong(textOutput("DR.o")), align = "left"), br(),
                     h5(textOutput("sday.o"), align = "left"),
                     h5(textOutput("eday.o"), align = "left"),
                     width=3,height=130),
                     #br(),hr(),
                 box(title="參與狀況",status="primary",solidHeader = TRUE,
                     #h4(strong(textOutput("SC.o")), align = "left"), br(),
                     h5(textOutput("learner.o"), align = "left"),
                     h5(textOutput("activelearner.o"), align = "left"),
                     width=3,height=130),
                     #br(),hr()
                box(title="論壇回饋",status="primary",solidHeader = TRUE,
                    #h4(strong(textOutput("DC.o")), align = "left"), br(),
                    h5(textOutput("ques.ans.o"), align = "left"),
                    h5(textOutput("rating.o"), align = "left"),
                    #br(),hr(),
                    width=3,height=130),
                box(title="學習成績",status="primary",solidHeader = TRUE,
                    #h4(strong(textOutput("IG.o")), align = "left"), br(),
                    h5(textOutput("pass.o"), align = "left"),
                    h5(textOutput("nonpass.o"), align = "left"),
                    #br(),hr()
                    width=3,height=130) , #hr(),
	            box(title="通過項目 [verified passed：通過並取得證書] [passed：通過但未取得證書] [not passed：未通過]",status="primary",solidHeader = TRUE,
	             	plotlyOutput("plotO.pass_item"), br(),
	             	width=8),#hr(),
	       		box(title="評分回饋 [1-5分] [軸標籤代表人數]",status="primary",solidHeader = TRUE,
	               	plotOutput("plotO.rating"), br(),
	               	width=4),


				box(title="參與狀況比較",status="primary",solidHeader = TRUE,
	               	column(1,
	               		selectInput(
				            "section1.participation..o",
				             #"Modules",
				             "班次選擇：",
				             choices = c("全部"="All", "其他"="Others"),
				             selected = "All"
			            ),
			           helpText(HTML(
			             paste( "",
			                    "►選擇左圖參與狀況之班次",
			                   " ",
			                   sep = "<br>")
			        	))
			        ),
	               	column(5,
	               		plotlyOutput("plotO.participation1")
	               	),
	               	column(5,
	               		plotlyOutput("plotO.participation2")
	               	),
	               	column(1,
	               		selectInput(
				            "section2.participation.o",
				             #"Modules",
				             "班次選擇：",
				             choices = c("全部"="All", "其他"="Others"),
				             selected = "All"
			            ),
			           helpText(HTML(
			             paste( "",
			                    "►選擇右圖參與狀況之班次",
			                   " ",
			                   sep = "<br>")
			        	))
			        ),
	               	br(),
	               	width=12),


				box(title="成績比較",status="primary",solidHeader = TRUE,
	               	column(1,
	               		selectInput(
				            "section1.grade.o",
				             #"Modules",
				             "班次選擇：",
				             choices = c("全部"="All", "其他"="Others"),
				             selected = "All"
			            ),
			           helpText(HTML(
			             paste( "",
			                    "►選擇左圖成績之班次",
			                   " ",
			                   sep = "<br>")
			        	))
			        ),
	               	column(5,
	               		plotlyOutput("plotO.grade1")
	               	),
	               	column(5,
	               		plotlyOutput("plotO.grade2")
	               	),
	               	column(1,
	               		selectInput(
				            "section2.grade.o",
				             #"Modules",
				             "班次選擇：",
				             choices = c("全部"="All", "其他"="Others"),
				             selected = "All"
			            ),
			           helpText(HTML(
			             paste( "",
			                    "►選擇右圖成績之班次",
			                   " ",
			                   sep = "<br>")
			        	))
			        ),
	               	br(),
	               	width=12),


             div(id="ql",
             	 br(),br(),
	             box(title="學生背景",status="primary",solidHeader = TRUE,
             	     h3(strong(textOutput("Q.o")), align = "center"),br(),
		             fluidRow(column(6,
		                             plotlyOutput("Q.Gender.o", height = "400px"), br(), br(),
		                             plotlyOutput("Q.Education.o", height = "400px"), br(), br(),
		                             plotlyOutput("Q.Know.o", height = "400px"),br(), br()
		                             #hr()
				             ),
				             column(6,
				                    
				                    plotlyOutput("Q.Age.o", height = "400px"), br(), br(),
				                    plotlyOutput("Q.Employ.o", height = "400px"),br(), br(),
				                    plotlyOutput("Q.Time.o", height = "400px"),br(), br()
				                    #hr(),br()
				                    #plotlyOutput("Q.LearnT.o"),br(),
                     				#plotlyOutput("Q.schedule.o"),br(),
				             )),
		             fluidRow(column(12,
				             plotlyOutput("Q.Background.o"),br(), br(),
				             plotlyOutput("Q.LearnM.o"),br(), br())
		             ),
		             br(), br(), br(), br(),
                     width=12
                 ),
                 div(id="ql_corr",
	                 box(title="問卷題目 [1-5分] [每一列的左圖和右圖爲前測和後測的對應題目]",status="primary",solidHeader = TRUE,
	                     column(5, h3("Pre-test 前測問卷", align = "center"), h3(textOutput("Q.pre.o"), align = "center"),br(),
				            lapply(c(10:24), function(i) { 
	  							plotlyOutput(paste0('Q.score.pre.o', i))
	  						}),br(), br(), br(), br()
				           
				          ),
						 column(5, h3("Post-test 後測問卷", align = "center"), h3(textOutput("Q.post.o"), align = "center"),br(),
							lapply(c(1:21)[!c(1:21) %in% c(6,7,13:16)], function(i) { 
	  							plotlyOutput(paste0('Q.score.post.o', i))
	  						 }),br(), br(), br(), br()
				         ),column(2, h3("前後測檢定", align = "center"), br(),br(),
				            lapply(c(1:15), function(i) { 
	  							htmlOutput(paste0('Q.score.ttest.o', i), style="height:400px;")
	  						}),br(), br(), br(), br()),
			             dataTableOutput('Q.Num.o'),br(), br(), br(), 
		             width=12)
		         )
            )
       )
       ))),

  tabItem(tabName = "participation",
   # Define the overall UI
   shinyUI(
     # Use a fluid Bootstrap layout
     fluidPage(
       # Give the page a title
       titlePanel(h2("參與狀況（Participation）", HTML("<br><br>"), align = "center")),
       #hr(),
       # Generate a row with a sidebar
       box(title="項目選單",status="primary",solidHeader = TRUE,
           selectInput(
             "module.SC",
             #"Modules",
             "單元：",
             choices = c("全部"="All", modulesList, "其他"="Others"),
             selected = "All"
           ),
           selectInput(
             "item.SC",
             #"Items:",
             "項目：",
             choices = c(),
             selected = ""
           ),
           #hr(),
           helpText(HTML(
             paste( "",
                    "►「單元」乃每星期老師開課的主題",
                    "►「項目」乃每星期老師開課主題裡面的子項目",
					"►綠線為「開始」任一項目",
					"►紅線為「完成」任一項目",
					"►藉此圖表可以了解此課程在不同時間觀賞的人數",
					"►將滑鼠滑到圖表中的點，可以看到更多的細項",
                    "►W4 代表 Week4, etc.",
                   " ",
                   sep = "<br>")
           )),
		width=3, height = "550px"),

	    tags$head(
	        tags$style(type="text/css",
	           "#plotSC.Histogram{visibility:inherit !important;}
	           #plotSC.Scatterplot > .main-svg{height:421px !important;}
	           .selectize-dropdown-content{max-height:400px;}"
	        )
	      ),

         # Create a spot for the barplot

           h5(textOutput("msg.SC"), align = "center"),
           div(id = "busy.SC",
               p("Calculation in progress...", align = "center")),
           div(
             id = "sub.SC",
             box(title="課程項目參與人數（進入 & 離開）",status="primary",solidHeader = TRUE,
            	 plotlyOutput("plotSC.Histogram", height = "400px",width="100%"),
             width=9, height = "550px"),
             br(),
	         box(title="課程項目參與人數資料",status="primary",solidHeader = TRUE,width=12,
	             HTML("<br><br>"),
	             div(style="display: inline-block;vertical-align:top;width:220px;",downloadButton('downloadData.SC', '下載資料表')),
	             div(style="display: inline-block;vertical-align:top;width:600px;",radioButtons(
	               "radioBtn.SC",
	               "Data Encoding:",
	               c("UTF-8 (For English-based operation System)" = "UTF-8", "BIG 5 (For Chinese-based operation System)" = "BIG5")
	             )),
	             HTML("<br><br>"),
             	DT::dataTableOutput("DT.SC")) 
           )
         )
     )),
  tabItem(tabName = "engagement",
   shinyUI(fluidPage(
     fluidRow(
       titlePanel(h2("學習成績（Engagement）",HTML("<br><br>"), align = "center")),
       #hr(),
      box(title="項目選單",status="primary",solidHeader = TRUE, width=3,height="570px",
           selectInput(
             "module.IG",
             #"Modules",
             "單元：",
             choices = c("全部"="All", modulesList),
             selected = "All"
           ),
           selectInput(
             "item.IG",
             #"Items:",
             "項目：",
             choices = c(),
             selected = ""
           ),
           #hr(),
           helpText(HTML(paste(
             "",
             "►Verified: 通過並取得證書",
             "►Passed: 通過未取得證書",
             "►Not passed: 未通過",
             "►趨勢線表現資料的線性走勢，判別變數（學習時間）與成績不通過（橘色）、通過（藍色）、認證通過（綠色）之間的關聯性是正相關（正斜率）或是負相關（負斜率）",
             " ",
             sep = "<br>"
           ))),
           tags$head(
             tags$style(
               "
               #loadmessage {
               position: fixed;
               top: 0px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: #CCFF66;
               z-index: 105;
               }
               "
         ))),
         #mainPanel(scatterD3Output("gdt1"))
         
           withMathJax(),
           h5(textOutput("msg.IG"), align = "center"),
          
           div(
             id = "sub.IG",
             box(title="學習時間與成績",status="primary",solidHeader = TRUE, div(id = "busy.IG",
               p("Calculation in progress...", align = "center")),
	             plotlyOutput("plotIG.Scatterplot", height ="350px",width="auto"),
             #HTML("<hr>"),
             checkboxInput('txt.IG_visible', width="900px", 'Trendline 點我看趨勢線', value = FALSE),
             uiOutput("txt.IG"),
	         width=9, height="570px"),

             #HTML("<hr>"),
             box(title="學習時間與通過狀態",status="primary",solidHeader = TRUE,
             	plotlyOutput("plotIG.Boxplot.Q", height ="350px",width="100%"),
             	width=12, height="100%"),
             #HTML("<hr><br><br>"),

             box(title="國籍與成績",status="primary",solidHeader = TRUE,
	             sliderInput(
	               "slider.sd.D.IG",
	               "標準差門檻：", #"Standard Deviation Filter:",
	               min = 0,
	               max = 1,
	               value = 0.3,
	               step = 0.01
	             ),
	             plotlyOutput("plotIG.Boxplot.D", height ="400px",width="100%"), #, width="780px"
	         width=12, height="100%"),
             
             #HTML("<hr><br><br>"),

             box(title="語言與成績",status="primary",solidHeader = TRUE,
	             sliderInput(
	               "slider.sd.L.IG",
	               "標準差門檻：", #"Standard Deviation Filter:",
	               min = 0,
	               max = 1,
	               value = 0.3,
	               step = 0.01
	             ),
	             plotlyOutput("plotIG.Boxplot.L", height ="400px",width="100%"),
	         width=12, height="100%"),  
             #, width="780px"
             #HTML("<br><br><br><hr>"),

             box(title="學習時間與成績資料",status="primary",solidHeader = TRUE,width=12,
	             br(),
	             	div(style="display: inline-block;vertical-align:top;width:220px;", downloadButton('downloadData.IG', '下載資料表')),
	             div(style="display: inline-block;vertical-align:top;width:600px;", radioButtons(
	               "radioBtn.IG",
	               "Data Encoding:",
	               c("UTF-8 (For English-based operation System)" = "UTF-8", "BIG 5 (For Chinese-based operation System)" = "BIG5")
	             )),
	             HTML("<br><br>"),
	             DT::dataTableOutput('DT.IG'))
       ))
         ))),

  tabItem(tabName = "discussion",
   shinyUI(fluidPage(
     fluidRow(
     	 tags$style(type="text/css","#plotDC.Scatterplot > .main-svg{height:370px !important;}"),
       titlePanel(h2("論壇回饋（Discussion）",HTML("<br><br>"), align = "center")),
       	 #hr(),  
         #mainPanel(scatterD3Output("gdt1"))
           withMathJax(),
           h5(textOutput("msg.DC"), align = "center"),
           div(
             id = "sub.DC",
             box(title="問答時序與熱絡度",status="primary",solidHeader = TRUE,
	             div(id = "busy.DC",
	                p("Calculation in progress...", align = "center")),
	             	column(3,#h4("課程項目選單"),
	             	   selectInput(
			             "module.DC",
			             #"Modules",
			             "單元：",
			             choices = c("全部"="All", modulesList, "其他"="Others"),
			             selected = "All"
			           ),
			           selectInput("item.DC",
			                       #"Items",
			                       "項目：",
			                       choices = c(),
			                       selected = ""),
			           checkboxInput('plot.DC_top30', '列出前30筆', TRUE),
			           #hr(),
			           helpText(HTML(paste(
			             "",
			             "►「+」代表問題，而「□」代表回答，兩者皆可對應到下面的的時間軸",
			             "►藉由看問題被回答了多少次（多少個「□」），可以了解問題在學生之間的共鳴性",
			             "►藉由看問題與回答之間的間隔，可以了解問題對於學生的急迫性與重要性",
			             "►縱軸代表著題目的題號，會與某題目一對一對應",
			             " ",
			             sep = "<br>"
			           )))),
	             	column(1),
	             	column(8,
						plotlyOutput("plotDC.Scatterplot", height ="400px",width="auto")
	             	),
	         	width = 12, height="550px"),

             HTML("<br>"),
             textOutput("summary.DC"),
             HTML("<br><hr>"),

             box(title="學生回答活躍貢獻度",status="primary",solidHeader = TRUE,
             		column(3, #h3("前幾名活躍學生人數"),
             			sliderInput(
				             "slider.max.activeStudents",
				             "前幾名活躍學生人數",
				             #"Number of Active Students",
				             min = 1,
				             max = 10,
				             value = 3
				           ),
				           helpText(HTML(paste( 
				           	 "",
				             "►學生 (User) 與 論壇問題 (Q) 的連線：代表該學生回答了該問題",
				             "►連線越粗：代表學生 (User) 回答該論壇問題 (Q) 次數越多,社群貢獻性越大",
				             "",
				             sep = "<br>"
				          ))),hr(),htmlOutput("text.question.title.DC")),
             		column(1),
             		column(8,
	             plotOutput("plotDC.Circlizeplot", height ="700px",width="700px")),
	          width = 12, height="760px"),
             HTML("<br><hr>"),
             

			 box(title="問答間隔標準差 [InterResponse: 回答與回答之間的天數間隔]",status="primary",solidHeader = TRUE,
	             sliderInput(
	               "slider.sd.DC",
	               "Standard Deviation Filter:",
	               min = 0,
	               max = 10,
	               value = 3,
	               step = 0.5
	             ),
	             plotlyOutput("plotDC.Scatterplot.Std", width ="85%"),
	             width = 12, height="100%"), 
	         box(title="原始內容",status="primary",solidHeader = TRUE,
	             br(),
	             div(style="display: inline-block;vertical-align:top;width:220px;", downloadButton('downloadData.DC', '下載資料表')),
	             div(style="display: inline-block;vertical-align:top;width:600px;",radioButtons(
	               "radioBtn.DC",
	               "Data Encoding:",
	               c("UTF-8 (For English-based operation System)" = "UTF-8", "BIG 5 (For Chineses-based operation System)" = "BIG5")
	             )),
	             HTML("<br><br>"),
	             DT::dataTableOutput('DT.DC'),
	             width = 12, height="100%")
           )
       )
     )
   )),

  tabItem(tabName = "wordcloud",
   shinyUI(fluidPage(
     fluidRow(
       titlePanel(h2(
         "文字雲（Word Cloud）", HTML("<br><br>"), align = "center"
       )),
       #hr(),
       # box(title="課程項目選單",status="primary",solidHeader = TRUE,
       #     useShinyjs(),
       #     selectInput(
       #       "module.WC",
       #       "Modules",
       #       choices = c(
       #         "All" = "A",
       #         "All Feedback" = "F",
       #         "All Discussion" = "D",
       #         modulesList,
       #         "Others"
       #       ),
       #       selected = "A"
       #     ),
       #     # selectInput("item.WC",
       #     #             "Items",
       #     #             choices = c()),
       #     #hr(),
       #     # sliderInput("freq",
       #     #             "Minimum Frequency:",
       #     #             min = 1,  max = 50, value = 15),
       #     sliderInput(
       #       "slider.max.WC",
       #       "Maximum Number of Words:",
       #       min = 1,
       #       max = 100,
       #       value = 30
       #     ),
       #     hr(),
       #     # actionButton("recal", "Recal"),
       #     # actionButton("clean", "Clean"),
       #     br(),
       #     helpText(HTML(paste(
       #       #"Source: NTU MOOCs",
       #       #"*Processing time of the wordcloud : 3~5 sec.",
       #        "*產生文字雲約需 3~5 秒",
       #        "",
       #        "*文字大小代表出現頻率",
       #       sep = "<br>"
       #     ))),
       #     tags$head(
       #       tags$style("#plotWC{height:70vh !important;}.actionButton{min-width:100% !important;}"),
       #        tags$script(
       #          HTML('Shiny.addCustomMessageHandler("myCallbackHandler_recal",
       #               function(message) {
       #               Shiny.onInputChange("deleteConfirmChoice_recal",eval(message.value));});
       #               Shiny.addCustomMessageHandler("myCallbackHandler_clean",
       #               function(message) {
       #               Shiny.onInputChange("deleteConfirmChoice_clean",eval(message.value));});
       #        '))
       #     ),
       #     width = 3, height="460px"
       #   ),
         
         # Show Word Cloud
         
           h5(textOutput("msg.WC"), align = "center"),
           
           div(
             id = "sub.WC", useShinyjs(),
             box(title="文字雲",status="primary",solidHeader = TRUE,
             	div(id = "busy.WC",p("Rendering... (3~5 seconds)", align = "center")),
				column(3,
					selectInput(
			             "module.WC",
			             #"Modules",
			             "單元：",
			             choices = c(
			               "全部" = "A",
			               "全部 Feedback" = "F",
			               "全部 Discussion" = "D",
			               modulesList,
			               "其他"
			             ),
			             selected = "A"
		           	),sliderInput(
			             "slider.max.WC",
			             "Maximum Number of Words:",
			             min = 1,
			             max = 100,
			             value = 30
			           ),
			           hr(),
			           # actionButton("recal", "Recal"),
			           # actionButton("clean", "Clean"),
			           br(),
			           helpText(HTML(paste(
			             #"Source: NTU MOOCs",
			             #"*Processing time of the wordcloud : 3~5 sec.",
			              "",
			              "►產生文字雲約需 3~5 秒",
			              "►文字大小代表出現頻率",
			              "",
			             sep = "<br>"
			           ))),
			           tags$head(
			             tags$style("#plotWC{height:70vh !important;}.actionButton{min-width:100% !important;}"),
			              tags$script(
			                HTML('Shiny.addCustomMessageHandler("myCallbackHandler_recal",
			                     function(message) {
			                     Shiny.onInputChange("deleteConfirmChoice_recal",eval(message.value));});
			                     Shiny.addCustomMessageHandler("myCallbackHandler_clean",
			                     function(message) {
			                     Shiny.onInputChange("deleteConfirmChoice_clean",eval(message.value));});
			              '))
			           )
		           	),
             	column(1),
             	column(8,plotOutput("plot.WC",height="580px"))
             	 ,width = 12, height="660px"
             ),
            box(title="關鍵字次數表",status="primary",solidHeader = TRUE,
	             HTML("<center><h3>Word Freq Table</h3></center>"),
	             br(),
	             div(style="display: inline-block;vertical-align:top;width:220px;",downloadButton('downloadData.WC.WF', '下載關鍵字次數表')),
	             div(style="display: inline-block;vertical-align:top;width:600px;",
	             	radioButtons(
	               "radioBtn.WC",
	               "Data Encoding:",
	               c("UTF-8 (For English-based operation system)" = "UTF-8", "BIG 5 (For Chinese-based operation system)" = "BIG5")
	             )),
	             HTML("<br><br>"),
	             DT::dataTableOutput('DT.WC.WordFreq'), 
	             width = 12, height="100%"),

            box(title="原始內容",status="primary",solidHeader = TRUE,
	             HTML("<center><h3>Original Content Table</h3></center>"),
	             br(),
	             div(style="display: inline-block;vertical-align:top;width:220px;",downloadButton('downloadData.WC.C', '下載資料表')),
	              div(style="display: inline-block;vertical-align:top;width:600px;",
	              	radioButtons(
	               "radioBtn.WF",
	               "Data Encoding:",
	               c("UTF-8 (For English-based operation system)" = "UTF-8", "BIG 5 (For Chinese-based operation system)" = "BIG5")
	             )),
	             HTML("<br><br>"),
	             DT::dataTableOutput('DT.WC.Content'),
	           width = 12, height="100%")
           )
         )
   ))),


    tabItem(tabName = "moocs" ,shinyUI(fluidPage(
      fluidRow(htmlOutput("inCourseList")))))
  )
)
}

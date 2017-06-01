server <- shinyServer(function(input, output, session) {
  # added "session" because updateSelectInput requires it
  
  
	data <- reactive({ 
		req(input$file1) ## ?req #  require that the input is available
		
		inFile <- input$file1 
	   
		df <- readLines(inFile$datapath)
		  
		return(df)
	})
  
	output$contents <- renderTable({
		data()
	})
  
	output$MyPlot <- renderPlot({
		
		#if(is.null(df)){return (NULL)}
		#data()

		#Read from chat history file
		texts = data();
		
		#texts <- readLines("samplechat.txt")
		text=texts;
		
		library("tm")
		library("SnowballC")
		library("wordcloud")
		library("RColorBrewer")
		library("syuzhet") 
		library("lubridate") 
		library("ggplot2")
		library("dplyr") 
		
		#fetch sentiment words from tweets
		mySentiment <- get_nrc_sentiment(texts)
		head(mySentiment)
		text <- cbind(texts, mySentiment)
		
		#count the sentiment words by category
		sentimentTotals <- data.frame(colSums(text[,c(2:11)]))
		names(sentimentTotals) <- "count"
		sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
		rownames(sentimentTotals) <- NULL
		
		#total sentiment score of all texts
		x <- ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
		  geom_bar(aes(fill = sentiment), stat = "identity") +
		  theme(legend.position = "none") +
		  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score")
		
		plot(x)
       
	})
  
	output$Word <- renderPlot({
		#load required libraries
		library(ggplot2)
		library(lubridate)
		library(Scale)
		library(reshape2)
		
		#Read from chat history file
		texts <- data()
		
		#load libraries for cloud
		library("tm")
		library("SnowballC")
		library("wordcloud")
		library("RColorBrewer")
		
		text=texts;
		docs <- Corpus(VectorSource(text))
		toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
		docs <- tm_map(docs, toSpace, "/")
		docs <- tm_map(docs, toSpace, "@")
		docs <- tm_map(docs, toSpace, "\\|")
		docs <- tm_map(docs, content_transformer(tolower))
		docs <- tm_map(docs, removeNumbers)
		docs <- tm_map(docs, removeWords, stopwords("english"))
		docs <- tm_map(docs, removeWords, c("omitted","â","pm","am","/+","media","omit","mitisha",
											"ankit","shweta","andheri","abhishek","cacr","pratik",
											"nichat","dinesh")) 
		docs <- tm_map(docs, removePunctuation)
		docs <- tm_map(docs, stripWhitespace)
		docs <- tm_map(docs, stemDocument)
		dtm <- TermDocumentMatrix(docs)
		m <- as.matrix(dtm)
		v <- sort(rowSums(m),decreasing=TRUE)
		d <- data.frame(word = names(v),freq=v)
		#head(d, 10)
		set.seed(1234)
		p<-wordcloud(words = d$word, freq = d$freq, min.freq = 1,
				  max.words=200, random.order=FALSE, rot.per=0.35, 
				  colors=brewer.pal(8, "Dark2"))
		plot.window(xlim = c(0,0),ylim =c(0,0),log="x",p)
		
	})
})
An R package that wraps the Google Analytics API

The code is ugly and probably very 'un-R-like'. It does, however, get
data into R for further hackery, which was my goal.

Example usage:

library(googleanalytics4r)
library(xts)
library(ggplot2)

build.trend.data <- function(xml,period="months",metric=NULL) {
	doc <- xmlTreeParse(xml,useInternalNodes=TRUE)
	
	m <- parse.metric.data(doc,metric)
	d <- parse.dimension.data(doc,"ga:date")
	site_name <- xpathSApply(doc,"//dxp:tableName",xmlValue)

	x <- try.xts(as.numeric(m),as.Date(d,"%Y%m%d"))
	ep <- endpoints(x,period)
	x.period <- period.apply(x,ep,sum)
	colnames(x.period) <- site_name
	
	return(x.period)
}

reshape.data <- function(l) {
  df <- do.call("cbind",lapply(l,function(x) reclass(as.data.frame(x))))
  df$date <- index(as.xts(df))
  df <- melt(df,id="date")
}

auth_token <- google.login(EMAIL,PASSWORD) # replace with you google login

start_date <- "2010-01-01"
end_date <- as.character(Sys.Date() - 1)

profile_ids <- c("ga:XXXXXXX","ga:XXXXXXX") # replace with your sites

period="weeks"

trend.xml <- lapply(profile_ids,
    get.analytics.data,
    auth_token=auth_token,
    start_date=start_date,
    end_date=end_date,
    dimensions="ga:date",
    metrics="ga:pageviews,ga:visits,ga:newVisits,ga:timeOnSite",
    sort="ga:date",
    filters="ga:browser!=Opera")
    
pageview.trend.data <- lapply(trend.xml,
    build.trend.data,
    period=period,metric="ga:pageviews")

pageviews <- reshape.data(pageview.trend.data)
colnames(pageviews) <- c("date","url","pageviews")

qplot(x=date,y=pageviews,data=pageviews,geom="line",colour=url,xlab="date",ylab="pageviews") + scale_y_continuous(formatter="comma") + opts(title="Weekly Pageviews")

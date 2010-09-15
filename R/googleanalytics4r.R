require(RCurl)
require(XML)

google.login <- function(email,passwd) {
  x <- postForm(uri="https://www.google.com/accounts/ClientLogin",
                Email=email,
                Passwd=passwd,
                accountType="GOOGLE",
                source="RCurl",
                service="analytics")
  x <- strsplit(x,"\n")
  x[[1]][3]
}

get.analytics.data <- function(auth_token,
                               profile_id,
                               start_date,
                               end_date,
                               dimensions,
                               metrics,
                               sort,
                               filters=NULL,
                               maxresults="10000",
                               startindex="1") {
  h <- basicTextGatherer()

  if(is.null(filters)) {
    curlPerform(url=paste("https://www.google.com/analytics/feeds/data?start-date=",
                  start_date,
                  "&end-date=",end_date,
                  "&dimensions=",dimensions,
                  "&metrics=",metrics,
                  "&sort=",sort,
                  "&ids=",profile_id,
                  "&start-index=",startindex,
                  "&max-results=",maxresults,
                  sep=""),
                .opts=list(httpheader=paste("Authorization: GoogleLogin",auth_token)),
                writeFunction=h$update)
  } else {
    curlPerform(url=paste("https://www.google.com/analytics/feeds/data?start-date=",
                  start_date,
                  "&end-date=",end_date,
                  "&dimensions=",dimensions,
                  "&metrics=",metrics,
                  "&sort=",sort,
                  "&ids=",profile_id,
                  "&filters=",filters,
                  "&start-index=",startindex,
                  "&max-results=",maxresults,
                  sep=""),
                .opts=list(httpheader=paste("Authorization: GoogleLogin",auth_token)),
                writeFunction=h$update)
  }

  return(h$value())
}

parse.metric.data <- function(doc,metric) {
  xpathSApply(doc,paste("//a:entry/dxp:metric[@name='",metric,"']",sep=""),
              xmlGetAttr,
              "value",
              namespaces=c(a = "http://www.w3.org/2005/Atom",
                dxp = "http://schemas.google.com/analytics/2009"))
}

parse.dimension.data <- function(doc,dimension) {
    xpathSApply(doc,paste("//a:entry/dxp:dimension[@name='",dimension,"']",sep=""),
                xmlGetAttr,
                "value",
                namespaces=c(a = "http://www.w3.org/2005/Atom",
                  dxp = "http://schemas.google.com/analytics/2009"))
}

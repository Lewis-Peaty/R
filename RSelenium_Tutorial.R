library(RSelenium)
RSelenium::checkForServer()
RSelenium::startServer()

remDr <- remoteDriver(remoteServerAddr = "localhost" 
                      , port = 4444
                      , browserName = "firefox"
)
remDr$open()
remDr$getStatus()
remDr$navigate("http://www.google.com/ncr")
webElem <- remDr$findElement(using = "xpath", "//*/input[@id = 'lst-ib']")
webElem$sendKeysToElement(list("R Cran", "\uE007"))

#####send plain email
InfromMe <- function(info){
  from <- "you@account.com"
  to <- "recipient@account.com"
  subject <- info$subject
  body <- info$body                    
  mailControl=list(smtpServer="serverinfo")
  sendmail(from=from,to=to,subject=subject,msg=body,control=mailControl)
}
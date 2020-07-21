#' @title ethereum token content generator
#'
#' @description writes research reports for erc20 tokens
#'
#' @param address
#'
#' @return NULL
#'
#' @examples ue_send_mail()
#'
#' @export ue_send_mail

ue_send_mail <- function(ue_id,ue_key = Sys.getenv('ue_key'),subject_line,email_content,recipient_ids,author_id,smtp_id) {
  pb <- list(subject = subject_line,
             content = email_content,
             receivers = recipient_ids,
             author = author_id,
             smtp = smtp_id)
  post_response <- POST(glue("https://{ue_id}.user.com/api/public/emails/send/"),
             add_headers(`Authorization` = glue('Token {ue_key}'),
                         `Content-Type` = 'application/json'),
             body=pb,encode = 'json') %>% content()
  
  return(post_response)
}


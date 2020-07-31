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

ue_add_user_to_list <- function(ue_id,ue_key = Sys.getenv('ue_key'),user_id,list_id) {
  pb <- list(list = list_id)
  post_response <- POST(glue("https://{ue_id}.user.com/api/public/users/:{user_id}/add_to_list/"),
             add_headers(`Authorization` = glue('Token {ue_key}'),
                         `Content-Type` = 'application/json'),
             body=pb,encode = 'json')
  
  return(post_response)
}


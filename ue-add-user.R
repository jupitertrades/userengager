#' @title ethereum token content generator
#'
#' @description writes research reports for erc20 tokens
#'
#' @param address
#'
#' @return NULL
#'
#' @examples ue_get_users()
#'
#' @export ue_get_users

ue_add_user <- function(ue_id,ue_key = Sys.getenv('ue_key'),email_address,f_name = NULL,l_name = NULL,c_id = NULL) {
  user_add_response <- POST(glue('https://{ue_id}.user.com/api/public/users/'),
                             add_headers(`Authorization` = glue('Token {ue_key}')),
                         body=list(email = email_address,first_name = f_name,last_name = l_name,
                                   company_id = c_id),encode = 'json')
  return(user_add_response)
}


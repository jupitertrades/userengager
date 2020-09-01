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

ue_get_users <- function(ue_id,ue_key = Sys.getenv('ue_key')) {
  all_users_list <- GET(glue('https://{ue_id}.user.com/api/public/users/'),
                             add_headers(`Authorization` = glue('Token {ue_key}'))) %>%
                          content() %>% pluck('results')
  null_replacer <- function(x) {
    if(is.null(x)) {
      return('0')
    }
    if(!is.null(x)) {
     return(x)
    }
  }
  aid <- all_users_list %>% map2('id',pluck) %>% enframe() %>% select(id = value) %>% tidyr::unnest(id)
  created_at <- all_users_list %>% map2('created_at',pluck) %>% enframe() %>% select(created_at = value) %>%
    tidyr::unnest(created_at)
  amail <- all_users_list %>% map2('email',pluck) %>% map(null_replacer) %>% enframe() %>% select(email = value) %>%
    tidyr::unnest(email)
  list_collector <- c()
  for(i in 1:length(all_users_list)) {
    adf <- all_users_list[[i]] %>% pluck('lists') %>% map2('id',pluck) %>% glue_collapse(sep=',')
    if(length(adf) > 0) {
      list_collector <-  c(list_collector,adf)
    }
    if(length(adf) == 0) {
      list_collector <-  c(list_collector,0)
    }
  }
  all_users_df <- tibble(id = aid$id, created_at = created_at$created_at, email = amail$email, lists = list_collector)
  return(all_users_df)
}


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
  page_list <- 1
  all_users_list <- list()
  i <- 1
  while(!is.null(page_list)) {
    page_list <- GET(glue('https://{ue_id}.user.com/api/public/users?page={i}'),
                          add_headers(`Authorization` = glue('Token {ue_key}'))) %>%
      content() %>% purrr::pluck('results')
    all_users_list <- c(all_users_list,page_list)
    i <- i + 1
  }

  null_replacer <- function(x) {
    if(is.null(x)) {
      return('0')
    }
    if(!is.null(x)) {
     return(x)
    }
  }
  aid <- all_users_list %>% map2('id',purrr::pluck) %>% enframe() %>% select(id = value) %>% tidyr::unnest(id)
  created_at <- all_users_list %>% map2('created_at',purrr::pluck) %>% enframe() %>% select(created_at = value) %>%
    tidyr::unnest(created_at)
  amail <- all_users_list %>% map2('email',purrr::pluck) %>% map(null_replacer) %>% enframe() %>% select(email = value) %>%
    tidyr::unnest(email)
  unsub_status <- all_users_list %>% map2('unsubscribed',purrr::pluck) %>% map(null_replacer) %>% enframe() %>% select(unsubscribed = value) %>%
    tidyr::unnest(unsubscribed)
  list_collector <- c()
  for(i in 1:length(all_users_list)) {
    adf <- all_users_list[[i]] %>% purrr::pluck('lists') %>% map2('id',purrr::pluck) %>% glue_collapse(sep=',')
    if(length(adf) > 0) {
      list_collector <-  c(list_collector,adf)
    }
    if(length(adf) == 0) {
      list_collector <-  c(list_collector,0)
    }
  }
  all_users_df <- tibble(id = aid$id, created_at = created_at$created_at, email = amail$email,
                         unsubscribed = unsub_status$unsubscribed,lists = list_collector) %>%
    group_by(email) %>% arrange(created_at) %>%
    slice(1) %>% ungroup()
  return(all_users_df)
}


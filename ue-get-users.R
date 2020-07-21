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
  for(i in 1:length(all_users_list)) {
    adf <- all_users_list[[i]] %>% compact() %>% bind_rows()
    if(!is.null(adf$lists)) {
      adf2_list <- adf$lists %>% bind_rows() %>% pull(id) %>% glue_collapse(sep =',',last=',')
      adf$lists = adf2_list
    }
    if(is.null(adf$lists)) {
      adf$lists <-  '0'
    }
    all_users_df <- bind_rows(all_users_df,adf)
  }
  return(all_users_df)
}


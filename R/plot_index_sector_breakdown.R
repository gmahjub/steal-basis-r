#' plot_index_sector_breakdown
#'
#' Bar plot of the sector breakdown, by count, number of companies per sector,
#' of the tibble object passed in as paramter. The input tibble object would be
#' most simply, the result of a call to tq_index(x = some index).
#'
#' @param idx_comps_tibble
#'
#' @return
#' @export
#'
#' @examples
plot_index_sector_breakdown_count<-function(idx_comps_tibble){
  
  idx_comps_tibble %>% group_by(sector) %>% dplyr::summarise(count = n()) %>% 
    ggplot(aes(x = sector %>% fct_reorder(count), y = count)) +
    geom_bar(stat = "identity") + geom_text(aes(label = count), size = 3, nudge_y = 4, 
                                            nudge_x = .1) + scale_y_continuous(limits = c(0,50)) + 
    ggtitle(label = "Sector Frequency Among SP500 Stocks") + xlab(label = "GICS Sector") + 
    theme(plot.title = element_text(size = 16)) + coord_flip()

}

#' plot_index_sector_breakdown_mkt_cap
#'
#' Plot a bar plot, sector weighting breakdown in the input index. The input
#' tibble object would be most simply, the result of a call to tq_index(x = some
#' index).
#'
#' @param idx_comps_tibble
#'
#' @return
#' @export
#'
#' @examples
plot_index_sector_breakdown_mkt_cap<-function(idx_comps_tibble){
  
  idx_comps_tibble %>% group_by(sector) %>% dplyr::summarise(sector_weight = round(sum(weight)*100.0, 2)) %>%
    ggplot(aes(x = sector %>% fct_reorder(sector_weight), y = sector_weight)) + 
    geom_bar(stat = "identity") + geom_text(aes(label = sector_weight), size = 3, nudge_y = 4,
                                            nudge_x = .1) + scale_y_continuous(limits = c(0,50)) + 
    ggtitle(label = "Sector Weighting Among SP500 Stocks") + xlab(label = "GICS Sector") +
    theme(plot.title = element_text(size = 16)) + coord_flip()
  
}
example_plot <- function(data, article) {
  data %>%
    mutate(Monat = as.Date(Monat)) %>%
    filter(article_detail == article) %>%
    ggplot(aes(x = Monat, y = y)) +
    geom_point() + geom_line()
}


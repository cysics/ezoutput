ez_item <- function(data, method="auto"){
    models <- data %>% mutate_all(~as.factor(.)) %>% map(levels) %>% map(length) %>% unlist() %>% data.frame() %>%
        rename(levels=1) %>% mutate(levels=ifelse(levels=="2", "2PL", "graded")) %>% pull(levels)
    models
}

#' Item Analysis
#'
#' @param data Data of item
#' @param method Method like "2PL", "graded", and "auto"
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' data <- survey[5:10]
#' strsplit(data)

ez_item <- function(data, method="auto"){
    models <- data %>% mutate_all(~as.factor(.)) %>% map(levels) %>%
        map(length) %>% unlist() %>% data.frame() %>%
        rename(levels=1) %>% mutate(levels=ifelse(levels=="2", "2PL", "graded")) %>%
        pull(levels)
    fit_mix <- mirt(data, model=1, itemtype=models)
    result <- ItemAnalysis(data, k=3, l=1, u=3) %>% select(gULI) %>% rownames_to_column("item") %>%
        mutate(disc=coef(fit_mix, IRTpars=T, verbose=F, simplify=T, order=T) %>% .[1] %>% data.frame() %>% pull(1)) %>%
        mutate(변별도CTT=ifelse(gULI>0.4, round(ifelse(4+(gULI-0.4)*10<5, 4+(gULI-0.4)*10, 5), 2),
                             ifelse(gULI>0.3, round(3+(gULI-0.3)*10, 2),
                                    ifelse(gULI>0.2, round(2+(gULI-0.2)*10, 2),
                                           ifelse(gULI>0, 1+round((gULI)*10, 2), 0))))) %>%
        mutate(변별도IRT=ifelse(disc>1.7, 5,
                             ifelse(disc>1.3, round(4+(disc-1.3)/0.4, 2),
                                    ifelse(disc>0.65, round(3+(disc-0.65)/0.65, 2),
                                           ifelse(disc>0.35, round(2+(disc-0.35)/0.35, 2),
                                                  ifelse(disc>0, 1, 0))))))
    # CTT : 2 낮음(0.2~0.3), 3 보통(0.3~0.4), 4 높음(0.4~)
    # IRT : 2 낮음(0.35~0.65), 3 보통(0.65~1.35), 4 높음(1.35~1.7), 5 매우 높음(1.7~)
    result
}

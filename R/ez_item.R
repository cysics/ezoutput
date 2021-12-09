#' Item Analysis
#'
#' @param data Data of item
#' @param method Method like "2PL", "graded", and "auto"
#'
#' @return A data.frame.
#' @export
#'
#' @examples
#' data <- survey[5:9]
#' ez_item(data)

# graded는 부분점수간에는 난이도 서열이 있다고 가정한다. 문항별 부분점수의 간격이 다를 수도 있다. 난이도와 변별도를 제공해준다.
# rsm은 rash계열로 문항당 동일한 수의 범주가 있고 모든 항목이 동일하다고 가정한다. (모든 문항당 범주 간격이 동일한 리커트척도에 적용하기 위해 개발)
# pcm도 rash계열이며 부분점끼리 서열이 없는 것으로 간주한다.
# gpcm은 부분점수끼리 서열이 없는 것으로 간주하기 때문에 부분점수끼리 난이도가 같게 나올 수도 있다.
# gpcm은 pcm과 달리 변별도를 제공하며 pcm과 마찬가지로 rash 계열이라 관련 정보를 제공한다.
# 정의적인 특성검사는 GGUM 모델이 타당하다.

ez_item <- function(data, method="auto", k=3, l=1, u=3, csv=FALSE){
    if(method=="auto"){
        models <- data %>% mutate_all(~as.factor(.)) %>% map(levels) %>%
            map(length) %>% unlist() %>% data.frame() %>%
            rename(levels=1) %>% mutate(levels=ifelse(levels=="2", "2PL", "graded")) %>%
            pull(levels)
    }

    item_fit <<- fit_irt <- mirt(data, model=1, itemtype=models)

    result <- ItemAnalysis(data, k=k, l=l, u=u) %>% select(Difficulty, SD, gULI) %>%
        rownames_to_column("item") %>%
        mutate(discrimination=coef(item_fit, IRTpars=T, verbose=F, simplify=T, order=T) %>% .[1] %>%
                   data.frame() %>% pull(1)) %>%
        mutate(b=coef(item_fit, IRTpars=T, verbose=F, simplify=T, order=T) %>% .[1] %>% data.frame() %>%
                   mutate(items.b=ifelse(length(unique(models))==1, rowMeans(.[2:ncol(.)], na.rm=T),
                                         ifelse(is.na(items.b), rowMeans(.[3:ncol(.)], na.rm=T), items.b))) %>%
                   pull(items.b)) %>% as_tibble() %>%
        mutate(diffCTT=ifelse(Difficulty>=0.8, "매우 쉬움",
                             ifelse(Difficulty>=0.6, "쉬움",
                                    ifelse(Difficulty>=0.4, "적절함",
                                           ifelse(Difficulty>=0.2, "어려움", "매우 어려움"))))) %>%
        mutate(diffIRT=ifelse(b<(-2), "매우 쉬움",
                             ifelse(b<(-0.5), "쉬움",
                                    ifelse(b<0.5, "중간",
                                           ifelse(b<2, "어려움", "매우 어려움"))))) %>%
        mutate(discCTT=ifelse(gULI<0, "부적절함",
                             ifelse(gULI<0.2, "매우 낮음",
                                    ifelse(gULI<0.3, "낮음",
                                           ifelse(gULI<0.4, "중간", "높음"))))) %>%
        mutate(discIRT=ifelse(b<0, "부적절함",
                             ifelse(b<0.35, "거의 없음",
                                    ifelse(b<0.65, "낮음",
                                           ifelse(b<1.35, "적절함",
                                                  ifelse(b<1.70, "높음", "매우 높음"))))))
    # CTT : 2 낮음(0.2~0.3), 3 보통(0.3~0.4), 4 높음(0.4~)
    # IRT : 2 낮음(0.35~0.65), 3 보통(0.65~1.35), 4 높음(1.35~1.7), 5 매우 높음(1.7~)

    item_result <<- result

    if(csv){
        write.csv(result, "discrimination.csv", row.names=FALSE)
    }

    result
}

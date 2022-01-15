#' Item Analysis
#'
#' @param data Data of item
#' @param method Method like "2PL", "graded", and "auto"
#' @param k Number of group
#' @param l Lower of group
#' @param u Upper of group
#'
#' @return A data.frame.
#' @export
#'
#' @examples
#' data <- survey[5:9]
#' ez_item(data)

ez_item <- function(Data, method="auto", k=3, l=1, u=3, csv=FALSE){
    Data <- Data %>% select(where(~n_distinct(.) > 1))
    minscore <- Data %>% summarise_all(~max(.x, na.rm=T)) %>% min()
    if(minscore>=1){minscore=1}
    ifelse(minscore<1, Data <- Data %>% map_df(~.x*ceiling(1/minscore)), Data <- Data)

    MaxSum <- sum(sapply(Data, max, na.rm = TRUE))
    MinSum <- sum(sapply(Data, min, na.rm = TRUE))
    TOT <- rowSums(Data, na.rm = TRUE) / (MaxSum - MinSum)
    Data <- Data[order(TOT), ]
    data <- Data %>% slice(as.integer((l - 1) * nrow(Data) / k + 1):as.integer(u * nrow(Data) / k))

    if(method=="auto"){
        models <- data %>% mutate_all(~as.factor(.)) %>% map(levels) %>%
            map(length) %>% unlist() %>% data.frame() %>%
            rename(levels=1) %>% mutate(levels=ifelse(levels=="2", "2PL", "graded")) %>%
            pull(levels)
        Models <- Data %>% mutate_all(~as.factor(.)) %>% map(levels) %>%
            map(length) %>% unlist() %>% data.frame() %>%
            rename(levels=1) %>% mutate(levels=ifelse(levels=="2", "2PL", "graded")) %>%
            pull(levels)
    }

    if(k>3){fit_IRT <- mirt(Data, model=1, itemtype=Models)}
    item_fit <<- fit_irt <- mirt(data, model=1, itemtype=models)

    ifelse(k>3,
           result <- Data %>% map_df(~.x/ceiling(1/minscore)) %>% ItemAnalysis(k=k, l=l, u=u) %>% select(Max.score, Mean, SD, Difficulty, ULI, gULI) %>%
               rename(Max=1, CTT_Mean_all=2, CTT_SD_all=3, CTT_Difficulty_all=4, CTT_Discrimination_all=5, CTT_Discrimination=6) %>%
               rownames_to_column("item") %>%
               left_join(data %>% map_df(~.x/ceiling(1/minscore)) %>% ItemAnalysis() %>%
                             select(Mean, SD, Difficulty) %>% rownames_to_column("item") %>%
                             rename(CTT_Mean=2, CTT_SD=3, CTT_Difficulty=4)) %>%
               select(item:CTT_Discrimination_all, CTT_Mean:CTT_Difficulty, CTT_Discrimination) %>%
               mutate(IRT_Discrimination_all=coef(fit_IRT, IRTpars=T, verbose=F, simplify=T, order=T) %>% .[1] %>%
                          data.frame() %>% pull(1)) %>%
               mutate(IRT_Discrimination=coef(fit_irt, IRTpars=T, verbose=F, simplify=T, order=T) %>% .[1] %>%
                          data.frame() %>% pull(1))
           ,
           result <- data %>% map_df(~.x/ceiling(1/minscore)) %>% ItemAnalysis() %>%
               select(Mean, SD, Difficulty, ULI) %>% rownames_to_column("item") %>%
                             rename(CTT_Mean=2, CTT_SD=3, CTT_Difficulty=4, CTT_Discrimination=5) %>%
               mutate(IRT_Discrimination=coef(fit_irt, IRTpars=T, verbose=F, simplify=T, order=T) %>% .[1] %>%
                          data.frame() %>% pull(1))
    )
    if(k>3){
        ifelse(length(unique(Models))==1 & Models[1]=="graded",
               result <- result %>%
                   mutate(IRT_Difficulty_all=coef(fit_IRT, IRTpars=T, verbose=F, simplify=T, order=T) %>% .[1] %>% data.frame() %>%
                              mutate(items.b=rowMeans(.[2:ncol(.)], na.rm=T)) %>%
                              mutate(items.b=ifelse(items.b>2.5, 2.5, items.b)) %>%
                              mutate(items.b=ifelse(items.b<(-2.5), -2.5, items.b)) %>%
                              pull(items.b)) %>% as_tibble()
               ,
               ifelse(length(unique(Models))==2 & Models[1]=="graded",
                      result <- result %>%
                          mutate(IRT_Difficulty_all=coef(fit_IRT, IRTpars=T, verbose=F, simplify=T, order=T) %>% .[1] %>% data.frame() %>%
                                     mutate(items.b=ifelse(is.na(items.b), rowMeans(.[2:(ncol(.)-3)], na.rm=T), items.b)) %>%
                                     mutate(items.b=ifelse(items.b>2.5, 2.5, items.b)) %>%
                                     mutate(times.b=ifelse(items.b<(-2.5), -2.5, items.b)) %>%
                                     pull(items.b)) %>% as_tibble()
                      ,
                      result <- result %>%
                          mutate(IRT_Difficulty_all=coef(fit_IRT, IRTpars=T, verbose=F, simplify=T, order=T) %>% .[1] %>% data.frame() %>%
                                     mutate(items.b=ifelse(length(unique(models))==2 & is.na(items.b), rowMeans(.[5:ncol(.)], na.rm=T), items.b)) %>%
                                     mutate(items.b=ifelse(items.b>2.5, 2.5, items.b)) %>%
                                     mutate(times.b=ifelse(items.b<(-2.5), -2.5, items.b)) %>%
                                     pull(items.b)) %>% as_tibble()
               )

        )}

    ifelse(length(unique(models))==1 & models[1]=="graded",
           result <- result %>%
               mutate(IRT_Difficulty=coef(fit_irt, IRTpars=T, verbose=F, simplify=T, order=T) %>% .[1] %>% data.frame() %>%
                          mutate(items.b=rowMeans(.[2:ncol(.)], na.rm=T)) %>%
                          mutate(items.b=ifelse(items.b>2.5, 2.5, items.b)) %>%
                          mutate(items.b=ifelse(items.b<(-2.5), -2.5, items.b)) %>%
                          pull(items.b)) %>% as_tibble()
           ,
           ifelse(length(unique(models))==2 & models[1]=="graded",
                  result <- result %>%
                      mutate(IRT_Difficulty=coef(fit_irt, IRTpars=T, verbose=F, simplify=T, order=T) %>% .[1] %>% data.frame() %>%
                                 mutate(items.b=ifelse(is.na(items.b), rowMeans(.[2:(ncol(.)-3)], na.rm=T), items.b)) %>%
                                 mutate(items.b=ifelse(items.b>2.5, 2.5, items.b)) %>%
                                 mutate(times.b=ifelse(items.b<(-2.5), -2.5, items.b)) %>%
                                 pull(items.b)) %>% as_tibble()
                  ,
                  result <- result %>%
                      mutate(IRT_Difficulty=coef(fit_irt, IRTpars=T, verbose=F, simplify=T, order=T) %>% .[1] %>% data.frame() %>%
                                 mutate(items.b=ifelse(length(unique(models))==2 & is.na(items.b), rowMeans(.[5:ncol(.)], na.rm=T), items.b)) %>%
                                 mutate(items.b=ifelse(items.b>2.5, 2.5, items.b)) %>%
                                 mutate(times.b=ifelse(items.b<(-2.5), -2.5, items.b)) %>%
                                 pull(items.b)) %>% as_tibble()
           )

    )
    result <- result %>%
        mutate(diffCTT=ifelse(CTT_Difficulty>=0.8, "매우 쉬움",
                              ifelse(CTT_Difficulty>=0.6, "쉬움",
                                     ifelse(CTT_Difficulty>=0.4, "적절함",
                                            ifelse(CTT_Difficulty>=0.2, "어려움", "매우 어려움"))))) %>%
        mutate(diffIRT=ifelse(IRT_Difficulty<(-2), "매우 쉬움",
                              ifelse(IRT_Difficulty<(-0.5), "쉬움",
                                     ifelse(IRT_Difficulty<0.5, "중간",
                                            ifelse(IRT_Difficulty<2, "어려움", "매우 어려움"))))) %>%
        mutate(discCTT=ifelse(CTT_Discrimination<0, "부적절함",
                              ifelse(CTT_Discrimination<0.2, "매우 낮음",
                                     ifelse(CTT_Discrimination<0.3, "낮음",
                                            ifelse(CTT_Discrimination<0.4, "중간", "높음"))))) %>%
        mutate(discIRT=ifelse(IRT_Discrimination<0, "부적절함",
                              ifelse(IRT_Discrimination<0.35, "거의 없음",
                                     ifelse(IRT_Discrimination<0.65, "낮음",
                                            ifelse(IRT_Discrimination<1.35, "적절함",
                                                   ifelse(IRT_Discrimination<1.70, "높음", "매우 높음")))))) %>%
        mutate(CTTdisc=CTT_Discrimination) %>%
        mutate(IRTdisc=ifelse(IRT_Discrimination>1.7, 0.5,
                              ifelse(IRT_Discrimination>1.3, round(0.4+(IRT_Discrimination-1.3)/4, 2),
                                     ifelse(IRT_Discrimination>0.65, round(0.3+(IRT_Discrimination-0.65)/6.5, 2),
                                            ifelse(IRT_Discrimination>0.35, round(0.2+(IRT_Discrimination-0.35)/3.5, 2),
                                                   ifelse(IRT_Discrimination>0, 0.1, 0)))))) %>%
        mutate(CTTdiff=CTT_Difficulty) %>%
        mutate(IRTdiff=ifelse(IRT_Difficulty<(-2), ifelse(round(0.8+(IRT_Difficulty*(-1)-2)/15, 3)>1, 1, round(0.8+(IRT_Difficulty*(-1)-2)/15, 3)),
                              ifelse(IRT_Difficulty<(-0.5), round(0.6+(IRT_Difficulty*(-1)-0.5)/15, 3),
                                     ifelse(IRT_Difficulty<0.5, round(0.4+(IRT_Difficulty+0.5)/10, 3),
                                            ifelse(IRT_Difficulty<2, round(0.2+(IRT_Difficulty-0.5)/15, 3),
                                                   ifelse(round(0.2-(IRT_Difficulty-2)/15, 3)>0, round(0.2-(IRT_Difficulty-2)/15, 3), 0))))))

    item_result <<- result

    p1 <- result %>%
        select(item, CTT_Difficulty, CTT_Discrimination) %>% rename(diff=2, disc=3) %>%
        pivot_longer(-item, names_to = "CTT") %>%
        ggbarplot(x="item", y="value", fill="CTT", xlab="", ylab="",
                  label=T, lab.pos="out", lab.vjust=0.5, lab.hjust=-0.2, lab.nb.digits=2,
                  add.params=list(group="CTT"), position=position_dodge(0.8), rotate=90)+
        geom_hline(yintercept=0.2, linetype=2, color="red")+
        geom_hline(yintercept=0.3, linetype=2, color="blue")+
        geom_hline(yintercept=0.4, linetype=2, color="blue")+
        scale_y_continuous(expand =expansion(mult=c(0, 0.1)))+
        scale_fill_brewer(palette="Set3")

    p2 <- result %>%
        select(item, IRT_Difficulty, IRT_Discrimination) %>% rename(diff=2, disc=3) %>%
        pivot_longer(-item, names_to = "IRT") %>%
        ggbarplot(x="item", y="value", fill="IRT", xlab="", ylab="",
                  label=T, lab.pos="out", lab.vjust=0.5, lab.hjust=-0.2, lab.nb.digits=2,
                  add.params=list(group="IRT"), position=position_dodge(0.8), rotate=90)+
        geom_hline(yintercept=0.35, linetype=2, color="red")+
        geom_hline(yintercept=0.65, linetype=2, color="blue")+
        geom_hline(yintercept=1.35, linetype=2, color="blue")+
        geom_hline(yintercept=1.7, linetype=2, color="blue")+
        scale_y_continuous(expand =expansion(mult=c(0, 0.1)))+
        scale_fill_brewer(palette="Set3")

    p3 <- result %>%
        select(item, CTTdiff, IRTdiff) %>% rename(CTT=2, IRT=3) %>%
        pivot_longer(-item, names_to = "난이도") %>%
        ggbarplot(x="item", y="value", fill="난이도", xlab="", ylab="",
                  label=T, lab.pos="out", lab.vjust=0.5, lab.hjust=-0.2, lab.nb.digits=2,
                  add.params=list(group="난이도"), position=position_dodge(0.8), rotate=90)+
        geom_hline(yintercept=0.2, linetype=2, color="red")+
        geom_hline(yintercept=0.4, linetype=2, color="blue")+
        geom_hline(yintercept=0.6, linetype=2, color="blue")+
        geom_hline(yintercept=0.8, linetype=2, color="red")+
        scale_y_continuous(expand =expansion(mult=c(0, 0.1)))+
        scale_fill_brewer(palette="Set3")

    p4 <- result %>%
        select(item, CTTdisc, IRTdisc) %>% rename(CTT=2, IRT=3) %>%
        pivot_longer(-item, names_to = "변별도") %>%
        ggbarplot(x="item", y="value", fill="변별도", xlab="", ylab="",
                  label=T, lab.pos="out", lab.vjust=0.5, lab.hjust=-0.2, lab.nb.digits=2,
                  add.params=list(group="변별도"), position=position_dodge(0.8), rotate=90)+
        geom_hline(yintercept=0.2, linetype=2, color="red")+
        geom_hline(yintercept=0.3, linetype=2, color="blue")+
        geom_hline(yintercept=0.4, linetype=2, color="blue")+
        geom_hline(yintercept=0.5, linetype=2, color="blue")+
        scale_y_continuous(expand =expansion(mult=c(0, 0.1)))+
        scale_fill_brewer(palette="Set3")

    if(csv){
        write.csv(result, "discrimination.csv", row.names=FALSE)
    }

    return(list(result, p4))
}

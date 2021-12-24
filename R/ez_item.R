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
                   data.frame() %>% pull(1))

    ifelse(length(unique(models))==1 & models[1]=="graded",
           result <- result %>%
               mutate(b=coef(item_fit, IRTpars=T, verbose=F, simplify=T, order=T) %>% .[1] %>% data.frame() %>%
                          mutate(items.b=rowMeans(.[2:ncol(.)], na.rm=T)) %>%
                          pull(items.b)) %>% as_tibble()
           ,
           ifelse(length(unique(models))==2 & models[1]=="graded",
                  result <- result %>%
                      mutate(b=coef(item_fit, IRTpars=T, verbose=F, simplify=T, order=T) %>% .[1] %>% data.frame() %>%
                                 mutate(items.b=ifelse(is.na(items.b), rowMeans(.[2:(ncol(.)-3)], na.rm=T), items.b)) %>%
                                 pull(items.b)) %>% as_tibble()
                  ,
                  result <- result %>%
                      mutate(b=coef(item_fit, IRTpars=T, verbose=F, simplify=T, order=T) %>% .[1] %>% data.frame() %>%
                                 mutate(items.b=ifelse(length(unique(models))==2 & is.na(items.b), rowMeans(.[5:ncol(.)], na.rm=T), items.b)) %>%
                                 pull(items.b)) %>% as_tibble()
                  )

    )
    result <- result %>%
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
        mutate(discIRT=ifelse(discrimination<0, "부적절함",
                              ifelse(discrimination<0.35, "거의 없음",
                                     ifelse(discrimination<0.65, "낮음",
                                            ifelse(discrimination<1.35, "적절함",
                                                   ifelse(discrimination<1.70, "높음", "매우 높음")))))) %>%
        mutate(CTTdisc=gULI) %>%
        mutate(IRTdisc=ifelse(discrimination>1.7, 0.5,
                              ifelse(discrimination>1.3, round(0.4+(discrimination-1.3)/4, 2),
                                     ifelse(discrimination>0.65, round(0.3+(discrimination-0.65)/6.5, 2),
                                            ifelse(discrimination>0.35, round(0.2+(discrimination-0.35)/3.5, 2),
                                                   ifelse(discrimination>0, 0.1, 0)))))) %>%
        mutate(CTTdiff=Difficulty) %>%
        mutate(IRTdiff=ifelse(b<(-2), ifelse(round(0.8+(b*(-1)-2)/15, 3)>1, 1, round(0.8+(b*(-1)-2)/15, 3)),
                              ifelse(b<(-0.5), round(0.6+(b*(-1)-0.5)/15, 3),
                                     ifelse(b<0.5, round(0.4+(b+0.5)/10, 3),
                                            ifelse(b<2, round(0.2+(b-0.5)/15, 3),
                                                   ifelse(round(0.2-(b-2)/15, 3)>0, round(0.2-(b-2)/15, 3), 0))))))

    item_result <<- result

    p1 <- result %>%
        select(item, Difficulty, gULI) %>% rename(diff=2, disc=3) %>%
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
        select(item, b, discrimination) %>% rename(diff=2, disc=3) %>%
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

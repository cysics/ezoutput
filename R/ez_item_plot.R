#' Item Analysis
#'
#' @param data Data of item
#' @param type Type like "auto", "CTT", "IRT"
#'
#' @return A graph
#' @export
#'
#' @examples
#' data <- survey[5:9]
#' ez_item_plot(data)


ez_item_plot <- function(data){
    graph <- data %>%
        mutate(discCTT=ifelse(ULI>0.4, round(ifelse(4+(ULI-0.4)*10<5, 4+(ULI-0.4)*10, 5), 2),
                             ifelse(ULI>0.3, round(3+(ULI-0.3)*10, 2),
                                    ifelse(ULI>0.2, round(2+(ULI-0.2)*10, 2),
                                           ifelse(ULI>0, 1+round((ULI)*10, 2), 0))))) %>%
        mutate(discIRT=ifelse(discrimination>1.7, 5,
                             ifelse(discrimination>1.3, round(4+(discrimination-1.3)/0.4, 2),
                                    ifelse(discrimination>0.65, round(3+(discrimination-0.65)/0.65, 2),
                                           ifelse(discrimination>0.35, round(2+(discrimination-0.35)/0.35, 2),
                                                  ifelse(discrimination>0, 1, 0)))))) %>%
        mutate(diffCTT=Difficulty) %>%
        mutate(diffIRT=ifelse(b<(-2), round(0.8+(b*(-1)-2)*0.2/1.5),
                             ifelse(b<(-0.5), round(0.6+(b*(-1)-0.5)*0.2/1.5),
                                    ifelse(b<0.5, round(0.4+((b-0.5)*(-1))*0.2/1),
                                           ifelse(b<2, round(0.2+(b-0.5)*0.2/1.5), round((b-0.5)*0.2/1.5))))))

    p1 <- graph %>%
        select(item, Difficulty, ULI) %>% rename(diff=2, disc=3) %>%
        pivot_longer(-item, names_to = "CTT") %>%
        ggbarplot(x="item", y="value", fill="CTT", xlab="", ylab="",
                  label=T, lab.pos="out", lab.vjust=0.5, lab.hjust=-0.2, lab.nb.digits=2,
                  add.params=list(group="CTT"), position=position_dodge(0.8), rotate=90)+
        geom_hline(yintercept=0.2, linetype=2, color="red")+
        geom_hline(yintercept=0.3, linetype=2, color="blue")+
        geom_hline(yintercept=0.4, linetype=2, color="blue")+
        geom_hline(yintercept=0.8, linetype=2, color="red")+
        scale_y_continuous(expand =expansion(mult=c(0, 0.1)))+
        scale_fill_brewer(palette="Set3")

    p2 <- graph %>%
        select(item, b, discrimination) %>% rename(diff=2, disc=3) %>%
        pivot_longer(-item, names_to = "IRT") %>%
        ggbarplot(x="item", y="value", fill="IRT", xlab="", ylab="",
                  label=T, lab.pos="out", lab.vjust=0.5, lab.hjust=-0.2, lab.nb.digits=2,
                  add.params=list(group="IRT"), position=position_dodge(0.8), rotate=90)+
        geom_hline(yintercept=-2, linetype=2, color="red")+
        geom_hline(yintercept=-0.5, linetype=2, color="red")+
        geom_hline(yintercept=0.5, linetype=2, color="red")+
        geom_hline(yintercept=2, linetype=2, color="red")+
        geom_hline(yintercept=0.35, linetype=2, color="blue")+
        geom_hline(yintercept=0.65, linetype=2, color="blue")+
        geom_hline(yintercept=1.35, linetype=2, color="blue")+
        geom_hline(yintercept=1.7, linetype=2, color="blue")+
        scale_y_continuous(expand =expansion(mult=c(0, 0.1)))+
        scale_fill_brewer(palette="Set3")

    p3 <- graph %>%
        select(item, diffCTT, diffIRT) %>% rename(CTT=2, IRT=3) %>%
        pivot_longer(-item, names_to = "난이도") %>%
        ggbarplot(x="item", y="value", fill="난이도", xlab="", ylab="",
                  label=T, lab.pos="out", lab.vjust=0.5, lab.hjust=-0.2, lab.nb.digits=2,
                  add.params=list(group="난이도"), position=position_dodge(0.8), rotate=90)+
        geom_hline(yintercept=0.2, linetype=2, color="blue")+
        geom_hline(yintercept=0.4, linetype=2, color="blue")+
        geom_hline(yintercept=0.6, linetype=2, color="blue")+
        geom_hline(yintercept=0.8, linetype=2, color="red")+
        scale_y_continuous(expand =expansion(mult=c(0, 0.1)))+
        scale_fill_brewer(palette="Set3")

    p4 <- graph %>%
        select(item, discCTT, discIRT) %>% rename(CTT=2, IRT=3) %>%
        pivot_longer(-item, names_to = "변별도") %>%
        ggbarplot(x="item", y="value", fill="변별도", xlab="", ylab="",
                  label=T, lab.pos="out", lab.vjust=0.5, lab.hjust=-0.2, lab.nb.digits=2,
                  add.params=list(group="변별도"), position=position_dodge(0.8), rotate=90)+
        geom_hline(yintercept=2, linetype=2, color="red")+
        geom_hline(yintercept=3, linetype=2, color="blue")+
        geom_hline(yintercept=4, linetype=2, color="blue")+
        scale_y_continuous(expand =expansion(mult=c(0, 0.1)))+
        scale_fill_brewer(palette="Set3")

    print(data)
    list(p1, p2, p3, p4)
}

install.packages("pcr")
library(pcr)
#relative expression:
ct <- read.csv2("/path...csv", header = TRUE)
group_var <- rep(c('CTR' , 'JQ' ), each = 12)
TAB <- pcr_ddct(ct,
                group_var = group_var,
                reference_gene = 'EF1',
                reference_group = 'CTR')

#stat:
stat <-pcr_test(ct,
                group_var = group_var,
                reference_gene = 'EF1',
                reference_group = 'CTR',
                test = 't.test')

#tibble for stat plot
stat.test <- tibble::tribble(
  ~group1, ~group2,   ~p.adj,
  "CTR1",     "JQ1", 0.008627693,
  "CTR2",     "JQ2", 0.005886804,
  "CTR3",     "JQ3", 0.449114499,
  "CTR4",     "JQ4", 0.020932723,
)
#ggplot chart:
gg <-ggplot(TAB, aes(x=genotype,y=relative_expression,fill=gene,pattern=type)) + geom_bar(stat ="identity",position=position_dodge()) +
  geom_bar_pattern(position = "dodge", stat = "identity",
                   color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.04,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6,
                   pattern_alpha = 0.5) +
  geom_errorbar( aes(ymin=relative_expression-error, ymax=relative_expression+error), width=0.1, colour="black") + scale_x_discrete(limits = c("CTR1","JQ1","CTR4","JQ4","CTR3","JQ3","CTR2","JQ2"))
print(gg)
gg + scale_fill_brewer(palette="Pastel2",  labels=c('ABR', 'SOD', 'RB','CHIT')) + theme_bw() + ylab("2^ ---  Ct relative expression") + scale_pattern_manual(values = c(CTR = "stripe", JQ = "none")) + guides(pattern = guide_legend(override.aes = list(fill = "white")),fill = guide_legend(override.aes = list(pattern = "none"))) + ggtitle("") + theme(axis.title.x = element_blank(),legend.title = element_blank()) + stat_pvalue_manual(stat.test, y.position = 6.5, label = "p.value")
print(gg)

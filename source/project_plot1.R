final project plot code



load("~/info201/project/workspace.RData")
plot_ly(data = cali_access_df1, x = ~Community.accessibility.to.family.planning.birth.control.services, y = ~Community.accesibility.to.abortion.services, color = ~response, type = "scatter", mode = "markers") %>% layout( title = "Community level of Accessibility to Family Planning and Birth Control Services", xaxis = list(title = "Community level of Accessibility to Family Planning and Birth Control Services", tickmode = "linear"), yaxis = list("Community level of Accessibility to Abortion Services", tickmode = "linear"))
 access_plot <- ggplot(cali_access_df1) + geom_col(mapping = aes(x = Community.accessibility.to.family.planning.birth.control.services, y = Community.accesibility.to.abortion.services, fill = response))
 access_plot
 print(access_plot + ggtitle("California Opinions on Acesss to Sexual Health Care"))
 print(access_plot + labs(y = "Community level of Accessibility to Family Planning and Birth Control Services", x = "Community level of Accessibility to Abortion Services"))
 print(access_plot + labs(colour = "Opinion Response"))
 access_plot
 print(access_plot + labs(y = "Community level of Accessibility to Family Planning and Birth Control Services", x = "Community level of Accessibility to Abortion Services"))


 plot_ly(data = cali_access_df1, x = ~Community.accessibility.to.family.planning.birth.control.services, y = ~Community.accesibility.to.abortion.services, color = ~response, type = "scatter", mode = "markers") %>% layout( title = "Community level of Accessibility to Family Planning and Birth Control Services", xaxis = list(title = "Community level of Accessibility to Family Planning and Birth Control Services", tickmode = "linear"), yaxis = list("Community level of Accessibility to Abortion Services", tickmode = "linear"))
 access_plot <- ggplot(cali_access_df1) + geom_col(mapping = aes(x = Community.accessibility.to.family.planning.birth.control.services, y = Community.accesibility.to.abortion.services, fill = response))
 access_plot
 print(access_plot + ggtitle("California Opinions on Acesss to Sexual Health Care"))
 print(access_plot + labs(y = "Community level of Accessibility to Family Planning and Birth Control Services", x = "Community level of Accessibility to Abortion Services"))
 print(access_plot + labs(colour = "Opinion Response"))

  cali_law_df1$Response <- c("Legal", "Illegal", "Don't know", "Refused answer/Web blank")

 law_plot <- ggplot(cali_law_df1, aes(x=Should.abortion.be.legal.or.legal.in.cases.of.rape.or.incest, fill=Response)) + geom_bar()

 cali_law_plot <- plot_ly(data = cali_law_df1, x = ~Should.abortion.be.legal.or.legal.in.cases.of.rape.or.incest, y = ~Should.abortion.be.legal.or.illegal.if.the.patient.s.life.is.endangered, color = ~Response, type = "scatter", mode = "markers")

 cali_law_df1_long <- cali_law_df1 %>% pivot_longer(!Response, names_to = "Type", values_to = "Count")

 cali_law_plot2 <- ggplot(cali_law_df1_long) + (mapping = aes(x = Type, y = Count, fill = Response)) + geom_col(position = position_dodge(0.7), width = 0.6, color = "black") + theme_minimal() + theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 45, vjust = 0.5, color = "gray33"), axis.text.y = element_text(color = "gray33"), axis.title = element_blank(), panel.grid.major.x = element_blank()) + scale_fill_manual(values = c("blue", "darkorange2", "gray", "red", "purple"))

 print(cali_law_plot2 + ggtitle("California Opinions on Abortion Legality"))

 cali_law_plot2 + theme(axis.text = element_text(size = 20))
 cali_law_plot2 + theme(axis.text = element_text(size = 10))
 print(cali_law_plot2 + ggtitle("California Opinions on Abortion Legality"))

 cali_law_plot2

 cali_law_plot2 + ggtitle("California Opinions on Abortion Legality") + theme(axis.text = element_text(size = 70)) + theme(axis.title = element_text(size = 40)) + theme(legend.text = element_text(size = 75)) + theme(plot.title = element_text(size = 200))
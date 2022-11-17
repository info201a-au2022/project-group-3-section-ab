final project plot code

R version 4.2.1 (2022-06-23 ucrt) -- "Funny-Looking Kid"
Copyright (C) 2022 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

> load("~/info201/project/workspace.RData")
plot_ly(data = cali_access_df1, x = ~Community.accessibility.to.family.planning.birth.control.services, y = ~Community.accesibility.to.abortion.services, color = ~response, type = "scatter", mode = "markers") %>% layout( title = "Community level of Accessibility to Family Planning and Birth Control Services", xaxis = list(title = "Community level of Accessibility to Family Planning and Birth Control Services", tickmode = "linear"), yaxis = list("Community level of Accessibility to Abortion Services", tickmode = "linear"))
> access_plot <- ggplot(cali_access_df1) + geom_col(mapping = aes(x = Community.accessibility.to.family.planning.birth.control.services, y = Community.accesibility.to.abortion.services, fill = response))
> access_plot
> print(access_plot + ggtitle("California Opinions on Acesss to Sexual Health Care"))
> print(access_plot + labs(y = "Community level of Accessibility to Family Planning and Birth Control Services", x = "Community level of Accessibility to Abortion Services"))
> print(access_plot + labs(colour = "Opinion Response"))
> access_plot
>                                             > print(access_plot + labs(y = "Community level of Accessibility to Family Planning and Birth Control Services", x = "Community level of Accessibility to Abortion Services"))
> View(cali_law_data)
> View(cali_law_df1)
> library("dplyr")

Attaching package: ‘dplyr’

The following objects are masked from ‘package:stats’:
  
  filter, lag

The following objects are masked from ‘package:base’:
  
  intersect, setdiff, setequal, union

Warning message:
  package ‘dplyr’ was built under R version 4.2.2 
> library("ggplot2")
Warning message:
  package ‘ggplot2’ was built under R version 4.2.2 
> library("plotly")

Attaching package: ‘plotly’

The following object is masked from ‘package:ggplot2’:
  
  last_plot

The following object is masked from ‘package:stats’:
  
  filter

The following object is masked from ‘package:graphics’:
  
  layout

Warning message:
  package ‘plotly’ was built under R version 4.2.2 
> library("tidyr")
Warning message:
  package ‘tidyr’ was built under R version 4.2.2 
> plot_ly(data = cali_access_df1, x = ~Community.accessibility.to.family.planning.birth.control.services, y = ~Community.accesibility.to.abortion.services, color = ~response, type = "scatter", mode = "markers") %>% layout( title = "Community level of Accessibility to Family Planning and Birth Control Services", xaxis = list(title = "Community level of Accessibility to Family Planning and Birth Control Services", tickmode = "linear"), yaxis = list("Community level of Accessibility to Abortion Services", tickmode = "linear"))
> access_plot <- ggplot(cali_access_df1) + geom_col(mapping = aes(x = Community.accessibility.to.family.planning.birth.control.services, y = Community.accesibility.to.abortion.services, fill = response))
> access_plot
> print(access_plot + ggtitle("California Opinions on Acesss to Sexual Health Care"))
> print(access_plot + labs(y = "Community level of Accessibility to Family Planning and Birth Control Services", x = "Community level of Accessibility to Abortion Services"))
> print(access_plot + labs(colour = "Opinion Response"))
> access_plot
>                                             > print(access_plot + labs(y = "Community level of Accessibility to Family Planning and Birth Control Services", x = "Community level of Accessibility to Abortion Services"))
Error: unexpected '>' in "                                            >"
> library("pscl")
Error in library("pscl") : there is no package called ‘pscl’
> library("stringr")
Warning message:
  package ‘stringr’ was built under R version 4.2.2 
> cali_law_df1_long <- cali_law_df1 %>% pivot_longer(!Response, names_to = "Type", values_to = "Count")
Error in `build_longer_spec()`:
  ! Can't subset columns that don't exist.
✖ Column `Response` doesn't exist.
Run `rlang::last_error()` to see where the error occurred.
> cali_law_df1$Response <- c("Legal", "Illegal", "Don't know", "Refused answer/Web blank")
> law_plot <- ggplot(cali_law_df1, aes(Response)) + geom_point(aes(y=Should.abortion.be.legal.or.legal.in.cases.of.rape.or.incest), color = "red") + geom_point(aes(y=Should.abortion.be.legal.or.illegal.if.the.patient.s.life.is.endangered), color = "green") + geom_point(aes(y=Should.abortion.be.legal.or.illegal.if.the.fetus.is.not.expected.to.survive), color = "blue") + geom_point(aes(y=Should.abortion.be.legal.or.illegal.if.the.fetus.is.expected.to.have.serious.birth.defects), color = "purple") + geom_point(aes(y=Should.abortion.be.legal.or.illegal.for.women.who.do.not.wish.to.be.pregnant), color = "orange")
> law_plot
> law_plot <- ggplot(cali_law_df1, aes(x=Should.abortion.be.legal.or.legal.in.cases.of.rape.or.incest, fill=Response)) + geom_bar()
> law_plot
> cali_law_plot <- plot_ly(data = cali_law_df1, x = ~Should.abortion.be.legal.or.legal.in.cases.of.rape.or.incest, y = ~Should.abortion.be.legal.or.illegal.if.the.patient.s.life.is.endangered, color = ~Response, type = "scatter", mode = "markers")
> law_plot
> cali_law_df1_long <- cali_law_df1 %>% pivot_longer(!Response, names_to = "Type", values_to = "Count")
> cali_law_plot2 <- ggplot(cali_law_df1_long) + geom_col(mapping = aes(x = Response, y = Count, fill = Type)) + geom_col(position = position_dodge(0.7), width = 0.6, color = "black") + theme_minimal() + theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 45, vjust = 0.5, color = "gray33"), axis.text.y = element_text(color = "gray33"), axis.title = element_blank(), panel.grid.major.x = element_blank()) + scale_fill_manual(values = c("blue", "darkorange2", "gray"))
> cali_law_plot2 <- ggplot(cali_law_df1_long) + (mapping = aes(x = Type, y = Count, fill = Response)) + geom_col(position = position_dodge(0.7), width = 0.6, color = "black") + theme_minimal() + theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 45, vjust = 0.5, color = "gray33"), axis.text.y = element_text(color = "gray33"), axis.title = element_blank(), panel.grid.major.x = element_blank()) + scale_fill_manual(values = c("blue", "darkorange2", "gray", "red", "purple")) + labs(title = "California Opinions on Sexual Health Laws", x = "Question Asked", y = "Question Response", color = "Responses")
> cali_law_plot2
> rm(cali_law_plot2)
> cali_law_plot
> cali_law_plot2 <- ggplot(cali_law_df1_long) + (mapping = aes(x = Response, y = Count, fill = Type)) + geom_col(position = position_dodge(0.7), width = 0.6, color = "black") + theme_minimal() + theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 45, vjust = 0.5, color = "gray33"), axis.text.y = element_text(color = "gray33"), axis.title = element_blank(), panel.grid.major.x = element_blank()) + scale_fill_manual(values = c("blue", "darkorange2", "gray", "red", "purple"))
> cali_law_plot2
> cali_law_plot2 <- ggplot(cali_law_df1_long) + (mapping = aes(x = Type, y = Count, fill = Response)) + geom_col(position = position_dodge(0.7), width = 0.6, color = "black") + theme_minimal() + theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 45, vjust = 0.5, color = "gray33"), axis.text.y = element_text(color = "gray33"), axis.title = element_blank(), panel.grid.major.x = element_blank()) + scale_fill_manual(values = c("blue", "darkorange2", "gray", "red", "purple"))
> 
> cali_law_plot2
> print(cali_law_plot2 + ggtitle("California Opinions on Abortion Legality"))
> cali_law_plot2(xpos, ypos, cex.axes = 2)
Error in cali_law_plot2(xpos, ypos, cex.axes = 2) : 
  could not find function "cali_law_plot2"
> cali_law_plot2 + theme(axis.text = element_text(size = 20))
> cali_law_plot2 + theme(axis.text = element_text(size = 10))
> cali_law_plot2
> print(cali_law_plot2 + ggtitle("California Opinions on Abortion Legality"))
> print(cali_law_plot2 + ggtitle("California Opinions on Abortion Legality") + theme(axis.text = element_text(size = 20))
+ cali_law_plot2
Error: unexpected symbol in:
"print(cali_law_plot2 + ggtitle("California Opinions on Abortion Legality") + theme(axis.text = element_text(size = 20))
       cali_law_plot2"
> cali_law_plot2
> cali_law_plot2 + ggtitle("California Opinions on Abortion Legality") + theme(text = element_text(size = 20))
> cali_law_plot2 + ggtitle("California Opinions on Abortion Legality") + theme(text = element_text(size = 100))
> cali_law_plot2 + ggtitle("California Opinions on Abortion Legality") + theme(text = element_text(size = 80))
> cali_law_plot2 + ggtitle("California Opinions on Abortion Legality") + theme(axis.text = element_text(size = 60)) + theme(axis.title = element_text(size = 65)) + theme(legend.text = element_text(size = 65)) + theme(plot.title = element_text(size = 50))
> cali_law_plot2 + ggtitle("California Opinions on Abortion Legality") + theme(axis.text = element_text(size = 60)) + theme(axis.title = element_text(size = 80)) + theme(legend.text = element_text(size = 65)) + theme(plot.title = element_text(size = 50))
> cali_law_plot2 + ggtitle("California Opinions on Abortion Legality") + theme(axis.text = element_text(size = 60)) + theme(axis.title = element_text(size = 200)) + theme(legend.text = element_text(size = 65)) + theme(plot.title = element_text(size = 50))
> cali_law_plot2 + ggtitle("California Opinions on Abortion Legality") + theme(axis.text = element_text(size = 70)) + theme(axis.title = element_text(size = 40)) + theme(legend.text = element_text(size = 75)) + theme(plot.title = element_text(size = 200))
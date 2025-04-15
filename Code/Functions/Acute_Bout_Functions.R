
options(scipen = 10)
#library(renv)
#renv::init()
#install.packages("readxl")
#install.packages("beepr")
library(beepr)
library(lme4)
library(car)
library(lmerTest)
library("readxl")
library(dplyr)
library(tidyr)
library(here)
library(snakecase)
library(ggplot2)
library(stringr)
library(writexl)
#renv::install("table1")
library(table1)
#renv::install("flextable")
#renv::install("officer")
library(flextable)
library(butils)
#install.packages("targets")
#library(targets)
#use_targets()
set.seed(42069)
#renv::install("DescTools")
library(DescTools)
library(LMMstar)
library(plotly)
library(multcomp)
library(beepr)
library(officer)

#set_here(path="C:/Users/mada0011/Desktop/Offline Statistics/Lupex-Statistics")
here::i_am("Code/Functions/Acute_Bout_Functions.R")




main_model_mult_comp2_table_emmeans<-function(x){
  multcomp2table(x,
                 transform = back2,
                 method = "glht",
                 method.multcomp="none",
                 link=c( "(Intercept)=0", #T-5
                         "(Intercept)+time_wd_glht0=0",
                         "(Intercept)+time_wd_glht10=0",
                         "(Intercept)+time_wd_glht14=0",
                         "(Intercept)+time_wd_glht35=0",
                         "(Intercept)+time_wd_glht45=0",
                         "(Intercept)+time_wd_glht60=0",
                         "(Intercept)+time_wd_glht90=0",
                         "(Intercept)+time_wd_glht105=0",
                         "(Intercept) + treat2control=0",#T-5
                         "(Intercept)+time_wd_glht0 + treat2control + time_wd_glht0:treat2control=0",
                         "(Intercept)+time_wd_glht10 + treat2control + time_wd_glht10:treat2control=0",
                         "(Intercept)+time_wd_glht14 + treat2control + time_wd_glht14:treat2control=0",
                         "(Intercept)+time_wd_glht35 + treat2control + time_wd_glht35:treat2control=0",
                         "(Intercept)+time_wd_glht45 + treat2control + time_wd_glht45:treat2control=0",
                         "(Intercept)+time_wd_glht60 + treat2control + time_wd_glht60:treat2control=0",
                         "(Intercept)+time_wd_glht90 + treat2control + time_wd_glht90:treat2control=0",
                         "(Intercept)+time_wd_glht105 + treat2control + time_wd_glht105:treat2control=0",
                         "(Intercept)+treat2exercise=0",#T-5
                         "(Intercept)+time_wd_glht0 + treat2exercise + time_wd_glht0:treat2exercise=0",
                         "(Intercept)+time_wd_glht10 + treat2exercise + time_wd_glht10:treat2exercise=0",
                         "(Intercept)+time_wd_glht14 + treat2exercise + time_wd_glht14:treat2exercise=0",
                         "(Intercept)+time_wd_glht35 + treat2exercise + time_wd_glht35:treat2exercise=0",
                         "(Intercept)+time_wd_glht45 + treat2exercise + time_wd_glht45:treat2exercise=0",
                         "(Intercept)+time_wd_glht60 + treat2exercise + time_wd_glht60:treat2exercise=0",
                         "(Intercept)+time_wd_glht90 + treat2exercise + time_wd_glht90:treat2exercise=0",
                         "(Intercept)+time_wd_glht105 + treat2exercise + time_wd_glht105:treat2exercise=0"
                 )
  )%>%
    mutate(
      time_within_day = rep(factor(x=c("-5","0","10","14","35","45","60","90","105"),
                                   levels = c("-5","0","10","14","35","45","60","90","105")),3),
      treat2 = c(rep("baseline",9),rep("control",9),rep("exercise",9)))
}

main_model_multcomp_table_contrasts<-
  function(x){

    multcomp2table(x,
                   transform = back2,
                   method = "glht",
                   method.multcomp="none",
                   link=c(
                     "treat2exercise - treat2control = 0",
                     "treat2exercise + time_wd_glht0:treat2exercise - treat2control - time_wd_glht0:treat2control = 0",
                     "treat2exercise + time_wd_glht10:treat2exercise - treat2control - time_wd_glht10:treat2control = 0",
                     "treat2exercise + time_wd_glht14:treat2exercise - treat2control - time_wd_glht14:treat2control = 0",
                     "treat2exercise + time_wd_glht35:treat2exercise - treat2control - time_wd_glht35:treat2control = 0",
                     "treat2exercise + time_wd_glht45:treat2exercise - treat2control - time_wd_glht45:treat2control = 0",
                     "treat2exercise + time_wd_glht60:treat2exercise - treat2control - time_wd_glht60:treat2control = 0",
                     "treat2exercise + time_wd_glht90:treat2exercise - treat2control - time_wd_glht90:treat2control = 0",
                     "treat2exercise + time_wd_glht105:treat2exercise - treat2control - time_wd_glht105:treat2control = 0"
                   )
    )%>%
      mutate(
        time_within_day = factor(x=c("-5","0","10","14","35","45","60","90","105"),
                                 levels = c("-5","0","10","14","35","45","60","90","105"))
      )
  }
### FUNCTION MAIN TO PLOT

model_main_to_plot <-
  function(x,
           type = "plot",
           bar = "crossbar",
           p_positions = c(3,3.66,4,3.33,3,3.66,4,3.33,3),
           rawdata = snake_case_Acute_Bout_Data){
    emmeans<-main_model_mult_comp2_table_emmeans(x)
    if (type == "emmeans"){
      return(emmeans)
      break
    }
    contrasts<-main_model_multcomp_table_contrasts(x)
    if(type == "contrast"){
      return(contrasts)
      break
    }
    if(
      length(p_positions) < 9){
      warning("p_positions less than 9, repeating to 9")
      if(length(p_positions) < 1){
        warning("p_positions less than 1, BREAK")
        break
      }
      p_positions <- rep(p_positions, 9/length(p_positions))[1:9]
    }
    if(
      length(p_positions) > 9){
      warning("p_positions more than 9, using 9 first values")
      p_positions <- p_positions[1:9]
    }
    plot<-
      emmeans%>%
      ggplot(aes(y = Estimate,
                 ymin = Lower,
                 ymax = Upper,
                 color = treat2,
                 x = as.numeric(as.character(time_within_day)),
                 group = treat2))+
      geom_line()+
      labs(y = x$outcome$var,
           x = "time during bout (min)",
           legend = "Treatment")+
      guides(
        color = guide_legend(title = "Treatment"))+
      geom_text(
        inherit.aes = F,
        data = contrasts,
        aes(
          x = as.numeric(as.character(time_within_day)),
          y = p_positions,
          label = paste0("p = ", p.value)

        ),
        position = position_dodge(2)
      )+
      theme_bw()

    if (bar == "errorbar"){
      plot <-
        plot+
        geom_errorbar(position = position_dodge(5))
    }
    if (bar == "crossbar"){
      plot <-
        plot+
        geom_crossbar(position = position_dodge(5))
    }

    if(type == "plot"){
      return(plot)
    }

    if(type == "point_plot"){
      return(
        plot +
          geom_point(
            data = rawdata,
            inherit.aes = F,
            aes_string(
              x = "continous_time_within_day",
              y = x$outcome$var,
              color = "treat2"),
            position=position_jitter(3),
            alpha = 0.2)
      )
    }
    {return(list(plot=plot, emmeans=emmeans, contrasts=contrasts))}
  }



terts<-function(x =
                  log2_IL_6_Acute_bout_interaction,
                var = "ifn_1_netto"){
  if (class(x) != "lmm"){
    warning("class must be lmm")
    break
  }
  low_ifn_1<-
    quantile(x$data[var], na.rm = T,
             probs = c(1/6,1/3+1/6,2/3+1/6))[1]
  mid_ifn_1<-
    quantile(x$data[var], na.rm = T,
             probs = c(1/6,1/3+1/6,2/3+1/6))[2]
  high_ifn_1<-
    quantile(x$data[var], na.rm = T,
             probs = c(1/6,1/3+1/6,2/3+1/6))[3]

  out_data =
    data.frame(
    )
  return(c(low_ifn_1, mid_ifn_1, high_ifn_1))
}


K<-model.tables(log2_IL_6_Acute_bout_interaction)%>%rownames()


smart_contrasts_interaction <-
  data.frame(
    name = K
  )

emmeans_interaction <-
  function(x){
    tertials<-terts(x)
    emmeans<-
      multcomp2table(x,
                     transform = back2,
                     method = "glht",
                     method.multcomp="none",
                     link=c(  #BASELINE
                       paste0("(Intercept)+ifn_1_netto", "*",as.character(tertials[1]),"= 0"),
                       paste0("(Intercept)+ifn_1_netto", "*",as.character(tertials[2]),"= 0"),
                       paste0("(Intercept)+ifn_1_netto", "*",as.character(tertials[3]),"= 0"),
                       paste0("(Intercept)+ifn_1_netto", "*",as.character(tertials[1]),
                              "+time_wd_glht0+",as.character(tertials[1]),
                              "*ifn_1_netto:time_wd_glht0", "= 0"),
                       paste0("(Intercept)+ifn_1_netto", "*",as.character(tertials[2]),
                              "+time_wd_glht0+",as.character(tertials[2]),
                              "*ifn_1_netto:time_wd_glht0", "= 0"),
                       paste0("(Intercept)+ifn_1_netto", "*",as.character(tertials[3]),
                              "+time_wd_glht0+",as.character(tertials[3]),
                              "*ifn_1_netto:time_wd_glht0", "= 0"),
                       paste0("(Intercept)+ifn_1_netto", "*",as.character(tertials[1]),
                              "+time_wd_glht10+",as.character(tertials[1]),
                              "*ifn_1_netto:time_wd_glht10", "= 0"),
                       paste0("(Intercept)+ifn_1_netto", "*",as.character(tertials[2]),
                              "+time_wd_glht10+",as.character(tertials[2]),
                              "*ifn_1_netto:time_wd_glht10", "= 0"),
                       paste0("(Intercept)+ifn_1_netto", "*",as.character(tertials[3]),
                              "+time_wd_glht10+",as.character(tertials[3]),
                              "*ifn_1_netto:time_wd_glht10", "= 0"),
                       paste0("(Intercept)+ifn_1_netto", "*",as.character(tertials[1]),
                              "+time_wd_glht14+",as.character(tertials[1]),
                              "*ifn_1_netto:time_wd_glht14", "= 0"),
                       paste0("(Intercept)+ifn_1_netto", "*",as.character(tertials[2]),
                              "+time_wd_glht14+",as.character(tertials[2]),
                              "*ifn_1_netto:time_wd_glht14", "= 0"),
                       paste0("(Intercept)+ifn_1_netto", "*",as.character(tertials[3]),
                              "+time_wd_glht14+",as.character(tertials[3]),
                              "*ifn_1_netto:time_wd_glht14", "= 0"),
                       paste0("(Intercept)+ifn_1_netto", "*",as.character(tertials[1]),
                              "+time_wd_glht35+",as.character(tertials[1]),
                              "*ifn_1_netto:time_wd_glht35", "= 0"),
                       paste0("(Intercept)+ifn_1_netto", "*",as.character(tertials[2]),
                              "+time_wd_glht35+",as.character(tertials[2]),
                              "*ifn_1_netto:time_wd_glht35", "= 0"),
                       paste0("(Intercept)+ifn_1_netto", "*",as.character(tertials[3]),
                              "+time_wd_glht35+",as.character(tertials[3]),
                              "*ifn_1_netto:time_wd_glht35", "= 0"),
                       paste0("(Intercept)+ifn_1_netto", "*",as.character(tertials[1]),
                              "+time_wd_glht45+",as.character(tertials[1]),
                              "*ifn_1_netto:time_wd_glht45", "= 0"),
                       paste0("(Intercept)+ifn_1_netto", "*",as.character(tertials[2]),
                              "+time_wd_glht45+",as.character(tertials[2]),
                              "*ifn_1_netto:time_wd_glht45", "= 0"),
                       paste0("(Intercept)+ifn_1_netto", "*",as.character(tertials[3]),
                              "+time_wd_glht45+",as.character(tertials[3]),
                              "*ifn_1_netto:time_wd_glht45", "= 0"),
                       paste0("(Intercept)+ifn_1_netto", "*",as.character(tertials[1]),
                              "+time_wd_glht60+",as.character(tertials[1]),
                              "*ifn_1_netto:time_wd_glht60", "= 0"),
                       paste0("(Intercept)+ifn_1_netto", "*",as.character(tertials[2]),
                              "+time_wd_glht60+",as.character(tertials[2]),
                              "*ifn_1_netto:time_wd_glht60", "= 0"),
                       paste0("(Intercept)+ifn_1_netto", "*",as.character(tertials[3]),
                              "+time_wd_glht60+",as.character(tertials[3]),
                              "*ifn_1_netto:time_wd_glht60", "= 0"),
                       paste0("(Intercept)+ifn_1_netto", "*",as.character(tertials[1]),
                              "+time_wd_glht90+",as.character(tertials[1]),
                              "*ifn_1_netto:time_wd_glht90", "= 0"),
                       paste0("(Intercept)+ifn_1_netto", "*",as.character(tertials[2]),
                              "+time_wd_glht90+",as.character(tertials[2]),
                              "*ifn_1_netto:time_wd_glht90", "= 0"),
                       paste0("(Intercept)+ifn_1_netto", "*",as.character(tertials[3]),
                              "+time_wd_glht90+",as.character(tertials[3]),
                              "*ifn_1_netto:time_wd_glht90", "= 0"),
                       paste0("(Intercept)+ifn_1_netto", "*",as.character(tertials[1]),
                              "+time_wd_glht105+",as.character(tertials[1]),
                              "*ifn_1_netto:time_wd_glht105", "= 0"),
                       paste0("(Intercept)+ifn_1_netto", "*",as.character(tertials[2]),
                              "+time_wd_glht105+",as.character(tertials[2]),
                              "*ifn_1_netto:time_wd_glht105", "= 0"),
                       paste0("(Intercept)+ifn_1_netto", "*",as.character(tertials[3]),
                              "+time_wd_glht105+",as.character(tertials[3]),
                              "*ifn_1_netto:time_wd_glht105", "= 0"),
                       #CONTROL
                       paste0("(Intercept)+treat2control+ifn_1_netto*",as.character(tertials[1]),
                              "+ifn_1_netto:time_wd_glhtmin5:treat2control*",as.character(tertials[1]),
                              "= 0"),
                       paste0("(Intercept)+treat2control+ifn_1_netto*",as.character(tertials[2]),
                              "+ifn_1_netto:time_wd_glhtmin5:treat2control*",as.character(tertials[2]),
                              "= 0"),
                       paste0("(Intercept)+treat2control+ifn_1_netto*",as.character(tertials[3]),
                              "+ifn_1_netto:time_wd_glhtmin5:treat2control*",as.character(tertials[3]),
                              "= 0"),
                       #control t = 0
                       paste0("(Intercept)+time_wd_glht0+treat2control+ifn_1_netto*",as.character(tertials[1]),
                              "+ifn_1_netto:time_wd_glht0:treat2control*",as.character(tertials[1]),
                              "= 0"),
                       paste0("(Intercept)+time_wd_glht0+treat2control+ifn_1_netto*",as.character(tertials[2]),
                              "+ifn_1_netto:time_wd_glht0:treat2control*",as.character(tertials[2]),
                              "= 0"),
                       paste0("(Intercept)+time_wd_glht0+treat2control+ifn_1_netto*",as.character(tertials[3]),
                              "+ifn_1_netto:time_wd_glht0:treat2control*",as.character(tertials[3]),
                              "= 0"),
                       #control t = 10
                       paste0("(Intercept)+time_wd_glht10+treat2control+ifn_1_netto*",as.character(tertials[1]),
                              "+ifn_1_netto:time_wd_glht10:treat2control*",as.character(tertials[1]),
                              "= 0"),
                       paste0("(Intercept)+time_wd_glht10+treat2control+ifn_1_netto*",as.character(tertials[2]),
                              "+ifn_1_netto:time_wd_glht10:treat2control*",as.character(tertials[2]),
                              "= 0"),
                       paste0("(Intercept)+time_wd_glht10+treat2control+ifn_1_netto*",as.character(tertials[3]),
                              "+ifn_1_netto:time_wd_glht10:treat2control*",as.character(tertials[3]),
                              "= 0"),
                       #control t = 14
                       paste0("(Intercept)+time_wd_glht14+treat2control+ifn_1_netto*",as.character(tertials[1]),
                              "+ifn_1_netto:time_wd_glht14:treat2control*",as.character(tertials[1]),
                              "= 0"),
                       paste0("(Intercept)+time_wd_glht14+treat2control+ifn_1_netto*",as.character(tertials[2]),
                              "+ifn_1_netto:time_wd_glht14:treat2control*",as.character(tertials[2]),
                              "= 0"),
                       paste0("(Intercept)+time_wd_glht14+treat2control+ifn_1_netto*",as.character(tertials[3]),
                              "+ifn_1_netto:time_wd_glht14:treat2control*",as.character(tertials[3]),
                              "= 0"),
                       #control t = 35
                       paste0("(Intercept)+time_wd_glht35+treat2control+ifn_1_netto*",as.character(tertials[1]),
                              "+ifn_1_netto:time_wd_glht35:treat2control*",as.character(tertials[1]),
                              "= 0"),
                       paste0("(Intercept)+time_wd_glht35+treat2control+ifn_1_netto*",as.character(tertials[2]),
                              "+ifn_1_netto:time_wd_glht35:treat2control*",as.character(tertials[2]),
                              "= 0"),
                       paste0("(Intercept)+time_wd_glht35+treat2control+ifn_1_netto*",as.character(tertials[3]),
                              "+ifn_1_netto:time_wd_glht35:treat2control*",as.character(tertials[3]),
                              "= 0"),
                       #control t = 45
                       paste0("(Intercept)+time_wd_glht45+treat2control+ifn_1_netto*",as.character(tertials[1]),
                              "+ifn_1_netto:time_wd_glht45:treat2control*",as.character(tertials[1]),
                              "= 0"),
                       paste0("(Intercept)+time_wd_glht45+treat2control+ifn_1_netto*",as.character(tertials[2]),
                              "+ifn_1_netto:time_wd_glht45:treat2control*",as.character(tertials[2]),
                              "= 0"),
                       paste0("(Intercept)+time_wd_glht45+treat2control+ifn_1_netto*",as.character(tertials[3]),
                              "+ifn_1_netto:time_wd_glht45:treat2control*",as.character(tertials[3]),
                              "= 0"),
                       #control t = 60
                       paste0("(Intercept)+time_wd_glht60+treat2control+ifn_1_netto*",as.character(tertials[1]),
                              "+ifn_1_netto:time_wd_glht60:treat2control*",as.character(tertials[1]),
                              "= 0"),
                       paste0("(Intercept)+time_wd_glht60+treat2control+ifn_1_netto*",as.character(tertials[2]),
                              "+ifn_1_netto:time_wd_glht60:treat2control*",as.character(tertials[2]),
                              "= 0"),
                       paste0("(Intercept)+time_wd_glht60+treat2control+ifn_1_netto*",as.character(tertials[3]),
                              "+ifn_1_netto:time_wd_glht60:treat2control*",as.character(tertials[3]),
                              "= 0"),
                       #control t = 90
                       paste0("(Intercept)+time_wd_glht90+treat2control+ifn_1_netto*",as.character(tertials[1]),
                              "+ifn_1_netto:time_wd_glht90:treat2control*",as.character(tertials[1]),
                              "= 0"),
                       paste0("(Intercept)+time_wd_glht90+treat2control+ifn_1_netto*",as.character(tertials[2]),
                              "+ifn_1_netto:time_wd_glht90:treat2control*",as.character(tertials[2]),
                              "= 0"),
                       paste0("(Intercept)+time_wd_glht90+treat2control+ifn_1_netto*",as.character(tertials[3]),
                              "+ifn_1_netto:time_wd_glht90:treat2control*",as.character(tertials[3]),
                              "= 0"),
                       #control t = 105
                       paste0("(Intercept)+time_wd_glht105+treat2control+ifn_1_netto*",as.character(tertials[1]),
                              "+ifn_1_netto:time_wd_glht105:treat2control*",as.character(tertials[1]),
                              "= 0"),
                       paste0("(Intercept)+time_wd_glht105+treat2control+ifn_1_netto*",as.character(tertials[2]),
                              "+ifn_1_netto:time_wd_glht105:treat2control*",as.character(tertials[2]),
                              "= 0"),
                       paste0("(Intercept)+time_wd_glht105+treat2control+ifn_1_netto*",as.character(tertials[3]),
                              "+ifn_1_netto:time_wd_glht105:treat2control*",as.character(tertials[3]),
                              "= 0"),
                       #exercise
                       paste0("(Intercept)+treat2exercise+ifn_1_netto*",as.character(tertials[1]),
                              "+ifn_1_netto:time_wd_glhtmin5:treat2exercise*",as.character(tertials[1]),
                              "= 0"),
                       paste0("(Intercept)+treat2exercise+ifn_1_netto*",as.character(tertials[2]),
                              "+ifn_1_netto:time_wd_glhtmin5:treat2exercise*",as.character(tertials[2]),
                              "= 0"),
                       paste0("(Intercept)+treat2exercise+ifn_1_netto*",as.character(tertials[3]),
                              "+ifn_1_netto:time_wd_glhtmin5:treat2exercise*",as.character(tertials[3]),
                              "= 0"),
                       #exercise t = 0
                       paste0("(Intercept)+time_wd_glht0+treat2exercise+ifn_1_netto*",as.character(tertials[1]),
                              "+ifn_1_netto:time_wd_glht0:treat2exercise*",as.character(tertials[1]),
                              "= 0"),
                       paste0("(Intercept)+time_wd_glht0+treat2exercise+ifn_1_netto*",as.character(tertials[2]),
                              "+ifn_1_netto:time_wd_glht0:treat2exercise*",as.character(tertials[2]),
                              "= 0"),
                       paste0("(Intercept)+time_wd_glht0+treat2exercise+ifn_1_netto*",as.character(tertials[3]),
                              "+ifn_1_netto:time_wd_glht0:treat2exercise*",as.character(tertials[3]),
                              "= 0"),
                       #exercise t = 10
                       paste0("(Intercept)+time_wd_glht10+treat2exercise+ifn_1_netto*",as.character(tertials[1]),
                              "+ifn_1_netto:time_wd_glht10:treat2exercise*",as.character(tertials[1]),
                              "= 0"),
                       paste0("(Intercept)+time_wd_glht10+treat2exercise+ifn_1_netto*",as.character(tertials[2]),
                              "+ifn_1_netto:time_wd_glht10:treat2exercise*",as.character(tertials[2]),
                              "= 0"),
                       paste0("(Intercept)+time_wd_glht10+treat2exercise+ifn_1_netto*",as.character(tertials[3]),
                              "+ifn_1_netto:time_wd_glht10:treat2exercise*",as.character(tertials[3]),
                              "= 0"),
                       #exercise t = 14
                       paste0("(Intercept)+time_wd_glht14+treat2exercise+ifn_1_netto*",as.character(tertials[1]),
                              "+ifn_1_netto:time_wd_glht14:treat2exercise*",as.character(tertials[1]),
                              "= 0"),
                       paste0("(Intercept)+time_wd_glht14+treat2exercise+ifn_1_netto*",as.character(tertials[2]),
                              "+ifn_1_netto:time_wd_glht14:treat2exercise*",as.character(tertials[2]),
                              "= 0"),
                       paste0("(Intercept)+time_wd_glht14+treat2exercise+ifn_1_netto*",as.character(tertials[3]),
                              "+ifn_1_netto:time_wd_glht14:treat2exercise*",as.character(tertials[3]),
                              "= 0"),
                       #exercise t = 35
                       paste0("(Intercept)+time_wd_glht35+treat2exercise+ifn_1_netto*",as.character(tertials[1]),
                              "+ifn_1_netto:time_wd_glht35:treat2exercise*",as.character(tertials[1]),
                              "= 0"),
                       paste0("(Intercept)+time_wd_glht35+treat2exercise+ifn_1_netto*",as.character(tertials[2]),
                              "+ifn_1_netto:time_wd_glht35:treat2exercise*",as.character(tertials[2]),
                              "= 0"),
                       paste0("(Intercept)+time_wd_glht35+treat2exercise+ifn_1_netto*",as.character(tertials[3]),
                              "+ifn_1_netto:time_wd_glht35:treat2exercise*",as.character(tertials[3]),
                              "= 0"),
                       #exercise t = 45
                       paste0("(Intercept)+time_wd_glht45+treat2exercise+ifn_1_netto*",as.character(tertials[1]),
                              "+ifn_1_netto:time_wd_glht45:treat2exercise*",as.character(tertials[1]),
                              "= 0"),
                       paste0("(Intercept)+time_wd_glht45+treat2exercise+ifn_1_netto*",as.character(tertials[2]),
                              "+ifn_1_netto:time_wd_glht45:treat2exercise*",as.character(tertials[2]),
                              "= 0"),
                       paste0("(Intercept)+time_wd_glht45+treat2exercise+ifn_1_netto*",as.character(tertials[3]),
                              "+ifn_1_netto:time_wd_glht45:treat2exercise*",as.character(tertials[3]),
                              "= 0"),
                       #exercise t = 60
                       paste0("(Intercept)+time_wd_glht60+treat2exercise+ifn_1_netto*",as.character(tertials[1]),
                              "+ifn_1_netto:time_wd_glht60:treat2exercise*",as.character(tertials[1]),
                              "= 0"),
                       paste0("(Intercept)+time_wd_glht60+treat2exercise+ifn_1_netto*",as.character(tertials[2]),
                              "+ifn_1_netto:time_wd_glht60:treat2exercise*",as.character(tertials[2]),
                              "= 0"),
                       paste0("(Intercept)+time_wd_glht60+treat2exercise+ifn_1_netto*",as.character(tertials[3]),
                              "+ifn_1_netto:time_wd_glht60:treat2exercise*",as.character(tertials[3]),
                              "= 0"),
                       #exercise t = 90
                       paste0("(Intercept)+time_wd_glht90+treat2exercise+ifn_1_netto*",as.character(tertials[1]),
                              "+ifn_1_netto:time_wd_glht90:treat2exercise*",as.character(tertials[1]),
                              "= 0"),
                       paste0("(Intercept)+time_wd_glht90+treat2exercise+ifn_1_netto*",as.character(tertials[2]),
                              "+ifn_1_netto:time_wd_glht90:treat2exercise*",as.character(tertials[2]),
                              "= 0"),
                       paste0("(Intercept)+time_wd_glht90+treat2exercise+ifn_1_netto*",as.character(tertials[3]),
                              "+ifn_1_netto:time_wd_glht90:treat2exercise*",as.character(tertials[3]),
                              "= 0"),
                       #exercise t = 105
                       paste0("(Intercept)+time_wd_glht105+treat2exercise+ifn_1_netto*",as.character(tertials[1]),
                              "+ifn_1_netto:time_wd_glht105:treat2exercise*",as.character(tertials[1]),
                              "= 0"),
                       paste0("(Intercept)+time_wd_glht105+treat2exercise+ifn_1_netto*",as.character(tertials[2]),
                              "+ifn_1_netto:time_wd_glht105:treat2exercise*",as.character(tertials[2]),
                              "= 0"),
                       paste0("(Intercept)+time_wd_glht105+treat2exercise+ifn_1_netto*",as.character(tertials[3]),
                              "+ifn_1_netto:time_wd_glht105:treat2exercise*",as.character(tertials[3]),
                              "= 0")
                     ))
    emmeans<-
      emmeans%>%
      mutate(
        tert = rep(factor(c("0-33%","33-66%","66-100%"),
                          levels = c("0-33%","33-66%","66-100%")), length(rownames(emmeans))/3),
        timepoint = factor(c(rep("baseline",27),rep("followup",54)
        ), levels=c("baseline","followup") ),
        treat2 = factor(c(rep("baseline",27), rep("control",27), rep("exercise",27)
        ), levels=c("baseline","control","exercise") ),

        time_within_day = rep(factor(x=sort(rep(c(-5,0,10,14,35,45,60,90,105),3)),
                                     levels = c("-5","0","10","14","35","45","60","90","105")),3)
      )
    return(emmeans)
  }

contrasts_interaction<-
  function(x){

    multcomp2table(x,
                   transform = back2,
                   method = "glht",
                   method.multcomp="none",
                   link=c(
                     "treat2exercise + ifn_1_netto:time_wd_glhtmin5:treat2exercise - treat2control - ifn_1_netto:time_wd_glhtmin5:treat2control = 0",
                     "treat2exercise + time_wd_glht0:treat2exercise +ifn_1_netto:time_wd_glht0:treat2exercise - treat2control - time_wd_glht0:treat2control = 0",
                     "treat2exercise + time_wd_glht10:treat2exercise + ifn_1_netto:time_wd_glht10:treat2exercise - treat2control - time_wd_glht10:treat2control - ifn_1_netto:time_wd_glht10:treat2control = 0",

                     "treat2exercise + time_wd_glht14:treat2exercise - treat2control - time_wd_glht14:treat2control - ifn_1_netto:time_wd_glht14:treat2control + ifn_1_netto:time_wd_glht14:treat2exercise= 0",

                     "treat2exercise + ifn_1_netto:time_wd_glht35:treat2exercise - treat2control - time_wd_glht35:treat2control - ifn_1_netto:time_wd_glht35:treat2control + time_wd_glht35:treat2exercise= 0",
                     "treat2exercise + ifn_1_netto:time_wd_glht45:treat2exercise - treat2control - time_wd_glht45:treat2control - ifn_1_netto:time_wd_glht45:treat2control + time_wd_glht45:treat2exercise= 0",
                     "treat2exercise + ifn_1_netto:time_wd_glht60:treat2exercise - treat2control - time_wd_glht60:treat2control - ifn_1_netto:time_wd_glht60:treat2control + time_wd_glht60:treat2exercise= 0",
                     "treat2exercise + ifn_1_netto:time_wd_glht90:treat2exercise - treat2control - time_wd_glht90:treat2control - ifn_1_netto:time_wd_glht90:treat2control + time_wd_glht90:treat2exercise= 0",
                     "treat2exercise + time_wd_glht105:treat2exercise - treat2control - time_wd_glht105:treat2control - ifn_1_netto:time_wd_glht105:treat2control + ifn_1_netto:time_wd_glht105:treat2exercise= 0"
                   )
    )%>%
      mutate(
        time_within_day = factor(x=c("-5","0","10","14","35","45","60","90","105"),
                                 levels = c("-5","0","10","14","35","45","60","90","105"))
      )
  }



model_interaction_to_plot <-  function(x,
                                       type = "plot",
                                       p_positions = rep(c(2,3,4),9),
                                       bar = "crossbar",
                                       rawdata = snake_case_Acute_Bout_Data) {
  emmeans<-emmeans_interaction(x)
  if (type == "emmeans"){
    return(emmeans)
    break
  }

  plot<-
    emmeans%>%
    ggplot(aes(y = Estimate,
               ymin = Lower,
               ymax = Upper,
               color = treat2,
               x = as.numeric(as.character(time_within_day)),
               group = treat2))+
    geom_line()+
    labs(y = x$outcome$var,
         x = "time during bout (min)",
         legend = "Treatment")+
    guides(
      color = guide_legend(title = "Treatment"))+
    theme_bw()+
    facet_wrap(~tert)

  if (bar == "errorbar"){
    plot<-plot+
      geom_errorbar(position = position_dodge(5))
  }
  if (bar == "crossbar"){
    plot<-plot+
      geom_crossbar(position = position_dodge(5))
  }

  contrasts<-contrasts_interaction(x)
  if(type == "contrast"){
    return(contrasts)
    break
  }

  if(!is.na(p_positions) ){
    if(
      length(p_positions) < 27){
      warning("p_positions less than 27, repeating to 27")
      if(length(p_positions) < 1){
        warning("p_positions less than 1, BREAK")
        break
      }
      p_positions <- rep(p_positions, 27/length(p_positions))[1:27]
    }
    if(
      length(p_positions) > 27){
      warning("p_positions more than 9, using 9 first values")
      p_positions <- p_positions[1:27]
    }

    plot<-
      plot+
      geom_text(
        inherit.aes = F,
        data = contrasts,
        aes(
          x = as.numeric(as.character(time_within_day)),
          y = p_positions,
          label = paste0("p = ", p.value)
        ),
        position = position_dodge(2))}

  if(type == "plot"){
    return(plot)
  } else if(type == "point_plot"){
    return(
      plot +
        geom_point(
          data = rawdata,
          inherit.aes = F,
          aes_string(
            x = "continous_time_within_day",
            y = x$outcome$var,
            color = "treat2"),
          position=position_jitter(3),
          alpha = 0.2)
    )
  } else
  {return(list(plot=plot, emmeans=emmeans, contrasts=contrasts))}
}

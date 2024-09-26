
library(renv)
library(readxl)
library(dplyr)
library(here)
library(snakecase)
library(lubridate)
library(ggplot2)
library(car)
library(writexl)
library(LMMstar)
library(plotly)
library(stringr)
#renv::install("BSDA")
#library(BSDA)
#renv::init()
#renv::install("zoo")
library(zoo)
#install.packages("targets")
#library(targets)
#use_targets()
set.seed(42069)
here()


getandtestvo2max <- function(
    directory ,
    metadata ,
    warmup.period = 300,
    shortest.analysed.test = warmup.period+120,
    sd.to.search.over = 2,
    end.period = 30,
    lag_time_stable_to_end = 0,
    id_start=1, #filenames must contain the ID at the points indicated by id_start and id_stop
    id_stop=6,
    width_to_sum_in_sec = 30,
    significant_difference_slope = 0.05,
    metadata_id = "id",
    metadata_timepoint = "timepoint", #this is to make sure you match the timepoint from the metadata with the name of the folder that the directory points to.
    #atm. TIMEPOINT MUST BE LOWER CASE
    metadata_borg = "borg",
    metadata_time_seconds = "VO2_test_time_seconds",
    outpath = getwd(),
    dataframes = T,
    graph = FALSE,
    graph_filetype = ".png",
    models = FALSE,
    anonymize = T,
    method = "poole", #atm, my own method doesn't work that well, it crashes, often, and through numerous bug fixes, it still doesn't fit a line that well. Pooles is a lot easier to implement and works reasonably well.
    minimum.stable.period = 120,
    poolecutoffs =c(120,360)){

  #Find current timepoint from directory, this is
  curr_timepoint <- tolower(word(directory, start = -1, sep = "/"))


  #Create out_data frame
  out_data <- data.frame(
    id = c(to_snake_case(stringr::str_sub(dir(directory), id_start, id_stop))),
    from_file = dir(directory),
    timepoint = curr_timepoint,
    borg = NA_real_,
    test.time = NA_real_,
    aerobiccapacity = NA_real_,
    VO2max = NA_real_,
    HRmax = NA_real_,
    MaxOxygenPulse = NA_real_,
    maxRQ = NA_real_
  )

  #This creates new variables in the metadata dataset, because I didn't know how to refer to them else.
  metadata$id<-metadata[metadata_id]
  metadata$timepoint<-metadata[metadata_timepoint]

  #If we need to create graphs or models, lets make some folders for that, also only create out_list if it's needed
  if(graph){
    dir.create(file.path(outpath, "/graphics"), showWarnings = FALSE)
    #  out_list <- list()
  }

  if(models){
    dir.create(file.path(outpath, "/models"), showWarnings = FALSE)
    # out_list <- list()
  }


  for (filename in dir(directory)) {
    #Looping through the filenames in directory, filename must contain the ID at the points indicated by id_start and id_stop
    curr_id <- to_snake_case(stringr::str_sub(filename, id_start, id_stop))
    p<-paste0(directory,"/",filename)
    df<-suppressWarnings(
      read_excel( path=p,
                  range = c("J1:BZ300"), col_names = T , col_types = "numeric")%>%
        filter(!row_number() %in% c(1, 2))%>%
        as.data.frame()%>%dplyr::select(-contains("...")))

    if (is.null(dim(df))|is.na(colnames(df)[1])){next}

    colnames(df)[1] <- "time.mad.excel.format"
    colnames(df)[colnames(df)=="VO2/Kg"]<-"VO2_per_kg"
    colnames(df)[colnames(df)=="VO2/kg"]<-"VO2_per_kg"

    #The metadata has the end of each test.
    test.time<-metadata%>%dplyr::filter( id==curr_id & timepoint==curr_timepoint)%>%pull(metadata_time_seconds)


    df<-df%>%
      mutate(
        time.seconds = round(time.mad.excel.format*86400 ,0)
      )%>%filter(rowSums(is.na(.)) != ncol(.))
    #filter(
    #  time.seconds <=test.time
    #)

    wvector<-sapply(seq_along(df$time.seconds), FUN=
                      function(i) {which(df$time.seconds[-(seq_len(i))] >=
                                           df$time.seconds[i]+width_to_sum_in_sec)[1]})
    wvector<-case_when(is.na(wvector) ~ 6,
                       T ~ wvector)
    df<-df%>%mutate(
      from_file = filename,
      id = curr_id,
      time.minuts = round(time.seconds/60,2),
      timepoint = curr_timepoint,
      aerob30secsum = #TODO clear up that this period depends on the input
        zoo::rollapply(VO2_per_kg,
                       width = wvector,
                       FUN = mean,
                       align="left",
                       fill =NA_real_),
      vo2.30.sec.sum =
        zoo::rollapply(`VO2`,
                       width = wvector,
                       FUN = mean,
                       align="left",
                       fill =NA_real_)  )

    dato=suppressWarnings(
      read_excel(path=p,
                 range = "E1:E1", col_names = F, col_types="text")[1,1])


    height = suppressWarnings(
      read_excel( path=p,
                  range = "B6:B6", col_types="numeric",col_names = F)[1,1])


    weight = suppressWarnings(
      read_excel( path=p,
                  range = "B7:B7", col_types="numeric",col_names = F)[1,1]
    )


    vo2max<-max(df$aerob30secsum, na.rm =T)
    time_vo2max <- df$time.seconds[min(which(df$aerob30secsum==vo2max))]

    if(!is.null(outpath)){
      if(dataframes){
        if(anonymize){df$from_file<-NULL}
        write_xlsx(x=df,
                   path = paste0(outpath,"/VO2maxWrittenByR_", curr_id,"_",curr_timepoint,".xlsx"))}}


    borg<-metadata%>%dplyr::filter( id==curr_id & timepoint==curr_timepoint)%>%pull(metadata_borg)
    #Write the data we have to the out_data, the unique identifier is filename. It should be noted that the function will do silly things if two files have the same name, but I doubt they can.
    out_data$id[out_data$from_file==filename] = curr_id
    out_data$timepoint[out_data$from_file==filename]  = curr_timepoint
    out_data$aerobiccapacity[out_data$from_file==filename]  = max(df$aerob30secsum, na.rm =T)
    out_data$VO2max[out_data$from_file==filename]  = max(df$vo2.30.sec.sum, na.rm =T)
    out_data$HRmax[out_data$from_file==filename]  = max(df$HR, na.rm =T)
    out_data$MaxOxygenPulse[out_data$from_file==filename]  = max(df$`VO2/HR`, na.rm =T)
    out_data$maxRQ[out_data$from_file==filename]  = max(df$RQ, na.rm =T)
    out_data$height[out_data$from_file==filename]  = height
    out_data$weight[out_data$from_file==filename]  = weight
    out_data$date[out_data$from_file==filename]  = dato
    out_data$borg[out_data$from_file==filename] = borg
    out_data$test.time[out_data$from_file==filename] = test.time

    out_data$vo2maxaftertestend[out_data$from_file==filename] = df$time.seconds[match(max(df$aerob30secsum,na.rm=T),df$aerob30secsum)] > test.time
    if(df$time.seconds[match(max(df$aerob30secsum,na.rm=T),df$aerob30secsum)] > test.time){next}


    df.warmup <- df %>% filter(time.seconds<=warmup.period)
    mean.VO2kg.warmup <- mean(df.warmup$`VO2/Kg`, na.rm = T)
    sd.VO2kg.warmup <- sd(df.warmup$`VO2/Kg`, na.rm = T)

    if (is.null(shortest.analysed.test)){
      shortest.analysed.test=warmup.period+120}

    #If maximal aerobic capacity is after the shortest analysed test, analyse it, if not, note it and break
    if(df$time.seconds[match(max(df$aerob30secsum, na.rm =T), df$aerob30secsum)]>=shortest.analysed.test){

      #if the test is longer than the shortest analysed test analyse it, if not note it and break
      if(max(df$time.seconds,na.rm=T)>=shortest.analysed.test ){
        #The tests especially Pooles need a test.time to run, so we'll define it this way if it isn't defined.
        if(is.na(test.time)){
          test.time <- max(df$time.seconds, na.rm =T)
          if(test.time<warmup.period){next}}
        #Pooles method is quite simple, he takes the time 6 to 2 minutes before the end of the test, creates a line, then creates a new line at the last 2 minutes and compares the regressions.

        if(method == "poole"){

          df.stable <- df%>%
            dplyr::filter(time.seconds >= warmup.period)%>%
            dplyr::filter(time.seconds >= test.time-poolecutoffs[2]) %>%
            dplyr::filter(time.seconds < test.time-poolecutoffs[1]) %>%
            mutate(period = "stable")
          df.end <- df%>%
            dplyr::filter(time.seconds >= test.time-poolecutoffs[1])%>%
            dplyr::filter(time.seconds <= test.time)%>%
            mutate(period = "end")
        }
        if(method == "MLA"){
          #  df.no.warmup <- df%>%dplyr::filter(time.seconds > warmup.period)

          #Below is the complicated code I tried to implement, I have made a simpler solution further below
          #Find timepoint were vo2 is above x SD of warmup period. (So we dont start the curve flat)

          #if( mean.VO2kg.warmup +
          #    sd.to.search.over*
          #    sd.VO2kg.warmup         < vo2max){
          #  time_vo2_over_warmup <-
          #    df.no.warmup$time.seconds[min(which(df.no.warmup$aerob30secsum >
          #                                          mean.VO2kg.warmup +
          #                                          sd.to.search.over*
          #                                          sd.VO2kg.warmup))]} else {
          #If the test never gets a VO2max thats 2SD abvoe the mean in the warmup. The stable and end periods are likely similar, so we just start the VO2max at the end of the warmup period
          # time_vo2_over_warmup <- warmup.period  }


          #  if (time_vo2_over_warmup < test.time -
          #                              end.period -
          #                              lag_time_stable_to_end &
          #      time_vo2_over_warmup > warmup.period){
          #    stable_start_time <- time_vo2_over_warmup
          #  }else{
          #    stable_start_time <- warmup.period
          #  }
          #Find timepoint were VO2 is within x SD of VO2max (so we dont end the curve at plateau)
          #  first_time_vo2_within_x_sd_of_vo2max <-
          #    df.no.warmup$time.seconds[min(which(df.no.warmup$aerob30secsum >
          #                                          vo2max -
          #                                          sd.to.search.over*
          #                                          sd.VO2kg.warmup))]


          #If the stable period is less than the minimum allowed stable period, extend it to the minimum allowed stable period.
          #  if (
          #    first_time_vo2_within_x_sd_of_vo2max - stable_start_time < minimum.stable.period
          #  ) {
          #    stable_end_time <- stable_start_time + minimum.stable.period
          #  } else { #Else, use the first time vo2max is within x sd of VO2max as the end for the stable
          #    stable_end_time <- first_time_vo2_within_x_sd_of_vo2max
          #  }

          #  df.stable <- df%>%
          #    dplyr::filter(
          #    time.seconds >= stable_start_time) %>%
          #    dplyr::filter( #The lag is deducted here, instead of deducting it higher up and adding it again to the end.
          #    time.seconds < stable_end_time - lag_time_stable_to_end
          #  )

          #Define the plateau at the end, it should be all times that are less than test.time & less than 30 sec after   the VO2max. If the VO2 falls below VO2max - 2SD it is probably a mask failure at test end and should be removed.

          #Find the time after VO2max were VO2 is more than x sd under vo2max
          #  df.after.max <- df %>% dplyr::filter(
          #    time.seconds >= time_vo2max)
          #  time_vo2_falls_again <- df.after.max$time.seconds[min(which(
          #    df.after.max$aerob30secsum <  vo2max -
          #                                  sd.to.search.over*
          #                                  sd.VO2kg.warmup))]
          #
          #  plateau_end <- min(c(time_vo2max + end.period,
          #                       test.time,
          #                       time_vo2_falls_again),
          #                     na.rm =T)


          #if the plateau is less than the end period, expand it to end period
          #  if(plateau_end - stable_end_time < end.period){
          #    plateau_end <- stable_end_time + end.period
          # }

          #  df.end <- df %>%
          #    dplyr::filter(
          #    time.seconds >= stable_end_time
          #  ) %>%dplyr::filter(
          #    time.seconds <= plateau_end
          #  )


          #Method MLA Simplified
          df.stable <- df %>%
            dplyr::filter(
              time.seconds >= warmup.period)%>%
            dplyr::filter(
              time.seconds < time_vo2max - end.period - lag_time_stable_to_end)


          df.end <- df %>%
            dplyr::filter(
              time.seconds >= time_vo2max - end.period
            ) %>% dplyr::filter(
              time.seconds <= time_vo2max + end.period
            ) %>% dplyr::filter(
              time.seconds <= test.time
            )
        }
        #Okay so we've defined which data should be included in each data frame. Which is really the difference between Pooles method and mine. Poole then suggest using the same method for validation as CS uses, but I used a significance testing approach which is a little more lenient.
        if (nrow(df.end)<2){next} #We are fitting two linear models, we need at least 2 data points in both DFs
        if (nrow(df.stable)<2){next}
        if (min(df.end$time.seconds,na.rm = T) > 6000) {next}
        if (max(df.end$time.seconds,na.rm = T) < 0) {next}

        df.end <- df.end %>%mutate(period = "end")
        df.stable<-df.stable %>% mutate (period = "stable")

        df.joined <- rbind(
          df.end , df.stable) %>%
          mutate(period = factor( period, levels = c("stable", "end")))

        lm.joined <- lm(aerob30secsum ~ period+time.seconds+time.seconds:period, data = df.joined)

        require(car)
        anova.lm.joined<-car::Anova(lm.joined, type = 3, singular.ok = T)
        p_diff <- anova.lm.joined$`Pr(>F)`[4]
        end.steeper.than.stable <- lm.joined$coefficients[4] > 0


        if(graph){
          graphic<-df.joined %>%ggplot(aes(x=time.seconds, y = aerob30secsum, colour=period)) + geom_point()+stat_smooth(method="lm")
          ggsave(filename = paste0(outpath , "/graphics/","graph_",curr_timepoint,"_",curr_id,graph_filetype),
                 plot = graphic)
          #I wanted to output a list, it didn't really work. Especially with the ggplot objects
          #curr_graph <- list(data.frame(timepoint=curr_timepoint,id=curr_id, graph=graphic))
          #out_list<-append(out_list,curr_graph)

        }

        if(models){
          saveRDS(object=lm.joined, file = paste0(outpath,"/models/",curr_timepoint,"_",curr_id,".rds" ))
          #I wanted to output a list, it didn't really work. Especially with the ggplot objects
          #curr_model <- list(data.frame(matrix(timepoint=curr_timepoint,id=curr_id, lm=lm.joined)))
          #out_list<-append(out_list,curr_model)
        }

        lmtestMLA <- case_when(end.steeper.than.stable ~ F,
                               p_diff < significant_difference_slope ~ T,
                               T ~ F)
        end.half.stable <- lm.joined$coefficients[4]+lm.joined$coefficients[3]<=(lm.joined$coefficients[3])/2


        lmtestCS <- end.half.stable #Also Pooles method


        out_data$lmtestMLA[out_data$from_file==filename] <- lmtestMLA
        out_data$lmtestCS[out_data$from_file==filename] <- lmtestCS
        out_data$endsteep[out_data$from_file==filename]  <- end.steeper.than.stable
        out_data$end.half.stable[out_data$from_file==filename]  <- end.half.stable
        out_data$tooshort[out_data$from_file==filename]  <- F
        out_data$aerobmaxinwarmup[out_data$from_file==filename]  <- F

      } else{
        out_data$tooshort[out_data$from_file==filename]  <- T
      }
    } else {
      out_data$aerobmaxinwarmup[out_data$from_file==filename]  <- T
    }
  }
  if (method == "MLA"){ #The main difference between mine and CS version is that I weigh the lmtest less,
    #because I don't think plateau can ever be decided in a yes/no question and therefore use the other guidelines equivalently.
    out_data<-out_data %>% mutate(
      Quality_Score_MLA =
        case_when(
          borg >= 17 ~ 1,
          borg < 17 ~ 0,
          T~NA_real_) +
        case_when (lmtestMLA ~ 1,
                   lmtestMLA == F ~ 0,
                   T~NA_real_) +
        case_when ( maxRQ >=1.1 ~ 1,
                    maxRQ <1.1 ~ 0,
                    T~NA_real_),

      QualityMeasurementMLA = Quality_Score_MLA >= 2)
  }
  out_data<-out_data %>% mutate(Quality_Score_CS =
                                  case_when(
                                    borg >= 17 ~ 1,
                                    borg < 17 ~ 0,
                                    T~NA_real_) +
                                  case_when (lmtestCS ~ 2,
                                             lmtestCS == F ~ 0,
                                             T~NA_real_) +
                                  case_when ( maxRQ >=1.1 ~ 1,
                                              maxRQ <1.1 ~ 0,
                                              T~NA_real_),
                                QualityMeasurementCS = Quality_Score_CS >= 2)


  return(out_data)
}

#-----------------------------------------------------------------

#                               VSLOPE

#-----------------------------------------------------------------


vslope <- function(
    directory ,
    metadata ,
    warmup.period = 300,
    shortest.analysed.test = warmup.period+120,
    sd.to.search.over = 2,
    end.period = 30,
    lag_time_stable_to_end = 0,
    id_start=1, #filenames must contain the ID at the points indicated by id_start and id_stop
    id_stop=6,
    width_to_sum_in_sec = 30,
    significant_difference_slope = 0.05,
    metadata_id = "id",
    metadata_timepoint = "timepoint", #this is to make sure you match the timepoint from the metadata with the name of the folder that the directory points to.
    #atm. TIMEPOINT MUST BE LOWER CASE
    metadata_borg = "borg",
    metadata_time_seconds = "VO2_test_time_seconds",
    outpath = getwd(),
    dataframes = T,
    graph = FALSE,
    graph_filetype = ".png",
    models = FALSE,
    anonymize = T,
    method = "poole", #atm, my own method doesn't work that well, it crashes, often, and through numerous bug fixes, it still doesn't fit a line that well. Pooles is a lot easier to implement and works reasonably well.
    minimum.stable.period = 120,
    poolecutoffs =c(120,360)){

  #Find current timepoint from directory, this is
  curr_timepoint <- tolower(word(directory, start = -1, sep = "/"))


  #Create out_data frame
  out_data <- data.frame(
    id = c(to_snake_case(stringr::str_sub(dir(directory), id_start, id_stop))),
    from_file = dir(directory),
    timepoint = curr_timepoint,
    borg = NA_real_,
    test.time = NA_real_,
    aerobiccapacity = NA_real_,
    VO2max = NA_real_,
    HRmax = NA_real_,
    MaxOxygenPulse = NA_real_,
    maxRQ = NA_real_
  )

  #This creates new variables in the metadata dataset, because I didn't know how to refer to them else.
  metadata$id<-metadata[metadata_id]
  metadata$timepoint<-metadata[metadata_timepoint]

  #If we need to create graphs or models, lets make some folders for that, also only create out_list if it's needed
  if(graph){
    dir.create(file.path(outpath, "/graphics"), showWarnings = FALSE)
    #  out_list <- list()
  }

  if(models){
    dir.create(file.path(outpath, "/models"), showWarnings = FALSE)
    # out_list <- list()
  }


  for (filename in dir(directory)) {
    #Looping through the filenames in directory, filename must contain the ID at the points indicated by id_start and id_stop
    curr_id <- to_snake_case(stringr::str_sub(filename, id_start, id_stop))
    p<-paste0(directory,"/",filename)
    df<-suppressWarnings(
      read_excel( path=p,
                  range = c("J1:BZ300"), col_names = T , col_types = "numeric")%>%
        filter(!row_number() %in% c(1, 2))%>%
        as.data.frame()%>%dplyr::select(-contains("...")))

    if (is.null(dim(df))|is.na(colnames(df)[1])){next}

    colnames(df)[1] <- "time.mad.excel.format"
    colnames(df)[colnames(df)=="VO2/Kg"]<-"VO2_per_kg"
    colnames(df)[colnames(df)=="VO2/kg"]<-"VO2_per_kg"

    #The metadata has the end of each test.
    test.time<-metadata%>%dplyr::filter( id==curr_id & timepoint==curr_timepoint)%>%pull(metadata_time_seconds)


    df<-df%>%
      mutate(
        time.seconds = round(time.mad.excel.format*86400 ,0)
      )%>%filter(rowSums(is.na(.)) != ncol(.))
    #filter(
    #  time.seconds <=test.time
    #)

    wvector<-sapply(seq_along(df$time.seconds), FUN=
                      function(i) {which(df$time.seconds[-(seq_len(i))] >=
                                           df$time.seconds[i]+width_to_sum_in_sec)[1]})
    wvector<-case_when(is.na(wvector) ~ 6,
                       T ~ wvector)
    df<-df%>%mutate(
      from_file = filename,
      id = curr_id,
      time.minuts = round(time.seconds/60,2),
      timepoint = curr_timepoint,
      aerob30secsum = #TODO clear up that this period depends on the input
        zoo::rollapply(VO2_per_kg,
                       width = wvector,
                       FUN = mean,
                       align="left",
                       fill =NA_real_),
      vo2.30.sec.sum =
        zoo::rollapply(`VO2`,
                       width = wvector,
                       FUN = mean,
                       align="left",
                       fill =NA_real_)  )

    dato=suppressWarnings(
      read_excel(path=p,
                 range = "E1:E1", col_names = F, col_types="text")[1,1])


    height = suppressWarnings(
      read_excel( path=p,
                  range = "B6:B6", col_types="numeric",col_names = F)[1,1])


    weight = suppressWarnings(
      read_excel( path=p,
                  range = "B7:B7", col_types="numeric",col_names = F)[1,1]
    )


    vo2max<-max(df$aerob30secsum, na.rm =T)
    time_vo2max <- df$time.seconds[min(which(df$aerob30secsum==vo2max))]

    if(!is.null(outpath)){
      if(dataframes){
        if(anonymize){df$from_file<-NULL}
        write_xlsx(x=df,
                   path = paste0(outpath,"/VO2maxWrittenByR_", curr_id,"_",curr_timepoint,".xlsx"))}}


    borg<-metadata%>%dplyr::filter( id==curr_id & timepoint==curr_timepoint)%>%pull(metadata_borg)
    #Write the data we have to the out_data, the unique identifier is filename. It should be noted that the function will do silly things if two files have the same name, but I doubt they can.
    out_data$id[out_data$from_file==filename] = curr_id
    out_data$timepoint[out_data$from_file==filename]  = curr_timepoint
    out_data$aerobiccapacity[out_data$from_file==filename]  = max(df$aerob30secsum, na.rm =T)
    out_data$VO2max[out_data$from_file==filename]  = max(df$vo2.30.sec.sum, na.rm =T)
    out_data$HRmax[out_data$from_file==filename]  = max(df$HR, na.rm =T)
    out_data$MaxOxygenPulse[out_data$from_file==filename]  = max(df$`VO2/HR`, na.rm =T)
    out_data$maxRQ[out_data$from_file==filename]  = max(df$RQ, na.rm =T)
    out_data$height[out_data$from_file==filename]  = height
    out_data$weight[out_data$from_file==filename]  = weight
    out_data$date[out_data$from_file==filename]  = dato
    out_data$borg[out_data$from_file==filename] = borg
    out_data$test.time[out_data$from_file==filename] = test.time

    out_data$vo2maxaftertestend[out_data$from_file==filename] = df$time.seconds[match(max(df$aerob30secsum,na.rm=T),df$aerob30secsum)] > test.time
    if(df$time.seconds[match(max(df$aerob30secsum,na.rm=T),df$aerob30secsum)] > test.time){next}


    df.warmup <- df %>% filter(time.seconds<=warmup.period)
    mean.VO2kg.warmup <- mean(df.warmup$`VO2/Kg`, na.rm = T)
    sd.VO2kg.warmup <- sd(df.warmup$`VO2/Kg`, na.rm = T)

    if (is.null(shortest.analysed.test)){
      shortest.analysed.test=warmup.period+120}

    #If maximal aerobic capacity is after the shortest analysed test, analyse it, if not, note it and break
    if(df$time.seconds[match(max(df$aerob30secsum, na.rm =T), df$aerob30secsum)]>=shortest.analysed.test){

      #if the test is longer than the shortest analysed test analyse it, if not note it and break
      if(max(df$time.seconds,na.rm=T)>=shortest.analysed.test ){
        #The tests especially Pooles need a test.time to run, so we'll define it this way if it isn't defined.
        if(is.na(test.time)){
          test.time <- max(df$time.seconds, na.rm =T)
          if(test.time<warmup.period){next}}
        #Pooles method is quite simple, he takes the time 6 to 2 minutes before the end of the test, creates a line, then creates a new line at the last 2 minutes and compares the regressions.

        if(method == "poole"){

          df.stable <- df%>%
            dplyr::filter(time.seconds >= warmup.period)%>%
            dplyr::filter(time.seconds >= test.time-poolecutoffs[2]) %>%
            dplyr::filter(time.seconds < test.time-poolecutoffs[1]) %>%
            mutate(period = "stable")
          df.end <- df%>%
            dplyr::filter(time.seconds >= test.time-poolecutoffs[1])%>%
            dplyr::filter(time.seconds <= test.time)%>%
            mutate(period = "end")
        }
        if(method == "MLA"){
          #  df.no.warmup <- df%>%dplyr::filter(time.seconds > warmup.period)

          #Below is the complicated code I tried to implement, I have made a simpler solution further below
          #Find timepoint were vo2 is above x SD of warmup period. (So we dont start the curve flat)

          #if( mean.VO2kg.warmup +
          #    sd.to.search.over*
          #    sd.VO2kg.warmup         < vo2max){
          #  time_vo2_over_warmup <-
          #    df.no.warmup$time.seconds[min(which(df.no.warmup$aerob30secsum >
          #                                          mean.VO2kg.warmup +
          #                                          sd.to.search.over*
          #                                          sd.VO2kg.warmup))]} else {
          #If the test never gets a VO2max thats 2SD abvoe the mean in the warmup. The stable and end periods are likely similar, so we just start the VO2max at the end of the warmup period
          # time_vo2_over_warmup <- warmup.period  }


          #  if (time_vo2_over_warmup < test.time -
          #                              end.period -
          #                              lag_time_stable_to_end &
          #      time_vo2_over_warmup > warmup.period){
          #    stable_start_time <- time_vo2_over_warmup
          #  }else{
          #    stable_start_time <- warmup.period
          #  }
          #Find timepoint were VO2 is within x SD of VO2max (so we dont end the curve at plateau)
          #  first_time_vo2_within_x_sd_of_vo2max <-
          #    df.no.warmup$time.seconds[min(which(df.no.warmup$aerob30secsum >
          #                                          vo2max -
          #                                          sd.to.search.over*
          #                                          sd.VO2kg.warmup))]


          #If the stable period is less than the minimum allowed stable period, extend it to the minimum allowed stable period.
          #  if (
          #    first_time_vo2_within_x_sd_of_vo2max - stable_start_time < minimum.stable.period
          #  ) {
          #    stable_end_time <- stable_start_time + minimum.stable.period
          #  } else { #Else, use the first time vo2max is within x sd of VO2max as the end for the stable
          #    stable_end_time <- first_time_vo2_within_x_sd_of_vo2max
          #  }

          #  df.stable <- df%>%
          #    dplyr::filter(
          #    time.seconds >= stable_start_time) %>%
          #    dplyr::filter( #The lag is deducted here, instead of deducting it higher up and adding it again to the end.
          #    time.seconds < stable_end_time - lag_time_stable_to_end
          #  )

          #Define the plateau at the end, it should be all times that are less than test.time & less than 30 sec after   the VO2max. If the VO2 falls below VO2max - 2SD it is probably a mask failure at test end and should be removed.

          #Find the time after VO2max were VO2 is more than x sd under vo2max
          #  df.after.max <- df %>% dplyr::filter(
          #    time.seconds >= time_vo2max)
          #  time_vo2_falls_again <- df.after.max$time.seconds[min(which(
          #    df.after.max$aerob30secsum <  vo2max -
          #                                  sd.to.search.over*
          #                                  sd.VO2kg.warmup))]
          #
          #  plateau_end <- min(c(time_vo2max + end.period,
          #                       test.time,
          #                       time_vo2_falls_again),
          #                     na.rm =T)


          #if the plateau is less than the end period, expand it to end period
          #  if(plateau_end - stable_end_time < end.period){
          #    plateau_end <- stable_end_time + end.period
          # }

          #  df.end <- df %>%
          #    dplyr::filter(
          #    time.seconds >= stable_end_time
          #  ) %>%dplyr::filter(
          #    time.seconds <= plateau_end
          #  )


          #Method MLA Simplified
          df.stable <- df %>%
            dplyr::filter(
              time.seconds >= warmup.period)%>%
            dplyr::filter(
              time.seconds < time_vo2max - end.period - lag_time_stable_to_end)


          df.end <- df %>%
            dplyr::filter(
              time.seconds >= time_vo2max - end.period
            ) %>% dplyr::filter(
              time.seconds <= time_vo2max + end.period
            ) %>% dplyr::filter(
              time.seconds <= test.time
            )
        }
        #Okay so we've defined which data should be included in each data frame. Which is really the difference between Pooles method and mine. Poole then suggest using the same method for validation as CS uses, but I used a significance testing approach which is a little more lenient.
        if (nrow(df.end)<2){next} #We are fitting two linear models, we need at least 2 data points in both DFs
        if (nrow(df.stable)<2){next}
        if (min(df.end$time.seconds,na.rm = T) > 6000) {next}
        if (max(df.end$time.seconds,na.rm = T) < 0) {next}

        df.end <- df.end %>%mutate(period = "end")
        df.stable<-df.stable %>% mutate (period = "stable")

        df.joined <- rbind(
          df.end , df.stable) %>%
          mutate(period = factor( period, levels = c("stable", "end")))

        lm.joined <- lm(aerob30secsum ~ period+time.seconds+time.seconds:period, data = df.joined)

        require(car)
        anova.lm.joined<-car::Anova(lm.joined, type = 3, singular.ok = T)
        p_diff <- anova.lm.joined$`Pr(>F)`[4]
        end.steeper.than.stable <- lm.joined$coefficients[4] > 0


        if(graph){
          graphic<-df.joined %>%ggplot(aes(x=time.seconds, y = aerob30secsum, colour=period)) + geom_point()+stat_smooth(method="lm")
          ggsave(filename = paste0(outpath , "/graphics/","graph_",curr_timepoint,"_",curr_id,graph_filetype),
                 plot = graphic)
          #I wanted to output a list, it didn't really work. Especially with the ggplot objects
          #curr_graph <- list(data.frame(timepoint=curr_timepoint,id=curr_id, graph=graphic))
          #out_list<-append(out_list,curr_graph)

        }

        if(models){
          saveRDS(object=lm.joined, file = paste0(outpath,"/models/",curr_timepoint,"_",curr_id,".rds" ))
          #I wanted to output a list, it didn't really work. Especially with the ggplot objects
          #curr_model <- list(data.frame(matrix(timepoint=curr_timepoint,id=curr_id, lm=lm.joined)))
          #out_list<-append(out_list,curr_model)
        }

        lmtestMLA <- case_when(end.steeper.than.stable ~ F,
                               p_diff < significant_difference_slope ~ T,
                               T ~ F)
        end.half.stable <- lm.joined$coefficients[4]+lm.joined$coefficients[3]<=(lm.joined$coefficients[3])/2


        lmtestCS <- end.half.stable #Also Pooles method


        out_data$lmtestMLA[out_data$from_file==filename] <- lmtestMLA
        out_data$lmtestCS[out_data$from_file==filename] <- lmtestCS
        out_data$endsteep[out_data$from_file==filename]  <- end.steeper.than.stable
        out_data$end.half.stable[out_data$from_file==filename]  <- end.half.stable
        out_data$tooshort[out_data$from_file==filename]  <- F
        out_data$aerobmaxinwarmup[out_data$from_file==filename]  <- F

      } else{
        out_data$tooshort[out_data$from_file==filename]  <- T
      }
    } else {
      out_data$aerobmaxinwarmup[out_data$from_file==filename]  <- T
    }
  }
  if (method == "MLA"){ #The main difference between mine and CS version is that I weigh the lmtest less,
    #because I don't think plateau can ever be decided in a yes/no question and therefore use the other guidelines equivalently.
    out_data<-out_data %>% mutate(
      Quality_Score_MLA =
        case_when(
          borg >= 17 ~ 1,
          borg < 17 ~ 0,
          T~NA_real_) +
        case_when (lmtestMLA ~ 1,
                   lmtestMLA == F ~ 0,
                   T~NA_real_) +
        case_when ( maxRQ >=1.1 ~ 1,
                    maxRQ <1.1 ~ 0,
                    T~NA_real_),

      QualityMeasurementMLA = Quality_Score_MLA >= 2)
  }
  out_data<-out_data %>% mutate(Quality_Score_CS =
                                  case_when(
                                    borg >= 17 ~ 1,
                                    borg < 17 ~ 0,
                                    T~NA_real_) +
                                  case_when (lmtestCS ~ 2,
                                             lmtestCS == F ~ 0,
                                             T~NA_real_) +
                                  case_when ( maxRQ >=1.1 ~ 1,
                                              maxRQ <1.1 ~ 0,
                                              T~NA_real_),
                                QualityMeasurementCS = Quality_Score_CS >= 2)


  return(out_data)
}


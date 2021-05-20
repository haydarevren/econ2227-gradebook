#Requirements: 

#stata.grades.csv: Student	ID,	SIS User ID,	SIS Login ID,	Section,	Attendance/Participation, Problem Set 0, 
#                  Problem Set 1, Problem Set 2, Problem Set 3, Quiz 1, Quiz 2, Quiz 3, Quiz 4

#rosters.csv: Course Number,	Eagle ID,	Last Name,	First Name,	Email Address,	School,	Major,	Grad Year,	Instructor

#instructor1-3 sections are required to adjusted (line 120) and instructor names

rm(list = ls())
# Clean the console (in RStudio you can do the same by pressing Ctrl+L)
cat("\f") 

# Prepare needed packages
packages <- c("readxl", "stargazer","ggplot2"
              , "lemon"
              , "gridExtra" # For Q1
              , "ggrepel"   # For labels in Q2.b
              , "scales"
              , "stringr")
for (i in 1:length(packages)) {
  if (!packages[i] %in% rownames(installed.packages())) {
    install.packages(packages[i])
  }
  library(packages[i], character.only = TRUE) # Loads package into you current library
}
# Remove unused objects
rm(packages)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


stata.grades <- read.csv("stata.grades.csv"
                  , check.names = FALSE
                  , stringsAsFactors = FALSE
                  , na.strings = ""
                  )


columnR <- function(dataset) {
  names <- strsplit(colnames(dataset), " \\(")
  names <- sapply(names, function(x) x[1])
  names <- tolower(gsub(" ", ".", names))
  colnames(dataset) <-names
  if("sis.user.id" %in% colnames(dataset)){dataset <- dataset[is.na(dataset$sis.user.id)==0,]}
  
  if("section" %in% colnames(dataset)){colnames(dataset)[colnames(dataset)=="section"] <-"stata.section"}
  if("id" %in% colnames(dataset)){colnames(dataset)[colnames(dataset)=="id"] <-"canvas.id"}
  if("sis.login.id" %in% colnames(dataset)){colnames(dataset)[colnames(dataset)=="sis.login.id"] <-"email.address"}
  if("attendance/participation" %in% colnames(dataset)){colnames(dataset)[colnames(dataset)=="attendance/participation"] <-"attendance"}
  
  
  if("student" %in% colnames(dataset)){
    students <- strsplit(dataset$student, ", ")
    
    last.name <- sapply(students, function(x) x[1])
    first.name <- sapply(students, function(x) x[2])
    dataset$last.name <- last.name
    dataset$first.name <- first.name
    dataset$student <- NULL
  }
  
  if("stata.section" %in% colnames(dataset)){
    sections <- strsplit(dataset$stata.section, " ")
    sections <- sapply(sections, function(x) x[1])
    sections <- gsub("\\.", "", sections)
    dataset$stata.section <- sections
  }
  
return(dataset)  
}



columnCSV <- function(dataset) {
  names <- gsub("\\.", " ", colnames(dataset))
  names <- str_to_title(names, locale = "en")
  colnames(dataset) <-names
  return(dataset)
}


stata.grades<- columnR(stata.grades)



variables <- c("last.name", "first.name", "canvas.id", "sis.user.id", "email.address", "stata.section", "problem.set.0", "problem.set.1", "problem.set.2", "problem.set.3"
               ,"quiz.1","quiz.2", "quiz.3" , "quiz.4","attendance")
quizes <- c("quiz.1","quiz.2", "quiz.3" , "quiz.4")
assignments <- c("problem.set.0", "problem.set.1", "problem.set.2", "problem.set.3","quiz.1","quiz.2", "quiz.3" , "quiz.4","attendance")

stata.grades <- stata.grades[,variables]


stata.grades[, assignments]<-as.numeric(sapply(stata.grades[, assignments],function(x) x ))
stata.grades[is.na(stata.grades)] <- 0


stata.grades$quiz.1 <- round(100/20*stata.grades$quiz.1,2)
stata.grades$quiz.2 <- round((100/15)*stata.grades$quiz.2,2)
stata.grades$quiz.3 <- round((100/20)*stata.grades$quiz.3,2)
stata.grades$quiz.4 <- round((100/15)*stata.grades$quiz.4,2)
stata.grades$problem.set.0 <- 10*stata.grades$problem.set.0


stata.grades$total.quiz.score <- round((1/3)*(stata.grades$quiz.1  + stata.grades$quiz.2+ stata.grades$quiz.3 
                                + stata.grades$quiz.4 - apply(stata.grades[,quizes], 1, FUN=min)) 
                                      ,2)


stata.grades$total.problem.set.score <- round(0.1*stata.grades$problem.set.0 + 0.3* stata.grades$problem.set.1 + 0.3*stata.grades$problem.set.2  +0.3*stata.grades$problem.set.3
                                              ,2)

stata.grades$total.score<- round(0.3*stata.grades$total.quiz.score+0.6*stata.grades$total.problem.set.score +stata.grades$attendance,
                                 2)

rm(variables,assignments,quizes,i)

instructor1 <- c("ECON222702","ECON222703","ECON222704")

instructor2 <- c("ECON222701","ECON222705","ECON222709")

instructor3 <- c("ECON222706","ECON222707","ECON222708")

stata.grades$stata.instructor <- ifelse(stata.grades$stata.section %in% instructor1, "Haydar"
                                        ,ifelse(stata.grades$stata.section %in% instructor2, "Jenny"
                                                ,ifelse(stata.grades$stata.section %in% instructor3, "Arnab",NA)
                                                                                
                                                                        
                                                                
                                                        
                                                )
                                        )


rosters <- read.csv("rosters.csv"
                         , check.names = FALSE
                         , stringsAsFactors = FALSE
                         , na.strings = ""
)

rosters <- columnR(rosters)

stata.grades$email.address <- NULL

stata.grades <- merge(stata.grades
                , rosters
                , by = c("last.name","first.name")
                , all.x = TRUE, all.y = TRUE
)

stata.grades<-stata.grades[order(stata.grades$stata.section,stata.grades$last.name,stata.grades$first.name),]


variables <- c("stata.section", "eagle.id", "last.name", "first.name", "email.address", "canvas.id", "sis.user.id", "stata.instructor"
               ,  "problem.set.0", "problem.set.1", "problem.set.2", "problem.set.3"
               ,"quiz.1","quiz.2", "quiz.3" , "quiz.4"
               , "attendance", "total.problem.set.score","total.quiz.score", "total.score")


stata.grades.haydar <- stata.grades[stata.grades$stata.instructor=="Haydar",variables]
stata.grades.haydar$stata.instructor <- NULL
stata.grades.jenny <- stata.grades[stata.grades$stata.instructor=="Jenny",variables]
stata.grades.jenny$stata.instructor <- NULL
stata.grades.arnab <- stata.grades[stata.grades$stata.instructor=="Arnab",variables]
stata.grades.arnab$stata.instructor <- NULL


assignments <- c("problem.set.0", "problem.set.1", "problem.set.2", "problem.set.3"
                 ,"quiz.1","quiz.2", "quiz.3" , "quiz.4","attendance","total.problem.set.score"
                 ,"total.quiz.score", "total.score")

mystats <- function(dataset,vars){
  stats.new <- summary(dataset[dataset$total.score>60,vars])
  
  stats.new<- rbind(stats.new
                    ,paste("Std.Dev:"
                           , round(sapply(dataset[dataset$total.score>60,vars], sd),2)
                           , sep =""
                    )
                    )
  return(stats.new)
}

stats.haydar <- mystats(stata.grades.haydar,assignments)
stats.jenny <- mystats(stata.grades.jenny,assignments)
stats.arnab <- mystats(stata.grades.arnab,assignments)


write.csv(stats.haydar, file ="stats.haydar.csv")
write.csv(stats.jenny, file ="stats.jenny.csv")
write.csv(stats.arnab, file ="stats.arnab.csv")

write.csv(columnCSV(stata.grades.haydar), file = "stata.grades.haydar.csv")
write.csv(columnCSV(stata.grades.arnab), file = "stata.grades.arnab.csv")
write.csv(columnCSV(stata.grades.jenny), file = "stata.grades.jenny.csv")

myhist <- function(dataset,minscore){
  dataset<-dataset[dataset$total.score>minscore,]
  new.hist <- ggplot(dataset, aes(x = total.score)) +
    geom_histogram(bins = 125
                   , aes(y = ..density..) # Here we now have density
                   , color = "darkblue"
                   , fill = "lightblue"
    ) +
    geom_density(aes(y = ..density..)
                 , color = "darkblue"
                 , size = 1.2
    ) + 
    # scale_y_continuous() +
    scale_x_continuous(breaks = seq(from = 60        
                                    , to = 100
                                    , by = 5
    )
    , labels = seq(from = 60        
                   , to = 100
                   , by = 5
    )
    ) +
    geom_vline(aes(xintercept = mean(total.score))
               , color = "red"
               , linetype = "dashed"
               , size = 1.2
    ) +
    geom_vline(aes(xintercept = median(total.score))
               , color = "dark green"
               , linetype = "dashed"
               , size = 1.2
    ) +
    annotate(geom = "text"
             , x = mean(dataset$total.score) - 1
             , y = 0.4 # This needs to be changed to accomodate new scale
             , color = "red"
             , size = 4
             , fontface = "bold"
             , label = paste0("Mean\n=\n"
                              , round(mean(dataset$total.score), 2)
             )
             , lineheight = 0.75  # Reduce line spacing
    ) +
    annotate(geom = "text"
             , x = median(dataset$total.score) +1.1
             , y = 0.5 # This needs to be changed to accomodate new scale
             , color = "dark green"
             , size = 4
             , fontface = "bold"
             , label = paste0("Median\n=\n"
                              , round(median(dataset$total.score),2)
             )
             , lineheight = 0.75  # Reduce line spacing
    ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5
                                    , face = "bold"
                                    , size = 20
                                    , color = "#912600"
    )
    , plot.subtitle = element_text(hjust = 0.5
                                   , face = "bold"
                                   , size = 14
                                   , color = "#912600"
    )
    , axis.title.x = element_text(face = "bold"
                                  , color = "#912600"
                                  , size = 14
    )
    , axis.text.x  = element_text(face = "bold"
                                  , vjust = 0.5
                                  , size = 12
    )
    , axis.title.y = element_text(face = "bold"
                                  , color = "#912600"
                                  , size = 14
    )
    , axis.text.y  = element_text(face = "bold"
                                  , vjust = 0.5
                                  , size = 12
    )
    )  + expand_limits(x = 60, y = 0.5)
  return(new.hist)
  
}


graph <- myhist(stata.grades.haydar,60) + 
  labs(title = "Total Score Distribution (Haydar)"
       , subtitle = "for total score higher than 60"
       , x = "Grade"
       , y = "Density"
  ) 
png(file = "stata.grades.haydar.png", width = 2880, height = 1920, res = 180)
graph
dev.off()


graph <-myhist(stata.grades.arnab,60)+ 
  labs(title = "Total Score Distribution (Arnab)"
       , subtitle = "for total score higher than 60"
       , x = "Grade"
       , y = "Density"
  ) 
png(file = "stata.grades.arnab.png", width = 2880, height = 1920, res = 180)
graph
dev.off()


graph <-myhist(stata.grades.jenny,60)+ 
  labs(title = "Total Score Distribution (Jenny)"
       , subtitle = "for total score higher than 60"
       , x = "Grade"
       , y = "Density"
  ) 
png(file = "stata.grades.jenny.png", width = 2880, height = 1920, res = 180)
graph
dev.off()



stata.grades<-stata.grades[order(stata.grades$course.number,stata.grades$last.name,stata.grades$first.name),]
variables <- c("course.number", "eagle.id", "last.name", "first.name", "email.address", "canvas.id", "sis.user.id", "stata.section", "instructor", "stata.instructor"
               ,  "problem.set.0", "problem.set.1", "problem.set.2", "problem.set.3"
               ,"quiz.1","quiz.2", "quiz.3" , "quiz.4"
               , "attendance", "total.problem.set.score","total.quiz.score", "total.score")

stata.grades <- stata.grades[,variables]
write.csv(columnCSV(stata.grades), file = "GRADES-ECON2227_SPRING_2021.csv")


variables <- c("course.number", "eagle.id", "last.name", "first.name", "email.address", "canvas.id", "sis.user.id", "total.score")

grades.maxwell <- stata.grades[stata.grades$instructor=="Maxwell",variables]
grades.maxwell$instructor <- NULL
write.csv(columnCSV(grades.maxwell), file = "grades.maxwell.csv")


grades.cox <- stata.grades[stata.grades$instructor=="Cox",variables]
grades.cox$instructor <- NULL
write.csv(columnCSV(grades.cox), file = "grades.cox.csv")


grades.sanzenbacher <- stata.grades[stata.grades$instructor=="Sanzenbacher",variables]
grades.sanzenbacher$instructor <- NULL
write.csv(columnCSV(grades.sanzenbacher), file = "grades.sanzenbacher.csv")


grades.khan <- stata.grades[stata.grades$instructor=="Khan",variables]
grades.khan$instructor <- NULL
write.csv(columnCSV(grades.khan), file = "grades.khan.csv")

grades.khan <- stata.grades[stata.grades$instructor=="Cichello",variables]
grades.khan$instructor <- NULL
write.csv(columnCSV(grades.khan), file = "grades.cichello.csv")

rm(assignments,instructor1,instructor2,instructor3,variables)






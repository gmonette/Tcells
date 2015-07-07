#'  Math Achievement in a sample of high school students
#' 
#'  A dataset containing math achievement scores, ses (socio-economic status),
#'  sex, type of school (Public or Catholic) of 1,997 students in 40 schools.
#' 
#'  This is a classical data set from the field of education used to illustrate
#'  multilevel data and models. It is a random subset of 40 schools from the 
#'  the full dataset of 160 schools used in the first edition of Bryk and
#'  Raudenbush. 
#'  
#'  @format A data frame with 1,997 rows and 7 variables:
#'  \describe{
#'      \item{school}{numerical school ID.}
#'      \item{mathach}{measure of math achievement.}
#'      \item{ses}{socio-economic status of family based primarily on educational level of father and/or mother.}
#'      \item{Sex}{a factor with levels \code{Female, Male}.}
#'      \item{Minority}{a factor with levels \code{No, Yes}.}
#'      \item{Size}{number of students in the school.}
#'      \item{Sector}{the sector, Catholic or Public, to which the school belongs.}
#'  }
#'  @examples
#'  library(nlme)
#'  fit <- lme( mathach ~ ses*Sector, hs, random = ~ 1 + ses | school)
#'  summary(fit)
"hs" <- NULL


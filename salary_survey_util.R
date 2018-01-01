library(dplyr)

# Functions to print peso values (salary_numeric) as Php 123,456
Php <- function(x) {
  sprintf("Php %s", comma(x))
}

# Functions to print peso values (salary_numeric) as Php 123K
# Useful mostly in scale_y_continuous to print neat labels
PhpK <- function(x) {
  Php(x/1000)
}  


preprocess_salary_survey <- function(raw_df) {
  df <- rename_columns(raw_df)
  
  # Convert sal_raw to factor and numeric based on salary_levels_df
  df$salary_numeric <- salary_string_to_numeric(df$sal_raw)
  salary_levels_df <- salary_levels(df$sal_raw)
  df$salary_level <- factor(df$sal_raw, salary_levels_df$salary_string, salary_levels_df$salary_label)
  
  # count less than 1 year as 0
  df$exp_num <- parse_number(df$exp_raw)
  df$exp_num[df$exp_raw == "Less than 1 year"] <- 0.5
  exp_lvls <- seq(0,20)
  exp_lvls[1] <- "Less than 1 year"
  df$exp_f <- factor(df$exp_raw, levels=exp_lvls, ordered=TRUE)
 
  # Geography: city as factor, and whether they are in or outside NCR 
  df$ph_city_f <- factor(df$ph_city)
  
  df$ncr <- c(NA)
  df$ncr[!endsWith(df$ph_city, "NCR")] <- "Outside NCR"
  df$ncr[endsWith(df$ph_city, "NCR")] <- "NCR"
  df$ncr[df$ph_city == "I'm not currently working in the Philippines"] <- "Overseas"
  
  # Convert company size to factors so we can control order of facets
  # http://stackoverflow.com/questions/14262497/fixing-the-order-of-facets-in-ggplot
  df$company_size_f <- factor(df$company_size, levels=c("1", 
                                                        "2 - 25", 
                                                        "25 - 100", 
                                                        "101 - 500", 
                                                        "501- 1,000", 
                                                        "1,001 - 2,500", 
                                                        "2,501 - 10,000", 
                                                        "10,001 or more"), ordered=TRUE)
  # Convert gender to factor
  df$company_industry_f <- factor(df$company_industry)
  df$gender <- factor(df$gender, levels=c("Male", "Female", "Prefer not to say"))
  
  # Convert company age to factor as well
  df$company_age_f <- factor(df$company_age, levels=c("less than 2 years", 
                                                      "2 - 5 years", 
                                                      "6 - 10 years", 
                                                      "11 - 20 years", 
                                                      "20 years or longer"), ordered=TRUE)
  
  # Convert the employment status and age to factors as well
  df$employment_f <- factor(df$employment_status, 
                            levels=c("Employed in a company, full or part-time", 
                                     "Freelance or independent contractor"), 
                            labels=c("Employed", "Freelance"))
  df$age_range_f <- factor(df$age_range, 
                           levels=c("Under 21", "21-25", "26-30", "31-35", "36-40", "41-45", "46-50", "51-55"),
                           ordered=TRUE)
  
  df$team_size_f <- factor(df$team_size,
                           levels=c("1 (just me)", "2", "3", "4", "5", "6-7", "8-10", "11-15", "16-20", "more than 20"),
                           ordered=TRUE)
  df$project_size_f <- factor(df$project_size,
                           levels=c("1 (just me)", "2", "3", "4", "5", "6-7", "8-10", "11-15", "16-20", "more than 20"),
                           ordered=TRUE)
  
  # University fixer
  df$university[grep("^AMA", df$university)] <- "AMA University System"
  df$university[grep("^AMA", df$university_other)] <- "AMA University System"
  df$university[grep("^STI", df$university)] <- "STI University System"
  df$university[grep("^STI", df$university_other)] <- "STI University System"
  
  # Course and concentration
  df$educ_masters <- FALSE
  df$educ_masters[grep("master's degree", df$educ_stmts)] <- TRUE
  df$educ_computer_science <- FALSE
  df$educ_computer_science[grep("computer science", df$educ_stmts)] <- TRUE
  df$educ_math_stat_physics <- FALSE
  df$educ_math_stat_physics[grep("mathematics", df$educ_stmts)] <- TRUE
  df$educ_it_mis <- FALSE
  df$educ_it_mis[grep("IT or MIS", df$educ_stmts)] <- TRUE
  
  df <- filter_outliers(df)
  # From this point on, no need to filter(!outlier)
  # return the resulting dataframe
  df
}

rename_columns <- function(df) {
  df %>% rename(job_title=`What is your job title?`,
                dept=`What is your department (if applicable)?`,
                sal_raw=`Please enter your approximate monthly base salary in Philippine Pesos.`,
                sal_increase_raw=`How much more is your current salary, than your salary three years ago?`,
                sal_bonus_raw=`Approximately how much was your bonus from last year (aside from any mandatory bonuses -- e.g 13th month)?`,
                in_ph=`Are you currently living/working in the Philippines?`,
                ph_city=`If you currently work in the Philippines, which city do you work in?`,
                age_range=`What is your age?`,
                exp_raw=`How many years of experience do you have in your field?`,
                gender=`What is your gender?`,
                educ_stmts=`Which of the following statements about your education are true?`,
                university=`Which university did you attend last?`,
                university_other=`If your university was not listed in the previous question, you may add it here.`,
                certifications=`What certifications do you currently have?`,
                employment_status=`Are you a full- or part-time employee of a company, or a freelancer?`,
                company_industry=`What is your company's business or industry?`,
                company_size=`How many employees work at your company?`,
                company_age=`How long has your company been in business?`,
                team_size=`How many people are on your team?`,
                project_size=`How large is your team for a typical coding project?`,
                os_used=`Operating systems`,
                proglang_used=`Programming Languages`,
                datatools_used=`Data tools`,
                cloud_used=`Cloud/Containers`,
                worktasks_noncollab=`Which of the following tasks play a part in your workday? [Writing code for non-collaborative projects (no one else will work on this code)]`,
                worktasks_collab=`Which of the following tasks play a part in your workday? [Writing code for collaborative projects]`,
                worktasks_collabreview = `Which of the following tasks play a part in your workday? [Reading/editing code originally written by others (e.g., using git)]`
                )
}

filter_outliers <- function(df) {
  ## Outlier labelling
  # We label as outliers those that:
  # a. are outside the Philippines
  # b. are outside Q3+3IQR in salary and years of experience
  
  # First, remove those that are clearly not in the Philippines
  df_in_ph <- df %>% filter(in_ph != "No") %>% filter(ph_city != "I'm not currently working in the Philippines")
  
  # Find the IQR for salary and experience, and the upper fence for outliers for both
  salary_iqr <- IQR(df_in_ph$salary_numeric)
  sal_upper_fence <- quantile(df_in_ph$salary_numeric, 0.75) + (1.5 * salary_iqr)
  
  exp_iqr <- IQR(df_in_ph$exp_num)
  exp_upper_fence <- quantile(df_in_ph$exp_num, 0.75) + 1.5 * exp_iqr
  
  filtered <- df_in_ph %>% filter(salary_numeric < sal_upper_fence & exp_num < exp_upper_fence)
  filtered
}

# 


extract_columns <- function(x, split) {
  lapply(strsplit(x, split = split), sort)
}

gather_strings <- function(char_l) {
  accumulator <- c()
  vapply(char_l, function(l) {accumulator <<- unique(c(accumulator, l)); length(accumulator)}, c(0))
  sort(accumulator)
}

salary_string_to_numeric <- function(salary_column) {
  # Uses the midpoint of the salary range 
  # Except for the lowest and highest ranges
  numeric_salaries <- parse_number(salary_column) + 2500.00
  numeric_salaries[salary_column == "under PHP 10,000"] <- 10000.00
  numeric_salaries[salary_column == "PHP 500,000 or higher"] <- 500000.00
  numeric_salaries
}

salary_levels <- function(salary_column) {
  # returns a data frame with salary string, salary label and a numeric value 
  # for each of the different salary levels
  # the intent is to convert the salary column (string) to an ordered factor:
  # lvls <- salary_levels(df$salary_col)
  # sal_factor <- factor(df$salary_col, lvls$salary_string, lvls$salary_label, ordered = TRUE)
  # df2 <- colbind(df, sal_factor)  # now we have a sal_factor column in df2
  unique_salaries <- unique(salary_column)
  numeric_salaries <- salary_string_to_numeric(unique_salaries)
  salary_labels <- get_salary_labels(numeric_salaries)
  df <- data.frame(salary_string=unique_salaries, salary_numeric = numeric_salaries, salary_label = salary_labels)
  o <- order(numeric_salaries)
  df[o,c('salary_string','salary_label', 'salary_numeric')]
}


get_salary_labels <- function(sal_levels) {
  sal_labels <- sprintf("%.0fK", sal_levels / 1000)
  sal_labels[sal_levels < 10000] <- "Under 10K"
  sal_labels[sal_levels > 500000] <- "Over 500K"
  sal_labels
}

# - Plotting ---


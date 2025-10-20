# Create the Patient DataFrame
print("K.Nichal haas,RegNo:22BCE9651")
patients <- data.frame(
  Patient_ID = c(101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 101),
  Name = c("Alice", "Bob", "200", "David", "Eve", "Frank", "Grace", NA, "Henry", "Ivy", "Alice"),
  Age = c(25, 30, 200, 45, NA, 33, 27, 29, 150, 40, 25),
  Gender = c("F", "M", "M", "M", "F", "M", "F", "F", "M", "F", "F"),
  Disease = c("Diabetes", "Hypertension", "Asthma", "Heart Disease", 
              "Heart Disease", "Cancer", "Flu", NA, "Covid-19", "Stroke", "Diabetes"),
  Bill_Amount = c(500, 1200, 300, -50, 2000, NA, 400, 750, 1000000, 950, 500)
)

print(patients)
#A)Keep only unique PatientID rows 

df<-patients[!duplicated(patients$Patient_ID),]
df

#B) Replace Empty string with “unknown” and NA values set to mean.  Replace Empty string with “unknown” and NA values set to mean. 
df[df==""]<-"unknown"
df
numeric_cols <- sapply(df, is.numeric)

for (col in names(df)[numeric_cols]) {
  mean_val <- mean(df[[col]], na.rm = TRUE)
  df[[col]][is.na(df[[col]])] <- mean_val
}

print(df)

#C)Remove the outliers with (PatientAge > 120 | PatientAge < 0) replace with Median of PatientAge and (Bill > 50000 | Bill < 0) replace with Bill with Median. 
median_df<-median(df$Age,na.rm=TRUE)
df$Age[df$Age>120|df$Age<0]<-median_df
df

#D)Create column Age_Group with PatientAge < 30, "Young", or PatientAge < 60, "Middle-Aged" Patient_Age>60, "Senior". 
df$Age_Group<-cut(df$Age,
                 breaks=c(-Inf,30,60,Inf),
                 label=c("Young","Middle-Aged","Senior")
                 )
df
#E)Create a column to classify patients by hospital expense level: Bill < 500 ~ "Low", Bill >= 500 & Bill < 5000 ~ "Medium", Bill >= 5000 ~ "High. 
df$expense<-ifelse(df$Bill_Amount<500,"Low",ifelse(df$Bill_Amount>=500 & df$Bill_Amount<5000,"Medium","High"))
df
#F)Find only “High” billing patients above 40 years old 
high_billing<-df[df$expense=="High"&df$Age>40,]
high_billing

#E)Find patients with Diabetes or Hypertension paying over $1000 
diab_hyper_patients <- subset(df, Disease %in% c("Diabetes", "Hypertension") & Bill_Amount > 1000)
print(diab_hyper_patients)




# Creates plots for some of the more important questions in the language dataset

# Second Person Plural

# Filter the data based on responses to the question in which we are interested
# Generate the answer file
question <- filter(lingData2, Q050 %in% c(1,4,7,9), long > -125)
answers <- all.ans[['50']]

# Make the column to join on.  They must be the same type.
answers$Q050 <- rownames(answers)
question$Q050 <- as.character(question$Q050)
question <- inner_join(question, answers, by="Q050")

# Scatterplot of latitude and longitude with color depending on responses
ggplot(data=NULL) +
  geom_point(data=question, aes(x=long, y=lat, color=ans), size=1.5, alpha=0.5) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme

# What do you call shoes
question <- filter(lingData2, Q073 %in% c(1,6), long > -125)
answers <- all.ans[['73']]
answers$Q073 <- rownames(answers)
question$Q073 <- as.character(question$Q073)
question <- inner_join(question, answers, by="Q073")
ggplot(data=NULL) +
  geom_point(data=question, aes(x=long, y=lat, color=ans), size=1.5, alpha=0.5) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme

# What do you call water fountains?
question <- filter(lingData2, Q103 %in% c(3,4), long > -125)
answers <- all.ans[['103']]
answers$Q103 <- rownames(answers)
question$Q103 <- as.character(question$Q103)
question <- inner_join(question, answers, by="Q103")
ggplot(data=NULL) +
  geom_point(data=question, aes(x=long, y=lat, color=ans), size=1.5, alpha=0.5) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme

# Pantyhose question
question <- filter(lingData2, Q056 %in% c(1,2), long > -125)
answers <- all.ans[['56']]
answers$Q056 <- rownames(answers)
question$Q056 <- as.character(question$Q056)
question <- inner_join(question, answers, by="Q056")
ggplot(data=NULL) +
  geom_point(data=question, aes(x=long, y=lat, color=ans), size=1.5, alpha=0.5) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme

# Knife Game
question <- filter(lingData2, Q059 %in% c(1,2,3,20,21), long > -125)
answers <- all.ans[['59']]
answers$Q059 <- rownames(answers)
question$Q059 <- as.character(question$Q059)
question <- inner_join(question, answers, by="Q059")
ggplot(data=NULL) +
  geom_point(data=question, aes(x=long, y=lat, color=ans), size=1.5, alpha=0.5) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme

# What do you call grandma?
question <- filter(lingData2, Q068 %in% c(3,4,7), long > -125)
answers <- all.ans[['68']]
answers$Q068 <- rownames(answers)
question$Q068 <- as.character(question$Q068)
question <- inner_join(question, answers, by="Q068")
ggplot(data=NULL) +
  geom_point(data=question, aes(x=long, y=lat, color=ans), size=1.5, alpha=0.5) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme

# Diagonal Across the street
question <- filter(lingData2, Q076 %in% c(1,4,7), long > -125)
answers <- all.ans[['76']]
answers$Q076 <- rownames(answers)
question$Q076 <- as.character(question$Q076)
question <- inner_join(question, answers, by="Q076")
ggplot(data=NULL) +
  geom_point(data=question, aes(x=long, y=lat, color=ans), size=1.5, alpha=0.5) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme

# Highway
question <- filter(lingData2, Q079 %in% c(1,2), long > -125)
answers <- all.ans[['79']]
answers$Q079 <- rownames(answers)
question$Q079 <- as.character(question$Q079)
question <- inner_join(question, answers, by="Q079")
ggplot(data=NULL) +
  geom_point(data=question, aes(x=long, y=lat, color=ans), size=1.5, alpha=0.5) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme

# Coleslaw
question <- filter(lingData2, Q089 %in% c(1,3), long > -125)
answers <- all.ans[['89']]
answers$Q089 <- rownames(answers)
question$Q089 <- as.character(question$Q089)
question <- inner_join(question, answers, by="Q089")
ggplot(data=NULL) +
  geom_point(data=question, aes(x=long, y=lat, color=ans), size=1.5, alpha=0.5) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme

# On or by accident
question <- filter(lingData2, Q098 %in% c(1,2,3), long > -125)
answers <- all.ans[['98']]
answers$Q098 <- rownames(answers)
question$Q098 <- as.character(question$Q098)
question <- inner_join(question, answers, by="Q098")
ggplot(data=NULL) +
  geom_point(data=question, aes(x=long, y=lat, color=ans), size=1.5, alpha=0.5) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme

# Road parallel to highway
question <- filter(lingData2, Q099 %in% c(1,2,3), long > -125)
answers <- all.ans[['99']]
answers$Q099 <- rownames(answers)
question$Q099 <- as.character(question$Q099)
question <- inner_join(question, answers, by="Q099")
ggplot(data=NULL) +
  geom_point(data=question, aes(x=long, y=lat, color=ans), size=1.5, alpha=0.5) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme

# Soda vs coke vs pop
question <- filter(lingData2, Q105 %in% c(1,2,3), long > -125)
answers <- all.ans[['105']]
answers$Q105 <- rownames(answers)
question$Q105 <- as.character(question$Q105)
question <- inner_join(question, answers, by="Q105")
ggplot(data=NULL) +
  geom_point(data=question, aes(x=long, y=lat, color=ans), size=1.5, alpha=0.5) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme

# TP'ing
question <- filter(lingData2, Q106 %in% c(1,2,3), long > -125)
answers <- all.ans[['106']]
answers$Q106 <- rownames(answers)
question$Q106 <- as.character(question$Q106)
question <- inner_join(question, answers, by="Q106")
ggplot(data=NULL) +
  geom_point(data=question, aes(x=long, y=lat, color=ans), size=1.5, alpha=0.5) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme

# Riding shotgun
question <- filter(lingData2, Q120 %in% c(1,2), long > -125)
answers <- all.ans[['120']]
answers$Q120 <- rownames(answers)
question$Q120 <- as.character(question$Q120)
question <- inner_join(question, answers, by="Q120")
ggplot(data=NULL) +
  geom_point(data=question, aes(x=long, y=lat, color=ans), size=1.5, alpha=0.5) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme



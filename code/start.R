source('optim.R')

df = extract.data(file.location = "../data/dataPlanning.csv", saturday.penalty = 2)$df

rest.preferences = extract.data(file.location = "../data/dataPlanning.csv",
                                saturday.penalty = 2)$rest.preferences

holiday.df = extract.holiday.data()

plan = generate.planning(n.week = 8,
                         num.bin.solns = 30,
                         holiday.data = holiday.df,
                         sol.number = 0,
                         penalty.factor = 2,
                         rest.preferences = rest.preferences)



# most.weekly.spread.conflicts(plan, df, rest.preferences)

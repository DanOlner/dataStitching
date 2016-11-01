library(ineq)
library(dplyr)

#order each census year unemply
allThree_EA_df_ordered <- allThree_EA_df %>% group_by(year) %>% 
  arrange(percentEmp)


#lorenz curve from ineq package
plot(Lc(allThree_EA_df_ordered$percentEmp[allThree_EA_df_ordered$year=='1991']))
plot(Lc(allThree_EA_df_ordered$percentEmp[allThree_EA_df_ordered$year=='2001']))
plot(Lc(allThree_EA_df_ordered$percentEmp[allThree_EA_df_ordered$year=='2011']))

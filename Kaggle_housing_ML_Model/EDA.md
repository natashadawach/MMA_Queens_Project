# Understanding Data

As with any modelling process, it is imperative to assess the data to get a sense of the explanatory variables and the data itself. With housing prices, we have two datasets: Train.csv and Test.csv. Here are the features that I explored the structure and summary of data with an initial eye:
  -	Train has 1,460 observations and Test has 1,459 observations
  -	Train has 81 variables including Sale prices and test has 80 variables (actual sale prices have been removed from test set)
  -	There are quite a few NAs, some are genuine as has been mentioned in the data dictionary, such NA for Pool Quality (PoolQC) means the house does not have a pool. While others     are real missing values which would need to be dealt with.
  -	Missing values are found across both categorical and numerical variables and in both datasets.
  -	Some errors in the observations that need fixing.

After getting a general idea, I decided to approach this in 3 steps which can be broadly categorized as follows:
1.	Data Cleaning, Exploration and Feature Engineering: This allowed me to dig deep beyond the surface and look for ways to clean and prepare the data for modelling. It included visualizations to look for patterns, outliers, and any skewness. 
2.	Regression Modelling: Once the data was prepared, I decided to run regression models to assess the Root-Mean-Squared-Error (RMSE) and other metrics.
3.	Re-assessing and Modifying Models: This step involved narrowing down the models to get one that provided the best results as well as helped reach a top rank on the leadership board of the competition.


# Exploratory Data Analysis
**Missing Values**
The first task of cleaning the data was the obvious one – to remap the “NA” in the below categories as ‘None’. This is because the actual context of these “NA” is that the following features are not present in a particular house, however, the software takes these NAs as missing values, therefore, the remapping allowed model to incorporate following observations in the analysis. 
1.	Alley
2.	Basement Quality
3.	Basement Condition
4.	Basement Exposure (refers to walkout or garden level access)
5.	Basement Finished Area Type 1 and 2 (Rating of basement finished area)
6.	Fireplace Quality
7.	Garage Type
8.	Garage Finish
9.	Garage Quality
10.	Garage Condition
11.	Pool Quality 
12.	Fence
13.	Miscellaneous Features

Once the above process was complete, the next task was to understand the remaining missing values. These missing values are the data observations that are genuinely missing or incomplete. Since a lot of these variables are linked to other variables in the dataset, there were 4 methods that were employed to impute for 4 different sets of variables:
-	Zero: Some variables were imputed with a factor of “None” in the place of NA (missing value). This is because another variable in the same family of category provided more insight on these missing values. For example, there are some missing values unfurnished Basement square feet, but we know from above that if other basement related variables such as basement quality, exposure are “None”, it means that that unfurnished basement square feet should be zero since basement does not exist in that house. Other variables that had similar treatment include finished basement square feet, total basement square feet, Basement Full and half bathrooms etc.

-	Mode (most frequent): Majority of the variables have either 1 or 2 missing values. For such categorical variables, it made sense to replace the missing values with the most frequently appearing observation (mode). Some variables for which this treatment was applied include Sale type, Kitchen quality, zoning classifications (MSZoning variable), utilities etc.

-	Multivariate imputation by chained equations (MICE): There were 3 features (Lot frontage, Masonry veneer type, Masonry veneer area) that had a relatively high number of missing values. A simple mode/average or zero could influence or skew the results. Therefore, a MICE imputation was used to iterate through the dataset, find relationship between the above missing values and other explanatory variables, and regress through the data to replace NAs with estimated values.

-	Special Variable: Garage year was a special variable because it is closely linked to the year the house was built. This makes sense since majority of the houses that have garages get built during the construction of the house. Therefore, for the missing values in garage year, a condition was put that if garage year has missing value, then check if other garage related variables such garage type have “none” observation (which means there is no garage in the house), then replace with zero, otherwise it was instructed to take the same value as the year built of the house.

**Distribution**

![image](https://user-images.githubusercontent.com/16128968/124367269-b3984900-dc23-11eb-87e4-a1e774b67e40.png)
![image](https://user-images.githubusercontent.com/16128968/124367302-0114b600-dc24-11eb-8269-df6af6e494b1.png)


![image](https://user-images.githubusercontent.com/16128968/124367308-0a9e1e00-dc24-11eb-9671-9f46b815d5d9.png)





![image](https://user-images.githubusercontent.com/16128968/124367277-c9a60980-dc23-11eb-89d1-f1e761a221d5.png)

![image](https://user-images.githubusercontent.com/16128968/124367278-ce6abd80-dc23-11eb-840f-2ee21393040e.png)
![image](https://user-images.githubusercontent.com/16128968/124367280-d1fe4480-dc23-11eb-8d83-db842480c5ee.png)


**Inconsistencies**
In the analysis, a few errors were also discovered and fixed. 
-	The year a house is remodeled appears before the year house was built. This seemed like a mistake as we expect the houses to be built first and then renovated or remodeled. To fix this, remodeled year was changed to built year for such observations.

![image](https://user-images.githubusercontent.com/16128968/124367329-3f11da00-dc24-11eb-901e-41085c01093d.png)

-	There was one observation under Garage year built which had year 2207. This was replaced with the Year built of 2006 since we have analyzed that garage year built and the house year built are mostly the same, hence, this is the best estimate 

![image](https://user-images.githubusercontent.com/16128968/124367330-41743400-dc24-11eb-97a8-b0dba9d538c9.png)


-	Year sold appearing before the year house is built. It is very well possible that the house was sold during the construction. However, since there is only one observation with year sold before year built, it is very likely that this is an error and therefore year sold was replaced with year built.

![image](https://user-images.githubusercontent.com/16128968/124367333-446f2480-dc24-11eb-9c51-81749bcbd1ab.png)


# Conclusion
 
Housing market can be impacted by numerous factors outside of our 79 variables, such as the economic forces, interest rates, general stability of the market and market expectations etc. The model selected here is one of the many models that can help predict housing prices with certain accuracy. With given tools, LASSO model was trained to predict house prices in an unseen data with RMSE of 0.12571. That being said, the model used in this report can be further improved with more advance techniques. 




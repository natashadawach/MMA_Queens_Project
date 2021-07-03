# Exploratory Data Analysis
The MET office data on global temperatures runs from 1850 to 2020. Temperatures in the original dataset came as anomalies, which were converted into absolute temperatures (anomalies added to 
baseline temperature) using the baseline temperature of 14°C. The baseline temperature of 14°C was derived from the reference period of 1961-1990 provided by the World Meteorological 
Organization (WMO)1. The reason our analysis included 14°C instead of other baseline period or later reference year is because as per WMO, “WMO still recommends the continuation of the 1961-
1990 period for the computation and tracking global climate anomalies”. Looking at the figure below, we see some interesting activity in the average temperature. We notice that the trend is stable between 1850 to 1930. From 1940 to 1950, there is minor positive jump in 
average temperatures, which stabilizes till 1980, after which we see an increasing trend. We can also find some peaks in the average global temperatures that correspond to some major events in 
the world:

1. Industrial Revolutions (1877 - 1878): Industrial revolutions brought industrial and 
technological advancements but at the expense of excessive use of fossil fuels and carbon 
dioxide. The release of carbon dioxide during this period is one of the reasons for a spike 
in the temperatures that we see in the figure below.
2. World War I and II: The other 2 peaks we see are during the periods of world war I and II. 
This makes sense. The ammunitions and war weapons used can cause a rise in the 
temperatures and these peaks are reflective of those weapons used.
3. Super El Niño: Since 1990s, there was El Niño effect, which is essentially the irregular 
pattern in winds that hamper the weather and lead to dry climate around the globe. We have been seeing a rise in the average temperature since the 90s, which is accompanied by 
frequent El Niño effects.

![image](https://user-images.githubusercontent.com/16128968/124367543-12f75880-dc26-11eb-9a95-9816208b02ce.png)

Additionally, we can break down the time series of Met office data to analyze the trend, seasonality, and noise. In the figure below, we can clearly see an upward trend as well as a strong seasonality in the decomposed graphs. There is also some white noise in the data. It becomes imperative to assess the trend and seasonality, as both of those factors will be used to determine a model that forecast temperatures by correctly recognizing the patterns in this dataset. 

![image](https://user-images.githubusercontent.com/16128968/124367557-328e8100-dc26-11eb-8ded-40e7b2194420.png)

# Cross-Validation and Metrics for Validation
The process of modelling time series data involves applying a series of models and then validating those models to select the best one that can provide good predictions. While there are many cross-validation techniques, our goal was to apply the same validation technique on all the models so that we could easily compare the results. The choice of cross-validation technique also relied on how expensive the technique is with regards to the time it takes to run the validation. While we understood the trade-off between the accuracy of validation and the price of validation in terms of time, we decided to perform time series cross-validation on ETS, TBAT, and ARIMA models, even though it costed a lot of time. This technique uses a window of training set to predict 1 value at a time. As the ARIMA model provided best results, we further built on the simple model to get ARIMA on residuals and tested it to see if it performs any better. ARIMA on residuals is only validated through rolling-horizon with a range of values predicted at a time as opposed to single value predicted in the case of ETS, and TBAT. 
In regards with the methods for comparison, I relied on a few metrics to select the best model. The three metrics that were used to pick the primary models for Met data and NASA are Mean Absolute Error (MAE), and Root Mean Squared Error (RMSE). RMSE and MAE work well in this case. RMSE penalized large errors, which can be quite helpful in our predictions of the weather.  I also realized that although RMSE has its advantage, it can also create an issue when a few bad predictions can increase the RMSE number for an otherwise great model that can generally give very good forecasts. Therefore, we coupled the RMSE metric with MAE calculation. MAE is easy to understand and avoids the issue posed by RMSE. Together, they allowed I to narrow down to the best model. 


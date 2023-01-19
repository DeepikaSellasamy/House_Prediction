 House_Prediction
 
 The main goal is we have to predict the sales price of houses in King County,It includes homes sold between May 2014 and May 2015
 
 The dataset cantains 20 house features plus the price, along with 21613 observations.
 
 The description for the 20 features is given below:

id :- It is the unique numeric number assigned to each house being sold.

date :- It is the date on which the house was sold out.

price:- It is the price of house which we have to predict so this is our target variable and aprat from it are our features.

bedrooms :- It determines number of bedrooms in a house.

bathrooms :- It determines number of bathrooms in a bedroom of a house.

sqft_living :- It is the measurement variable which determines the measurement of house in square foot.

sqft_lot : It is also the measurement variable which determines square foot of the lot.

floors: It determines total floors means levels of house.

waterfront : This feature determines whether a house has a view to waterfront 0 means no 1 means yes.

view : This feature determines whether a house has been viewed or not 0 means no 1 means yes.

condition : It determines the overall condition of a house on a scale of 1 to 5.

grade : It determines the overall grade given to the housing unit, based on King County grading system on a scale of 1 to 11

sqft_above : It determines square footage of house apart from basement.

sqft_basement : It determines square footage of the basement of the house.

yr_built : It detrmines the date of building of the house.

yr_renovated : It detrmines year of renovation of house.

zipcode : It determines the zipcode of the location of the house.

lat : It determines the latitude of the location of the house.

long : It determines the longitude of the location of the house.

sqft_living15 : Living room area in 2015(implies-- some renovations)

sqft_lot15 : lotSize area in 2015(implies-- some renovations)
 
 By observing the data, we can know that the price is dependent on various features like bedrooms(which is most dependent feature), bathrooms, sqft_living(second most important feature), sqft_lot, floors etc. 
 
 We can analyse the data using R:
 
 Data cleaning 
 
 Data visualization
 
 EDA
 
 Feature selection
 
 Model building-Linear,Decision tree,KNN model,Random forest
 
 Prediction using test data
 
 Finding the accuracy of the model

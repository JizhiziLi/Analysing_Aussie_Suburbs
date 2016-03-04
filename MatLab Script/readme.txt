==========MATLAB==============
age_Cosine.m is used for question A1 and A2, it takes 12 features(all the percentage of different age) from 2012 populations structure of 34 suburbs as input, calculate the cosine distance of each two suburbs and turn the distance to a 34*34 symmetric matrix. Implemented PCA algorithm for each row to decrease the dimension to 2. Pass result to plotByR.r script to plot it. Pre-processing of features are by python script.

age_Mahalanobis.m is same with age_Cosine.m, only difference is it has used Mahalanobis distance instead of Cosine distance.

land_Cosine.m is same with age_Cosines.m, instead of taking features from age structure, it takes 5 features from land use category for measuring similarity. The output is a 34*2 matrix which has decreased the dimension of instance from 34 to 2. 

land_Mahalanobis.m is similar with land_Cosine.m, just use Mahalanobis distance instead of Cosine one.



 
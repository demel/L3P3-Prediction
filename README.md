L3P3 - Prediction Area
====

![logo-com](https://raw.githubusercontent.com/centeropenmiddleware/l3p3-dashboard/master/images/logoCOM3.png) ![logo-produban](https://raw.githubusercontent.com/centeropenmiddleware/l3p3-dashboard/master/images/produban-big.png)


This repository contains all the work done in the area of predicting failures over Probudan's infrastructure. Specifically, over part of the infrastructure's network. The scripts it contains have been coded in R and present different algorithms for the creation of forecasting models. The list of what it contains is self explanatory based on each folder's name, but the project highlights are contained in:

* **2, Regularized Logistic Regression**: this experiment contains the Elastic Net Rare Events Logistic Regression model that modelled SCF successfully for the first time in the project. It is the foundation of folders 3, 4, 5 and 6. Each of them expands the capabilities of this model or applies it to a different environment.
* **3, Node-Aware Elastic Net**: this experiment tries to predict the node in which a failure will happen, apart from the type of failure.
* **4, Resources-and-Node Aware Elastic Net**: this last iteration of the Elastic Net adds resources as input variables to try and exploit the information they contain to improve the Node-Aware Elastic Net's performance. In several of its iterations it uses the [Breakout Detection package released by Twitter](https://github.com/twitter/BreakoutDetection) to detect changes in a time series trend.

Apart from specific prediction work, parsing scripts can be found in the parsers folder, used to create the algorithms' input.

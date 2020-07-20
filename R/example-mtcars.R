#' mtcars Dataset
#'
#' These cars go vroom!
#'
#' @col [out] mpg
#' Numeric.
#' `mpg` or "Miles Per Gallon" is how many miles you can drive on
#' a gallon of fuel.
#' @col [out] cyl
#' Integer
#' Number of cylinders
#' @col [out] disp
#' Integer.
#' I literally don't know what this means
#' @col [out] hp
#' Integer
#' Horsepower is a measure of the number of miniature horses this car
#' can fit inside itself, which run on a treadmill that make it go fast.
#' @col [out] drat
#' Numeric
#' This is a word you say when something doesn't go the way you hoped.
#' **Drat!**
#' @col [out] wt
#' Numeric
#' Stands for "What the..." - An exclamation of surprise!
#'
#' Or **weight**, maybe.
#' @col [out] qsec
#' Numeric.
#' Number of seconds you need to line up in a queue in order to buy this car.
#' @col [out] vs
#' Logical, Does the car ever compete?
#' @col [out] am
#' Logical, is this car okay to use in the morning, or should you save it for
#' after midday?
#' @col [out] gear
#' Integer,
#'
#' How many different gears transmissions does this car have.
#' @col [out] carb
#' Integer, Number of grams of carbohydrates contained in this car.
#' Typically pretty low, as cars are not particularly nutritious.
#' @md
#' @family example_datasets
#' @name mtcars_dataset
NULL


#' Linear Models
#'
#' See [stats::lm()] for more info :)
#'
#' @col [out] coefficients
#' a named vector of coefficients
#' @col [out] residuals
#' The residuals, that is response minus fitted values.
#' @col [out] fitted.values
#' The fitted mean values.
#' @col [out] rank
#' the numeric rank of the fitted linear model.
#' @col [out] weights
#' (only for weighted fits) the specified weights.
#' @col [out] df.residual
#' the residual degrees of freedom.
#' @col [out] call
#' the matched call.
#' @col [out] terms
#' the [stats::terms()] object used.
#' @col [out] contrasts
#' (only where relevant) the contrasts used.
#' @col [out] xlevels
#' (only where relevant) a record of the levels of the factors used in fitting.
#' @col [out] offset
#' the offset used (missing if none were used).
#' @col [out] y
#' if requested, the response used.
#' @col [out] x
#' if requested, the model matrix used.
#' @col [out] model
#' if requested (the default), the model frame used.
#' @col [out] na.action
#' (where relevant) information returned by [stats::model.frame()] on the special handling of NAs.
#' @name lm_object
#' @family example_datasets
NULL


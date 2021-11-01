# Introduction to Generalized Linear Models

In this two day course, we provide a comprehensive practical and theoretical introduction to generalized linear models using R. Generalized linear models are generalizations of linear regression models for situations where the outcome variable is, for example, a binary, or ordinal, or count variable, etc. The specific models we cover include binary, binomial, ordinal, and categorical logistic regression, Poisson and negative binomial regression for count variables. We will also cover zero-inflated Poisson and negative binomial regression models. On the first day, we begin by providing a brief overview of the normal general linear model. Understanding this model is vital for the proper understanding of how it is generalized in generalized linear models. Next, we introduce the widely used binary logistic regression model, which is is a regression model for when the outcome variable is binary. Next, we cover the ordinal logistic regression model, specifically the cumulative logit ordinal regression model, which is used for the ordinal outcome data. We then cover the case of the categorical, also known as the multinomial, logistic regression, which is for modelling outcomes variables that are polychotomous, i.e., have more than two categorically distinct values. On the second day, we begin by covering Poisson regression, which is widely used for modelling outcome variables that are counts (i.e the number of times something has happened). We then cover the binomial logistic and negative binomial models, which are used for similar types of problems as those for which Poisson models are used, but make different or less restrictive assumptions. Finally, we will cover zero inflated Poisson and negative binomial models, which are for count data with excessive numbers of zero observations.

## Intended Audience

This course is aimed at anyone who is interested in advanced statistical modelling as it is practised widely throughout academic scientific research, as well as widely throughout the public and private sectors.

## Teaching Format

This course will be largely practical, hands-on, and workshop based. For each topic, there will first be some lecture style presentation, i.e., using slides or blackboard, to introduce and explain key concepts and theories. Then, we will cover how to perform the various statistical analyses using R. Any code that the instructor produces during these sessions will be uploaded to a publicly available GitHub site after each session.

The course will take place online using Zoom. On each day, the live video broadcasts will occur between (UK local time, GMT/UTC timezone) at:

* 10am-12pm
* 1pm-3pm
* 4pm-6pm
 
All sessions will be video recorded and made available to all attendees as soon as possible, hopefully soon after each 2hr session.
  
Attendees in different time zones will be able to join in to some of these live broadcasts, even if all of them are not convenient times.

Although not strictly required, using a large monitor or preferably even a second monitor will make the learning experience better, as you will be able to see my RStudio and your own RStudio simultaneously. 

All the sessions will be video recorded, and made available immediately on a private video hosting website. Any materials, such as slides, data sets, etc., will be shared via GitHub.

## Assumed quantitative knowledge

We will assume familiarity with general statistical concepts, linear models, statistical inference (p-values, confidence intervals, etc). Anyone who has taken undergraduate (Bachelor's) level introductory courses on (applied) statistics can be assumed to have sufficient familiarity with these concepts.

## Assumed computer background

Minimal prior experience with R and RStudio is required. Attendees should be familiar with some basic R syntax and commands, how to write code in the RStudio console and script editor, how to load up data from files, etc. 

## Equipment and software requirements

All the software that is needed for this course is free and open source and will run identically on Macs, Windows, and Linux.

Instructions on how to install the software is [here](software.md).

# Course programme 

## Day 1 

* Topic 1: *The general linear model*. We begin by providing an overview of the normal, as in normal distribution, general linear model, including using categorical predictor variables. Although this model is not the focus of the course, it is the foundation on which generalized linear models are based and so must be understood to understand generalized linear models.
* Topic 2: *Binary logistic regression*. Our first generalized linear model is the binary logistic regression model, for use when modelling binary outcome data. We will present the assumed theoretical model behind logistic regression, implement it using R's `glm`, and then show how to interpret its results, perform predictions, and (nested) model comparisons.
* Topic 3: *Ordinal logistic regression*. Here, we show how the binary logistic regresion can be extended to deal with ordinal data. We will present the mathematical model behind the so-called cumulative logit ordinal model, and show how it is implemented in the `clm` command in the `ordinal` package.
* Topic 4: *Categorical logistic regression*. Categorical logistic regression, also known as multinomial logistic regression, is for modelling polychotomous data, i.e. data taking more than two categorically distinct values. Like ordinal logistic regression, categorical logistic regression is also based on an extension of the binary logistic regression case.

## Day 2

* Topic 5: *Poisson regression*. Poisson regression is a widely used technique for modelling count data, i.e., data where the variable denotes the number of times an event has occurred.
* Topic 6: *Binomial logistic regression*. When the data are counts but there is a maximum number of times the event could occur, e.g. the number of items correct on a multichoice test, the data is better modelled by a binomial logistic regression rather than a Poisson regression. 
* Topic 7: *Negative binomial regression*. The negative binomial model is, like the Poisson regression model, used for unbounded count data, but it is less restrictive than Poisson regression, specifically by dealing with overdispersed data.
* Topic 8: *Zero inflated models*. Zero inflated count data is where there are excessive numbers of zero counts that can be modelled using either a Poisson or negative binomial model. Zero inflated Poisson or negative binomial models are types of latent variable models.




# README

For the full forecast, please see the following links:

-   [Senate](https://www.thedatadiary.net/senate/)
-   [House](https://www.thedatadiary.net/house/)
-   [Governor](https://www.thedatadiary.net/governor/)

# Model Updates

## punk november 2.1

###### 9-14-22

-   Updated candidate roster post-primary

## punk november 2.0

###### 9-7-22

-   Updated poll model to scale loess span relative to days out from
    election:
    -   `last_poll` \> 100 days -\> `span = 1`
    -   `last_poll` \<= 100 days -\>
        `span = 0.25 * (last_poll/100) + 0.75`
-   Updated all historical data as well.

## punk november 1.0

###### 9-5-22

-   Initial release

# House

![Current House Distribution](diagnostics/current_house_topline.png)

![Rolling House
Distribution](diagnostics/rolling_house_distribution.png)

![Rolling House Probability](diagnostics/rolling_house_probability.png)

# Senate

![Current Senate Distribution](diagnostics/current_senate_topline.png)

![Rolling Senate
Distribution](diagnostics/rolling_senate_distribution.png)

![Rolling Senate
Probability](diagnostics/rolling_senate_probability.png)

# Random Race

![Random Race - Voteshare](diagnostics/random_race.png)

# Training Diagnostics

## Pre-fit EDA

![Demographics vs result](diagnostics/training/demographics.png)

![Poll model output vs result](diagnostics/training/poll_model.png)

![Poll model output with
interaction](diagnostics/training/poll_model_interactions.png)

![PVI vs result](diagnostics/training/pvi.png)

## Post-fit EDA

![Predictions by race](diagnostics/training/predictions_race.png)

![Model residuals](diagnostics/training/predictions_residuals.png)

![Most polled races](diagnostics/training/most_polled_races.png)

![Biggest model misses](diagnostics/training/model_misses.png)

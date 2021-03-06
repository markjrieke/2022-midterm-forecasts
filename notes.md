notes
================

## to-do:

-   add 2022 `region_similarity` scores once districts are finalized.
-   add in features (listed below) to training dataset for xgb model
-   add in method for automatically gathering features for xgb model
    once live (`{rvest}`, polls from FTE, etc.)
-   train baseline xgb mod
-   create 1,000 xgb mods from bootstraps (use `{butcher}` to keep file
    size in check)
-   write script to aggregate output from bootstraps
    -   only predict based on one day (i.e., if I miss a manual day,
        have it only update the missed days, not every day)
    -   better to output to a single df (i.e., all preds happnen in the
        script?) or save outputs from each mod to csv?
-   create landing pages for races:
    -   house/gov/senate overview
        -   map
        -   p\_control
        -   expected \# of members
    -   ind races
        -   table (show top 10 based on closeness; p\_win/expected
            margin)
            -   gt()?
        -   have a search fn to show an individual race
        -   individual race p\_win
        -   individual race voteshare
-   add model ethics to site (see below)
-   add a *how-this-works* landing page (maybe just a link at the bottom
    of every page to a blog post)
-   dig into huffpost polls & the pollstR package/api???

## notes:

-   ratings ranges:
    -   p &lt; 0.65 -&gt; uncertain
    -   p &lt; 0.85 -&gt; likely
    -   p &lt; 0.99 -&gt; very likely
    -   p &gt;= 0.99 -&gt; safe
-   possibly can use d3.js via `{r2d3}` for main viz
-   want to add really useful packages for the project
    -   `{furrr}`
    -   `{tidycensus}`
    -   `{shadowtext}`
    -   `{r2d3}` (?)
    -   `{butcher}`
    -   `{tidymodels}`
    -   `{tidyverse}`
    -   `{gt}` (?)
    -   `{rvest}`
    -   `{polite}`
-   model ethics card
    -   racial makeup
        -   what: only includes white/black/hispanic/aapi/other
        -   problem: ignores other racial demographics
        -   why: data consistency - FTE’s district demographics only
            include these races. ACS data not available for 2022
            districts.
        -   future: access up-to-date ACS data via `{tidycensus}`
    -   data leakage
        -   what: `poll_weighting.R` trains poll weighting on entire
            dataset
        -   problem: **this is data leakage** - polls in the testing
            dataset were used to train the averaging model.
        -   why: wanted to use as much data as possible for poll
            averaging model
        -   future: utilize an `initial_split()` stratified by
            cycle/race for keeping testing & training data separate,
            even prior to training the main xgb model.
    -   similarity scores
        -   what: currently based only on race
        -   problem: race is only one part of a region’s identity and
            doesn’t fully capture how similar two regions may be.
        -   why: at the time of training the poll averaging model, only
            had racial data available for 2022 districts.
        -   future: use ACS district data to eek out addional features.
            For current model, may be able to retrain based on
            educational attainment (found in POLITICO’s summaries of
            districts). (if you do this you should also address the
            aforementioned data leakage issue)
    -   top D/R
        -   what: only looking at the top D/R candidates in each race
        -   problem: model doesn’t handle races where one party has
            multiple candidates (e.g., warnock/loeffler/collins), or
            account for third parties, or address races with only one
            candidate.
        -   why: beta-distribution based poll weighting model only
            handles two candidates.
        -   future: possibly come up with a new weighting model that can
            handle multiple candidates, candidates from the same party,
            etc (maybe a loess?)
    -   election
        -   what: only looking at general election, not runoff
        -   problem: general election may not be decisive
        -   why: considered general/runoff environments different
        -   future: possibly add in runoffs as their own election data
            points, or just utilize current model for evaluating runoff
            elections.
    -   some features missing
        -   what: some features that definitely have signal aren’t
            included (financial makeup of region, prez approval, etc.)
        -   problem: losing potential signal
        -   why: only have two cycles (2018/2020) worth of polling data,
            so some datapoints are impractical to include (n = 2). Some
            just don’t have data available (e.g., 2020 census data via
            `{tidycensus}` returns small set of demographic data, and
            FTE district data only includes race).
        -   future: dig for more polling data? Use up-do-date district
            ACS data.
    -   only two cycles worth of data
        -   what: only utilizing 2018/2020 cycles’ data
        -   problem: if there was dem/rep over/under performance during
            these cycles, may throw model off. (e.g., 2018 was a “blue
            wave” and represents a good chunk of the training data)
        -   why: only had two cycles worth of polling data to reference.
        -   future: append current model with 2022 data, also dig for
            more polling data?
    -   some FEC filing not available on ballotpedia
        -   what: some campaign finance filings aren’t not available on
            ballotpedia
        -   problem: losing signal for those that are missing
        -   why: ballotpedia only includes FEC filings for “official
            candidates,” based on their selection criteria.
        -   future: impute values? or do some extra digging…
-   potential features
    -   data currently available in repo:
        -   polls
        -   polls ci spread
        -   polls ci lower
        -   polls ci upper
        -   number of polls for the race
        -   number of polls in the last x weeks
        -   racial makeup
        -   incumbency
        -   prev-party (does this make sense for the new districts
            though?)
    -   data available online
        -   years in seat
        -   years in gov’t
        -   endorsements
        -   total fundraising
        -   cash on hand?
        -   total spent?
        -   spending ratio (dem/rep & rep/dem)
        -   partisan lean? (maybe…)
        -   experts’ ratings (maybe…)
        -   candidate demographics (age, gender, !race, maybe…)
        -   candidate idealogy (dw nominate? - not sure if this is
            available for new candidates)
    -   data maybe available in the future
        -   median or mean age
        -   population density
        -   religious affiliation
        -   educational attainment
-   Things I wish I’d done differently
    -   File structure
        -   file structure isn’t haphazard, but it wasn’t planned out.
        -   sometimes difficult to keep all the connections
            straightforward in my head
        -   next time, I’d prefer to plan out layout in advance
    -   Better use of git/github
        -   I’ve been making commits, but it may have been better to
            make PRs
    -   Use github actions
        -   I got burned once when trying to setup github actions where
            the .renv lockfile somehow caused R to move all the
            libraries around on my desktop — link to issue in renv
    -   More generalizable poll model
        -   current uses binary beta dist - good for applying pollster
            weights but doesn’t scale to multiple candidates
        -   maybe next time can use a gam or loess
        -   that way, could model 3rd party candidates
        -   might also be faster than the crazy long training process
            for the current model lol
        -   may also want to build model based on days before election?
    -   Using more polls/data
        -   FTE’s polling database only goes back so far
        -   Huffpo goes back to 2012 & there is an api for it
        -   maybe next time use a model that incorporates the daily
            delta/days till election day?
    -   Working with nested/hierarchical data!
        -   polls!!!
        -   endorsements
-   Things I’ve learned
    -   purrr
    -   furrr
    -   deeper understanding of tidymodels
    -   bootstrapping for CIs
    -   interacting with model engines more directly
    -   tidy evaluation

Lytics SegmentML Dashboard
==========================

A dashboard for creating and visualizing Lytics [SegmentML models](http://www.getlytics.com/developers/rest-api#segment-m-l-group).

## Setup/Installation

Either run the script via `Rscript install.R`, or source the installation file from an R session (`source("install.R")`).

## Serving the Dashboard
Either run the script via `Rscript app.R`, or source the installation file from an R session (`source("app.R")`).

This will start serving the dashboard on [http://localhost:1235](http://localhost:1235).

## Navigating

Then provide your Lytics account ID and API key in the form in the navigation sidebar.  Clicking "Update!" will load your account data and models.

### Model Summary

Visualize existing models with [variable importance summaries](https://www.coursera.org/learn/predictive-analytics/lecture/MACer/random-forests-variable-importance), histograms of model features for source and target segments, prediction densities, and [partial dependence plots](http://scikit-learn.org/stable/auto_examples/ensemble/plot_partial_dependence.html).

### Model Creation

Create models with the following inputs:

| Input | Description |
| ----- | ----------- |
| **Source Segment** | Segment of users to perform predictions against.  Examples could include unknown users, users who haven't registered yet, etc. |
| **Target Segment** | Segment of users indicating target behavior to predict.  Examples could include registered users, high purchasing / high value users, users who open emails, etc. |
| **Target Field** | (optional) Optional numeric or string field to choose as a model target.  Numeric predictions will be written to the `segment_prediction` user field, and categorical classifications will be written to the `class_prediction` user field. |
| **Aspect Collections** | (optional) Pre-defined segment categories that can serve as useful feature bundles. Unless you have a good reason to include them, the only aspect collection you should apply by default is "Content". |
| **Custom Fields** | (optional) Any additional user fields to include as model features. |
| **Use Scores** | Should Lytics' behavioral scores be used in the model? |
| **Build Only?** | Should the model be ran in training mode *only*?  While experimenting with model configurations, this should always be selected.  Deselecting and running the model in evaluation mode will add model evaluation to the user scoring pipeline.  |
| **Save Segments** | Should a segment be created to identify users in the source segment with model scores above a decision threshold chosen by via ROC curves. |
| **Tune Model** | Should a model be selected using cross-validation based on model R^2. |
| **Tags** | (optional) Arbitrary set of tags/labels to apply to apply to a model, for organizational purposes. |
| **Additional Custom Fields** | (optional) Useful for specifying entries in maps or sets.  For example, for a mapcount field named `email_actions`, specifying `email_actions.open` would include the count of email opens as a distinct feature in the feature matrix. |
| **Sample Size** | Defaults to 5000. |
| **Model Type** | Select either "Random Forest" or "Gradient Boosting Machine". |

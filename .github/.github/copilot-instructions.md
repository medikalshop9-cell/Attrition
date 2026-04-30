# Copilot Instructions

## Project Context
This project uses R for machine learning and React for the frontend interface.

---

## Backend (R) Guidelines

### Language
- STRICTLY use R (no Python)

### Structure
- data_cleaning.R
- eda.R
- logistic_model.R
- decision_tree.R
- clustering.R
- api.R (plumber API)

### Libraries
- dplyr
- ggplot2
- caret
- rpart
- plumber

### Modeling
- Logistic Regression → `glm(..., family = "binomial")`
- Decision Tree → `rpart()`
- Clustering → `kmeans()`

### API
- Use plumber to expose endpoints:
  - `/predict`
  - `/health`
- Input: JSON employee data
- Output: prediction + probability

---

## Frontend (React) Guidelines

### Stack
- React (Vite or Create React App)
- Axios for API calls

### Features
- Form to input employee data
- Display prediction:
  - Attrition risk (Yes/No)
  - Probability score
- Optional dashboards:
  - Charts (Recharts)

---

## Ethics Reminder
- Avoid biased outputs
- Clearly explain predictions
- Do not infer causation blindly
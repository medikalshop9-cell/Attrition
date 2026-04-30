# AGENTS.md

## Project Overview
This project predicts employee attrition using machine learning models built in R, and exposes results through a React-based user interface.

---

## Agents (Conceptual Roles)

### 1. Data Exploration Agent (R)
- Performs descriptive statistics
- Creates visualizations (ggplot2)
- Identifies attrition patterns

### 2. Modeling Agent (R)
- Logistic Regression (`glm`)
- Decision Tree (`rpart`)
- Model evaluation:
  - Confusion matrix
  - Accuracy, Precision, Recall

### 3. Clustering Agent (R)
- K-Means clustering
- Employee segmentation (risk profiles)

### 4. API / Integration Agent (R)
- Exposes trained model via API using:
  - plumber (recommended)
- Handles prediction requests from frontend

### 5. Frontend Agent (React)
- Builds UI dashboard
- Displays:
  - Attrition predictions
  - Risk scores
  - Visual insights
- Communicates with R API

### 6. Ethics & Insights Agent
- Ensures fairness and interpretability
- Translates results into HR insights

---

## Workflow
1. Data Cleaning (R)
2. Exploratory Data Analysis (R)
3. Model Training (R)
4. Model Export / API (R plumber)
5. Frontend Development (React)
6. Integration (API ↔ UI)
7. Final Reporting

---

## Tools
- R (core ML)
- ggplot2, dplyr, caret, rpart
- plumber (API layer)
- React (frontend)

---

## Output
- Trained ML models
- API endpoints
- React dashboard
- Final report & presentation
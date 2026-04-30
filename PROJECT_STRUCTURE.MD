# Project Structure

project/
│
├── backend/
│   ├── data/
│   │   └── IBM_HR.csv
│   │
│   ├── scripts/
│   │   ├── data_cleaning.R
│   │   ├── eda.R
│   │   ├── logistic_model.R
│   │   ├── decision_tree.R
│   │   ├── clustering.R
│   │   └── save_model.R
│   │
│   ├── models/
│   │   └── attrition_model.rds
│   │
│   ├── api/
│   │   └── api.R   # plumber API
│   │
│   └── outputs/
│       ├── plots/
│       └── reports/
│
├── frontend/
│   ├── public/
│   ├── src/
│   │   ├── components/
│   │   │   ├── PredictionForm.jsx
│   │   │   ├── ResultCard.jsx
│   │   │   └── Dashboard.jsx
│   │   │
│   │   ├── services/
│   │   │   └── api.js
│   │   │
│   │   ├── App.jsx
│   │   └── main.jsx
│   │
│   └── package.json
│
├── docs/
│   ├── AGENTS.md
│   ├── SKILLS.md
│   ├── ETHICS.md
│   └── PROJECT_STRUCTURE.md
│
├── README.md
└── .github/
    └── copilot-instructions.md
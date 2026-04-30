# API Specification (R plumber)

## Base URL
http://localhost:8000

---

## 1. Health Check
GET /health

Response:
{
  "status": "ok"
}

---

## 2. Predict Attrition
POST /predict

### Input (JSON)
{
  "Age": 35,
  "MonthlyIncome": 5000,
  "JobSatisfaction": 3,
  "WorkLifeBalance": 2
}

### Output
{
  "prediction": "Yes",
  "probability": 0.78
}

---

## Notes
- Model loaded from .rds file
- Preprocessing must match training pipeline
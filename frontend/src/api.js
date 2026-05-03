import axios from 'axios'

const BASE_URL = import.meta.env.VITE_API_URL || 'http://localhost:8000'

const client = axios.create({ baseURL: BASE_URL, timeout: 6000 })

export async function healthCheck() {
  const { data } = await client.get('/health')
  return data
}

/**
 * POST /predict
 * @param {Object} employeeData - object with all 30 required fields
 * @returns {{ prediction: string, probability: number, threshold: number, risk_level: string }}
 */
export async function predict(employeeData) {
  const { data } = await client.post('/predict', employeeData)
  return data
}

/**
 * GET /insights
 * @returns the full insights_report.json (metrics, predictors, recommendations, ethics)
 */
export async function getInsights() {
  const { data } = await client.get('/insights')
  return data
}

/**
 * GET /feature_importance
 * @returns feature_importance.json (Phase 12 analysis)
 */
export async function getFeatureImportance() {
  const { data } = await client.get('/feature_importance')
  return data
}

/**
 * GET /shap_metadata
 * @returns shap_metadata.json (intercept, betas, beeswarm data)
 */
export async function getSHAPMetadata() {
  const { data } = await client.get('/shap_metadata')
  return data
}

/**
 * POST /shap
 * Computes LinearSHAP values for a single employee server-side.
 * @param {Object} employeeData - same 30-field payload as /predict
 * @returns {{ risk_score, risk_label, baseline_prob, intercept, shap_values[] }}
 */
export async function getSHAPExplanation(employeeData) {
  const { data } = await client.post('/shap', employeeData)
  return data
}

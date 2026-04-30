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

import { BrowserRouter, Routes, Route, Navigate } from 'react-router-dom'
import Layout from './components/Layout'
import PredictionTool from './pages/PredictionTool'
import ModelInsights from './pages/ModelInsights'
import RiskWatch from './pages/RiskWatch'
import ModelSHAP from './pages/ModelSHAP'
import { ToastProvider } from './components/ui/Toast'
import { DarkModeProvider } from './context/DarkMode'

export default function App() {
  return (
    <DarkModeProvider>
    <ToastProvider>
      <BrowserRouter>
        <Routes>
          <Route path="/" element={<Layout />}>
            <Route index element={<Navigate to="/predict" replace />} />
            <Route path="predict" element={<PredictionTool />} />
            <Route path="insights" element={<ModelInsights />} />
            <Route path="risk-watch" element={<RiskWatch />} />
            <Route path="shap" element={<ModelSHAP />} />
          </Route>
        </Routes>
      </BrowserRouter>
    </ToastProvider>
    </DarkModeProvider>
  )
}

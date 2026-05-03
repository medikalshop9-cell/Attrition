import { useState, useEffect } from 'react'
import { motion, AnimatePresence } from 'framer-motion'
import { getInsights } from '../api'

// ── Fallback static data (shown while API is loading / unavailable) ──────────
const STATIC_METRICS = [
  { label: 'Test AUC',   value: '0.780', icon: 'show_chart',              color: 'text-blue-700' },
  { label: 'Test F1',    value: '0.615', icon: 'target',                  color: 'text-purple-600' },
  { label: 'Precision',  value: '0.571', icon: 'precision_manufacturing', color: 'text-amber-600' },
  { label: 'Recall',     value: '0.667', icon: 'radar',                   color: 'text-green-600' },
  { label: 'Accuracy',   value: '86.4%', icon: 'check_circle',            color: 'text-teal-600' },
  { label: 'Threshold',  value: '0.26',  icon: 'tune',                    color: 'text-slate-600' },
]

const STATIC_BENCHMARKS = [
  { name: 'Logistic Regression', threshold: 0.26, acc: 0.864, prec: 0.571, recall: 0.667, f1: 0.615, auc: 0.780, selected: true },
  { name: 'glmnet Ridge',        threshold: 0.18, acc: 0.795, prec: 0.418, recall: 0.639, f1: 0.505, auc: 0.821, selected: false },
  { name: 'SVM',                 threshold: 0.18, acc: 0.809, prec: 0.438, recall: 0.583, f1: 0.500, auc: 0.857, selected: false },
  { name: 'XGBoost',             threshold: 0.45, acc: 0.805, prec: 0.429, recall: 0.583, f1: 0.494, auc: 0.860, selected: false },
  { name: 'Random Forest',       threshold: 0.16, acc: 0.768, prec: 0.364, recall: 0.556, f1: 0.440, auc: 0.868, selected: false },
  { name: 'Decision Tree',       threshold: 0.10, acc: 0.741, prec: 0.316, recall: 0.500, f1: 0.387, auc: 0.749, selected: false },
]

const PRIORITY_STYLES = {
  critical: { bg: 'bg-red-50 dark:bg-red-500/10 border-red-200 dark:border-red-500/30',     badge: 'bg-red-100 dark:bg-red-500/20 text-red-700 dark:text-red-400',     icon: '⚡' },
  high:     { bg: 'bg-amber-50 dark:bg-amber-500/10 border-amber-200 dark:border-amber-500/30', badge: 'bg-amber-100 dark:bg-amber-500/20 text-amber-700 dark:text-amber-400', icon: '⚠️' },
  medium:   { bg: 'bg-blue-50 dark:bg-blue-500/10 border-blue-200 dark:border-blue-500/30',   badge: 'bg-blue-100 dark:bg-blue-500/20 text-blue-700 dark:text-blue-400',   icon: '💡' },
}

// ── Sub-components ────────────────────────────────────────────────────────────

function MetricCard({ label, value, icon, color, index = 0 }) {
  return (
    <motion.div
      className="bg-white dark:bg-slate-900 border border-slate-200 dark:border-slate-800 rounded-2xl p-5 flex items-center gap-4 shadow-sm hover:shadow-md transition-shadow"
      initial={{ opacity: 0, y: 16 }}
      animate={{ opacity: 1, y: 0 }}
      transition={{ duration: 0.35, delay: index * 0.07, ease: 'easeOut' }}
    >
      <div className={`w-12 h-12 rounded-xl bg-slate-50 dark:bg-slate-800 flex items-center justify-center ${color}`}>
        <span className="material-symbols-outlined text-[24px]">{icon}</span>
      </div>
      <div>
        <p className="text-[11px] font-semibold text-slate-500 dark:text-slate-400 uppercase tracking-wider">{label}</p>
        <p className={`text-2xl font-black ${color}`}>{value}</p>
      </div>
    </motion.div>
  )
}

function MetricBadge({ value, isBest }) {
  return (
    <span className={`font-bold text-right tabular-nums ${
      isBest ? 'bg-blue-700 text-white px-2 py-0.5 rounded-lg' : 'text-slate-700 dark:text-slate-300'
    }`}>
      {value}
    </span>
  )
}

// ── Page ──────────────────────────────────────────────────────────────────────

export default function ModelInsights() {
  const [insights, setInsights]   = useState(null)
  const [loading, setLoading]     = useState(true)
  const [apiError, setApiError]   = useState(false)
  const [activeTab, setActiveTab] = useState('overview')
  const [selectedModelName, setSelectedModelName] = useState('Logistic Regression')

  useEffect(() => {
    getInsights()
      .then(data => { setInsights(data); setLoading(false) })
      .catch(() => { setApiError(true); setLoading(false) })
  }, [])

  const benchmarks = insights?.model_comparison ?? STATIC_BENCHMARKS
  const selectedBenchmark = benchmarks.find(m => m.name === selectedModelName) ?? benchmarks[0]
  const isLRSelected = selectedModelName === 'Logistic Regression'
  const cm         = isLRSelected ? (insights?.confusion_matrix ?? { tn: 166, fp: 18, fn: 12, tp: 24 }) : null
  const predictors = insights?.top_predictors    ?? []
  const recommendations  = insights?.hr_recommendations   ?? []
  const ethicalItems     = insights?.ethical_implications ?? []

  const metrics = isLRSelected
    ? (insights
        ? [
            { label: 'Test AUC',   value: insights.test_metrics.auc.toFixed(3),       icon: 'show_chart',              color: 'text-blue-700' },
            { label: 'Test F1',    value: insights.test_metrics.f1.toFixed(3),        icon: 'target',                  color: 'text-purple-600' },
            { label: 'Precision',  value: insights.test_metrics.precision.toFixed(3), icon: 'precision_manufacturing', color: 'text-amber-600' },
            { label: 'Recall',     value: insights.test_metrics.recall.toFixed(3),    icon: 'radar',                   color: 'text-green-600' },
            { label: 'Accuracy',   value: (insights.test_metrics.accuracy * 100).toFixed(1) + '%', icon: 'check_circle', color: 'text-teal-600' },
            { label: 'Threshold',  value: String(insights.threshold),                 icon: 'tune',                    color: 'text-slate-600' },
          ]
        : STATIC_METRICS)
    : [
        { label: 'Val AUC',    value: Number(selectedBenchmark?.auc).toFixed(3),    icon: 'show_chart',              color: 'text-blue-700' },
        { label: 'Val F1',     value: Number(selectedBenchmark?.f1).toFixed(3),     icon: 'target',                  color: 'text-purple-600' },
        { label: 'Precision',  value: Number(selectedBenchmark?.prec).toFixed(3),   icon: 'precision_manufacturing', color: 'text-amber-600' },
        { label: 'Recall',     value: Number(selectedBenchmark?.recall).toFixed(3), icon: 'radar',                   color: 'text-green-600' },
        { label: 'Accuracy',   value: (Number(selectedBenchmark?.acc) * 100).toFixed(1) + '%', icon: 'check_circle', color: 'text-teal-600' },
        { label: 'Threshold',  value: String(selectedBenchmark?.threshold ?? '—'), icon: 'tune', color: 'text-slate-600' },
      ]

  const tabs = [
    { id: 'overview',        label: 'Overview' },
    { id: 'predictors',      label: 'Top Predictors' },
    { id: 'recommendations', label: 'HR Recommendations' },
    { id: 'ethics',          label: 'Ethics' },
  ]

  return (
    <div>
      {/* Header */}
      <div className="mb-6 flex items-end justify-between">
        <div>
          <nav className="flex items-center gap-2 text-[11px] text-slate-400 mb-3 uppercase tracking-widest font-semibold">
            <span>Analytics</span>
            <span className="material-symbols-outlined text-[14px]">chevron_right</span>
            <span className="text-blue-600 dark:text-blue-400">Model Insights</span>
          </nav>
          <h2 className="text-3xl font-black tracking-tight text-primary dark:text-blue-400">Model Evaluation Dashboard</h2>
          <p className="text-sm text-slate-500 dark:text-slate-400 mt-2 leading-relaxed">
            Comparative analysis, HR recommendations and ethical review for the attrition model.
          </p>
        </div>
        {loading && (
          <div className="text-xs text-blue-700 dark:text-blue-400 bg-blue-50 dark:bg-blue-500/10 border border-blue-200 dark:border-blue-500/30 rounded-xl px-3 py-2 flex items-center gap-2">
            <motion.span
              className="material-symbols-outlined text-[16px]"
              animate={{ rotate: 360 }}
              transition={{ repeat: Infinity, duration: 1, ease: 'linear' }}
            >refresh</motion.span>
            Loading live insights…
          </div>
        )}
      </div>

      {/* Model selector + KPI strip */}
      <section className="mb-6">
        <div className="flex flex-wrap gap-2 mb-4">
          {(insights?.model_comparison ?? STATIC_BENCHMARKS).map(m => (
            <button
              key={m.name}
              onClick={() => setSelectedModelName(m.name)}
              className={`px-3 py-1.5 rounded-xl text-xs font-semibold border transition-all ${
                selectedModelName === m.name
                  ? 'bg-primary text-white border-primary shadow-sm shadow-blue-900/20'
                  : 'bg-white dark:bg-slate-800 text-slate-600 dark:text-slate-300 border-slate-200 dark:border-slate-700 hover:bg-slate-50 dark:hover:bg-slate-700'
              }`}
            >
              {m.name}
              {m.selected && <span className="ml-1.5 text-[9px] opacity-75 uppercase">★ deployed</span>}
            </button>
          ))}
        </div>
        <h3 className="text-[11px] font-bold uppercase tracking-wider text-slate-500 dark:text-slate-400 mb-4">
          {selectedModelName} — {isLRSelected ? 'Test Set Performance' : 'Validation Set Performance'}
        </h3>
        <div className="grid grid-cols-2 md:grid-cols-3 xl:grid-cols-6 gap-4">
          {metrics.map((m, i) => <MetricCard key={m.label} {...m} index={i} />)}
        </div>
      </section>

      {/* Tabs */}
      <div className="flex gap-1 mb-6 bg-slate-100 dark:bg-slate-800/60 p-1 rounded-xl w-fit">
        {tabs.map(tab => (
          <button
            key={tab.id}
            onClick={() => setActiveTab(tab.id)}
            className={`px-4 py-2 rounded-lg text-sm font-semibold transition-all ${
              activeTab === tab.id
                ? 'bg-white dark:bg-slate-700 text-primary dark:text-blue-300 shadow-sm'
                : 'text-slate-500 dark:text-slate-400 hover:text-slate-700 dark:hover:text-slate-200'
            }`}
          >
            {tab.label}
          </button>
        ))}
      </div>

      <AnimatePresence mode="wait">
        {/* ── OVERVIEW TAB ── */}
        {activeTab === 'overview' && (
          <motion.div key="overview" initial={{ opacity: 0, y: 8 }} animate={{ opacity: 1, y: 0 }} exit={{ opacity: 0 }} transition={{ duration: 0.2 }}>
            <div className="grid grid-cols-12 gap-6 mb-8">
              {/* Benchmarks table */}
              <div className={`col-span-12 ${isLRSelected ? 'lg:col-span-8' : 'lg:col-span-12'} bg-white dark:bg-slate-900 border border-slate-200 dark:border-slate-800 rounded-2xl overflow-hidden shadow-sm`}>
                <div className="px-6 py-4 border-b border-slate-100 dark:border-slate-800 flex justify-between items-center bg-slate-50/60 dark:bg-slate-900/60">
                  <h3 className="text-base font-bold text-primary dark:text-blue-400">Model Benchmarks</h3>
                  <span className="bg-blue-900 text-white text-[10px] px-2 py-1 rounded-lg font-bold uppercase tracking-wider">
                    Validation Set · Optimal Thresholds
                  </span>
                </div>
                <div className="overflow-x-auto">
                  <table className="w-full border-collapse">
                    <thead>
                      <tr className="bg-slate-50 dark:bg-slate-800/50">
                        {['Model Name','Threshold','Accuracy','Precision','Recall','F1 Score','AUC'].map(h => (
                          <th key={h} className={`px-4 py-3 text-[11px] font-bold uppercase tracking-wider text-slate-500 dark:text-slate-400 border-b border-slate-200 dark:border-slate-700 ${h === 'Model Name' ? 'text-left' : 'text-right'}`}>{h}</th>
                        ))}
                      </tr>
                    </thead>
                    <tbody>
                      {benchmarks.map(m => {
                        const isHighlighted = m.name === selectedModelName
                        return (
                          <tr key={m.name} className={`border-b border-slate-100 dark:border-slate-800 hover:bg-slate-50 dark:hover:bg-slate-800/40 transition-colors ${isHighlighted ? 'bg-blue-50/40 dark:bg-blue-500/5' : ''}`}>
                            <td className="px-4 py-3.5 font-semibold text-primary dark:text-blue-400 flex items-center gap-2">
                              {m.name}
                              {m.selected && <span className="text-[10px] bg-green-100 dark:bg-green-500/20 text-green-700 dark:text-green-400 px-1.5 py-0.5 rounded-lg font-bold">Deployed</span>}
                              {isHighlighted && !m.selected && <span className="text-[10px] bg-blue-100 dark:bg-blue-500/20 text-blue-700 dark:text-blue-400 px-1.5 py-0.5 rounded-lg font-bold">Viewing</span>}
                            </td>
                            <td className="px-4 py-3.5 text-right tabular-nums text-slate-700 dark:text-slate-300">{m.threshold}</td>
                            <td className="px-4 py-3.5 text-right tabular-nums text-slate-700 dark:text-slate-300">{Number(m.acc).toFixed(3)}</td>
                            <td className="px-4 py-3.5 text-right tabular-nums text-slate-700 dark:text-slate-300">{Number(m.prec).toFixed(3)}</td>
                            <td className="px-4 py-3.5 text-right tabular-nums text-slate-700 dark:text-slate-300">{Number(m.recall).toFixed(3)}</td>
                            <td className="px-4 py-3.5 text-right"><MetricBadge value={Number(m.f1).toFixed(3)} isBest={isHighlighted} /></td>
                            <td className="px-4 py-3.5 text-right text-blue-700 dark:text-blue-400 font-bold tabular-nums">{Number(m.auc).toFixed(3)}</td>
                          </tr>
                        )
                      })}
                    </tbody>
                  </table>
                </div>
              </div>

              {isLRSelected && cm ? (
                <div className="col-span-12 lg:col-span-4 bg-white dark:bg-slate-900 border border-slate-200 dark:border-slate-800 rounded-2xl flex flex-col shadow-sm">
                  <div className="px-6 py-4 border-b border-slate-100 dark:border-slate-800">
                    <h3 className="text-base font-bold text-primary dark:text-blue-400">Confusion Matrix</h3>
                    <p className="text-xs text-slate-500 dark:text-slate-400">Logistic Regression — Test Set (n=220)</p>
                  </div>
                  <div className="p-6 flex-1 flex flex-col justify-center">
                    <div className="text-center text-[11px] font-bold uppercase tracking-wider text-slate-500 dark:text-slate-400 mb-3">Predicted</div>
                    <div className="grid grid-cols-2 gap-2 mb-3">
                      <div className="p-4 bg-green-50 dark:bg-green-500/10 border border-green-200 dark:border-green-500/30 rounded-2xl text-center">
                        <p className="text-[10px] font-bold text-green-700 dark:text-green-400 uppercase mb-1">True Negative</p>
                        <p className="text-[40px] font-black text-green-700 dark:text-green-400 leading-none">{cm.tn}</p>
                        <p className="text-[10px] text-green-600 mt-1">Predicted No · Actual No</p>
                      </div>
                      <div className="p-4 bg-red-50 dark:bg-red-500/10 border border-red-200 dark:border-red-500/30 rounded-2xl text-center">
                        <p className="text-[10px] font-bold text-red-500 dark:text-red-400 uppercase mb-1">False Positive</p>
                        <p className="text-[40px] font-black text-red-400 leading-none">{cm.fp}</p>
                        <p className="text-[10px] text-red-400 mt-1">Predicted Yes · Actual No</p>
                      </div>
                      <div className="p-4 bg-amber-50 dark:bg-amber-500/10 border border-amber-200 dark:border-amber-500/30 rounded-2xl text-center">
                        <p className="text-[10px] font-bold text-amber-600 dark:text-amber-400 uppercase mb-1">False Negative</p>
                        <p className="text-[40px] font-black text-amber-500 leading-none">{cm.fn}</p>
                        <p className="text-[10px] text-amber-500 mt-1">Predicted No · Actual Yes</p>
                      </div>
                      <div className="p-4 bg-blue-50 dark:bg-blue-500/10 border border-blue-200 dark:border-blue-500/30 rounded-2xl text-center">
                        <p className="text-[10px] font-bold text-blue-700 dark:text-blue-400 uppercase mb-1">True Positive</p>
                        <p className="text-[40px] font-black text-blue-700 dark:text-blue-400 leading-none">{cm.tp}</p>
                        <p className="text-[10px] text-blue-600 dark:text-blue-500 mt-1">Predicted Yes · Actual Yes</p>
                      </div>
                    </div>
                    <p className="text-[11px] text-slate-500 dark:text-slate-400 text-center">
                      The model correctly identifies <strong>{Math.round(cm.tp / (cm.tp + cm.fn) * 100)}%</strong> of actual leavers (recall).
                    </p>
                  </div>
                </div>
              ) : !isLRSelected ? (
                <div className="col-span-12 lg:col-span-4 bg-white dark:bg-slate-900 border border-slate-200 dark:border-slate-800 rounded-2xl shadow-sm flex flex-col items-center justify-center p-8 text-center">
                  <span className="material-symbols-outlined text-slate-300 dark:text-slate-600 text-[48px] mb-3">grid_off</span>
                  <p className="text-sm font-bold text-slate-500 dark:text-slate-400 mb-1">No Confusion Matrix</p>
                  <p className="text-xs text-slate-400 dark:text-slate-500">Confusion matrix data is only available for the deployed Logistic Regression model.</p>
                </div>
              ) : null}
            </div>

            {/* F1 bar chart */}
            <div className="bg-white dark:bg-slate-900 border border-slate-200 dark:border-slate-800 rounded-2xl shadow-sm p-6">
              <h3 className="text-base font-bold text-primary dark:text-blue-400 mb-6">F1 Score Comparison</h3>
              <div className="space-y-3">
                {[...benchmarks].sort((a, b) => b.f1 - a.f1).map(m => (
                  <div key={m.name} className="flex items-center gap-4">
                    <span className={`w-44 text-sm text-right shrink-0 ${m.name === selectedModelName ? 'font-bold text-primary dark:text-blue-400' : 'text-slate-600 dark:text-slate-400'}`}>{m.name}</span>
                    <div className="flex-1 h-7 bg-slate-100 dark:bg-slate-800 rounded-full overflow-hidden">
                      <motion.div
                        className={`h-full rounded-full flex items-center px-3 ${m.name === selectedModelName ? 'bg-primary' : 'bg-slate-400 dark:bg-slate-600'}`}
                        initial={{ width: 0 }}
                        animate={{ width: `${(m.f1 / 0.7) * 100}%` }}
                        transition={{ duration: 0.7, ease: 'easeOut' }}
                      >
                        <span className="text-[11px] font-bold text-white">{Number(m.f1).toFixed(3)}</span>
                      </motion.div>
                    </div>
                  </div>
                ))}
              </div>
            </div>
          </motion.div>
        )}

        {/* ── PREDICTORS TAB ── */}
        {activeTab === 'predictors' && (
          <motion.div key="predictors" initial={{ opacity: 0, y: 8 }} animate={{ opacity: 1, y: 0 }} exit={{ opacity: 0 }} transition={{ duration: 0.2 }}>
            <div className="bg-white dark:bg-slate-900 border border-slate-200 dark:border-slate-800 rounded-2xl shadow-sm p-6">
              <div className="flex items-center gap-2 mb-2">
                <span className="material-symbols-outlined text-primary dark:text-blue-400 text-[22px]">bar_chart</span>
                <h3 className="text-base font-bold text-primary dark:text-blue-400">Top Predictors — Logistic Regression Coefficients</h3>
              </div>
              <p className="text-sm text-slate-500 dark:text-slate-400 mb-6">
                Positive coefficients increase attrition probability; negative coefficients are protective. Magnitude reflects relative importance.
              </p>
              {predictors.length === 0 && (
                <p className="text-slate-400 dark:text-slate-500 text-sm italic">Start the R API to load live coefficients.</p>
              )}
              <div className="space-y-3">
                {predictors.map((f, i) => {
                  const maxAbs = Math.max(...predictors.map(x => Math.abs(x.coefficient)))
                  const barWidth = (Math.abs(f.coefficient) / maxAbs) * 100
                  const isRisk   = f.direction === 'risk'
                  return (
                    <motion.div
                      key={f.feature}
                      initial={{ opacity: 0, x: -12 }}
                      animate={{ opacity: 1, x: 0 }}
                      transition={{ duration: 0.3, delay: i * 0.05, ease: 'easeOut' }}
                      className="flex items-center gap-4"
                    >
                      <div className="w-52 text-right shrink-0">
                        <p className="text-sm font-semibold text-slate-800 dark:text-slate-200 leading-tight">{f.label}</p>
                        <p className={`text-xs font-bold ${isRisk ? 'text-red-600 dark:text-red-400' : 'text-green-700 dark:text-green-400'}`}>{f.coeff_fmt}</p>
                      </div>
                      <div className="flex-1 h-8 bg-slate-100 dark:bg-slate-800 rounded-xl overflow-hidden">
                        <motion.div
                          className={`h-full rounded-xl flex items-center px-3 ${isRisk ? 'bg-red-400' : 'bg-green-500'}`}
                          initial={{ width: 0 }}
                          animate={{ width: `${barWidth}%` }}
                          transition={{ duration: 0.6, delay: i * 0.05, ease: 'easeOut' }}
                        >
                          <span className="text-[11px] font-bold text-white truncate">{f.label}</span>
                        </motion.div>
                      </div>
                      <div className={`text-xs px-2 py-1 rounded-lg font-bold w-20 text-center ${isRisk ? 'bg-red-100 dark:bg-red-500/20 text-red-700 dark:text-red-400' : 'bg-green-100 dark:bg-green-500/20 text-green-700 dark:text-green-400'}`}>
                        {isRisk ? '↑ Risk' : '↓ Protect'}
                      </div>
                    </motion.div>
                  )
                })}
              </div>
            </div>
          </motion.div>
        )}

        {/* ── RECOMMENDATIONS TAB ── */}
        {activeTab === 'recommendations' && (
          <motion.div key="recommendations" initial={{ opacity: 0, y: 8 }} animate={{ opacity: 1, y: 0 }} exit={{ opacity: 0 }} transition={{ duration: 0.2 }}>
            <div className="mb-4">
              <h3 className="text-base font-bold text-primary dark:text-blue-400">HR Recommendations</h3>
              <p className="text-sm text-slate-500 dark:text-slate-400 mt-1">
                Actionable strategies derived from the model's top attrition drivers. Prioritised by predicted impact.
              </p>
            </div>
            {recommendations.length === 0 && (
              <p className="text-slate-400 dark:text-slate-500 text-sm italic">Start the R API to load recommendations.</p>
            )}
            <div className="space-y-4">
              {recommendations.map((r, i) => {
                const style = PRIORITY_STYLES[r.priority] ?? PRIORITY_STYLES.medium
                return (
                  <motion.div
                    key={r.driver}
                    initial={{ opacity: 0, y: 12 }}
                    animate={{ opacity: 1, y: 0 }}
                    transition={{ duration: 0.3, delay: i * 0.07 }}
                    className={`border rounded-2xl p-5 ${style.bg}`}
                  >
                    <div className="flex items-start justify-between gap-4 mb-3">
                      <div className="flex items-center gap-2">
                        <span className="text-lg">{style.icon}</span>
                        <h4 className="font-bold text-slate-900 dark:text-slate-100 text-base">{r.driver}</h4>
                      </div>
                      <div className="flex items-center gap-2 shrink-0">
                        <span className={`text-[11px] font-bold px-2.5 py-1 rounded-full uppercase ${style.badge}`}>{r.priority}</span>
                        <span className="text-xs text-slate-500 dark:text-slate-400 font-mono bg-white/70 dark:bg-slate-900/70 px-2 py-0.5 rounded-lg border border-slate-200 dark:border-slate-700">{r.coefficient}</span>
                      </div>
                    </div>
                    <p className="text-sm text-slate-800 dark:text-slate-200 leading-relaxed mb-2">{r.recommendation}</p>
                    <p className="text-xs text-slate-500 dark:text-slate-400 italic border-t border-current/10 pt-2">{r.impact}</p>
                  </motion.div>
                )
              })}
            </div>
          </motion.div>
        )}

        {/* ── ETHICS TAB ── */}
        {activeTab === 'ethics' && (
          <motion.div key="ethics" initial={{ opacity: 0, y: 8 }} animate={{ opacity: 1, y: 0 }} exit={{ opacity: 0 }} transition={{ duration: 0.2 }}>
            <div className="mb-4">
              <h3 className="text-base font-bold text-primary dark:text-blue-400">Ethical Implications</h3>
              <p className="text-sm text-slate-500 dark:text-slate-400 mt-1">
                Responsible AI considerations for deploying this attrition prediction system in an HR context.
              </p>
            </div>
            {ethicalItems.length === 0 && (
              <p className="text-slate-400 dark:text-slate-500 text-sm italic">Start the R API to load ethical review.</p>
            )}
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
              {ethicalItems.map((e, i) => (
                <motion.div
                  key={e.aspect}
                  initial={{ opacity: 0, y: 12 }}
                  animate={{ opacity: 1, y: 0 }}
                  transition={{ duration: 0.3, delay: i * 0.06 }}
                  className="bg-white dark:bg-slate-900 border border-slate-200 dark:border-slate-800 rounded-2xl shadow-sm p-5"
                >
                  <div className="flex items-center gap-2 mb-3">
                    <div className="w-9 h-9 rounded-xl bg-blue-50 dark:bg-blue-500/10 flex items-center justify-center">
                      <span className="material-symbols-outlined text-blue-700 dark:text-blue-400 text-[18px]">policy</span>
                    </div>
                    <h4 className="font-bold text-slate-900 dark:text-slate-100 text-sm">{e.aspect}</h4>
                  </div>
                  <p className="text-sm text-slate-500 dark:text-slate-400 leading-relaxed">{e.detail}</p>
                </motion.div>
              ))}
            </div>
          </motion.div>
        )}
      </AnimatePresence>
    </div>
  )
}

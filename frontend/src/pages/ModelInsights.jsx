import { useState, useEffect } from 'react'
import { motion, AnimatePresence } from 'framer-motion'
import { getInsights } from '../api'

// â”€â”€ Fallback static data (shown while API is loading / unavailable) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
const STATIC_METRICS = [
  { label: 'Test AUC',   value: '0.800', icon: 'show_chart',              color: 'text-blue-700' },
  { label: 'Test F1',    value: '0.595', icon: 'target',                  color: 'text-purple-600' },
  { label: 'Precision',  value: '0.579', icon: 'precision_manufacturing', color: 'text-amber-600' },
  { label: 'Recall',     value: '0.611', icon: 'radar',                   color: 'text-green-600' },
  { label: 'Accuracy',   value: '86.4%', icon: 'check_circle',            color: 'text-teal-600' },
  { label: 'Threshold',  value: '0.25',  icon: 'tune',                    color: 'text-slate-600' },
]

const STATIC_BENCHMARKS = [
  { name: 'Logistic Regression', threshold: 0.25, acc: 0.851, prec: 0.608, recall: 0.705, f1: 0.653, auc: 0.859, selected: true },
  { name: 'SVM',                 threshold: 0.31, acc: 0.864, prec: 0.675, recall: 0.614, f1: 0.643, auc: 0.850, selected: false },
  { name: 'glmnet Ridge',        threshold: 0.50, acc: 0.887, prec: 0.880, recall: 0.500, f1: 0.638, auc: 0.855, selected: false },
  { name: 'XGBoost',             threshold: 0.39, acc: 0.810, prec: 0.516, recall: 0.727, f1: 0.604, auc: 0.829, selected: false },
  { name: 'Random Forest',       threshold: 0.24, acc: 0.837, prec: 0.618, recall: 0.477, f1: 0.538, auc: 0.757, selected: false },
  { name: 'Decision Tree',       threshold: 0.11, acc: 0.796, prec: 0.489, recall: 0.523, f1: 0.505, auc: 0.705, selected: false },
]

const PRIORITY_STYLES = {
  critical: { bg: 'bg-red-50 border-red-200',   badge: 'bg-red-100 text-red-700',   icon: 'âš¡' },
  high:     { bg: 'bg-amber-50 border-amber-200', badge: 'bg-amber-100 text-amber-700', icon: 'âš ï¸' },
  medium:   { bg: 'bg-blue-50 border-blue-200',  badge: 'bg-blue-100 text-blue-700', icon: 'ðŸ’¡' },
}

// â”€â”€ Sub-components â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

function MetricCard({ label, value, icon, color, index = 0 }) {
  return (
    <motion.div
      className="bg-white border border-slate-200 rounded-xl p-6 flex items-center gap-4"
      initial={{ opacity: 0, y: 16 }}
      animate={{ opacity: 1, y: 0 }}
      transition={{ duration: 0.35, delay: index * 0.07, ease: 'easeOut' }}
    >
      <div className={`w-12 h-12 rounded-xl bg-slate-50 flex items-center justify-center ${color}`}>
        <span className="material-symbols-outlined text-[24px]">{icon}</span>
      </div>
      <div>
        <p className="text-label-md text-secondary uppercase">{label}</p>
        <p className={`text-headline-lg font-bold ${color}`}>{value}</p>
      </div>
    </motion.div>
  )
}

function MetricBadge({ value, isBest }) {
  const bg = isBest ? 'bg-blue-700 text-white' : 'text-slate-700'
  return (
    <span className={`font-data-tabular text-right tabular-nums ${bg} ${isBest ? 'px-2 py-0.5 rounded' : ''}`}>
      {value}
    </span>
  )
}

// â”€â”€ Page â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

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

  // Build display values — use live data when available, fall back to statics
  const benchmarks = insights?.model_comparison ?? STATIC_BENCHMARKS
  const selectedBenchmark = benchmarks.find(m => m.name === selectedModelName) ?? benchmarks[0]
  const isLRSelected = selectedModelName === 'Logistic Regression'
  const cm         = isLRSelected ? (insights?.confusion_matrix ?? { tn: 168, fp: 14, fn: 16, tp: 22 }) : null
  const predictors = insights?.top_predictors    ?? []
  const recommendations   = insights?.hr_recommendations   ?? []
  const ethicalItems      = insights?.ethical_implications ?? []

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
    { id: 'overview',         label: 'Overview' },
    { id: 'predictors',       label: 'Top Predictors' },
    { id: 'recommendations',  label: 'HR Recommendations' },
    { id: 'ethics',           label: 'Ethics' },
  ]

  return (
    <div>
      {/* Header */}
      <div className="mb-6 flex items-end justify-between">
        <div>
          <nav className="flex items-center gap-2 text-label-md text-slate-400 mb-2 uppercase">
            <span>Analytics</span>
            <span className="material-symbols-outlined text-[14px]">chevron_right</span>
            <span className="text-blue-700">Model Insights</span>
          </nav>
          <h2 className="text-headline-xl font-bold text-primary">Model Evaluation Dashboard</h2>
          <p className="text-body-lg text-secondary mt-1">
            Comparative analysis, HR recommendations and ethical review for the attrition model.
          </p>
        </div>
        {apiError && (
          <div className="text-xs text-amber-700 bg-amber-50 border border-amber-200 rounded-lg px-3 py-2 flex items-center gap-2">
            <span className="material-symbols-outlined text-[16px]">wifi_off</span>
            API offline â€” showing cached data
          </div>
        )}
        {loading && (
          <div className="text-xs text-blue-700 bg-blue-50 border border-blue-200 rounded-lg px-3 py-2 flex items-center gap-2">
            <motion.span
              className="material-symbols-outlined text-[16px]"
              animate={{ rotate: 360 }}
              transition={{ repeat: Infinity, duration: 1, ease: 'linear' }}
            >refresh</motion.span>
            Loading live insightsâ€¦
          </div>
        )}
      </div>

      {/* Model selector + KPI strip */}
      <section className="mb-6">
        {/* Model selector pills */}
        <div className="flex flex-wrap gap-2 mb-4">
          {(insights?.model_comparison ?? STATIC_BENCHMARKS).map(m => (
            <button
              key={m.name}
              onClick={() => setSelectedModelName(m.name)}
              className={`px-3 py-1.5 rounded-lg text-xs font-semibold border transition-colors ${
                selectedModelName === m.name
                  ? 'bg-primary text-white border-primary shadow-sm'
                  : 'bg-white text-slate-600 border-slate-200 hover:bg-slate-50 dark:bg-slate-800 dark:text-slate-300 dark:border-slate-700'
              }`}
            >
              {m.name}
              {m.selected && <span className="ml-1.5 text-[9px] opacity-75 uppercase">★ deployed</span>}
            </button>
          ))}
        </div>
        <h3 className="text-label-md uppercase text-secondary mb-4">
          {selectedModelName} — {isLRSelected ? 'Test Set Performance' : 'Validation Set Performance'}
        </h3>
        <div className="grid grid-cols-2 md:grid-cols-3 xl:grid-cols-6 gap-4">
          {metrics.map((m, i) => <MetricCard key={m.label} {...m} index={i} />)}
        </div>
      </section>

      {/* Tabs */}
      <div className="flex gap-1 mb-6 bg-slate-100 p-1 rounded-xl w-fit">
        {tabs.map(tab => (
          <button
            key={tab.id}
            onClick={() => setActiveTab(tab.id)}
            className={`px-4 py-2 rounded-lg text-sm font-medium transition-all ${
              activeTab === tab.id
                ? 'bg-white text-primary shadow-sm'
                : 'text-slate-500 hover:text-slate-700'
            }`}
          >
            {tab.label}
          </button>
        ))}
      </div>

      <AnimatePresence mode="wait">
        {/* â”€â”€ OVERVIEW TAB â”€â”€ */}
        {activeTab === 'overview' && (
          <motion.div key="overview" initial={{ opacity: 0, y: 8 }} animate={{ opacity: 1, y: 0 }} exit={{ opacity: 0 }} transition={{ duration: 0.2 }}>
            <div className="grid grid-cols-12 gap-6 mb-8">
              {/* Benchmarks table */}
              <div className={`col-span-12 ${isLRSelected ? "lg:col-span-8" : "lg:col-span-12"} bg-white border border-slate-200 rounded-xl overflow-hidden`}>
                <div className="p-4 border-b border-slate-100 flex justify-between items-center bg-slate-50/50">
                  <h3 className="text-headline-md font-semibold text-primary">Model Benchmarks</h3>
                  <span className="bg-blue-900 text-white text-[10px] px-2 py-1 rounded font-bold uppercase tracking-wider">
                    Validation Set Â· Optimal Thresholds
                  </span>
                </div>
                <div className="overflow-x-auto">
                  <table className="w-full border-collapse">
                    <thead>
                      <tr className="bg-slate-50">
                        {['Model Name','Threshold','Accuracy','Precision','Recall','F1 Score','AUC'].map(h => (
                          <th key={h} className={`p-4 font-label-md text-label-md text-secondary border-b border-slate-200 ${h === 'Model Name' ? 'text-left' : 'text-right'}`}>{h}</th>
                        ))}
                      </tr>
                    </thead>
                    <tbody>
                      {benchmarks.map(m => {
                        const isHighlighted = m.name === selectedModelName
                        return (
                        <tr key={m.name} className={`border-b border-slate-100 hover:bg-slate-50 transition-colors ${isHighlighted ? 'bg-blue-50/40' : ''}`}>
                          <td className="p-4 font-semibold text-primary flex items-center gap-2">
                            {m.name}
                            {m.selected && <span className="text-[10px] bg-green-100 text-green-700 px-1.5 py-0.5 rounded font-bold">Deployed</span>}
                            {isHighlighted && !m.selected && <span className="text-[10px] bg-blue-100 text-blue-700 px-1.5 py-0.5 rounded font-bold">Viewing</span>}
                          </td>
                          <td className="p-4 text-right tabular-nums">{m.threshold}</td>
                          <td className="p-4 text-right tabular-nums">{Number(m.acc).toFixed(3)}</td>
                          <td className="p-4 text-right tabular-nums">{Number(m.prec).toFixed(3)}</td>
                          <td className="p-4 text-right tabular-nums">{Number(m.recall).toFixed(3)}</td>
                          <td className="p-4 text-right"><MetricBadge value={Number(m.f1).toFixed(3)} isBest={isHighlighted} /></td>
                          <td className="p-4 text-right text-blue-700 font-bold tabular-nums">{Number(m.auc).toFixed(3)}</td>
                        </tr>
                      )})}
                    </tbody>
                  </table>
                </div>
              </div>

              {isLRSelected && cm ? (
              <div className="col-span-12 lg:col-span-4 bg-white border border-slate-200 rounded-xl flex flex-col">
                <div className="p-4 border-b border-slate-100">
                  <h3 className="text-headline-md font-semibold text-primary">Confusion Matrix</h3>
                  <p className="text-xs text-secondary">Logistic Regression — Test Set (n=220)</p>
                </div>
                <div className="p-6 flex-1 flex flex-col justify-center">
                  <div className="text-center text-label-md text-secondary uppercase mb-3">Predicted</div>
                  <div className="grid grid-cols-2 gap-2 mb-3">
                    <div className="p-4 bg-green-50 border border-green-200 rounded-xl text-center">
                      <p className="text-[10px] font-bold text-green-700 uppercase mb-1">True Negative</p>
                      <p className="text-[40px] font-black text-green-700 leading-none">{cm.tn}</p>
                      <p className="text-[10px] text-green-600 mt-1">Predicted No · Actual No</p>
                    </div>
                    <div className="p-4 bg-red-50 border border-red-200 rounded-xl text-center">
                      <p className="text-[10px] font-bold text-red-500 uppercase mb-1">False Positive</p>
                      <p className="text-[40px] font-black text-red-400 leading-none">{cm.fp}</p>
                      <p className="text-[10px] text-red-400 mt-1">Predicted Yes · Actual No</p>
                    </div>
                    <div className="p-4 bg-amber-50 border border-amber-200 rounded-xl text-center">
                      <p className="text-[10px] font-bold text-amber-600 uppercase mb-1">False Negative</p>
                      <p className="text-[40px] font-black text-amber-500 leading-none">{cm.fn}</p>
                      <p className="text-[10px] text-amber-500 mt-1">Predicted No · Actual Yes</p>
                    </div>
                    <div className="p-4 bg-blue-50 border border-blue-200 rounded-xl text-center">
                      <p className="text-[10px] font-bold text-blue-700 uppercase mb-1">True Positive</p>
                      <p className="text-[40px] font-black text-blue-700 leading-none">{cm.tp}</p>
                      <p className="text-[10px] text-blue-600 mt-1">Predicted Yes · Actual Yes</p>
                    </div>
                  </div>
                  <p className="text-[11px] text-secondary text-center">
                    The model correctly identifies <strong>{Math.round(cm.tp / (cm.tp + cm.fn) * 100)}%</strong> of actual leavers (recall).
                  </p>
                </div>
              </div>
              ) : !isLRSelected ? (
              <div className="col-span-12 lg:col-span-4 bg-white border border-slate-200 rounded-xl flex flex-col items-center justify-center p-8 text-center">
                <span className="material-symbols-outlined text-slate-300 text-[48px] mb-3">grid_off</span>
                <p className="text-sm font-semibold text-slate-500 mb-1">No Confusion Matrix</p>
                <p className="text-xs text-secondary">Confusion matrix data is only available for the deployed Logistic Regression model.</p>
              </div>
              ) : null}
            </div>

            {/* F1 bar chart */}
            <div className="bg-white border border-slate-200 rounded-xl p-6">
              <h3 className="text-headline-md font-semibold text-primary mb-6">F1 Score Comparison</h3>
              <div className="space-y-3">
                {[...benchmarks].sort((a, b) => b.f1 - a.f1).map(m => (
                  <div key={m.name} className="flex items-center gap-4">
                    <span className={`w-44 text-body-md text-right shrink-0 ${m.name === selectedModelName ? 'font-bold text-primary' : 'text-slate-600'}`}>{m.name}</span>
                    <div className="flex-1 h-7 bg-slate-100 rounded-full overflow-hidden">
                      <motion.div
                        className={`h-full rounded-full flex items-center px-3 ${m.name === selectedModelName ? 'bg-primary' : 'bg-slate-400'}`}
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

        {/* â”€â”€ PREDICTORS TAB â”€â”€ */}
        {activeTab === 'predictors' && (
          <motion.div key="predictors" initial={{ opacity: 0, y: 8 }} animate={{ opacity: 1, y: 0 }} exit={{ opacity: 0 }} transition={{ duration: 0.2 }}>
            <div className="bg-white border border-slate-200 rounded-xl p-6">
              <div className="flex items-center gap-2 mb-2">
                <span className="material-symbols-outlined text-primary text-[22px]">bar_chart</span>
                <h3 className="text-headline-md font-semibold text-primary">Top Predictors â€” Logistic Regression Coefficients</h3>
              </div>
              <p className="text-sm text-secondary mb-6">
                Positive coefficients increase attrition probability; negative coefficients are protective. Magnitude reflects relative importance.
              </p>
              {predictors.length === 0 && (
                <p className="text-secondary text-sm italic">Start the R API to load live coefficients.</p>
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
                        <p className="text-sm font-medium text-on-surface leading-tight">{f.label}</p>
                        <p className={`text-xs font-bold ${isRisk ? 'text-red-600' : 'text-green-700'}`}>{f.coeff_fmt}</p>
                      </div>
                      <div className="flex-1 h-8 bg-slate-100 rounded-lg overflow-hidden">
                        <motion.div
                          className={`h-full rounded-lg flex items-center px-3 ${isRisk ? 'bg-red-400' : 'bg-green-500'}`}
                          initial={{ width: 0 }}
                          animate={{ width: `${barWidth}%` }}
                          transition={{ duration: 0.6, delay: i * 0.05, ease: 'easeOut' }}
                        >
                          <span className="text-[11px] font-bold text-white truncate">{f.label}</span>
                        </motion.div>
                      </div>
                      <div className={`text-xs px-2 py-0.5 rounded font-bold w-16 text-center ${isRisk ? 'bg-red-100 text-red-700' : 'bg-green-100 text-green-700'}`}>
                        {isRisk ? 'â†‘ Risk' : 'â†“ Protect'}
                      </div>
                    </motion.div>
                  )
                })}
              </div>
            </div>
          </motion.div>
        )}

        {/* â”€â”€ RECOMMENDATIONS TAB â”€â”€ */}
        {activeTab === 'recommendations' && (
          <motion.div key="recommendations" initial={{ opacity: 0, y: 8 }} animate={{ opacity: 1, y: 0 }} exit={{ opacity: 0 }} transition={{ duration: 0.2 }}>
            <div className="mb-4">
              <h3 className="text-headline-md font-semibold text-primary">HR Recommendations</h3>
              <p className="text-sm text-secondary mt-1">
                Actionable strategies derived from the model's top attrition drivers. Prioritised by predicted impact.
              </p>
            </div>
            {recommendations.length === 0 && (
              <p className="text-secondary text-sm italic">Start the R API to load recommendations.</p>
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
                    className={`border rounded-xl p-5 ${style.bg}`}
                  >
                    <div className="flex items-start justify-between gap-4 mb-3">
                      <div className="flex items-center gap-2">
                        <span className="text-lg">{style.icon}</span>
                        <h4 className="font-bold text-on-surface text-base">{r.driver}</h4>
                      </div>
                      <div className="flex items-center gap-2 shrink-0">
                        <span className={`text-[11px] font-bold px-2 py-0.5 rounded uppercase ${style.badge}`}>{r.priority}</span>
                        <span className="text-xs text-secondary font-mono bg-white/70 px-2 py-0.5 rounded border">{r.coefficient}</span>
                      </div>
                    </div>
                    <p className="text-sm text-on-surface leading-relaxed mb-2">{r.recommendation}</p>
                    <p className="text-xs text-secondary italic border-t border-current/10 pt-2">{r.impact}</p>
                  </motion.div>
                )
              })}
            </div>
          </motion.div>
        )}

        {/* â”€â”€ ETHICS TAB â”€â”€ */}
        {activeTab === 'ethics' && (
          <motion.div key="ethics" initial={{ opacity: 0, y: 8 }} animate={{ opacity: 1, y: 0 }} exit={{ opacity: 0 }} transition={{ duration: 0.2 }}>
            <div className="mb-4">
              <h3 className="text-headline-md font-semibold text-primary">Ethical Implications</h3>
              <p className="text-sm text-secondary mt-1">
                Responsible AI considerations for deploying this attrition prediction system in an HR context.
              </p>
            </div>
            {ethicalItems.length === 0 && (
              <p className="text-secondary text-sm italic">Start the R API to load ethical review.</p>
            )}
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
              {ethicalItems.map((e, i) => (
                <motion.div
                  key={e.aspect}
                  initial={{ opacity: 0, y: 12 }}
                  animate={{ opacity: 1, y: 0 }}
                  transition={{ duration: 0.3, delay: i * 0.06 }}
                  className="bg-white border border-slate-200 rounded-xl p-5"
                >
                  <div className="flex items-center gap-2 mb-3">
                    <div className="w-8 h-8 rounded-lg bg-blue-50 flex items-center justify-center">
                      <span className="material-symbols-outlined text-blue-700 text-[18px]">policy</span>
                    </div>
                    <h4 className="font-bold text-on-surface text-sm">{e.aspect}</h4>
                  </div>
                  <p className="text-sm text-secondary leading-relaxed">{e.detail}</p>
                </motion.div>
              ))}
            </div>
          </motion.div>
        )}
      </AnimatePresence>
    </div>
  )
}

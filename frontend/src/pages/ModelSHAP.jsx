import { useState, useEffect, useMemo, useRef } from 'react'
import { motion, AnimatePresence } from 'framer-motion'
import * as Tabs from '@radix-ui/react-tabs'
import { db } from '../firebase'
import { collection, query, orderBy, limit, onSnapshot } from 'firebase/firestore'
import featureData from '../data/feature_importance.json'
import shapMeta from '../data/shap_metadata.json'
import { getSHAPExplanation } from '../api'

// ─────────────────────────────────────────────────────────────────────────────
// Constants
// ─────────────────────────────────────────────────────────────────────────────
const FEATURE_LABELS = {
  OverTime: 'OverTime', MonthlyIncome: 'Monthly Income',
  NumCompaniesWorked: 'No. Companies Worked', BusinessTravel: 'Business Travel',
  'MaritalStatus.Single': 'Marital: Single', DistanceFromHome: 'Distance from Home',
  JobInvolvement: 'Job Involvement', TenureGroup: 'Tenure Group',
  YearsSinceLastPromotion: 'Yrs Since Promotion', YearsAtCompany: 'Years at Company',
  'EducationField.Life Sciences': 'EduField: Life Sciences',
  'JobRole.Research Director': 'Role: Research Director',
  IncomePerTenureYear: 'Income / Tenure Yr',
  'JobRole.Manufacturing Director': 'Role: Mfg Director',
  'EducationField.Other': 'EduField: Other', YearsInCurrentRole: 'Years in Role',
  WorkLifeBalance: 'Work-Life Balance', 'EducationField.Medical': 'EduField: Medical',
  Age: 'Age', Gender: 'Gender', YearsWithCurrManager: 'Yrs with Manager',
  StockOptionLevel: 'Stock Option Level', TrainingTimesLastYear: 'Training Times/Yr',
  'MaritalStatus.Married': 'Marital: Married', DailyRate: 'Daily Rate',
  'JobRole.Manager': 'Role: Manager', PerformanceRating: 'Performance Rating',
  'EducationField.Marketing': 'EduField: Marketing', PercentSalaryHike: 'Salary Hike %',
  MonthlyRate: 'Monthly Rate', 'EducationField.Technical Degree': 'EduField: Tech Degree',
  Education: 'Education Level', HourlyRate: 'Hourly Rate',
}
const label = (f) => FEATURE_LABELS[f] ?? f

const RISK_COLORS = {
  High:   { badge: 'bg-red-100 dark:bg-red-500/20 text-red-700 dark:text-red-400 border-red-200 dark:border-red-500/30',    dot: 'bg-red-500',   text: 'text-red-600 dark:text-red-400' },
  Medium: { badge: 'bg-amber-100 dark:bg-amber-500/20 text-amber-700 dark:text-amber-400 border-amber-200 dark:border-amber-500/30', dot: 'bg-amber-400', text: 'text-amber-600 dark:text-amber-400' },
  Low:    { badge: 'bg-green-100 dark:bg-green-500/20 text-green-700 dark:text-green-400 border-green-200 dark:border-green-500/30', dot: 'bg-green-500', text: 'text-green-600 dark:text-green-400' },
}

const DEMO_EMPLOYEE = {
  id: 'demo', employeeName: 'Demo Employee', probability: 0.72, risk_level: 'High',
  prediction: 'Yes',
  Age: 29, BusinessTravel: 'Travel_Frequently', DailyRate: 501, Department: 'Sales',
  DistanceFromHome: 26, Education: 3, EducationField: 'Life Sciences',
  EnvironmentSatisfaction: 2, Gender: 'Male', HourlyRate: 54,
  JobInvolvement: 2, JobLevel: 1, JobRole: 'Sales Representative', JobSatisfaction: 2,
  MaritalStatus: 'Single', MonthlyIncome: 2500, MonthlyRate: 9000,
  NumCompaniesWorked: 5, OverTime: 'Yes', PercentSalaryHike: 11,
  PerformanceRating: 3, RelationshipSatisfaction: 2, StockOptionLevel: 0,
  TotalWorkingYears: 6, TrainingTimesLastYear: 2, WorkLifeBalance: 2,
  YearsAtCompany: 1, YearsInCurrentRole: 1, YearsSinceLastPromotion: 0,
  YearsWithCurrManager: 0,
}

// ─────────────────────────────────────────────────────────────────────────────
// Preprocessing: raw form → 33 model features
// ─────────────────────────────────────────────────────────────────────────────
function preprocessForm(form) {
  const OverTime       = form.OverTime === 'Yes' ? 1 : 0
  const Gender         = form.Gender   === 'Male' ? 1 : 0
  const BusinessTravel = form.BusinessTravel === 'Non-Travel' ? 0
    : form.BusinessTravel === 'Travel_Rarely' ? 1 : 2
  const YearsAtCompany = Number(form.YearsAtCompany)
  const TenureGroup    = YearsAtCompany <= 3 ? 0 : YearsAtCompany <= 10 ? 1 : 2
  const MI  = Number(form.MonthlyIncome)
  const TWY = Number(form.TotalWorkingYears || 0)
  const IncomePerTenureYear = MI / (TWY + 1)
  const ef = form.EducationField || ''
  const jr = form.JobRole || ''
  const ms = form.MaritalStatus || ''
  return {
    Age: Number(form.Age), BusinessTravel, DailyRate: Number(form.DailyRate),
    DistanceFromHome: Number(form.DistanceFromHome), Education: Number(form.Education),
    Gender, HourlyRate: Number(form.HourlyRate), JobInvolvement: Number(form.JobInvolvement),
    MonthlyIncome: MI, MonthlyRate: Number(form.MonthlyRate),
    NumCompaniesWorked: Number(form.NumCompaniesWorked), OverTime,
    PercentSalaryHike: Number(form.PercentSalaryHike),
    PerformanceRating: Number(form.PerformanceRating),
    StockOptionLevel: Number(form.StockOptionLevel),
    TrainingTimesLastYear: Number(form.TrainingTimesLastYear),
    WorkLifeBalance: Number(form.WorkLifeBalance), YearsAtCompany,
    YearsInCurrentRole: Number(form.YearsInCurrentRole),
    YearsSinceLastPromotion: Number(form.YearsSinceLastPromotion),
    YearsWithCurrManager: Number(form.YearsWithCurrManager),
    TenureGroup, IncomePerTenureYear,
    'EducationField.Life Sciences':      ef === 'Life Sciences' ? 1 : 0,
    'EducationField.Marketing':          ef === 'Marketing' ? 1 : 0,
    'EducationField.Medical':            ef === 'Medical' ? 1 : 0,
    'EducationField.Other':              ef === 'Other' ? 1 : 0,
    'EducationField.Technical Degree':   ef === 'Technical Degree' ? 1 : 0,
    'JobRole.Manager':                   jr === 'Manager' ? 1 : 0,
    'JobRole.Manufacturing Director':    jr === 'Manufacturing Director' ? 1 : 0,
    'JobRole.Research Director':         jr === 'Research Director' ? 1 : 0,
    'MaritalStatus.Married':             ms === 'Married' ? 1 : 0,
    'MaritalStatus.Single':              ms === 'Single' ? 1 : 0,
  }
}

function sigmoid(x) { return 1 / (1 + Math.exp(-x)) }

// Compute log-odds contributions for each feature; map to probability space proportionally
function computeShap(form) {
  const processed   = preprocessForm(form)
  const intercept   = shapMeta.intercept
  const baseProbability = sigmoid(intercept)

  const contributions = shapMeta.features.map((feat) => {
    const x     = processed[feat.name] ?? 0
    const denom = feat.is_binary ? 2 * feat.sd : feat.sd
    const x_scaled = denom > 0 ? (x - feat.mean) / denom : 0
    return {
      feature:            feat.name,
      direction:          feat.direction,
      contribution_lo:    feat.beta * x_scaled,
      raw_value:          x,
    }
  })

  const sum_lo   = contributions.reduce((s, c) => s + c.contribution_lo, 0)
  const finalProb = sigmoid(intercept + sum_lo)
  const probDiff  = finalProb - baseProbability
  const totalAbsLo = contributions.reduce((s, c) => s + Math.abs(c.contribution_lo), 0)

  const withProb = contributions.map((c) => ({
    ...c,
    contribution_prob: totalAbsLo > 0
      ? (c.contribution_lo / totalAbsLo) * Math.abs(probDiff) * Math.sign(probDiff)
      : 0,
  }))
  withProb.sort((a, b) => Math.abs(b.contribution_prob) - Math.abs(a.contribution_prob))

  return { baseProbability, finalProb, probDiff, contributions: withProb.slice(0, 10), allContributions: withProb }
}

// Interpolate colour for beeswarm dots (blue → white → red)
function valueColor(norm) {
  const t = Math.max(0, Math.min(1, norm))
  if (t < 0.5) {
    const s = t * 2
    return `rgb(${Math.round(59 + (248-59)*s)},${Math.round(130 + (113-130)*s)},${Math.round(246 + (70-246)*s)})`
  }
  const s = (t - 0.5) * 2
  return `rgb(${Math.round(248 + (239-248)*s)},${Math.round(113 + (68-113)*s)},${Math.round(70 + (68-70)*s)})`
}

// ─────────────────────────────────────────────────────────────────────────────
// Chart components
// ─────────────────────────────────────────────────────────────────────────────

function GlobalImportanceChart({ features, n = 10 }) {
  const top = features.slice(0, n)
  const maxVal = Math.max(...top.map((f) => f.perm_auc_drop), 0.001)
  const ROW = 30, PAD = 16, LABEL = 172, BAR = 220, VAL = 48
  const W = LABEL + BAR + VAL + PAD * 2
  const H = top.length * ROW + PAD * 2

  return (
    <svg viewBox={`0 0 ${W} ${H}`} className="w-full" style={{ fontFamily: 'Inter, sans-serif' }}>
      {top.map((f, i) => {
        const y    = PAD + i * ROW
        const bw   = (f.perm_auc_drop / maxVal) * BAR
        const isRisk = f.direction === 'risk'
        const fill = isRisk ? '#ef4444' : '#3b82f6'
        return (
          <g key={f.feature}>
            <text x={LABEL - 8} y={y + ROW * 0.62} textAnchor="end" fontSize="11" fill="#475569">{label(f.feature)}</text>
            <rect x={LABEL} y={y + 4} width={Math.max(bw, 2)} height={ROW - 8} fill={fill} rx="3" opacity="0.85" />
            <text x={LABEL + bw + 6} y={y + ROW * 0.62} fontSize="10" fill="#64748b">{f.perm_auc_drop.toFixed(4)}</text>
          </g>
        )
      })}
      <text x={LABEL + BAR / 2} y={H - 2} textAnchor="middle" fontSize="9" fill="#94a3b8">Mean AUC Drop (permutation importance)</text>
    </svg>
  )
}

function BeeswarmPlot({ beeswarmData, n = 10 }) {
  const features = (beeswarmData || []).slice(0, n)
  if (!features.length) return null

  const allContribs = features.flatMap((f) => f.contributions)
  const xMin = Math.min(...allContribs) * 1.15
  const xMax = Math.max(...allContribs) * 1.15

  const ROW = 28, PAD_L = 160, PAD_R = 16, W = 460, H = features.length * ROW + 40
  const toX = (v) => PAD_L + ((v - xMin) / (xMax - xMin)) * (W - PAD_L - PAD_R)
  const zeroX = toX(0)

  return (
    <svg viewBox={`0 0 ${W} ${H}`} className="w-full" style={{ fontFamily: 'Inter, sans-serif' }}>
      <line x1={zeroX} y1={20} x2={zeroX} y2={H - 20} stroke="#e2e8f0" strokeWidth="1" />
      {features.map((feat, i) => {
        const cy = 20 + i * ROW + ROW / 2
        return (
          <g key={feat.feature}>
            <text x={PAD_L - 8} y={cy + 4} textAnchor="end" fontSize="10" fill="#475569">{label(feat.feature)}</text>
            {feat.contributions.map((c, j) => {
              const cx = toX(c)
              const jitter = Math.sin(j * 2.17 + i) * (ROW * 0.32)
              const col = valueColor(feat.feature_values[j] ?? 0.5)
              return <circle key={j} cx={cx} cy={cy + jitter} r="3" fill={col} opacity="0.75" />
            })}
          </g>
        )
      })}
      <text x={zeroX}           y={H - 4} textAnchor="middle" fontSize="9" fill="#94a3b8">0</text>
      <text x={PAD_L}           y={H - 4} textAnchor="middle" fontSize="9" fill="#3b82f6">Low impact</text>
      <text x={W - PAD_R - 12}  y={H - 4} textAnchor="end"    fontSize="9" fill="#ef4444">High impact</text>
      <defs>
        <linearGradient id="beeGrad" x1="0" y1="0" x2="1" y2="0">
          <stop offset="0%"   stopColor="#3b82f6" />
          <stop offset="50%"  stopColor="#f8716e" />
          <stop offset="100%" stopColor="#ef4444" />
        </linearGradient>
      </defs>
      <rect x={W - 80} y={8} width="64" height="6" rx="3" fill="url(#beeGrad)" />
      <text x={W - 80} y={22} fontSize="8" fill="#94a3b8">Low</text>
      <text x={W - 24} y={22} textAnchor="end" fontSize="8" fill="#94a3b8">High</text>
    </svg>
  )
}

function DirectionTable({ features, n = 12 }) {
  const top = features.slice(0, n)
  return (
    <div className="space-y-1.5">
      {top.map((f) => {
        const isRisk = f.direction === 'risk'
        return (
          <div key={f.feature} className="flex items-center justify-between py-1.5 border-b border-slate-100 last:border-0">
            <span className="text-xs text-slate-600 dark:text-slate-300 font-medium truncate max-w-[160px]">{label(f.feature)}</span>
            {isRisk
              ? <span className="flex items-center gap-1 text-xs font-bold text-red-600 shrink-0">
                  <span className="material-symbols-outlined text-[14px]">trending_up</span>Increases Risk
                </span>
              : <span className="flex items-center gap-1 text-xs font-bold text-green-600 shrink-0">
                  <span className="material-symbols-outlined text-[14px]">trending_down</span>Decreases Risk
                </span>
            }
          </div>
        )
      })}
    </div>
  )
}

// Force / waterfall chart for employee explanation — per-feature horizontal bar chart
function WaterfallChart({ baseProbability, finalProb, contributions }) {
  const items = [...contributions]
    .sort((a, b) => Math.abs(b.contribution_prob) - Math.abs(a.contribution_prob))
    .slice(0, 8)

  if (!items.length) return null

  const maxAbs = Math.max(...items.map((c) => Math.abs(c.contribution_prob)), 0.001)

  const ROW_H  = 34
  const PAD_L  = 168
  const BAR_HALF = 220
  const PAD_R  = 68
  const PAD_T  = 8
  const PAD_B  = 28
  const W = PAD_L + BAR_HALF * 2 + PAD_R
  const H = PAD_T + items.length * ROW_H + PAD_B
  const ZERO_X = PAD_L + BAR_HALF

  const toBarW = (v) => (Math.abs(v) / maxAbs) * (BAR_HALF - 8)

  const fmt = (v) => {
    if (typeof v === 'number') return Number.isInteger(v) ? String(v) : v.toFixed(1)
    return String(v ?? '—')
  }

  return (
    <div className="overflow-x-auto rounded-lg border border-slate-100 bg-slate-50 p-2">
      <svg viewBox={`0 0 ${W} ${H}`} className="w-full" style={{ fontFamily: 'Inter, sans-serif' }}>
        <line x1={ZERO_X} y1={PAD_T} x2={ZERO_X} y2={H - PAD_B + 6} stroke="#cbd5e1" strokeWidth="1.5" strokeDasharray="4 3" />

        {items.map((c, i) => {
          const y      = PAD_T + i * ROW_H
          const isPos  = c.contribution_prob >= 0
          const bw     = toBarW(c.contribution_prob)
          const barX   = isPos ? ZERO_X : ZERO_X - bw
          const pct    = (c.contribution_prob * 100).toFixed(1)
          const sign   = isPos ? '+' : ''
          const fill   = isPos ? '#ef4444' : '#3b82f6'
          const fillBg = isPos ? '#fee2e2' : '#dbeafe'
          const textCol = isPos ? '#dc2626' : '#1d4ed8'
          const lbl    = label(c.feature)
          const shortLbl = lbl.length > 22 ? lbl.slice(0, 21) + '…' : lbl

          return (
            <g key={c.feature}>
              {i % 2 === 0 && <rect x={0} y={y + 1} width={W} height={ROW_H - 2} fill="#f8fafc" rx="2" />}
              <text
                x={PAD_L - 10} y={y + ROW_H * 0.62}
                textAnchor="end" fontSize="10.5" fill="#334155" fontWeight="500"
              >{shortLbl}</text>
              <rect x={ZERO_X - BAR_HALF + 4} y={y + 8} width={BAR_HALF * 2 - 8} height={ROW_H - 16} fill="#f1f5f9" rx="3" />
              <rect x={barX} y={y + 8} width={Math.max(bw, 2)} height={ROW_H - 16} fill={fillBg} rx="3" />
              <rect x={barX} y={y + 10} width={Math.max(bw, 2)} height={ROW_H - 20} fill={fill} rx="2" opacity="0.85" />
              <text
                x={isPos ? barX + bw + 6 : barX - 6}
                y={y + ROW_H * 0.62}
                textAnchor={isPos ? 'start' : 'end'}
                fontSize="10" fill={textCol} fontWeight="700"
              >{sign}{pct}%</text>
              <text x={W - 4} y={y + ROW_H * 0.62} textAnchor="end" fontSize="9.5" fill="#94a3b8">
                {fmt(c.raw_value)}
              </text>
            </g>
          )
        })}

        <text x={ZERO_X}                  y={H - 8} textAnchor="middle" fontSize="8.5" fill="#94a3b8">0%</text>
        <text x={ZERO_X - BAR_HALF + 12}  y={H - 8} textAnchor="start"  fontSize="8.5" fill="#3b82f6">← Lowers risk</text>
        <text x={ZERO_X + BAR_HALF - 12}  y={H - 8} textAnchor="end"    fontSize="8.5" fill="#ef4444">Raises risk →</text>
        <text x={W - 4} y={PAD_T - 2} textAnchor="end" fontSize="8" fill="#cbd5e1" fontWeight="600">VALUE</text>
      </svg>
    </div>
  )
}

function RelationshipsInsights({ features }) {
  const insights = [
    {
      icon: 'schedule', color: 'text-red-600', bg: 'bg-red-50 border-red-200',
      title: 'OverTime — #1 Attrition Driver',
      body: 'OverTime is the single strongest predictor of attrition across all three analysis methods (standardised coefficients, permutation importance, and McFadden R²). Employees working overtime are ~3× more likely to leave.',
      stat: 'AUC drop: 0.047',
    },
    {
      icon: 'payments', color: 'text-blue-600', bg: 'bg-blue-50 border-blue-200',
      title: 'Monthly Income — Protective Factor',
      body: 'Higher monthly income strongly reduces attrition risk. The negative standardised coefficient (−0.73) indicates that employees above the median salary are significantly less likely to leave, independent of other factors.',
      stat: 'Std. coeff: −0.73',
    },
    {
      icon: 'work_history', color: 'text-amber-700', bg: 'bg-amber-50 border-amber-200',
      title: 'Job Mobility — Career Pattern Risk',
      body: 'Employees who have worked at more companies show higher attrition intent. NumCompaniesWorked ranks #3 overall, suggesting that historical job-hopping is a reliable signal of future departure.',
      stat: 'McFadden R²: 0.018',
    },
    {
      icon: 'flight_takeoff', color: 'text-purple-600', bg: 'bg-purple-50 border-purple-200',
      title: 'Business Travel — Burnout Signal',
      body: 'Frequent business travel correlates strongly with attrition. This likely reflects burnout and work-life imbalance. The ordinal encoding (0=none, 1=rare, 2=frequent) gives consistent gradient behaviour.',
      stat: 'McFadden R²: 0.017',
    },
    {
      icon: 'sentiment_dissatisfied', color: 'text-slate-600', bg: 'bg-slate-50 border-slate-200',
      title: 'Job Involvement — Engagement Retains',
      body: 'Job Involvement is the 2nd most important predictor by McFadden R² (0.025), despite ranking lower in permutation importance. Highly engaged employees are significantly less likely to leave.',
      stat: 'McFadden R²: 0.025',
    },
    {
      icon: 'trending_up', color: 'text-green-700', bg: 'bg-green-50 border-green-200',
      title: 'Years Since Promotion — Stagnation Risk',
      body: 'Employees who have not been promoted in over 3 years show elevated attrition. This career stagnation signal ranks consistently across all three importance methods, suggesting it is a genuine predictor.',
      stat: 'Rank: #9 consensus',
    },
  ]
  return (
    <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
      {insights.map((ins, i) => (
        <motion.div
          key={ins.title}
          className={`rounded-2xl border p-5 ${ins.bg}`}
          initial={{ opacity: 0, y: 12 }}
          animate={{ opacity: 1, y: 0 }}
          transition={{ delay: i * 0.06, duration: 0.3 }}
        >
          <div className={`flex items-center gap-2 mb-3 ${ins.color}`}>
            <span className="material-symbols-outlined text-[20px]">{ins.icon}</span>
            <span className="font-bold text-sm">{ins.title}</span>
          </div>
          <p className="text-xs text-slate-600 dark:text-slate-300 leading-relaxed mb-3">{ins.body}</p>
          <span className="text-[10px] font-bold uppercase tracking-wide text-slate-400">{ins.stat}</span>
        </motion.div>
      ))}
    </div>
  )
}

// ─────────────────────────────────────────────────────────────────────────────
// Employee Selector
// ─────────────────────────────────────────────────────────────────────────────
function EmployeeSelector({ employees, selectedId, onSelect }) {
  const [open, setOpen] = useState(false)
  const [search, setSearch] = useState('')
  const searchRef = useRef(null)
  const selected = employees.find((e) => e.id === selectedId)
  const displayName = selectedId === 'demo'
    ? 'Demo Employee'
    : selected
      ? (selected.employeeName || `EMP-${selected.id.slice(-5).toUpperCase()}`)
      : 'Select Employee'

  const filtered = search.trim()
    ? employees.filter((e) => {
        const q = search.toLowerCase()
        return (
          (e.employeeName ?? '').toLowerCase().includes(q) ||
          (e.JobRole ?? '').toLowerCase().includes(q) ||
          (e.Department ?? '').toLowerCase().includes(q)
        )
      })
    : employees

  const handleOpen = () => {
    setOpen((o) => !o)
    if (!open) setTimeout(() => searchRef.current?.focus(), 80)
  }

  return (
    <div className="relative">
      <button
        onClick={handleOpen}
        className="flex items-center gap-2 px-4 py-2.5 bg-white dark:bg-slate-800 border border-slate-200 dark:border-slate-700 rounded-xl text-sm font-medium text-slate-700 dark:text-slate-300 hover:border-blue-400 dark:hover:border-blue-500 transition-colors shadow-sm min-w-[200px] justify-between"
      >
        <div className="flex items-center gap-2">
          <span className="material-symbols-outlined text-[18px] text-slate-400">person</span>
          <span className="truncate max-w-[140px]">{displayName}</span>
        </div>
        <span className="material-symbols-outlined text-[18px] text-slate-400">{open ? 'expand_less' : 'expand_more'}</span>
      </button>

      <AnimatePresence>
        {open && (
          <motion.div
            className="absolute right-0 top-full mt-2 w-72 bg-white dark:bg-slate-900 border border-slate-200 dark:border-slate-700 rounded-xl shadow-xl z-50 overflow-hidden"
            initial={{ opacity: 0, y: -8 }}
            animate={{ opacity: 1, y: 0 }}
            exit={{ opacity: 0, y: -8 }}
            transition={{ duration: 0.15 }}
          >
            <div className="p-2 border-b border-slate-100">
              <div className="relative">
                <span className="material-symbols-outlined absolute left-2.5 top-1/2 -translate-y-1/2 text-[15px] text-slate-400">search</span>
                <input
                  ref={searchRef}
                  type="text"
                  value={search}
                  onChange={(e) => setSearch(e.target.value)}
                  placeholder="Search employees…"
                  className="w-full pl-8 pr-3 py-1.5 text-xs border border-slate-200 dark:border-slate-700 rounded-lg focus:ring-1 focus:ring-blue-400 outline-none bg-white dark:bg-slate-800 text-slate-700 dark:text-slate-300"
                />
              </div>
            </div>

            <div className="max-h-60 overflow-y-auto">
              {!search.trim() && (
                <button
                  onClick={() => { onSelect('demo'); setOpen(false); setSearch('') }}
                  className={`w-full text-left px-4 py-3 text-sm hover:bg-blue-50 transition-colors border-b border-slate-100 ${selectedId === 'demo' ? 'bg-blue-50' : ''}`}
                >
                  <div className="font-medium text-slate-700">Demo Employee</div>
                  <div className="text-xs text-red-600 font-semibold">High Risk · 72%</div>
                </button>
              )}

              {filtered.map((e) => {
                const nm  = e.employeeName || `EMP-${e.id.slice(-5).toUpperCase()}`
                const c   = RISK_COLORS[e.risk_level] ?? RISK_COLORS.Low
                const pct = Math.round((e.probability ?? 0) * 100)
                return (
                  <button
                    key={e.id}
                    onClick={() => { onSelect(e.id); setOpen(false); setSearch('') }}
                    className={`w-full text-left px-4 py-3 text-sm hover:bg-blue-50 transition-colors border-b border-slate-100 last:border-0 ${selectedId === e.id ? 'bg-blue-50' : ''}`}
                  >
                    <div className="font-medium text-slate-700 truncate">{nm}</div>
                    <div className={`text-xs font-semibold ${c.text}`}>{e.risk_level} · {pct}% · {e.JobRole ?? '—'}</div>
                  </button>
                )
              })}

              {filtered.length === 0 && (
                <div className="px-4 py-4 text-xs text-slate-400 text-center">
                  {search.trim() ? `No results for "${search}"` : 'No saved employees — run a prediction first'}
                </div>
              )}
            </div>
          </motion.div>
        )}
      </AnimatePresence>
    </div>
  )
}

// ─────────────────────────────────────────────────────────────────────────────
// Summary Panel
// ─────────────────────────────────────────────────────────────────────────────
function SummaryPanel({ emp }) {
  if (!emp) return null
  const pct    = Math.round((emp.probability ?? 0) * 100)
  const rl     = emp.risk_level ?? 'Low'
  const rc     = RISK_COLORS[rl] ?? RISK_COLORS.Low

  const cards = [
    {
      label: 'Selected Employee',
      value: emp.employeeName || `EMP-${emp.id?.slice(-5).toUpperCase() ?? '?????'}`,
      sub: emp.JobRole ?? '—',
      icon: 'badge', color: 'text-blue-700', bg: 'bg-blue-50',
    },
    {
      label: 'Predicted Attrition Risk',
      value: `${pct}%`,
      sub: rl + ' Risk',
      subColor: rc.text,
      icon: 'crisis_alert', color: pct >= 60 ? 'text-red-600' : pct >= 30 ? 'text-amber-600' : 'text-green-600',
      bg: pct >= 60 ? 'bg-red-50' : pct >= 30 ? 'bg-amber-50' : 'bg-green-50',
    },
    {
      label: 'Actual Attrition',
      value: emp.prediction ?? '—',
      sub: emp.prediction === 'Yes' ? 'Left Company' : emp.prediction === 'No' ? 'Still Active' : 'Unknown',
      icon: 'fact_check', color: emp.prediction === 'Yes' ? 'text-red-600' : 'text-green-600', bg: emp.prediction === 'Yes' ? 'bg-red-50' : 'bg-green-50',
    },
    {
      label: 'Model',
      value: 'Logistic Regression',
      sub: `Threshold = ${shapMeta.threshold}`,
      icon: 'model_training', color: 'text-slate-600', bg: 'bg-slate-50',
    },
  ]

  return (
    <div className="grid grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
      {cards.map((c, i) => (
        <motion.div
          key={c.label}
          className="bg-white dark:bg-slate-900 border border-slate-200 dark:border-slate-800 rounded-2xl p-4 flex items-start gap-3 shadow-sm"
          initial={{ opacity: 0, y: 12 }} animate={{ opacity: 1, y: 0 }}
          transition={{ duration: 0.3, delay: i * 0.06 }}
        >
          <div className={`w-10 h-10 rounded-xl ${c.bg} flex items-center justify-center shrink-0`}>
            <span className={`material-symbols-outlined text-[20px] ${c.color}`}>{c.icon}</span>
          </div>
          <div className="min-w-0">
            <p className="text-[10px] font-bold uppercase text-slate-400 tracking-wide">{c.label}</p>
            <p className={`text-base font-extrabold truncate ${c.color}`}>{c.value}</p>
            <p className={`text-[11px] ${c.subColor ?? 'text-slate-400'} font-medium`}>{c.sub}</p>
          </div>
        </motion.div>
      ))}
    </div>
  )
}

// ─────────────────────────────────────────────────────────────────────────────
// Empty / loading states
// ─────────────────────────────────────────────────────────────────────────────
function EmptyExplanation() {
  return (
    <div className="flex flex-col items-center justify-center py-20 text-slate-400">
      <span className="material-symbols-outlined text-[48px] mb-4">person_search</span>
      <p className="text-sm font-medium">Select an employee to see their attrition explanation</p>
      <p className="text-xs mt-1">Use the dropdown above or run a prediction in the Attrition Risk tool</p>
    </div>
  )
}

// ─────────────────────────────────────────────────────────────────────────────
// Tab styles
// ─────────────────────────────────────────────────────────────────────────────
const TAB_BASE   = 'px-5 py-3 text-sm font-semibold border-b-2 transition-colors cursor-pointer whitespace-nowrap'
const TAB_ACTIVE = 'border-blue-700 dark:border-blue-400 text-blue-700 dark:text-blue-400'
const TAB_IDLE   = 'border-transparent text-slate-500 dark:text-slate-400 hover:text-slate-700 dark:hover:text-slate-200'

// ─────────────────────────────────────────────────────────────────────────────
// Main page
// ─────────────────────────────────────────────────────────────────────────────
export default function ModelSHAP() {
  const [employees, setEmployees] = useState([])
  const [selectedId, setSelectedId] = useState('demo')
  const [tab, setTab] = useState('global')
  const [apiShapRaw, setApiShapRaw] = useState(null)
  const [shapLoading, setShapLoading] = useState(false)

  useEffect(() => {
    const q = query(collection(db, 'predictions'), orderBy('timestamp', 'desc'), limit(100))
    const unsub = onSnapshot(q,
      (snap) => setEmployees(snap.docs.map((d) => ({ id: d.id, ...d.data() }))),
      () => setEmployees([])
    )
    return unsub
  }, [])

  const emp = selectedId === 'demo'
    ? DEMO_EMPLOYEE
    : employees.find((e) => e.id === selectedId) ?? null

  useEffect(() => {
    if (!emp) { setApiShapRaw(null); return }
    setApiShapRaw(null)
    setShapLoading(true)
    getSHAPExplanation(emp)
      .then((data) => { if (!data?.error) setApiShapRaw(data) })
      .catch(() => { /* silent — falls back to client-side computation */ })
      .finally(() => setShapLoading(false))
  }, [emp?.id])

  const apiShap = useMemo(() => {
    if (!apiShapRaw) return null
    const contributions = (apiShapRaw.shap_values ?? []).map((v) => ({
      feature: v.feature,
      direction: v.direction,
      contribution_lo: v.impact,
      raw_value: v.raw_value,
      contribution_prob: 0,
    }))
    const totalAbsLo = contributions.reduce((s, c) => s + Math.abs(c.contribution_lo), 0)
    const probDiff   = apiShapRaw.risk_score - apiShapRaw.baseline_prob
    const withProb   = contributions.map((c) => ({
      ...c,
      contribution_prob: totalAbsLo > 0
        ? (c.contribution_lo / totalAbsLo) * Math.abs(probDiff) * Math.sign(probDiff)
        : 0,
    }))
    withProb.sort((a, b) => Math.abs(b.contribution_prob) - Math.abs(a.contribution_prob))
    return {
      baseProbability:  apiShapRaw.baseline_prob,
      finalProb:        apiShapRaw.risk_score,
      probDiff,
      contributions:    withProb.slice(0, 10),
      allContributions: withProb,
    }
  }, [apiShapRaw])

  const clientShap = useMemo(() => (emp ? computeShap(emp) : null), [emp?.id, emp?.probability])
  const shap = apiShap ?? clientShap
  const shapSource = shapLoading ? 'loading' : apiShap ? 'api' : 'client'

  const importanceFeatures = useMemo(
    () => [...(featureData.features ?? [])].sort((a, b) => b.perm_auc_drop - a.perm_auc_drop),
    []
  )

  const beeswarmData = shapMeta.beeswarm ?? []

  return (
    <div>
      {/* ── Page Header ───────────────────────────────────────────────── */}
      <div className="mb-6 flex flex-col sm:flex-row sm:items-end justify-between gap-4">
        <div>
          <nav className="flex items-center gap-2 text-[11px] text-slate-400 mb-2 uppercase tracking-widest font-semibold">
            <span>Analytics</span>
            <span className="material-symbols-outlined text-[13px]">chevron_right</span>
            <span className="text-blue-600 dark:text-blue-400">Model Interpretability</span>
          </nav>
          <h2 className="text-3xl font-black tracking-tight text-primary dark:text-blue-400">Model Interpretability (SHAP)</h2>
          <p className="text-sm text-slate-500 dark:text-slate-400 mt-1">Understand what drives attrition predictions</p>
        </div>
        <div className="flex items-center gap-3 shrink-0">
          <span className="text-xs font-medium text-slate-500">Select Employee</span>
          <EmployeeSelector employees={employees} selectedId={selectedId} onSelect={setSelectedId} />
        </div>
      </div>

      {/* ── Employee Summary ──────────────────────────────────────────── */}
      <SummaryPanel emp={emp} />

      {/* ── Tabs ──────────────────────────────────────────────────────── */}
      <Tabs.Root value={tab} onValueChange={setTab}>
        <Tabs.List className="flex border-b border-slate-200 dark:border-slate-700 mb-6 overflow-x-auto">
          <Tabs.Trigger value="global"        className={`${TAB_BASE} ${tab === 'global'        ? TAB_ACTIVE : TAB_IDLE}`}>Global Insights</Tabs.Trigger>
          <Tabs.Trigger value="employee"      className={`${TAB_BASE} ${tab === 'employee'      ? TAB_ACTIVE : TAB_IDLE}`}>Employee Explanation</Tabs.Trigger>
          <Tabs.Trigger value="relationships" className={`${TAB_BASE} ${tab === 'relationships' ? TAB_ACTIVE : TAB_IDLE}`}>Feature Relationships</Tabs.Trigger>
        </Tabs.List>

        {/* ── Global Insights ─────────────────────────────────────────── */}
        <Tabs.Content value="global">
          <div className="grid grid-cols-1 xl:grid-cols-3 gap-5">
            {/* 1. Global Feature Importance */}
            <div className="bg-white dark:bg-slate-900 border border-slate-200 dark:border-slate-800 rounded-2xl shadow-sm p-5">
              <div className="flex items-center gap-2 mb-4">
                <span className="material-symbols-outlined text-[18px] text-blue-600 dark:text-blue-400">bar_chart</span>
                <h3 className="font-bold text-sm text-slate-800 dark:text-slate-200">1. Global Feature Importance</h3>
              </div>
              <p className="text-[11px] text-slate-400 mb-4">Mean AUC drop (permutation importance, 30 repeats)</p>
              <GlobalImportanceChart features={importanceFeatures} n={10} />
              <div className="flex gap-4 mt-3 justify-center">
                <span className="flex items-center gap-1.5 text-[11px] text-slate-500"><span className="w-3 h-3 rounded-sm bg-red-400 inline-block"></span>Risk factor</span>
                <span className="flex items-center gap-1.5 text-[11px] text-slate-500"><span className="w-3 h-3 rounded-sm bg-blue-400 inline-block"></span>Protective</span>
              </div>
            </div>

            {/* 2. SHAP Summary Plot */}
            <div className="bg-white dark:bg-slate-900 border border-slate-200 dark:border-slate-800 rounded-2xl shadow-sm p-5">
              <div className="flex items-center gap-2 mb-4">
                <span className="material-symbols-outlined text-[18px] text-purple-600 dark:text-purple-400">scatter_plot</span>
                <h3 className="font-bold text-sm text-slate-800 dark:text-slate-200">2. SHAP Summary Plot</h3>
              </div>
              <p className="text-[11px] text-slate-400 mb-4">Each point is one employee · colour = feature value magnitude</p>
              <BeeswarmPlot beeswarmData={beeswarmData} n={10} />
            </div>

            {/* 3. Feature Impact Direction */}
            <div className="bg-white dark:bg-slate-900 border border-slate-200 dark:border-slate-800 rounded-2xl shadow-sm p-5">
              <div className="flex items-center gap-2 mb-4">
                <span className="material-symbols-outlined text-[18px] text-green-600 dark:text-green-400">compare_arrows</span>
                <h3 className="font-bold text-sm text-slate-800 dark:text-slate-200">3. Feature Impact Direction</h3>
              </div>
              <p className="text-[11px] text-slate-400 mb-4">How feature values push attrition predictions</p>
              <div className="grid grid-cols-2 gap-x-3 text-[11px] font-bold text-slate-400 uppercase mb-2 pb-1 border-b border-slate-200">
                <span>Feature</span><span className="text-right">Impact on Risk</span>
              </div>
              <DirectionTable features={importanceFeatures} n={12} />
            </div>
          </div>
        </Tabs.Content>

        {/* ── Employee Explanation ─────────────────────────────────────── */}
        <Tabs.Content value="employee">
          <AnimatePresence mode="wait">
          {shap ? (
            <motion.div
              key={selectedId}
              className="space-y-6"
              initial={{ opacity: 0 }}
              animate={{ opacity: 1 }}
              exit={{ opacity: 0 }}
              transition={{ duration: 0.2 }}
            >
              {/* Force plot card */}
              <div className="bg-white dark:bg-slate-900 border border-slate-200 dark:border-slate-800 rounded-2xl shadow-sm p-6">
                <div className="flex items-center justify-between mb-2">
                  <div className="flex items-center gap-2">
                    <span className="material-symbols-outlined text-[18px] text-orange-500">waterfall_chart</span>
                    <h3 className="font-bold text-sm text-slate-800 dark:text-slate-200">4. Employee Explanation (Local SHAP)</h3>
                  </div>
                  {/* Source indicator */}
                  <div className="flex items-center gap-1.5">
                    {shapSource === 'loading' ? (
                      <span className="flex items-center gap-1 text-[11px] font-semibold text-blue-500 bg-blue-50 border border-blue-200 px-2 py-0.5 rounded-full">
                        <span className="material-symbols-outlined text-[13px] animate-spin">refresh</span>Updating…
                      </span>
                    ) : shapSource === 'api' ? (
                      <span className="flex items-center gap-1 text-[11px] font-semibold text-green-600 bg-green-50 border border-green-200 px-2 py-0.5 rounded-full">
                        <span className="material-symbols-outlined text-[13px]">cloud_done</span>API
                      </span>
                    ) : (
                      <span className="flex items-center gap-1 text-[11px] font-semibold text-slate-500 bg-slate-50 border border-slate-200 px-2 py-0.5 rounded-full">
                        <span className="material-symbols-outlined text-[13px]">memory</span>Client-side
                      </span>
                    )}
                  </div>
                </div>
                <p className="text-[11px] text-slate-400 mb-5">
                  SHAP-style decomposition — how each feature shifts the predicted attrition probability away from the population average ({(shap.baseProbability * 100).toFixed(1)}%)
                </p>

                {/* Force plot header stats */}
                <div className="flex flex-wrap gap-6 mb-6">
                  <div>
                    <p className="text-[10px] uppercase font-bold text-slate-400">Base Value</p>
                    <p className="text-xl font-extrabold text-slate-700 dark:text-slate-200">{(shap.baseProbability * 100).toFixed(1)}%</p>
                    <p className="text-[10px] text-slate-400">Average prediction</p>
                  </div>
                  <div>
                    <p className="text-[10px] uppercase font-bold text-slate-400">Employee Prediction</p>
                    <p className={`text-xl font-extrabold ${shap.finalProb >= 0.26 ? 'text-red-600' : 'text-green-600'}`}>{(shap.finalProb * 100).toFixed(1)}%</p>
                    <p className="text-[10px] text-slate-400">This employee</p>
                  </div>
                  <div>
                    <p className="text-[10px] uppercase font-bold text-slate-400">Risk Difference</p>
                    <p className={`text-xl font-extrabold ${shap.probDiff >= 0 ? 'text-red-600' : 'text-green-600'}`}>
                      {shap.probDiff >= 0 ? '+' : ''}{(shap.probDiff * 100).toFixed(1)}%
                    </p>
                    <p className="text-[10px] text-slate-400">{shap.probDiff >= 0 ? 'Higher than average' : 'Lower than average'}</p>
                  </div>
                </div>

                <WaterfallChart {...shap} />

                <div className="flex gap-6 justify-center mt-4">
                  <span className="flex items-center gap-1.5 text-xs text-slate-500"><span className="w-3 h-2 rounded-sm bg-red-400 inline-block"></span>Pushes risk higher</span>
                  <span className="flex items-center gap-1.5 text-xs text-slate-500"><span className="w-3 h-2 rounded-sm bg-blue-400 inline-block"></span>Pushes risk lower</span>
                </div>
              </div>

              {/* Feature contribution table */}
              <div className="bg-white dark:bg-slate-900 border border-slate-200 dark:border-slate-800 rounded-2xl shadow-sm p-6">
                <h3 className="font-bold text-sm text-slate-800 dark:text-slate-200 mb-4">Top Feature Contributions</h3>
                <div className="space-y-3">
                  {shap.contributions.map((c, i) => {
                    const pct   = (c.contribution_prob * 100).toFixed(1)
                    const isPos = c.contribution_prob >= 0
                    const barW  = Math.abs(c.contribution_prob) / Math.max(...shap.contributions.map((x) => Math.abs(x.contribution_prob))) * 100
                    return (
                      <div key={c.feature} className="flex items-center gap-3">
                        <span className="w-5 text-xs font-bold text-slate-300 text-right">{i + 1}</span>
                        <span className="w-40 text-xs text-slate-600 font-medium truncate">{label(c.feature)}</span>
                        <div className="flex-1 h-5 bg-slate-50 dark:bg-slate-800 rounded-full overflow-hidden">
                          <motion.div
                            className={`h-full rounded-full ${isPos ? 'bg-red-400' : 'bg-blue-400'}`}
                            initial={{ width: 0 }}
                            animate={{ width: `${barW}%` }}
                            transition={{ duration: 0.5, delay: i * 0.04, ease: 'easeOut' }}
                          />
                        </div>
                        <span className={`w-14 text-xs font-bold text-right ${isPos ? 'text-red-600' : 'text-blue-600'}`}>
                          {isPos ? '+' : ''}{pct}%
                        </span>
                        <span className="w-12 text-[11px] text-slate-400 text-right">{c.raw_value?.toFixed ? c.raw_value.toFixed(1) : c.raw_value}</span>
                      </div>
                    )
                  })}
                </div>
              </div>
            </motion.div>
          ) : shapLoading ? (
            <motion.div
              key="loading"
              className="flex flex-col items-center justify-center py-20 text-slate-400"
              initial={{ opacity: 0 }} animate={{ opacity: 1 }} exit={{ opacity: 0 }}
            >
              <span className="material-symbols-outlined text-[52px] animate-spin mb-4 text-blue-400">refresh</span>
              <p className="text-sm font-semibold text-slate-600">Computing SHAP explanation…</p>
              <p className="text-xs mt-1">Requesting from API</p>
            </motion.div>
          ) : (
            <motion.div key="empty" initial={{ opacity: 0 }} animate={{ opacity: 1 }} exit={{ opacity: 0 }}>
              <EmptyExplanation />
            </motion.div>
          )}
          </AnimatePresence>
        </Tabs.Content>

        {/* ── Feature Relationships ───────────────────────────────────── */}
        <Tabs.Content value="relationships">
          <div className="space-y-4">
            <p className="text-sm text-slate-500">Key drivers of attrition — model insights and business implications</p>
            <RelationshipsInsights features={importanceFeatures} />
            {/* Method comparison table */}
            <div className="bg-white dark:bg-slate-900 border border-slate-200 dark:border-slate-800 rounded-2xl shadow-sm p-6 mt-2">
              <h3 className="font-bold text-sm text-slate-800 dark:text-slate-200 mb-4">Consensus Ranking — Three-Method Agreement</h3>
              <div className="overflow-x-auto">
                <table className="w-full text-xs">
                  <thead>
                    <tr className="border-b border-slate-200">
                      {['Rank', 'Feature', 'Std. Coef', 'Perm. AUC Drop', 'McFadden R²', 'Direction'].map((h) => (
                        <th key={h} className="text-left py-2 px-3 font-bold text-slate-400 uppercase tracking-wide">{h}</th>
                      ))}
                    </tr>
                  </thead>
                  <tbody>
                    {(featureData.features ?? []).slice(0, 15).map((f, i) => (
                      <tr key={f.feature} className="border-b border-slate-50 hover:bg-slate-50 transition-colors">
                        <td className="py-2 px-3 font-bold text-slate-400">{f.rank}</td>
                        <td className="py-2 px-3 font-semibold text-slate-700">{label(f.feature)}</td>
                        <td className={`py-2 px-3 font-mono ${f.std_coef > 0 ? 'text-red-600' : 'text-blue-600'}`}>{f.std_coef > 0 ? '+' : ''}{f.std_coef.toFixed(3)}</td>
                        <td className="py-2 px-3 font-mono text-slate-600">{f.perm_auc_drop.toFixed(4)}</td>
                        <td className="py-2 px-3 font-mono text-slate-600">{f.partial_r2.toFixed(4)}</td>
                        <td className="py-2 px-3">
                          {f.direction === 'risk'
                            ? <span className="inline-flex items-center gap-1 text-red-600 font-bold"><span className="material-symbols-outlined text-[12px]">arrow_upward</span>Risk</span>
                            : <span className="inline-flex items-center gap-1 text-green-600 font-bold"><span className="material-symbols-outlined text-[12px]">arrow_downward</span>Protective</span>}
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            </div>
          </div>
        </Tabs.Content>
      </Tabs.Root>
    </div>
  )
}

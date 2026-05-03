import { useEffect, useRef, useState } from 'react'
import { motion, AnimatePresence } from 'framer-motion'
import { db } from '../firebase'
import { collection, query, orderBy, onSnapshot, limit } from 'firebase/firestore'
import { useNavigate } from 'react-router-dom'

const RISK_STYLES = {
  High: { badge: 'bg-red-50 dark:bg-red-500/10 text-red-700 dark:text-red-400 ring-1 ring-inset ring-red-200 dark:ring-red-500/20', dot: 'bg-red-500 shadow-sm shadow-red-400', icon: 'warning' },
  Medium: { badge: 'bg-amber-50 dark:bg-amber-500/10 text-amber-700 dark:text-amber-400 ring-1 ring-inset ring-amber-200 dark:ring-amber-500/20', dot: 'bg-amber-400 shadow-sm shadow-amber-300', icon: 'info' },
  Low: { badge: 'bg-green-50 dark:bg-green-500/10 text-green-700 dark:text-green-400 ring-1 ring-inset ring-green-200 dark:ring-green-500/20', dot: 'bg-green-500 shadow-sm shadow-green-400', icon: 'check_circle' },
}

function RiskBadge({ level }) {
  const s = RISK_STYLES[level] ?? RISK_STYLES.Low
  return (
    <span className={`inline-flex items-center gap-1.5 px-2.5 py-1 rounded-full text-[11px] font-semibold tracking-wide uppercase leading-none ${s.badge}`}>
      <span className={`w-2 h-2 rounded-full ${s.dot}`} />
      {level} Risk
    </span>
  )
}

function ProbBar({ value }) {
  const pct = Math.round(value * 100)
  const color = pct >= 70 ? 'bg-red-500' : pct >= 40 ? 'bg-amber-400' : 'bg-green-500'
  return (
    <div className="flex items-center gap-2 min-w-0">
      <div className="flex-1 h-2 bg-slate-100 dark:bg-slate-800 rounded-full overflow-hidden">
        <motion.div
          className={`h-full rounded-full ${color}`}
          initial={{ width: '0%' }}
          animate={{ width: `${pct}%` }}
          transition={{ duration: 0.6, ease: 'easeOut' }}
        />
      </div>
      <span className="text-xs font-bold tabular-nums w-10 text-right text-slate-700 dark:text-slate-300">{pct}%</span>
    </div>
  )
}

function StatCard({ label, value, icon, color, index = 0 }) {
  return (
    <motion.div
      className="bg-white dark:bg-slate-900 border border-slate-200 dark:border-slate-800 rounded-2xl p-5 flex items-center gap-4 shadow-sm hover:shadow-md transition-shadow"
      initial={{ opacity: 0, y: 12 }}
      animate={{ opacity: 1, y: 0 }}
      transition={{ duration: 0.3, delay: index * 0.07, ease: 'easeOut' }}
    >
      <div className={`w-12 h-12 rounded-xl flex items-center justify-center ${color}`}>
        <span className="material-symbols-outlined text-[22px]">{icon}</span>
      </div>
      <div>
        <p className="text-[11px] font-semibold text-slate-500 dark:text-slate-400 uppercase tracking-wider">{label}</p>
        <p className="text-2xl font-black text-slate-900 dark:text-white tabular-nums">{value}</p>
      </div>
    </motion.div>
  )
}

export default function RiskWatch() {
  const navigate = useNavigate()
  const [records, setRecords] = useState([])
  const [loading, setLoading] = useState(true)
  const [firebaseError, setFirebaseError] = useState(false)
  const [filter, setFilter] = useState('All')
  const [search, setSearch] = useState('')
  const searchRef = useRef(null)

  useEffect(() => {
    const q = query(collection(db, 'predictions'), orderBy('timestamp', 'desc'), limit(100))
    const unsub = onSnapshot(
      q,
      (snap) => {
        const docs = snap.docs.map((d) => ({ id: d.id, ...d.data() }))
        setRecords(docs)
        setLoading(false)
      },
      (err) => {
        console.warn('Firestore unavailable:', err.message)
        setFirebaseError(true)
        setLoading(false)
      }
    )
    return unsub
  }, [])

  const byRisk = filter === 'All' ? records : records.filter((r) => r.risk_level === filter)
  const filtered = search.trim()
    ? byRisk.filter((r) => {
        const q = search.trim().toLowerCase()
        return (
          (r.employeeName ?? '').toLowerCase().includes(q) ||
          (r.JobRole ?? '').toLowerCase().includes(q) ||
          (r.Department ?? '').toLowerCase().includes(q)
        )
      })
    : byRisk

  const high = records.filter((r) => r.risk_level === 'High').length
  const medium = records.filter((r) => r.risk_level === 'Medium').length
  const low = records.filter((r) => r.risk_level === 'Low').length

  return (
    <div>
      {/* Breadcrumb + Title */}
      <div className="mb-8 flex items-end justify-between flex-wrap gap-4">
        <div>
          <nav className="flex items-center gap-2 text-[11px] text-slate-400 mb-3 uppercase tracking-widest font-semibold">
            <span>Analytics</span>
            <span className="material-symbols-outlined text-[14px]">chevron_right</span>
            <span className="text-blue-600 dark:text-blue-400">Employee Risk Watch</span>
          </nav>
          <h2 className="text-3xl font-black tracking-tight text-slate-900 dark:text-white">Employee Risk Watch</h2>
          <p className="text-sm text-slate-500 dark:text-slate-400 mt-2">Real-time attrition risk monitoring for all evaluated employees.</p>
        </div>
        <button
          onClick={() => navigate('/predict')}
          className="bg-primary text-white px-6 py-2.5 rounded-xl font-bold text-sm flex items-center gap-2 shadow-lg shadow-blue-900/20 hover:bg-blue-800 hover:-translate-y-0.5 transition-all"
        >
          <span className="material-symbols-outlined text-[20px]">add</span>
          New Prediction
        </button>
      </div>

      {/* Stat cards */}
      <div className="grid grid-cols-2 md:grid-cols-4 gap-4 mb-8">
        <StatCard label="Total Records" value={records.length} icon="database" color="bg-blue-50 text-blue-700" index={0} />
        <StatCard label="High Risk" value={high} icon="warning" color="bg-red-50 text-red-600" index={1} />
        <StatCard label="Medium Risk" value={medium} icon="info" color="bg-amber-50 text-amber-600" index={2} />
        <StatCard label="Low Risk" value={low} icon="check_circle" color="bg-green-50 text-green-700" index={3} />
      </div>

      {/* Firebase error notice */}
      {firebaseError && (
        <div className="mb-6 p-4 bg-amber-50 dark:bg-amber-500/10 border border-amber-200 dark:border-amber-500/30 rounded-2xl flex items-start gap-3">
          <span className="material-symbols-outlined text-amber-600 mt-0.5">info</span>
          <div>
            <p className="font-bold text-amber-800 dark:text-amber-300">Firebase not configured</p>
            <p className="text-sm text-amber-700 dark:text-amber-400 mt-1">
              Update <code className="bg-amber-100 dark:bg-amber-500/20 px-1 rounded text-xs">src/firebase.js</code> with your Firebase project credentials to enable live data. Predictions will still work but won't be persisted.
            </p>
          </div>
        </div>
      )}

      {/* Search + Filter row */}
      <div className="flex flex-col sm:flex-row gap-3 mb-6">
        {/* Search input */}
        <div className="relative flex-1 max-w-xs">
          <span className="material-symbols-outlined pointer-events-none absolute left-3 top-1/2 -translate-y-1/2 text-slate-400 text-[18px]">search</span>
          <input
            ref={searchRef}
            type="text"
            value={search}
            onChange={(e) => setSearch(e.target.value)}
            placeholder="Search employees…"
            className="w-full pl-9 pr-9 py-2.5 border border-slate-200 dark:border-slate-700 rounded-xl text-sm bg-white dark:bg-slate-800 text-slate-800 dark:text-slate-200 focus:ring-2 focus:ring-primary/30 focus:border-primary outline-none placeholder:text-slate-400 shadow-sm transition-all"
          />
          {search && (
            <button
              type="button"
              onClick={() => { setSearch(''); searchRef.current?.focus() }}
              className="absolute right-3 top-1/2 -translate-y-1/2 text-slate-400 hover:text-slate-600 transition-colors"
              aria-label="Clear search"
            >
              <span className="material-symbols-outlined text-[16px]">close</span>
            </button>
          )}
        </div>

        {/* Filter tabs */}
        <div className="flex gap-2">
        {['All', 'High', 'Medium', 'Low'].map((level) => (
          <motion.button
            key={level}
            whileTap={{ scale: 0.94 }}
            onClick={() => setFilter(level)}
            className={`px-4 py-2 rounded-xl text-sm font-semibold transition-all ${
              filter === level ? 'bg-primary text-white shadow-sm shadow-blue-900/20' : 'bg-white dark:bg-slate-800 border border-slate-200 dark:border-slate-700 text-slate-600 dark:text-slate-300 hover:bg-slate-50 dark:hover:bg-slate-700'
            }`}
          >
            {level}
            {level !== 'All' && (
              <span className="ml-2 text-[11px] opacity-70">
                ({level === 'High' ? high : level === 'Medium' ? medium : low})
              </span>
            )}
          </motion.button>
        ))}
        </div>
      </div>

      {/* Table */}
      <div className="bg-white dark:bg-slate-900 border border-slate-200 dark:border-slate-800 rounded-2xl overflow-hidden shadow-sm">
        <div className="px-6 py-4 border-b border-slate-100 dark:border-slate-800 flex justify-between items-center bg-slate-50/60 dark:bg-slate-900/60">
          <h3 className="text-base font-bold text-primary dark:text-blue-400">Prediction History</h3>
          <span className="text-[12px] font-semibold text-slate-500 dark:text-slate-400">
            {filtered.length} record{filtered.length !== 1 ? 's' : ''}
            {search.trim() ? ` matching “${search.trim()}”` : ''}
          </span>
        </div>

        {loading ? (
          <div className="flex flex-col items-center justify-center py-16 text-slate-400">
            <span className="material-symbols-outlined text-[48px] animate-spin mb-3">refresh</span>
            <p className="text-sm">Loading prediction history…</p>
          </div>
        ) : filtered.length === 0 ? (
          <div className="flex flex-col items-center justify-center py-16 text-slate-400">
            <span className="material-symbols-outlined text-[56px] mb-4">manage_search</span>
            <p className="text-base font-bold text-slate-700 dark:text-slate-300 mb-2">
              {search ? 'No matching records' : 'No records yet'}
            </p>
            <p className="text-sm text-center max-w-sm">
              {search
                ? `No results for “${search}”. Try a different name, role, or department.`
                : 'Run a prediction using the Attrition Risk tool. Each result is saved here automatically.'}
            </p>
            <button
              onClick={() => navigate('/predict')}
              className="mt-6 bg-primary text-white px-6 py-2.5 rounded-xl font-bold text-sm flex items-center gap-2 shadow-lg shadow-blue-900/20 hover:bg-blue-800 transition-all"
            >
              <span className="material-symbols-outlined text-[18px]">bolt</span>
              Run First Prediction
            </button>
          </div>
        ) : (
          <div className="overflow-x-auto">
            <table className="w-full border-collapse">
              <thead>
                <tr className="bg-slate-50 dark:bg-slate-800/50">
                  {['#', 'Employee', 'Department', 'Job Role', 'Age', 'OverTime', 'Risk Level', 'Probability', 'Prediction', 'Date'].map((h) => (
                    <th key={h} className={`px-4 py-3 text-[11px] font-bold uppercase tracking-wider text-slate-500 dark:text-slate-400 border-b border-slate-200 dark:border-slate-700 whitespace-nowrap ${h === '#' || h === 'Age' ? 'text-center' : 'text-left'}`}>
                      {h}
                    </th>
                  ))}
                </tr>
              </thead>
              <tbody>
                <AnimatePresence initial={false}>
                {filtered.map((r, i) => {
                  const ts = r.timestamp?.toDate?.()
                  const dateStr = ts
                    ? ts.toLocaleDateString('en-GB', { day: '2-digit', month: 'short', year: 'numeric' })
                    : '—'
                  return (
                    <motion.tr
                      key={r.id}
                      initial={{ opacity: 0, y: 6 }}
                      animate={{ opacity: 1, y: 0 }}
                      exit={{ opacity: 0 }}
                      transition={{ duration: 0.25, delay: i * 0.03 }}
                      className="border-b border-slate-100 dark:border-slate-800 hover:bg-slate-50 dark:hover:bg-slate-800/40 transition-colors"
                    >
                      <td className="px-4 py-3.5 text-center text-xs text-slate-400 tabular-nums">{i + 1}</td>
                      <td className="px-4 py-3.5 text-sm font-semibold text-slate-900 dark:text-slate-100 whitespace-nowrap">{r.employeeName ?? <span className="text-slate-400 italic text-[12px]">—</span>}</td>
                      <td className="px-4 py-3.5 text-sm font-medium text-slate-700 dark:text-slate-300 whitespace-nowrap">{r.Department ?? '—'}</td>
                      <td className="px-4 py-3.5 text-sm text-slate-600 dark:text-slate-400 whitespace-nowrap">{r.JobRole ?? '—'}</td>
                      <td className="px-4 py-3.5 text-center text-sm font-bold tabular-nums text-slate-700 dark:text-slate-300">{r.Age ?? '—'}</td>
                      <td className="px-4 py-3.5">
                        <span className={`text-[11px] font-bold px-2 py-1 rounded-lg ${
                          r.OverTime === 'Yes' 
                            ? 'bg-red-50 dark:bg-red-500/10 text-red-700 dark:text-red-400' 
                            : 'bg-slate-100 dark:bg-slate-800 text-slate-600 dark:text-slate-400'
                        }`}>
                          {r.OverTime ?? '—'}
                        </span>
                      </td>
                      <td className="px-4 py-3.5">
                        <RiskBadge level={r.risk_level ?? 'Low'} />
                      </td>
                      <td className="px-4 py-3.5 min-w-[140px]">
                        <ProbBar value={r.probability ?? 0} />
                      </td>
                      <td className="px-4 py-3.5">
                        <span className={`text-sm font-bold ${
                          r.prediction === 'Yes' ? 'text-red-600 dark:text-red-400' : 'text-green-700 dark:text-green-400'
                        }`}>
                          {r.prediction ?? '—'}
                        </span>
                      </td>
                      <td className="px-4 py-3.5 text-xs text-slate-500 dark:text-slate-400 whitespace-nowrap tabular-nums">{dateStr}</td>
                    </motion.tr>
                  )
                })}
                </AnimatePresence>
              </tbody>
            </table>
          </div>
        )}
      </div>
    </div>
  )
}

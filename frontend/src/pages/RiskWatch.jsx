import { useEffect, useState } from 'react'
import { motion, AnimatePresence } from 'framer-motion'
import { db } from '../firebase'
import { collection, query, orderBy, onSnapshot, limit } from 'firebase/firestore'
import { useNavigate } from 'react-router-dom'

const RISK_STYLES = {
  High: { badge: 'bg-red-100 text-red-700 border-red-200', dot: 'bg-red-500', icon: 'warning' },
  Medium: { badge: 'bg-amber-100 text-amber-700 border-amber-200', dot: 'bg-amber-400', icon: 'info' },
  Low: { badge: 'bg-green-100 text-green-700 border-green-200', dot: 'bg-green-500', icon: 'check_circle' },
}

function RiskBadge({ level }) {
  const s = RISK_STYLES[level] ?? RISK_STYLES.Low
  return (
    <span className={`inline-flex items-center gap-1.5 px-3 py-1 rounded-full border text-label-md font-bold ${s.badge}`}>
      <span className={`w-2 h-2 rounded-full ${s.dot}`} />
      {level}
    </span>
  )
}

function ProbBar({ value }) {
  const pct = Math.round(value * 100)
  const color = pct >= 70 ? 'bg-red-500' : pct >= 40 ? 'bg-amber-400' : 'bg-green-500'
  return (
    <div className="flex items-center gap-2 min-w-0">
      <div className="flex-1 h-1.5 bg-slate-100 rounded-full overflow-hidden">
        <motion.div
          className={`h-full rounded-full ${color}`}
          initial={{ width: '0%' }}
          animate={{ width: `${pct}%` }}
          transition={{ duration: 0.6, ease: 'easeOut' }}
        />
      </div>
      <span className="text-data-tabular font-semibold tabular-nums w-10 text-right">{pct}%</span>
    </div>
  )
}

function StatCard({ label, value, icon, color, index = 0 }) {
  return (
    <motion.div
      className="bg-white border border-slate-200 rounded-xl p-5 flex items-center gap-4"
      initial={{ opacity: 0, y: 12 }}
      animate={{ opacity: 1, y: 0 }}
      transition={{ duration: 0.3, delay: index * 0.07, ease: 'easeOut' }}
    >
      <div className={`w-10 h-10 rounded-lg flex items-center justify-center ${color}`}>
        <span className="material-symbols-outlined text-[20px]">{icon}</span>
      </div>
      <div>
        <p className="text-label-md text-secondary uppercase">{label}</p>
        <p className="text-headline-md font-bold text-on-surface">{value}</p>
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

  const filtered = filter === 'All' ? records : records.filter((r) => r.risk_level === filter)

  const high = records.filter((r) => r.risk_level === 'High').length
  const medium = records.filter((r) => r.risk_level === 'Medium').length
  const low = records.filter((r) => r.risk_level === 'Low').length

  return (
    <div>
      {/* Breadcrumb + Title */}
      <div className="mb-8 flex items-end justify-between flex-wrap gap-4">
        <div>
          <nav className="flex items-center gap-2 text-label-md text-slate-400 mb-2 uppercase">
            <span>Analytics</span>
            <span className="material-symbols-outlined text-[14px]">chevron_right</span>
            <span className="text-blue-700">Employee Risk Watch</span>
          </nav>
          <h2 className="text-headline-xl font-bold text-on-surface">Employee Risk Watch</h2>
          <p className="text-body-md text-secondary mt-1">Real-time attrition risk monitoring for all evaluated employees.</p>
        </div>
        <button
          onClick={() => navigate('/predict')}
          className="bg-primary text-white px-6 py-2.5 rounded-lg font-bold text-body-md flex items-center gap-2 hover:opacity-90 transition-opacity"
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
        <div className="mb-6 p-4 bg-amber-50 border border-amber-200 rounded-xl flex items-start gap-3">
          <span className="material-symbols-outlined text-amber-600 mt-0.5">info</span>
          <div>
            <p className="font-semibold text-amber-800">Firebase not configured</p>
            <p className="text-body-md text-amber-700 mt-1">
              Update <code className="bg-amber-100 px-1 rounded">src/firebase.js</code> with your Firebase project credentials to enable live data. Predictions will still work but won't be persisted.
            </p>
          </div>
        </div>
      )}

      {/* Filter tabs */}
      <div className="flex gap-2 mb-6">
        {['All', 'High', 'Medium', 'Low'].map((level) => (
          <motion.button
            key={level}
            whileTap={{ scale: 0.94 }}
            onClick={() => setFilter(level)}
            className={`px-4 py-2 rounded-lg text-body-md font-semibold transition-colors ${
              filter === level ? 'bg-primary text-white shadow-sm' : 'bg-white border border-slate-200 text-slate-600 hover:bg-slate-50'
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

      {/* Table */}
      <div className="bg-white border border-slate-200 rounded-xl overflow-hidden">
        <div className="p-4 border-b border-slate-100 flex justify-between items-center bg-slate-50/50">
          <h3 className="text-headline-md font-semibold text-primary">Prediction History</h3>
          <span className="text-label-md text-secondary">{filtered.length} record{filtered.length !== 1 ? 's' : ''}</span>
        </div>

        {loading ? (
          <div className="flex flex-col items-center justify-center py-16 text-slate-400">
            <span className="material-symbols-outlined text-[48px] animate-spin mb-3">refresh</span>
            <p className="text-body-md">Loading prediction history…</p>
          </div>
        ) : filtered.length === 0 ? (
          <div className="flex flex-col items-center justify-center py-16 text-slate-400">
            <span className="material-symbols-outlined text-[56px] mb-4">manage_search</span>
            <p className="text-headline-md font-semibold text-on-surface mb-2">No records yet</p>
            <p className="text-body-md text-center max-w-sm">
              Run a prediction using the Attrition Risk tool. Each result is saved here automatically.
            </p>
            <button
              onClick={() => navigate('/predict')}
              className="mt-6 bg-primary text-white px-6 py-2.5 rounded-lg font-bold text-body-md flex items-center gap-2 hover:opacity-90 transition-opacity"
            >
              <span className="material-symbols-outlined text-[18px]">bolt</span>
              Run First Prediction
            </button>
          </div>
        ) : (
          <div className="overflow-x-auto">
            <table className="w-full border-collapse">
              <thead>
                <tr className="bg-slate-50">
                  {['#', 'Employee', 'Department', 'Job Role', 'Age', 'OverTime', 'Risk Level', 'Probability', 'Prediction', 'Date'].map((h) => (
                    <th key={h} className={`p-4 text-label-md font-label-md text-secondary border-b border-slate-200 whitespace-nowrap ${h === '#' || h === 'Age' ? 'text-center' : 'text-left'}`}>
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
                      className="border-b border-slate-100 hover:bg-slate-50 transition-colors"
                    >
                      <td className="p-4 text-center text-label-md text-secondary tabular-nums">{i + 1}</td>
                      <td className="p-4 text-body-md font-medium text-on-surface whitespace-nowrap">{r.employeeName ?? <span className="text-slate-400 italic text-[12px]">—</span>}</td>
                      <td className="p-4 text-body-md font-medium text-on-surface whitespace-nowrap">{r.Department ?? '—'}</td>
                      <td className="p-4 text-body-md text-slate-600 whitespace-nowrap">{r.JobRole ?? '—'}</td>
                      <td className="p-4 text-center text-data-tabular tabular-nums">{r.Age ?? '—'}</td>
                      <td className="p-4">
                        <span className={`text-label-md font-bold px-2 py-0.5 rounded ${r.OverTime === 'Yes' ? 'bg-red-100 text-red-700' : 'bg-slate-100 text-slate-600'}`}>
                          {r.OverTime ?? '—'}
                        </span>
                      </td>
                      <td className="p-4">
                        <RiskBadge level={r.risk_level ?? 'Low'} />
                      </td>
                      <td className="p-4 min-w-[140px]">
                        <ProbBar value={r.probability ?? 0} />
                      </td>
                      <td className="p-4">
                        <span className={`text-label-md font-bold ${r.prediction === 'Yes' ? 'text-red-600' : 'text-green-700'}`}>
                          {r.prediction ?? '—'}
                        </span>
                      </td>
                      <td className="p-4 text-data-tabular text-secondary whitespace-nowrap">{dateStr}</td>
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

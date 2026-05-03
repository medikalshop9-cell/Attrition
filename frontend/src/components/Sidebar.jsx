import { useState } from 'react'
import { NavLink } from 'react-router-dom'
import { motion, AnimatePresence } from 'framer-motion'

const navItems = [
  { to: '/predict',    icon: 'analytics',   label: 'Attrition Risk' },
  { to: '/risk-watch', icon: 'group',        label: 'Employee Risk Watch' },
  { to: '/insights',   icon: 'query_stats',  label: 'Model Insights' },
  { to: '/shap',       icon: 'psychology',   label: 'Model Interpretability' },
]

function NavContent({ onClose }) {
  return (
    <>
      {/* Branding */}
      <div className="px-6 mb-8 flex items-start justify-between">
        <div>
          <h1 className="text-xl font-black text-blue-900 dark:text-blue-300">WorkforceX Insights</h1>
          <p className="text-[10px] uppercase tracking-widest text-slate-400 dark:text-slate-500 mt-1">Enterprise Analytics</p>
        </div>
        {onClose && (
          <button
            onClick={onClose}
            className="p-1 text-slate-400 hover:text-slate-600 dark:hover:text-slate-200 transition-colors"
            aria-label="Close menu"
          >
            <span className="material-symbols-outlined text-[24px]">close</span>
          </button>
        )}
      </div>

      {/* Nav items */}
      <div className="flex-1 space-y-1">
        {navItems.map(({ to, icon, label }) => (
          <NavLink
            key={to}
            to={to}
            onClick={onClose}
            className={({ isActive }) =>
              isActive
                ? 'bg-white dark:bg-slate-800 text-blue-700 dark:text-blue-300 font-bold border-r-4 border-blue-700 dark:border-blue-400 px-4 py-3 flex items-center gap-3 transition-all duration-200'
                : 'text-slate-600 dark:text-slate-400 px-4 py-3 flex items-center gap-3 hover:bg-slate-100 dark:hover:bg-slate-800 transition-all duration-200'
            }
          >
            <span className="material-symbols-outlined text-[20px]">{icon}</span>
            {label}
          </NavLink>
        ))}
      </div>

      {/* Footer */}
      <div className="px-6 mt-auto">
        <div className="flex items-center gap-3 p-3 bg-white dark:bg-slate-800 rounded-xl border border-slate-200 dark:border-slate-700">
          <div className="w-8 h-8 rounded-full bg-blue-900 dark:bg-blue-700 flex items-center justify-center text-white">
            <span className="material-symbols-outlined text-sm">shield</span>
          </div>
          <div>
            <p className="text-xs font-bold text-blue-900 dark:text-blue-300 leading-none">System Status</p>
            <p className="text-[10px] text-green-600 dark:text-green-400 font-medium">Model Active · v1.0</p>
          </div>
        </div>
      </div>
    </>
  )
}

export default function Sidebar() {
  const [open, setOpen] = useState(false)

  return (
    <>
      {/* ── Desktop sidebar ─────────────────────────────────────────── */}
      <nav className="hidden md:flex flex-col h-screen w-64 fixed left-0 top-0 z-50 bg-slate-50 dark:bg-slate-900 border-r border-slate-200 dark:border-slate-700 py-6 font-inter font-medium text-sm">
        <NavContent />
      </nav>

      {/* ── Mobile hamburger button ──────────────────────────────────── */}
      <button
        onClick={() => setOpen(true)}
        className="md:hidden fixed top-3 left-4 z-50 w-10 h-10 flex items-center justify-center rounded-full bg-white dark:bg-slate-800 border border-slate-200 dark:border-slate-700 shadow-sm text-slate-600 dark:text-slate-300 transition-colors hover:bg-slate-50"
        aria-label="Open menu"
      >
        <span className="material-symbols-outlined text-[22px]">menu</span>
      </button>

      {/* ── Mobile overlay + drawer ──────────────────────────────────── */}
      <AnimatePresence>
        {open && (
          <>
            <motion.div
              className="md:hidden fixed inset-0 z-40 bg-black/40 backdrop-blur-sm"
              initial={{ opacity: 0 }}
              animate={{ opacity: 1 }}
              exit={{ opacity: 0 }}
              onClick={() => setOpen(false)}
            />
            <motion.nav
              className="md:hidden fixed left-0 top-0 h-full w-72 z-50 bg-slate-50 dark:bg-slate-900 border-r border-slate-200 dark:border-slate-700 flex flex-col py-6 font-inter font-medium text-sm shadow-2xl"
              initial={{ x: '-100%' }}
              animate={{ x: 0 }}
              exit={{ x: '-100%' }}
              transition={{ type: 'spring', damping: 28, stiffness: 300 }}
            >
              <NavContent onClose={() => setOpen(false)} />
            </motion.nav>
          </>
        )}
      </AnimatePresence>
    </>
  )
}

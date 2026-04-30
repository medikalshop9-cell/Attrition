import { NavLink } from 'react-router-dom'

const navItems = [
  { to: '/predict', icon: 'analytics', label: 'Attrition Risk' },
  { to: '/risk-watch', icon: 'group', label: 'Employee Risk Watch' },
  { to: '/insights', icon: 'query_stats', label: 'Model Insights' },
]

export default function Sidebar() {
  return (
    <nav className="hidden md:flex flex-col h-screen w-64 fixed left-0 top-0 z-50 bg-slate-50 dark:bg-slate-900 border-r border-slate-200 dark:border-slate-700 py-6 font-inter font-medium text-sm">
      {/* Branding */}
      <div className="px-6 mb-8">
        <h1 className="text-xl font-black text-blue-900 dark:text-blue-300">WorkforceX Insights</h1>
        <p className="text-[10px] uppercase tracking-widest text-slate-400 dark:text-slate-500 mt-1">Enterprise Analytics</p>
      </div>

      {/* Nav items */}
      <div className="flex-1 space-y-1">
        <NavLink
          to="/predict"
          className={({ isActive }) =>
            isActive
              ? 'bg-white dark:bg-slate-800 text-blue-700 dark:text-blue-300 font-bold border-r-4 border-blue-700 dark:border-blue-400 px-4 py-3 flex items-center gap-3 transition-all duration-200'
              : 'text-slate-600 dark:text-slate-400 px-4 py-3 flex items-center gap-3 hover:bg-slate-100 dark:hover:bg-slate-800 transition-all duration-200'
          }
        >
          <span className="material-symbols-outlined text-[20px]">analytics</span>
          Attrition Risk
        </NavLink>
        <NavLink
          to="/risk-watch"
          className={({ isActive }) =>
            isActive
              ? 'bg-white dark:bg-slate-800 text-blue-700 dark:text-blue-300 font-bold border-r-4 border-blue-700 dark:border-blue-400 px-4 py-3 flex items-center gap-3 transition-all duration-200'
              : 'text-slate-600 dark:text-slate-400 px-4 py-3 flex items-center gap-3 hover:bg-slate-100 dark:hover:bg-slate-800 transition-all duration-200'
          }
        >
          <span className="material-symbols-outlined text-[20px]">group</span>
          Employee Risk Watch
        </NavLink>
        <NavLink
          to="/insights"
          className={({ isActive }) =>
            isActive
              ? 'bg-white dark:bg-slate-800 text-blue-700 dark:text-blue-300 font-bold border-r-4 border-blue-700 dark:border-blue-400 px-4 py-3 flex items-center gap-3 transition-all duration-200'
              : 'text-slate-600 dark:text-slate-400 px-4 py-3 flex items-center gap-3 hover:bg-slate-100 dark:hover:bg-slate-800 transition-all duration-200'
          }
        >
          <span className="material-symbols-outlined text-[20px]">query_stats</span>
          Model Insights
        </NavLink>
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
    </nav>
  )
}

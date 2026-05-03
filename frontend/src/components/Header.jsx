import { useDarkMode } from '../context/DarkMode'

export default function Header({ placeholder = 'Search employees...' }) {
  const { dark, toggle } = useDarkMode()
  return (
    <header className="flex justify-between items-center pl-16 pr-6 md:px-6 h-16 w-full sticky top-0 bg-white/90 dark:bg-slate-900/90 backdrop-blur-md border-b border-slate-200 dark:border-slate-700 z-40 font-inter antialiased text-sm">
      <div className="flex items-center gap-6">
        <span className="text-lg font-bold tracking-tight text-blue-900 dark:text-blue-300">WorkforceX Insights</span>
        <div className="hidden lg:flex items-center bg-slate-100 dark:bg-slate-800 rounded-full px-4 py-1.5 gap-2 border border-slate-200 dark:border-slate-600">
          <span className="material-symbols-outlined text-slate-400 text-sm">search</span>
          <input
            className="bg-transparent border-none focus:ring-0 text-xs w-48 outline-none dark:text-slate-200 dark:placeholder:text-slate-500"
            placeholder={placeholder}
            type="text"
            readOnly
          />
        </div>
      </div>
      <div className="flex items-center gap-2">
        {/* Dark mode toggle */}
        <button
          onClick={toggle}
          aria-label="Toggle dark mode"
          className="relative w-10 h-10 flex items-center justify-center rounded-full text-slate-500 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition-colors"
        >
          <span className="material-symbols-outlined text-[20px]">
            {dark ? 'light_mode' : 'dark_mode'}
          </span>
        </button>
        <button className="text-slate-500 dark:text-slate-400 hover:bg-slate-50 dark:hover:bg-slate-800 p-2 rounded-full transition-colors">
          <span className="material-symbols-outlined">help</span>
        </button>
      </div>
    </header>
  )
}

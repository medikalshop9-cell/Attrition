import { cn } from '../lib/utils'

export function Badge({ children, variant = 'default', className }) {
  const variants = {
    default: 'bg-primary dark:bg-blue-600 text-white',
    secondary: 'bg-slate-100 dark:bg-slate-800 text-slate-600 dark:text-slate-300 ring-1 ring-inset ring-slate-200 dark:ring-slate-700',
    high: 'bg-red-50 dark:bg-red-500/10 text-red-700 dark:text-red-400 ring-1 ring-inset ring-red-200 dark:ring-red-500/20',
    medium: 'bg-amber-50 dark:bg-amber-500/10 text-amber-700 dark:text-amber-400 ring-1 ring-inset ring-amber-200 dark:ring-amber-500/20',
    low: 'bg-green-50 dark:bg-green-500/10 text-green-700 dark:text-green-400 ring-1 ring-inset ring-green-200 dark:ring-green-500/20',
    outline: 'border border-slate-200 dark:border-slate-700 text-slate-700 dark:text-slate-300 bg-transparent',
  }
  return (
    <span className={cn('inline-flex items-center gap-1.5 px-2.5 py-1 rounded-full text-[11px] font-semibold tracking-wide uppercase leading-none', variants[variant], className)}>
      {children}
    </span>
  )
}

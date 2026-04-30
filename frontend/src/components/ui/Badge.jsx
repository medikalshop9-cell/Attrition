import { cn } from '../lib/utils'

export function Badge({ children, variant = 'default', className }) {
  const variants = {
    default: 'bg-primary text-white',
    secondary: 'bg-slate-100 text-slate-700',
    high: 'bg-red-100 text-red-700 border border-red-200',
    medium: 'bg-amber-100 text-amber-700 border border-amber-200',
    low: 'bg-green-100 text-green-700 border border-green-200',
    outline: 'border border-slate-200 text-slate-700 bg-white',
  }
  return (
    <span className={cn('inline-flex items-center gap-1.5 px-2.5 py-1 rounded-full text-[11px] font-bold leading-none', variants[variant], className)}>
      {children}
    </span>
  )
}

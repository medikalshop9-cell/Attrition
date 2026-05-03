import { motion, AnimatePresence } from 'framer-motion'
import { cn } from '../../lib/utils'

const RISK_STYLES = {
  High: {
    gradient: 'from-red-50 to-rose-100/60 dark:from-red-500/10 dark:to-rose-500/5',
    border: 'border-red-200 dark:border-red-500/30',
    text: 'text-red-700 dark:text-red-400',
    bar: 'bg-red-500',
    badge: 'bg-red-100 dark:bg-red-500/20 text-red-700 dark:text-red-300 border-red-200 dark:border-red-500/30',
    icon: 'warning',
    glow: 'shadow-red-100 dark:shadow-red-900/20',
  },
  Medium: {
    gradient: 'from-amber-50 to-yellow-100/60 dark:from-amber-500/10 dark:to-yellow-500/5',
    border: 'border-amber-200 dark:border-amber-500/30',
    text: 'text-amber-700 dark:text-amber-400',
    bar: 'bg-amber-400',
    badge: 'bg-amber-100 dark:bg-amber-500/20 text-amber-700 dark:text-amber-300 border-amber-200 dark:border-amber-500/30',
    icon: 'info',
    glow: 'shadow-amber-100 dark:shadow-amber-900/20',
  },
  Low: {
    gradient: 'from-green-50 to-emerald-100/60 dark:from-green-500/10 dark:to-emerald-500/5',
    border: 'border-green-200 dark:border-green-500/30',
    text: 'text-green-700 dark:text-green-400',
    bar: 'bg-green-500',
    badge: 'bg-green-100 dark:bg-green-500/20 text-green-700 dark:text-green-300 border-green-200 dark:border-green-500/30',
    icon: 'check_circle',
    glow: 'shadow-green-100 dark:shadow-green-900/20',
  },
}

/**
 * Animated prediction result card shown after running the model.
 * Props: result { prediction, probability, risk_level, threshold }
 */
export function PredictionResultCard({ result }) {
  if (!result) return null
  const pct = Math.round(result.probability * 100)
  const s = RISK_STYLES[result.risk_level] ?? RISK_STYLES.Low

  return (
    <AnimatePresence mode="wait">
      <motion.div
        key={result.probability}
        className={cn(
          'rounded-2xl border-2 p-6 bg-gradient-to-br relative overflow-hidden shadow-xl',
          s.gradient, s.border, s.glow
        )}
        initial={{ opacity: 0, scale: 0.92, y: 20 }}
        animate={{ opacity: 1, scale: 1, y: 0 }}
        exit={{ opacity: 0, scale: 0.92 }}
        transition={{ type: 'spring', duration: 0.5, bounce: 0.25 }}
      >
        {/* Decorative blob */}
        <div className="absolute -right-12 -top-12 w-40 h-40 rounded-full bg-current opacity-5" />

        <p className="text-[10px] font-bold uppercase tracking-widest text-slate-500 dark:text-slate-400 mb-4">Attrition Probability</p>

        {/* Big number */}
        <div className="flex items-end gap-2 mb-4">
          <motion.span
            className={cn('text-[64px] font-black leading-none', s.text)}
            initial={{ opacity: 0, y: 10 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ delay: 0.15 }}
          >
            {pct}
          </motion.span>
          <span className={cn('text-3xl font-bold mb-2', s.text)}>%</span>
        </div>

        {/* Risk badge */}
        <span className={cn('inline-flex items-center gap-2 px-4 py-1.5 rounded-full border font-bold text-label-md mb-6', s.badge)}>
          <span className="material-symbols-outlined text-[16px]">{s.icon}</span>
          {result.risk_level.toUpperCase()} RISK
        </span>

        {/* Progress bar */}
        <div className="w-full h-3 bg-black/10 dark:bg-white/10 rounded-full overflow-hidden mb-1">
          <motion.div
            className={cn('h-full rounded-full', s.bar)}
            initial={{ width: '0%' }}
            animate={{ width: `${pct}%` }}
            transition={{ duration: 0.8, delay: 0.2, ease: 'easeOut' }}
          />
        </div>
        <div className="flex justify-between text-[10px] font-bold text-slate-400 dark:text-slate-500">
          <span>LOW</span><span>HIGH</span>
        </div>
      </motion.div>
    </AnimatePresence>
  )
}

import { motion } from 'framer-motion'
import { cn } from '../../lib/utils'

/**
 * Animated card wrapper — fades + slides up on mount
 */
export function Card({ children, className, delay = 0 }) {
  return (
    <motion.div
      className={cn('bg-white dark:bg-slate-900 border border-slate-200 dark:border-slate-800 rounded-2xl shadow-sm overflow-hidden', className)}
      initial={{ opacity: 0, y: 12 }}
      animate={{ opacity: 1, y: 0 }}
      transition={{ duration: 0.35, delay, ease: 'easeOut' }}
    >
      {children}
    </motion.div>
  )
}

export function CardHeader({ children, className }) {
  return (
    <div className={cn('px-6 py-4 border-b border-slate-100 dark:border-slate-800 flex items-center justify-between bg-slate-50/60 dark:bg-slate-900/60', className)}>
      {children}
    </div>
  )
}

export function CardBody({ children, className }) {
  return <div className={cn('p-6', className)}>{children}</div>
}

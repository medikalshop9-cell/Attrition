import { motion } from 'framer-motion'
import { cn } from '../../lib/utils'

/**
 * Animated card wrapper — fades + slides up on mount
 */
export function Card({ children, className, delay = 0 }) {
  return (
    <motion.div
      className={cn('bg-white border border-slate-200 rounded-xl', className)}
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
    <div className={cn('p-6 border-b border-slate-100 flex items-center justify-between', className)}>
      {children}
    </div>
  )
}

export function CardBody({ children, className }) {
  return <div className={cn('p-6', className)}>{children}</div>
}

import { useState, useCallback, useEffect, createContext, useContext } from 'react'
import { motion, AnimatePresence } from 'framer-motion'
import { X, CheckCircle, AlertCircle, Info } from 'lucide-react'
import { cn } from '../../lib/utils'

const ToastContext = createContext(null)

const ICONS = {
  success: <CheckCircle size={18} className="text-green-600 shrink-0" />,
  error: <AlertCircle size={18} className="text-red-600 shrink-0" />,
  info: <Info size={18} className="text-blue-600 shrink-0" />,
}
const BORDER = {
  success: 'border-green-200 bg-green-50',
  error: 'border-red-200 bg-red-50',
  info: 'border-blue-200 bg-blue-50',
}

function ToastItem({ id, type = 'info', title, message, onDismiss }) {
  useEffect(() => {
    const t = setTimeout(() => onDismiss(id), 4000)
    return () => clearTimeout(t)
  }, [id, onDismiss])

  return (
    <motion.div
      layout
      initial={{ opacity: 0, y: 32, scale: 0.95 }}
      animate={{ opacity: 1, y: 0, scale: 1 }}
      exit={{ opacity: 0, y: 16, scale: 0.95 }}
      transition={{ type: 'spring', duration: 0.4, bounce: 0.2 }}
      className={cn(
        'w-80 flex items-start gap-3 px-4 py-3 rounded-xl border shadow-lg backdrop-blur-sm',
        BORDER[type]
      )}
    >
      {ICONS[type]}
      <div className="flex-1 min-w-0">
        {title && <p className="text-sm font-bold text-slate-800 leading-snug">{title}</p>}
        {message && <p className="text-xs text-slate-600 leading-snug mt-0.5">{message}</p>}
      </div>
      <button onClick={() => onDismiss(id)} className="text-slate-400 hover:text-slate-600 transition-colors shrink-0 mt-0.5">
        <X size={14} />
      </button>
    </motion.div>
  )
}

export function ToastProvider({ children }) {
  const [toasts, setToasts] = useState([])

  const toast = useCallback(({ type = 'info', title, message }) => {
    const id = Math.random().toString(36).slice(2)
    setToasts((prev) => [...prev, { id, type, title, message }])
  }, [])

  const dismiss = useCallback((id) => {
    setToasts((prev) => prev.filter((t) => t.id !== id))
  }, [])

  return (
    <ToastContext.Provider value={toast}>
      {children}
      <div className="fixed bottom-6 right-6 z-[100] flex flex-col gap-2 pointer-events-none">
        <AnimatePresence mode="popLayout">
          {toasts.map((t) => (
            <div key={t.id} className="pointer-events-auto">
              <ToastItem {...t} onDismiss={dismiss} />
            </div>
          ))}
        </AnimatePresence>
      </div>
    </ToastContext.Provider>
  )
}

export function useToast() {
  const ctx = useContext(ToastContext)
  if (!ctx) throw new Error('useToast must be used inside <ToastProvider>')
  return ctx
}

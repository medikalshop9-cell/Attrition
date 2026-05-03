import * as TabsPrimitive from '@radix-ui/react-tabs'
import { cn } from '../../lib/utils'

export const Tabs = TabsPrimitive.Root

export function TabsList({ className, children, ...props }) {
  return (
    <TabsPrimitive.List
      className={cn('inline-flex items-center gap-1 rounded-xl bg-slate-100 dark:bg-slate-800/60 p-1', className)}
      {...props}
    >
      {children}
    </TabsPrimitive.List>
  )
}

export function TabsTrigger({ className, children, ...props }) {
  return (
    <TabsPrimitive.Trigger
      className={cn(
        'inline-flex items-center justify-center px-4 py-2 text-sm font-semibold rounded-lg transition-all duration-200',
        'text-slate-500 dark:text-slate-400 hover:text-slate-700 dark:hover:text-slate-200',
        'data-[state=active]:bg-white dark:data-[state=active]:bg-slate-700 data-[state=active]:text-primary dark:data-[state=active]:text-blue-300 data-[state=active]:shadow-sm',
        className
      )}
      {...props}
    >
      {children}
    </TabsPrimitive.Trigger>
  )
}

export function TabsContent({ className, children, ...props }) {
  return (
    <TabsPrimitive.Content className={cn('mt-4 outline-none', className)} {...props}>
      {children}
    </TabsPrimitive.Content>
  )
}

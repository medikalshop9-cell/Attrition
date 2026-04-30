import * as TabsPrimitive from '@radix-ui/react-tabs'
import { cn } from '../../lib/utils'

export const Tabs = TabsPrimitive.Root

export function TabsList({ className, children, ...props }) {
  return (
    <TabsPrimitive.List
      className={cn('inline-flex items-center gap-1 rounded-xl bg-slate-100 p-1', className)}
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
        'text-slate-500 hover:text-slate-700',
        'data-[state=active]:bg-white data-[state=active]:text-primary data-[state=active]:shadow-sm',
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

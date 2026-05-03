import * as ProgressPrimitive from '@radix-ui/react-progress'
import { cn } from '../../lib/utils'

export function Progress({ value = 0, className, indicatorClassName }) {
  return (
    <ProgressPrimitive.Root
      className={cn('relative h-2 w-full overflow-hidden rounded-full bg-slate-100 dark:bg-slate-800 shadow-inner', className)}
      value={value}
    >
      <ProgressPrimitive.Indicator
        className={cn('h-full rounded-full transition-all duration-700 ease-out', indicatorClassName ?? 'bg-primary dark:bg-blue-500')}
        style={{ transform: `translateX(-${100 - (value || 0)}%)` }}
      />
    </ProgressPrimitive.Root>
  )
}

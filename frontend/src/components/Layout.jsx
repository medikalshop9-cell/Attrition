import { Outlet } from 'react-router-dom'
import Sidebar from './Sidebar'
import Header from './Header'

export default function Layout() {
  return (
    <div className="bg-background dark:bg-slate-950 text-on-background dark:text-slate-100 min-h-screen">
      <Sidebar />
      <div className="md:ml-64 min-h-screen">
        <Header />
        <main className="p-6 lg:p-8 max-w-7xl mx-auto">
          <Outlet />
        </main>
      </div>
    </div>
  )
}

import { useState } from 'react'
import { motion, AnimatePresence } from 'framer-motion'
import { predict } from '../api'
import { db } from '../firebase'
import { collection, addDoc, serverTimestamp } from 'firebase/firestore'
import { useToast } from '../components/ui/Toast'
import { PredictionResultCard } from '../components/ui/PredictionResultCard'

const fadeUp = { hidden: { opacity: 0, y: 14 }, show: (i) => ({ opacity: 1, y: 0, transition: { delay: i * 0.07, duration: 0.3, ease: 'easeOut' } }) }

const DEFAULT_FORM = {
  Age: 34,
  BusinessTravel: 'Travel_Rarely',
  DailyRate: 802,
  Department: 'Research & Development',
  DistanceFromHome: 9,
  Education: 3,
  EducationField: 'Life Sciences',
  EnvironmentSatisfaction: 3,
  Gender: 'Male',
  HourlyRate: 66,
  JobInvolvement: 3,
  JobLevel: 2,
  JobRole: 'Research Scientist',
  JobSatisfaction: 3,
  MaritalStatus: 'Single',
  MonthlyIncome: 5993,
  MonthlyRate: 19479,
  NumCompaniesWorked: 2,
  OverTime: 'No',
  PercentSalaryHike: 13,
  PerformanceRating: 3,
  RelationshipSatisfaction: 3,
  StockOptionLevel: 1,
  TotalWorkingYears: 8,
  TrainingTimesLastYear: 2,
  WorkLifeBalance: 3,
  YearsAtCompany: 5,
  YearsInCurrentRole: 3,
  YearsSinceLastPromotion: 1,
  YearsWithCurrManager: 3,
}

function computeRiskFactors(form) {
  const items = []
  if (form.OverTime === 'Yes')                   items.push({ icon: 'schedule',             color: 'bg-red-100 text-red-600',        label: 'OverTime Active',           desc: 'Working extra hours significantly raises burnout and departure risk.' })
  if (form.BusinessTravel === 'Travel_Frequently') items.push({ icon: 'flight_takeoff',        color: 'bg-amber-100 text-amber-600',    label: 'Frequent Business Travel',  desc: 'Frequent travel correlates with 12% higher attrition rate.' })
  if (form.JobSatisfaction <= 2)                 items.push({ icon: 'sentiment_dissatisfied', color: 'bg-red-100 text-red-600',        label: 'Low Job Satisfaction',      desc: 'Low satisfaction is a leading indicator of voluntary departure.' })
  if (form.WorkLifeBalance <= 2)                 items.push({ icon: 'balance',                color: 'bg-red-100 text-red-600',        label: 'Poor Work-Life Balance',    desc: 'Low work-life balance is correlated with burnout and attrition.' })
  if (form.EnvironmentSatisfaction <= 2)         items.push({ icon: 'place',                  color: 'bg-orange-100 text-orange-600',  label: 'Low Environment Satisfaction', desc: 'Dissatisfaction with work environment raises flight risk.' })
  if (form.YearsSinceLastPromotion >= 3)         items.push({ icon: 'trending_up',            color: 'bg-amber-100 text-amber-600',    label: 'No Recent Promotion',       desc: `${form.YearsSinceLastPromotion} years without promotion increases flight risk.` })
  if (form.NumCompaniesWorked >= 5)              items.push({ icon: 'work_history',           color: 'bg-blue-100 text-blue-700',      label: 'High Job Mobility',         desc: `Worked at ${form.NumCompaniesWorked} companies — indicates mobility tendency.` })
  if (form.MaritalStatus === 'Single')           items.push({ icon: 'person',                 color: 'bg-purple-100 text-purple-700',  label: 'Single Marital Status',     desc: 'Single employees historically show higher attrition rates.' })
  if (form.MonthlyIncome < 3500)                 items.push({ icon: 'payments',               color: 'bg-blue-100 text-blue-700',      label: 'Below-median Income',       desc: `Monthly income ($${form.MonthlyIncome.toLocaleString()}) is below the company median.` })
  // Positive retention factors
  if (form.YearsAtCompany >= 8)                  items.push({ icon: 'check_circle',           color: 'bg-green-100 text-green-700',    label: 'Long Company Tenure',       desc: `${form.YearsAtCompany} years at company — strong retention indicator.` })
  if (form.JobSatisfaction >= 4)                 items.push({ icon: 'sentiment_very_satisfied', color: 'bg-green-100 text-green-700', label: 'High Job Satisfaction',     desc: 'High satisfaction reduces attrition risk by ~30%.' })
  if (form.MonthlyIncome >= 8000)                items.push({ icon: 'savings',                color: 'bg-green-100 text-green-700',    label: 'High Compensation',         desc: 'Above-average income is a strong retention factor.' })
  // Ensure at least one factor
  if (items.length === 0) items.push({ icon: 'info', color: 'bg-slate-100 text-slate-600', label: 'Profile within normal range', desc: 'No strong individual risk factors detected for this profile.' })
  return items.slice(0, 5)
}

function SegmentedButtons({ value, onChange, options }) {
  return (
    <div className="flex gap-1.5">
      {options.map((opt) => (
        <motion.button
          key={opt}
          type="button"
          whileTap={{ scale: 0.92 }}
          onClick={() => onChange(opt)}
          className={`flex-1 py-2 rounded-lg text-xs font-bold transition-colors border ${
            value === opt
              ? 'border-primary text-primary bg-blue-50'
              : 'border-outline-variant text-slate-600 bg-white hover:bg-slate-50'
          }`}
        >
          {opt}
        </motion.button>
      ))}
    </div>
  )
}

function SelectField({ label, name, value, onChange, options }) {
  return (
    <label className="block">
      <span className="text-label-md font-label-md text-secondary mb-1 block uppercase">{label}</span>
      <select
        name={name}
        value={value}
        onChange={onChange}
        className="w-full bg-white border border-outline-variant rounded-lg px-4 py-2.5 focus:ring-2 focus:ring-primary focus:border-primary text-body-md outline-none"
      >
        {options.map((o) => (
          <option key={o.value ?? o} value={o.value ?? o}>{o.label ?? o}</option>
        ))}
      </select>
    </label>
  )
}

function NumberField({ label, name, value, onChange, prefix }) {
  return (
    <label className="block">
      <span className="text-label-md font-label-md text-secondary mb-1 block uppercase">{label}</span>
      <div className="relative">
        {prefix && (
          <span className="absolute left-4 top-1/2 -translate-y-1/2 text-slate-400 font-bold">{prefix}</span>
        )}
        <input
          type="number"
          name={name}
          value={value}
          onChange={onChange}
          className={`w-full bg-white border border-outline-variant rounded-lg ${prefix ? 'pl-8 pr-4' : 'px-4'} py-2.5 focus:ring-2 focus:ring-primary focus:border-primary text-body-md outline-none`}
        />
      </div>
    </label>
  )
}

export default function PredictionTool() {
  const [form, setForm] = useState(DEFAULT_FORM)
  const [employeeName, setEmployeeName] = useState('')
  const [threshold, setThreshold] = useState(0.25)
  const [rawResult, setRawResult] = useState(null)
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState(null)
  const toast = useToast()

  // Derive result from rawResult + current threshold
  const result = rawResult ? {
    ...rawResult,
    prediction: rawResult.probability >= threshold ? 'Yes' : 'No',
    risk_level: rawResult.probability >= 0.6 ? 'High' : rawResult.probability >= 0.3 ? 'Medium' : 'Low',
  } : null

  function handleChange(e) {
    const { name, value, type } = e.target
    setForm((f) => ({ ...f, [name]: type === 'number' ? Number(value) : value }))
  }

  function handleSegmented(name, value) {
    setForm((f) => ({ ...f, [name]: Number(value) }))
  }

  async function handleSubmit(e) {
    e.preventDefault()
    setLoading(true)
    setError(null)
    setRawResult(null)
    try {
      const payload = { ...form }
      const res = await predict(payload)
      setRawResult(res)
      // Save to Firestore predictions collection
      try {
        await addDoc(collection(db, 'predictions'), {
          ...payload,
          employeeName: employeeName.trim() || null,
          prediction: res.prediction,
          probability: res.probability,
          risk_level: res.risk_level,
          timestamp: serverTimestamp(),
        })
        toast({ type: 'success', title: 'Saved to Risk Watch', message: `${res.risk_level} risk prediction stored in Firestore.` })
      } catch {
        toast({ type: 'info', title: 'Prediction complete', message: 'Firebase not configured — result not saved.' })
      }
    } catch (err) {
      const isTimeout = err.code === 'ECONNABORTED' || err.message?.includes('timeout')
      const isOffline = err.code === 'ERR_NETWORK' || err.message?.includes('Network Error')
      const msg = isTimeout || isOffline
        ? 'R API is not running. Start the API on port 8000 first.'
        : err.response?.data?.error || err.message || 'Prediction failed.'
      setError(msg)
      toast({ type: 'error', title: 'API Unavailable', message: msg })
    } finally {
      setLoading(false)
    }
  }

  function handleReset() {
    setForm(DEFAULT_FORM)
    setRawResult(null)
    setError(null)
    setEmployeeName('')
  }

  const factors = result ? computeRiskFactors(form) : []

  return (
    <div>
      {/* Breadcrumb + Title */}
      <div className="mb-8">
        <nav className="flex items-center gap-2 text-label-md text-slate-400 mb-2 uppercase">
          <span>Analytics</span>
          <span className="material-symbols-outlined text-[14px]">chevron_right</span>
          <span className="text-blue-700">Attrition Prediction Tool</span>
        </nav>
        <h2 className="text-headline-xl font-bold text-on-surface">Predict Employee Churn</h2>
        <p className="text-body-md text-secondary mt-1">Input employee metrics to calculate risk probability based on historical data patterns.</p>
      </div>

      {/* Bento Grid */}
      <div className="grid grid-cols-1 lg:grid-cols-12 gap-6 items-start">
        {/* Form — left 8 cols */}
        <section className="lg:col-span-8 bg-white border border-slate-200 rounded-xl overflow-hidden">
          <div className="p-6 border-b border-slate-200 flex items-center justify-between bg-white">
            <div className="flex items-center gap-2">
              <span className="material-symbols-outlined text-primary">person_search</span>
              <h3 className="text-headline-md font-semibold text-on-surface">Employee Profile Details</h3>
            </div>
            <span className="text-label-md text-secondary bg-slate-100 px-3 py-1 rounded-full uppercase">Active Model: LR v1.0</span>
          </div>

          <form onSubmit={handleSubmit} className="p-6 space-y-8">
            {/* Employee Name (metadata, not a model feature) */}
            <div>
              <label className="block">
                <span className="text-label-md font-label-md text-secondary mb-1 block uppercase">Employee Name <span className="text-[10px] normal-case font-normal text-slate-400">(optional — for record-keeping)</span></span>
                <input
                  type="text"
                  value={employeeName}
                  onChange={(e) => setEmployeeName(e.target.value)}
                  placeholder="e.g. Jane Smith"
                  className="w-full bg-white border border-outline-variant rounded-lg px-4 py-2.5 focus:ring-2 focus:ring-primary focus:border-primary text-body-md outline-none"
                />
              </label>
            </div>

            {/* Row 1: Demographic */}
            <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
              {/* Left col */}
              <div className="space-y-4">
                <NumberField label="Age" name="Age" value={form.Age} onChange={handleChange} />
                <SelectField
                  label="Business Travel"
                  name="BusinessTravel"
                  value={form.BusinessTravel}
                  onChange={handleChange}
                  options={['Travel_Rarely', 'Travel_Frequently', 'Non-Travel']}
                />
                <label className="block">
                  <span className="text-label-md font-label-md text-secondary mb-1 block uppercase">Education Level</span>
                  <SegmentedButtons
                    value={form.Education}
                    onChange={(v) => handleSegmented('Education', v)}
                    options={[1, 2, 3, 4, 5]}
                  />
                </label>
                <label className="block">
                  <span className="text-label-md font-label-md text-secondary mb-1 block uppercase">Distance From Home (km)</span>
                  <input
                    type="range" name="DistanceFromHome" min={1} max={50}
                    value={form.DistanceFromHome}
                    onChange={(e) => setForm((f) => ({ ...f, DistanceFromHome: Number(e.target.value) }))}
                    className="w-full h-1.5 bg-slate-200 rounded-full appearance-none cursor-pointer accent-primary"
                  />
                  <div className="flex justify-between text-[10px] text-slate-400 mt-1">
                    <span>1 km</span><span className="font-semibold text-slate-600">{form.DistanceFromHome} km</span><span>50 km</span>
                  </div>
                </label>
                <SelectField
                  label="Gender"
                  name="Gender"
                  value={form.Gender}
                  onChange={handleChange}
                  options={['Male', 'Female']}
                />
                <SelectField
                  label="Marital Status"
                  name="MaritalStatus"
                  value={form.MaritalStatus}
                  onChange={handleChange}
                  options={['Single', 'Married', 'Divorced']}
                />
                <SelectField
                  label="Education Field"
                  name="EducationField"
                  value={form.EducationField}
                  onChange={handleChange}
                  options={['Human Resources', 'Life Sciences', 'Marketing', 'Medical', 'Other', 'Technical Degree']}
                />
              </div>

              {/* Right col — Professional */}
              <div className="space-y-4">
                <SelectField
                  label="Department"
                  name="Department"
                  value={form.Department}
                  onChange={handleChange}
                  options={['Research & Development', 'Sales', 'Human Resources']}
                />
                <SelectField
                  label="Job Role"
                  name="JobRole"
                  value={form.JobRole}
                  onChange={handleChange}
                  options={[
                    'Healthcare Representative', 'Human Resources', 'Laboratory Technician',
                    'Manager', 'Manufacturing Director', 'Research Director',
                    'Research Scientist', 'Sales Executive', 'Sales Representative',
                  ]}
                />
                <NumberField label="Monthly Income ($)" name="MonthlyIncome" value={form.MonthlyIncome} onChange={handleChange} prefix="$" />
                {/* OverTime Toggle */}
                <div className="p-4 bg-slate-50 border border-dashed border-slate-300 rounded-lg">
                  <div className="flex items-center justify-between">
                    <div>
                      <span className="text-label-md font-semibold text-on-surface block">OverTime Status</span>
                      <span className="text-[10px] text-secondary">Does employee work extra hours?</span>
                    </div>
                    <motion.button
                      type="button"
                      whileTap={{ scale: 0.9 }}
                      onClick={() => setForm((f) => ({ ...f, OverTime: f.OverTime === 'Yes' ? 'No' : 'Yes' }))}
                      className={`w-12 h-6 rounded-full relative transition-colors duration-300 flex-shrink-0 ${form.OverTime === 'Yes' ? 'bg-primary' : 'bg-slate-300'}`}
                    >
                      <motion.div
                        animate={{ x: form.OverTime === 'Yes' ? 22 : 2 }}
                        transition={{ type: 'spring', stiffness: 500, damping: 30 }}
                        className="w-4 h-4 bg-white rounded-full shadow absolute top-1"
                      />
                    </motion.button>
                  </div>
                </div>
                <label className="block">
                  <span className="text-label-md font-label-md text-secondary mb-1 block uppercase">Job Level</span>
                  <SegmentedButtons value={form.JobLevel} onChange={(v) => handleSegmented('JobLevel', v)} options={[1, 2, 3, 4, 5]} />
                </label>
                <label className="block">
                  <span className="text-label-md font-label-md text-secondary mb-1 block uppercase">Stock Option Level</span>
                  <SegmentedButtons value={form.StockOptionLevel} onChange={(v) => handleSegmented('StockOptionLevel', v)} options={[0, 1, 2, 3]} />
                </label>
              </div>
            </div>

            {/* Row 2: Satisfaction & Experience */}
            <div>
              <h4 className="text-label-md uppercase text-secondary mb-4 border-t border-slate-100 pt-4">Satisfaction & Experience</h4>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                <div className="space-y-4">
                  <label className="block">
                    <span className="text-label-md font-label-md text-secondary mb-1 block uppercase">Environment Satisfaction (1–4)</span>
                    <SegmentedButtons value={form.EnvironmentSatisfaction} onChange={(v) => handleSegmented('EnvironmentSatisfaction', v)} options={[1, 2, 3, 4]} />
                  </label>
                  <label className="block">
                    <span className="text-label-md font-label-md text-secondary mb-1 block uppercase">Job Satisfaction (1–4)</span>
                    <SegmentedButtons value={form.JobSatisfaction} onChange={(v) => handleSegmented('JobSatisfaction', v)} options={[1, 2, 3, 4]} />
                  </label>
                  <label className="block">
                    <span className="text-label-md font-label-md text-secondary mb-1 block uppercase">Relationship Satisfaction (1–4)</span>
                    <SegmentedButtons value={form.RelationshipSatisfaction} onChange={(v) => handleSegmented('RelationshipSatisfaction', v)} options={[1, 2, 3, 4]} />
                  </label>
                  <label className="block">
                    <span className="text-label-md font-label-md text-secondary mb-1 block uppercase">Work-Life Balance (1–4)</span>
                    <SegmentedButtons value={form.WorkLifeBalance} onChange={(v) => handleSegmented('WorkLifeBalance', v)} options={[1, 2, 3, 4]} />
                  </label>
                  <label className="block">
                    <span className="text-label-md font-label-md text-secondary mb-1 block uppercase">Job Involvement (1–4)</span>
                    <SegmentedButtons value={form.JobInvolvement} onChange={(v) => handleSegmented('JobInvolvement', v)} options={[1, 2, 3, 4]} />
                  </label>
                </div>
                <div className="space-y-4">
                  <NumberField label="Total Working Years" name="TotalWorkingYears" value={form.TotalWorkingYears} onChange={handleChange} />
                  <NumberField label="Years at Company" name="YearsAtCompany" value={form.YearsAtCompany} onChange={handleChange} />
                  <NumberField label="Years in Current Role" name="YearsInCurrentRole" value={form.YearsInCurrentRole} onChange={handleChange} />
                  <NumberField label="Years Since Last Promotion" name="YearsSinceLastPromotion" value={form.YearsSinceLastPromotion} onChange={handleChange} />
                  <NumberField label="Years with Current Manager" name="YearsWithCurrManager" value={form.YearsWithCurrManager} onChange={handleChange} />
                </div>
              </div>
            </div>

            {/* Row 3: Compensation & Training */}
            <div>
              <h4 className="text-label-md uppercase text-secondary mb-4 border-t border-slate-100 pt-4">Compensation & Training</h4>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                <div className="space-y-4">
                  <NumberField label="Daily Rate ($)" name="DailyRate" value={form.DailyRate} onChange={handleChange} prefix="$" />
                  <NumberField label="Hourly Rate ($)" name="HourlyRate" value={form.HourlyRate} onChange={handleChange} prefix="$" />
                  <NumberField label="Monthly Rate ($)" name="MonthlyRate" value={form.MonthlyRate} onChange={handleChange} prefix="$" />
                  <NumberField label="Percent Salary Hike (%)" name="PercentSalaryHike" value={form.PercentSalaryHike} onChange={handleChange} />
                </div>
                <div className="space-y-4">
                  <NumberField label="Num. Companies Worked" name="NumCompaniesWorked" value={form.NumCompaniesWorked} onChange={handleChange} />
                  <NumberField label="Training Times Last Year" name="TrainingTimesLastYear" value={form.TrainingTimesLastYear} onChange={handleChange} />
                  <label className="block">
                    <span className="text-label-md font-label-md text-secondary mb-1 block uppercase">Performance Rating (1–4)</span>
                    <SegmentedButtons value={form.PerformanceRating} onChange={(v) => handleSegmented('PerformanceRating', v)} options={[1, 2, 3, 4]} />
                  </label>
                </div>
              </div>
            </div>

            {/* Actions */}
            <div className="pt-6 flex items-center justify-end gap-4 border-t border-slate-200">
              <motion.button type="button" whileTap={{ scale: 0.96 }} onClick={handleReset}
                className="px-6 py-2.5 text-body-md font-bold text-secondary hover:bg-slate-100 rounded-lg transition-colors">
                Clear Form
              </motion.button>
              <motion.button type="submit" disabled={loading} whileTap={{ scale: 0.96 }}
                className="px-8 py-2.5 bg-primary text-white text-body-md font-bold rounded-lg shadow-lg shadow-blue-900/20 flex items-center gap-2 hover:opacity-90 transition-all disabled:opacity-60">
                {loading
                  ? <><span className="w-4 h-4 border-2 border-white/40 border-t-white rounded-full animate-spin" />Predicting…</>
                  : <><span className="material-symbols-outlined text-[20px]">bolt</span>Run Prediction</>
                }
              </motion.button>
            </div>

            <AnimatePresence>
              {error && (
                <motion.div initial={{ opacity: 0, y: -8 }} animate={{ opacity: 1, y: 0 }} exit={{ opacity: 0 }}
                  className="mt-2 p-4 bg-red-50 border border-red-200 rounded-lg text-red-700 text-body-md">
                  <span className="material-symbols-outlined text-[16px] mr-2 align-middle">error</span>{error}
                </motion.div>
              )}
            </AnimatePresence>
          </form>
        </section>

        {/* Right panel — 4 cols */}
        <aside className="lg:col-span-4 space-y-6">
          {/* Probability Card */}
          <AnimatePresence mode="wait">
            {result ? (
              <PredictionResultCard key={result.probability} result={result} />
            ) : (
              <motion.div key="empty" className="bg-white border border-slate-200 rounded-xl p-6"
                initial={{ opacity: 0 }} animate={{ opacity: 1 }} exit={{ opacity: 0 }}>
                <p className="text-label-md uppercase text-secondary mb-6">Attrition Probability</p>
                <div className="flex flex-col items-center justify-center py-8 text-slate-400">
                  <motion.span className="material-symbols-outlined text-[48px] mb-2"
                    animate={{ scale: [1, 1.08, 1] }} transition={{ repeat: Infinity, duration: 2.5, ease: 'easeInOut' }}>
                    analytics
                  </motion.span>
                  <p className="text-body-md text-center">Run a prediction to see the probability score here.</p>
                </div>
              </motion.div>
            )}
          </AnimatePresence>

          {/* Top Factors Card */}
          <motion.div className="bg-slate-50 border border-slate-200 rounded-xl p-6"
            initial={{ opacity: 0, y: 12 }} animate={{ opacity: 1, y: 0 }} transition={{ delay: 0.15 }}>
            <div className="flex items-center gap-2 mb-6">
              <span className="material-symbols-outlined text-primary text-[20px]">insights</span>
              <h4 className="text-headline-md font-semibold text-on-surface">Top Risk Factors</h4>
            </div>
            <AnimatePresence mode="wait">
              {result ? (
                <motion.div key="factors-list" className="space-y-3">
                  {factors.map((f, i) => (
                    <motion.div key={f.label} custom={i} variants={fadeUp} initial="hidden" animate="show"
                      className="p-4 bg-white rounded-lg border border-slate-200 flex items-start gap-3">
                      <div className={`w-8 h-8 rounded flex items-center justify-center flex-shrink-0 ${f.color}`}>
                        <span className="material-symbols-outlined text-[18px]">{f.icon}</span>
                      </div>
                      <div>
                        <p className="text-body-md font-bold text-on-surface">{f.label}</p>
                        <p className="text-[12px] text-secondary leading-tight mt-1">{f.desc}</p>
                      </div>
                    </motion.div>
                  ))}
                </motion.div>
              ) : (
                <motion.p key="empty-factors" initial={{ opacity: 0 }} animate={{ opacity: 1 }} exit={{ opacity: 0 }}
                  className="text-body-md text-slate-400 text-center py-4">
                  Factors will appear after prediction.
                </motion.p>
              )}
            </AnimatePresence>
          </motion.div>

          {/* Model Info Card */}
          <div className="bg-white border border-slate-200 rounded-xl p-6">
            <div className="flex items-center gap-2 mb-4">
              <span className="material-symbols-outlined text-primary text-[20px]">model_training</span>
              <h4 className="text-headline-md font-semibold text-on-surface">Active Model</h4>
            </div>
            <dl className="space-y-2 text-body-md mb-5">
              <div className="flex justify-between"><dt className="text-secondary">Algorithm</dt><dd className="font-semibold">Logistic Regression</dd></div>
              <div className="flex justify-between"><dt className="text-secondary">Test AUC</dt><dd className="font-semibold text-blue-700">0.800</dd></div>
              <div className="flex justify-between"><dt className="text-secondary">Test F1</dt><dd className="font-semibold text-blue-700">0.595</dd></div>
              <div className="flex justify-between"><dt className="text-secondary">Recall</dt><dd className="font-semibold">0.611</dd></div>
            </dl>
            {/* Threshold slider */}
            <div className="border-t border-slate-100 pt-4">
              <div className="flex justify-between items-center mb-2">
                <span className="text-label-md uppercase text-secondary">Risk Threshold</span>
                <span className="text-label-md font-bold text-primary tabular-nums">{threshold.toFixed(2)}</span>
              </div>
              <input
                type="range"
                min={0.05} max={0.95} step={0.01}
                value={threshold}
                onChange={(e) => setThreshold(Number(e.target.value))}
                className="w-full h-1.5 bg-slate-200 rounded-full appearance-none cursor-pointer accent-primary"
              />
              <div className="flex justify-between text-[10px] text-slate-400 mt-1">
                <span>0.05 (sensitive)</span>
                <span>0.95 (conservative)</span>
              </div>
              <p className="text-[11px] text-secondary mt-2">Probability ≥ threshold → predicts attrition. Drag to adjust sensitivity.</p>
            </div>
          </div>
        </aside>
      </div>
    </div>
  )
}

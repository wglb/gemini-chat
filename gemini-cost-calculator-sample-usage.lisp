(apply 'print-cost-report (multiple-value-list (gemini-cost-calculator:calculate-cost-from-log *api-log-entry*)))

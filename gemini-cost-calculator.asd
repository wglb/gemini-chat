(asdf:defsystem :gemini-cost-calculator
  :version "0.1.0"
  :author "wgl@ciex-security.com"
  :license :mit
  :description "Calculates the USD cost of Gemini API usage based on token counts."
  :depends-on (:cl-ppcre) ; Used for converting the model string to a keyword
  :components ((:file "gemini-cost-calculator-pkg")
               (:file "gemini-cost-calculator")))

===========
 ChangeLog
===========

* Function ``get-transaction-hook`` was exported from ``cl-prevalence``
  package.
* Now transaction can return multiple values. Previously, it function
  ``execute`` only returned the first value, discarding others.
* Method ``backup`` was fixed for ``guarded-prevalence-system`` class.
* test/demo2.lisp was fixed.
  

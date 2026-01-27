(declare (uses configf struct-indexer))
(use test)
(use srfi-69)

    
(use ini-file)

(let ((ini (read-ini "ini-file_1.ini")))
  (print "ini-file: lu database.file=["
         (lu ini "database.file")"]"))
  
(let ((ini (read-config "configf_1.ini" #f #f)))
  (print "configf: lu database.file=["
         (lu ini "database.file")"]"))

  
(exit 0)



  


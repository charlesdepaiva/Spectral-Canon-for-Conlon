  
;=================== Spectral CANON for COLON Nancarrow =============================

;utils

(defun symmetry (m n)
  (- m (abs (- m n))))

(defun symmetry2 (m n)
  (let ((m (+ m 0.5)))
    (symmetry m n)))

(defun transpose (m)
    (apply #'mapcar #'list m))


;canon

(defun log2-ratio (n)
  (log (/ (1+ n) n)'2))

;(defun log2-ratio-symmetric (m n)
;  (log2-ratio (symmetry2 m n)))

(defun find-k-value (1st-dur n)
  (/ 1st-dur (log2-ratio n)))

(defun point-of-symmetry (number-of-voices ratio)
  (* (1- number-of-voices) ratio))

(defun nth-log2-ratio-symmetric (number-of-voices ratio n)
  (log2-ratio (+ (1- (symmetry2 (point-of-symmetry number-of-voices ratio) n))
		 ratio)))

(defun nth-duration (1st-dur number-of-voices ratio n)
  (* (find-k-value 1st-dur ratio)
     (nth-log2-ratio-symmetric number-of-voices ratio n)))


(defun series-length (number-of-voices ratio)
  (* 2 (* (1- number-of-voices) ratio)))


(defun series-of-durations (1st-dur number-of-voices ratio)
  (loop
     for i from 1 to (series-length number-of-voices ratio)
     collect (nth-duration 1st-dur number-of-voices ratio i)))


(defun starting-time (voice-number 1st-dur ratio)
  (* (find-k-value 1st-dur ratio)
     (log voice-number '2)))

(defun onset-calc (durations voice-number 1st-dur ratio)
  (let ((start (starting-time voice-number 1st-dur ratio)))
    (loop
       for i in durations
       minimize i into z
       sum i into x
       collect (+ (- x z) start) into y
       finally (return y))))


(defun freq-calc (durations voice-number fundamental 1st-partial)
  (loop
     for i in durations
     collect (* fundamental (+ 1st-partial voice-number))))


(defun canon-data (fundamental 1st-partial 1st-dur number-of-voices ratio)
  (let ((durations (series-of-durations 1st-dur number-of-voices ratio)))
    (loop
       for voice-number from 1 upto number-of-voices
       collect (list (onset-calc durations voice-number 1st-dur ratio)
		     (freq-calc durations voice-number fundamental 1st-partial)
		     durations
		     (make-list (length durations) :initial-element voice-number)))))


(defun adjust-canon-data (data)
  (let ((transdata
	 (loop
	    for datum in data
	    append (transpose datum))))
    (sort transdata #'< :key #'car)))


;extended features

(defun stretch-canon-data (data distortion)
  (loop
     for datum in data
     collect (mapcar #'(lambda (x) (expt x distortion)) (subseq datum 0 3))))


;output

(defun save-canon-data (data &optional (filename "spectralcanon"))
  (with-open-file
      (outdata (make-pathname :name filename :type "dat" :defaults *default-pathname-defaults*)
	       :direction :output
	       :if-exists :supersede
	       :if-does-not-exist :create)
    (format outdata "~:{~A ~A ~A ~A ~%~}" (append (list '(onset frequency duration voice-number)) data))
    (namestring outdata)))


(defun save-canon-gpl (datafile &optional (term "aqua"))
  (with-open-file
      (gpl (make-pathname :name (pathname-name datafile) :type "gpl" :defaults *default-pathname-defaults*)
	   :direction :output
	   :if-exists :supersede
	   :if-does-not-exist :create)
    (format gpl "~{~a~}" (list
			  (format nil "set term ~a~%" term)
			  (format nil "unset key~%")
			  (format nil "set title 'The Spectral CANON for CONLON Nancarrow'~%")
			  (format nil "set xlabel 'Onsets (s)'~%")
			  (format nil "set ylabel 'Frequencies (Hz)'~%")
			  (format nil "plot ")
			  (format nil "[] [] ")
			  (format nil "~S " datafile)
			  (format nil "every ::1 ") ;skips the header line.
			  (format nil "with points pointsize 0.25~%")))
    (namestring gpl)))

;;;calling externals --implementation dependent--
    
;; (defun run-gnuplot (gplfile)
;;   (run-program "/opt/local/bin/gnuplot" (list gplfile) :output *standard-output*))

(defun run-gnuplot (gplfile)
  (let ((gnuplot (with-output-to-string (gnuplot-path)
		   (run-program "/usr/bin/which" '("gnuplot") :output gnuplot-path)
		   gnuplot-path)))
    (run-program (string-trim '(#\Newline) gnuplot) (list gplfile) :output *standard-output*)))

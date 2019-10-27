(ql:quickload 'cl-readline)
(ql:quickload 'uiop)

(import 'uiop:split-string)

(setf *random-state*
      (sb-ext:seed-random-state (get-universal-time)))

(defparameter *gojuon*
  #(#(#(a  あ ア) #(i   い イ) #(u   う ウ) #(e  え エ) #(o  お オ))
    #(#(ka か カ) #(ki  き キ) #(ku  く ク) #(ke け ケ) #(ko こ コ))
    #(#(sa さ サ) #(shi し シ) #(su  す ス) #(se せ セ) #(so そ ソ))
    #(#(ta た タ) #(chi ち チ) #(tsu つ ツ) #(te て テ) #(to と ト))
    #(#(na な ナ) #(ni  に ニ) #(nu  ぬ ヌ) #(ne ね ネ) #(no の ノ))
    #(#(ha は ハ) #(hi  ひ ヒ) #(fu  ふ フ) #(he へ ヘ) #(ho ほ ホ))
    #(#(ma ま マ) #(mi  み ミ) #(mu  む ム) #(me め メ) #(mo も モ))
    #(#(ya や ヤ)              #(yu  ゆ ユ)             #(yo よ ヨ))
    #(#(ra ら ラ) #(ri  り リ) #(ru  る ル) #(re れ レ) #(ro ろ ロ))
    #(#(wa わ ワ) #(wi  ゐ ヰ)              #(we ゑ ヱ) #(wo を ヲ))))

(defparameter +red+ (format nil "~c[31m" #\ESC))
(defparameter +reset+ (format nil "~c[0m" #\ESC))

(defmacro with-red ((stream) &body body)
  `(progn (format ,stream "~A" +red+)
          ,@body
          (format ,stream "~A" +reset+)))
;; (progn (format t "foo~%")
;;        (with-red (t) (format t "red~%"))
;;        (format t "bar~%"))

(defun develop-p ()
  (find-package 'swank))

(defun shuffle-list-f (lst)
  (dotimes (i (length lst))
    (rotatef (nth i lst) (nth (random (length lst)) lst)))
  lst)

(defun select-gojuon (count lines)
  (let ((lst '()))
    (dotimes (i lines)
      (let* ((line (aref *gojuon* i)))
        (dotimes (j (length line))
          (push (aref line j) lst))))
    (shuffle-list-f lst)
    (subseq lst 0 count)))
;;(select-gojuon 5 1)

(defun test-read (count lines type)
  (handler-case
      (loop
         (let* ((lst (select-gojuon count lines))
                (chars (mapcar (lambda (a)
                                 (aref a (case type
                                           (hiragana 1)
                                           (katakana 2)
                                           (random (1+ (random 2)))
                                           (otherwise (progn (error "case") 0)))))
                               lst))
                (s (with-output-to-string (stream)
                     (dolist (c chars)
                       (format stream "~A " c))
                     (format stream "~%")
                     (format stream "your answer: "))))
           (let* ((input-string (cl-readline:readline :prompt s))
                  (user-answer (split-string (string-upcase input-string)))
                  (real-answer (mapcar (lambda (a) (aref a 0)) lst)))
             (format t "real answer: ~A~%"
                     (with-output-to-string (stream)
                       (dotimes (i count)
                         (let* ((real (string-downcase
                                       (symbol-name (nth i real-answer))))
                                (user (string-downcase (nth i user-answer))))
                           (if (string= real user)
                               (format stream "~A " real)
                               (with-red (stream)
                                 (format stream "~A " real)))))))))
         (write-line ""))
    (SB-SYS:INTERACTIVE-INTERRUPT () (format t "~%"))))

(defun main-loop ()
  (let* ((last-function #'test-read)
         (count 5)
         (lines 5)
         ;(type 'hiragana)
         (type 'random))
    (cl-readline:prep-terminal nil)
    (handler-case
        (loop (let* ((user-input (string-upcase
                                  (cl-readline:readline :prompt "> ")))
                     (splited-user-input (split-string user-input)))
                (let ((cmd (car splited-user-input)))
                  (cond ((member cmd '("R" "READ") :test #'string-equal)
                         (progn (test-read count lines type)
                                (setf last-function #'test-read)))
                        ((or (null cmd) (string= cmd "QUIT"))
                         (funcall last-function count lines type))
                        ((member cmd '("QUIT" "Q") :test #'string=)
                         (return))
                        (t (format *standard-output*
                                   "wrong command:~A ~%" user-input))))))
      (SB-SYS:INTERACTIVE-INTERRUPT () (progn nil))))
  (cl-readline:deprep-terminal)
  (write-line "")
  (if (not (develop-p)) (sb-ext:exit)))

(format t "========================~%")

(if (not (develop-p))
    (main-loop))

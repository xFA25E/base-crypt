;;;; This program should create crypt message with bases

(defparameter *alpha*
  (concatenate 'string (loop for i from 32 to 126 collect (code-char i)))
  "the dictionary of letters")

(defun create-base-to-decimal-function (alpha)
  "This function create another function which is based
on given alpha. The returning function is a closure that
takes a word as an argument and returns a sum of decimals
that corresponds to a given letters. alpha is a string of
possible letters."
  (let ((base (length alpha)))
    #'(lambda (word)
        (let ((w (reverse word)))
          (apply '+
                 (map 'list #'(lambda (letter index)
                                (* (expt base index)
                                   (position letter alpha)))
                      w
                      (loop for i from 0 to (length w) collect i)))))))

(defun create-decimal-to-base-function (alpha)
  "This function create another function which is based
on given alpha. The returning function is a closure that
takes a decimal number as an argument and returns a string,
which is a number in given base."
  (let ((base (length alpha)) (result nil))
    (labels ((transform (number)
               (print number)
               (let ((the-mod (mod number base)))
                 (push (elt alpha the-mod) result)
                 (if (< number base)
                     (let ((fr (concatenate 'string result)))
                       (setf result nil)
                       fr)
                     (transform (/ (- number the-mod) base))))))
      #'transform)))

(defun initialize-functions ()
  (let ((mode (init-mode))
        (key (init-key)))
    (defparameter input->decimal
      (create-base-to-decimal-function (if (equal mode "c")
                                           *alpha*
                                           (subseq *alpha* 0 key))))
    (defparameter decimal->output
      (create-decimal-to-base-function (if (equal mode "c")
                                           (subseq *alpha* 0 key)
                                           *alpha*)))))

(defun init-mode ()
  (let ((mode (prompt-read "Insert mode. Crypt or decrypt? [c/d]")))
    (if (or (equal mode "c") (equal mode "d"))
        mode
        (progn
          (format *query-io* "Please, insert \"c\" or \"d\".")
          (init-mode)))))

(defun init-key ()
  (let ((key (parse-integer (prompt-read "Insert key (0 < n < 95)") :junk-allowed t)))
    (if (and (numberp key)
             (< 0 key (length *alpha*)))
        key
        (progn
          (format *query-io* "Key must be an integer: 0 < key < ~a"
                  (length *alpha*))
          (init-key)))))

(defun prompt-read (prompt)
  (format *query-io* "~%~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun main ()
  (initialize-functions)
  (let ((message (prompt-read "Input your message")))
    (format *query-io* "Processing...")
    (format *query-io* "~%Your crypt mesage:~%<<~a>>~%"
            (funcall decimal->output (funcall input->decimal message)))))

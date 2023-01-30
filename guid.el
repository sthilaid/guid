
(defun guid-generate-random ()
  (let ((guid (make-string 16 0)))
    (dotimes (i 16)
      (aset guid i (random #xFF)))

    ;; guid version 4 stamping
    (aset guid 7 (logior #x40 (logand #xF (elt guid 7))))
    (aset guid 9 (logior #x80 (logand #x3F (elt guid 9))))

    ;; final formatting
    (let ((stamped-guid (seq-reduce (lambda (x acc) (concat acc x))
                                    (seq-map (lambda (x) (format "%02x" x)) guid)
                                    "")))
      (if (not (= (length stamped-guid) 32)) (error "Invalid GUID format generated"))
      (format "%s-%s-%s-%s-%s" (substring stamped-guid 0 8) (substring stamped-guid 8 12) (substring stamped-guid 12 16) (substring stamped-guid 16 20) (substring stamped-guid 20 32)))))

(defun guid (&optional upcase?)
  "generate a random guid (version 4)"
  (interactive (list (y-or-n-p "generate upcase guid?")))
  (let* ((guid (guid-generate-random))
         (cased-guid (if upcase? (upcase guid) guid)))
    (kill-new cased-guid)
    (message (concat cased-guid " [copied to killring]"))))

(provide 'guid)

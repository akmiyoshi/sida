(require 'eieio)

(defconst *sida-foreground-color* "green")
;(defconst *sida-background-color* "black")
(defconst *sida-background-color* "rgb(0,0,0)")

(defun W1x ($x $y) (+ (* 0.836 $x) (* 0.044 $y)))
(defun W1y ($x $y) (+ (* -0.044 $x) (* 0.836 $y) 0.169))
(defun W2x ($x $y) (+ (* -0.141 $x) (* 0.302 $y)))
(defun W2y ($x $y) (+ (* 0.302 $x) (* 0.141 $y) 0.127))
(defun W3x ($x $y) (- (* 0.141 $x) (* 0.302 $y)))
(defun W3y ($x $y) (+ (* 0.302 $x) (* 0.141 $y) 0.169))
(defun W4x ($x $y) 0)
(defun W4y ($x $y) (* 0.175337 $y))

(defclass <sida> ()
  ((width  :initarg :width)
   (height :initarg :height)
   (bitmap :initarg :bitmap)
   (circle :initarg :circle :initform nil)
   (count  :initarg :count  :initform 0)))

(defmethod initialize-instance :after ((this <sida>) &rest $slots)
  (with-slots (width height bitmap) this
    (setf bitmap (make-bool-vector (* width height) nil))))

(defmethod !f ((this <sida>) $k $x $y)
  (with-slots (width height) this
    (if (> $k 0)
        (progn
          (!f this (1- $k) (W1x $x $y) (W1y $x $y))
          (when (< (random 10) 3) (!f this (1- $k) (W2x $x $y) (W2y $x $y)))
          (when (< (random 10) 3) (!f this (1- $k) (W3x $x $y) (W3y $x $y)))
          (when (< (random 10) 3) (!f this (1- $k) (W4x $x $y) (W4y $x $y))))
      (!plot this
             (+ (* $x width 0.98) (* width 0.5))
             (- height (* $y height 0.98))))))

(defmethod !print-start-tag ((this <sida>) $tag $standalone &rest $rest)
  (when $standalone (princ "  " (current-buffer)))
  (princ "<" (current-buffer))
  (princ $tag (current-buffer))
  (princ " " (current-buffer))
  (cl-loop
   with $key with $val
   for $sep = "" then " "
   for $top on $rest
   by #'cddr
   do
   (setf $key (nth 0 $top))
   (setf $val (nth 1 $top))
   (when (symbolp $key) (setf $key (symbol-name $key)))
   (when (string-match-p "^:" $key) (setf $key (substring $key 1)))
   (princ $sep (current-buffer))
   (princ $key (current-buffer))
   (princ "=\"" (current-buffer))
   (princ $val (current-buffer))
   (princ "\"" (current-buffer)))
  (if $standalone
      (princ " />" (current-buffer))
    (princ " >" (current-buffer)))
  (princ "\n" (current-buffer)))

(defmethod !print-end-tag ((this <sida>) $tag)
  (princ "</" (current-buffer))
  (princ $tag (current-buffer))
  (princ ">\n" (current-buffer)))


(defmethod !plot ((this <sida>) $x $y)
  (with-slots (width height bitmap circle count) this
    (let (($x (truncate $x))
          ($y (truncate $y)))
      (cond
       ((< $x 0) nil)
       ((>= $x width) nil)
       ((< $y 0) nil)
       ((>= $y height) nil)
       (t (let (($index (+ (* width $y) $x)))
            (when (and (>= $index 0) (< $index (length bitmap)))
              (unless (aref bitmap $index)
                (aset bitmap $index t)
                (if circle
                    (!print-start-tag
                     this "circle" t
                     :cx $x :cy $y :r 0.5
                     :style (format "fill:%s" *sida-foreground-color*))
                  (!print-start-tag
                   this "rect" t :x $x :y $y :width 1.0 :height 1.0
                   :style (format "fill:%s" *sida-foreground-color*)))
                (setf count (1+ count))
                (when (zerop (% count 1000)) (sit-for 0))
                ))))))))

(defun sida ($arg)
  (interactive "P")
  (let (($sida (make-instance <sida> :width 500 :height 500)))
    (with-slots (width height circle) $sida
      (setf circle $arg)
      (ignore-errors (kill-buffer "sida.svg"))
      (switch-to-buffer "sida.svg")
      (!print-start-tag $sida "svg" nil
                        :xmlns "http://www.w3.org/2000/svg"
                        :version "1.1"
                        :width width
                        :height height)
      (!print-start-tag $sida "rect" t
                        :x 0
                        :y 0
                        :width width
                        :height height
                        :style (format "fill:%s" *sida-background-color*))
      (sit-for 1.5)
      (!f $sida 20 0 0)
      (!print-end-tag $sida "svg")
      (sit-for 1.5)
      (write-file "~/sida.svg")
      (image-mode))))

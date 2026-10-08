;;;; Playing with "dynamic" SVG rendering
;;;; Note: The SVG is really just a monsterious list
;;;; Each "shape" to be drawn is consed on via strings and lists
;;;; Generally too slow to be truly used as some "engine"
;;;; Ryan Burnside 2026

(require 'svg)
(require 'cl-lib)

(defconst dodecahedron-palette
  ["#DAA520"
   "#568203"
   "#CC5500"
   "#996515"
   "#E1AD01"
   "#5C4033"
   "#882D17"
   "#4A5D4E"
   "#B7410E"
   "#005F73"
   "#CD7F32"
   "#BA797D"
   "#CC7722"
   "#6B8E23"
   "#7B3F00"
   "#9A5B3B"
   "#F4C430"
   "#87A96B"
   "#E2725B"
   "#008080"])

(defvar dodecahedron-timer nil
  "Timer object for driving the rotation animation.")

(defvar dodecahedron-angles (list 0.0 0.0 0.0)
  "Current rotation angles (X Y Z) in radians.")

;;; 1. Geometry Setup

(defconst dodecahedron-vertices
  '([-0.57735 -0.57735 -0.57735]  ; 0
    [-0.57735 -0.57735  0.57735]  ; 1
    [-0.57735  0.57735 -0.57735]  ; 2
    [-0.57735  0.57735  0.57735]  ; 3
    [ 0.57735 -0.57735 -0.57735]  ; 4
    [ 0.57735 -0.57735  0.57735]  ; 5
    [ 0.57735  0.57735 -0.57735]  ; 6
    [ 0.57735  0.57735  0.57735]  ; 7
    [ 0.0     -0.35682 -0.93417]  ; 8
    [ 0.0     -0.35682  0.93417]  ; 9
    [ 0.0      0.35682 -0.93417]  ; 10
    [ 0.0      0.35682  0.93417]  ; 11
    [-0.35682 -0.93417  0.0    ]  ; 12
    [-0.35682  0.93417  0.0    ]  ; 13
    [ 0.35682 -0.93417  0.0    ]  ; 14
    [ 0.35682  0.93417  0.0    ]  ; 15
    [-0.93417  0.0     -0.35682]  ; 16
    [-0.93417  0.0      0.35682]  ; 17
    [ 0.93417  0.0     -0.35682]  ; 18
    [ 0.93417  0.0      0.35682]) ; 19
  "Normalized unit-sphere vertices for a regular dodecahedron.")

(defconst dodecahedron-edges
  '((0 . 8)  (0 . 12) (0 . 16)
    (1 . 9)  (1 . 12) (1 . 17)
    (2 . 10) (2 . 13) (2 . 16)
    (3 . 11) (3 . 13) (3 . 17)
    (4 . 8)  (4 . 14) (4 . 18)
    (5 . 9)  (5 . 14) (5 . 19)
    (6 . 10) (6 . 15) (6 . 18)
    (7 . 11) (7 . 15) (7 . 19)
    (8 . 10) (9 . 11) (12 . 14)
    (13 . 15) (16 . 17) (18 . 19)
    (8 . 0)  (8 . 4)  (9 . 1)   (9 . 5)
    (10 . 2) (10 . 6) (11 . 3)  (11 . 7)
    (12 . 0) (12 . 1) (13 . 2)  (13 . 3)
    (14 . 4) (14 . 5) (15 . 6)  (15 . 7))
  "Direct index pairs defining all 30 unique edges.")

;;; 2. Math & 3D Projection

(defun dodecahedron--rotate (point rx ry rz)
  "Rotate 3D POINT (x y z) by RX, RY, RZ angles."
  (let* ((x (elt point 0))
         (y (elt point 1))
         (z (elt point 2))
         ;; X axis rotation
         (y1 (- (* y (cos rx)) (* z (sin rx))))
         (z1 (+ (* y (sin rx)) (* z (cos rx))))
         ;; Y axis rotation
         (x2 (+ (* x (cos ry)) (* z1 (sin ry))))
         (z2 (- (* z1 (cos ry)) (* x (sin ry))))
         ;; Z axis rotation
         (x3 (- (* x2 (cos rz)) (* y1 (sin rz))))
         (y3 (+ (* x2 (sin rz)) (* y1 (cos rz)))))
    (list x3 y3 z2)))

(defun dodecahedron--project (point width height scale)
  "Project 3D POINT into 2D canvas coordinates (X . Y)."
  (let ((x (nth 0 point))
        (y (nth 1 point))
        (z (nth 2 point))
        (fov 4.0))
    (cons (+ (/ width 2.0) (* (/ (* x fov) (+ z fov)) scale))
          (+ (/ height 2.0) (* (/ (* y fov) (+ z fov)) scale)))))

;;; 3. SVG Destruction & Rendering

(defun dodecahedron-clear (svg)
  "Destructively remove all line elements from SVG."
  ;; Remove top-level line elements directly from the DOM list
  (setcdr (cdr svg)
          (cl-remove-if (lambda (child)
                          (and (listp child)
                               (eq (car child) 'line)))
                        (cddr svg))))

(defun dodecahedron-render (svg rx ry rz)
  "Render projected dodecahedron lines into SVG structure."
  (dodecahedron-clear svg)
  (let* ((scale 120.0)
         (transformed
          (mapcar (lambda (v)
                    (dodecahedron--project
                     (dodecahedron--rotate v rx ry rz) 300 300 scale))
                  dodecahedron-vertices)))
    (cl-loop  for edge in dodecahedron-edges
              for i from 0
              for p1 = (nth (car edge) transformed)
              for p2 =  (nth (cdr edge) transformed)
              do
              (svg-line svg
                        (car p1) (cdr p1)
                        (car p2) (cdr p2)
                        :stroke (elt dodecahedron-palette (mod i 20))
                        :stroke-width 4))))

;;; 4. Animation Control & Execution

(defun dodecahedron-stop ()
  "Stop the active rotation timer if running."
  (interactive)
  (when (timerp dodecahedron-timer)
    (cancel-timer dodecahedron-timer)
    (setq dodecahedron-timer nil)))

(defun dodecahedron-start ()
  "Initialize SVG image, insert at point, and start rotation timer loop."
  (interactive)
  (dodecahedron-stop)
  (let ((dodecahedron-viewer (svg-create 300 300 :stroke "currentColor")))
    (svg-insert-image dodecahedron-viewer)

    (setq dodecahedron-angles (list 0.0 0.0 0.0))
    (setq dodecahedron-timer
          (run-with-timer
           0 0.10
           (lambda (svg)
             (setq dodecahedron-angles
                   (list (+ (nth 0 dodecahedron-angles) 0.05)
                         (+ (nth 1 dodecahedron-angles) 0.07)
                         (+ (nth 2 dodecahedron-angles) 0.03)))
             (apply #'dodecahedron-render svg dodecahedron-angles))
           dodecahedron-viewer))))

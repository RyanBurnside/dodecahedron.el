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

(defvar dodecahedron-angles (vector 0.0 0.0 0.0)
  "Current rotation angles (X Y Z) in radians.")

;;; 1. Geometry Setup

(defconst dodecahedron-vertices
  '[[-0.57735 -0.57735 -0.57735]  ; 0
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
    [ 0.93417  0.0      0.35682]] ; 19
  "Normalized unit-sphere vertices for a regular dodecahedron.")

(defconst dodecahedron-edges
  '((0 . 8) (0 . 12) (0 . 16) (1 . 9) (1 . 12)
    (1 . 17) (2 . 10) (2 . 13) (2 . 16) (3 . 11)
    (3 . 13) (3 . 17) (4 . 8) (4 . 14) (4 . 18)
    (5 . 9) (5 . 14) (5 . 19) (6 . 10) (6 . 15)
    (6 . 18) (7 . 11) (7 . 15) (7 . 19) (8 . 10)
    (9 . 11) (12 . 14) (13 . 15) (16 . 17) (18 . 19)))


(defvar transformed (make-vector (length dodecahedron-vertices) nil)
  "A final buffer for transformed verticies (into 2D cons cells")

;;; 2. Math & 3D Projection

(defun dodecahedron--rotate (point rx ry rz)
  "Rotate 3D POINT (x y z) by RX, RY, RZ angles."
  (let* ((x (aref point 0))
         (y (aref point 1))
         (z (aref point 2))
         ;; X axis rotation
         (y1 (- (* y (cos rx)) (* z (sin rx))))
         (z1 (+ (* y (sin rx)) (* z (cos rx))))
         ;; Y axis rotation
         (x2 (+ (* x (cos ry)) (* z1 (sin ry))))
         (z2 (- (* z1 (cos ry)) (* x (sin ry))))
         ;; Z axis rotation
         (x3 (- (* x2 (cos rz)) (* y1 (sin rz))))
         (y3 (+ (* x2 (sin rz)) (* y1 (cos rz)))))
    (vector x3 y3 z2)))

(defun dodecahedron--project (point width height scale)
  "Project 3D POINT into 2D canvas coordinates (X . Y)."
  (let ((x (aref point 0))
        (y (aref point 1))
        (z (aref point 2))
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
  (cl-loop with scale = 120.0
           for i from 0 
           for v across dodecahedron-vertices
           do (aset transformed i
                    (dodecahedron--project (dodecahedron--rotate v rx ry rz)
                                           300 300 scale)))
  (cl-loop  for edge in dodecahedron-edges
            for i from 0
            for p1 = (aref transformed (car edge))
            for p2 = (aref transformed (cdr edge))
            do (svg-line svg
                         (car p1) (cdr p1)
                         (car p2) (cdr p2)
                         :stroke (aref dodecahedron-palette (mod i 20))
                         :stroke-width 2)))

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
    (setq dodecahedron-angles (vector 0.0 0.0 0.0))
    (setq dodecahedron-timer
          (run-with-timer
           0 0.1
           (lambda (svg)
             (cl-incf (aref dodecahedron-angles 0) 0.05)
             (cl-incf (aref dodecahedron-angles 1) 0.07)
             (cl-incf (aref dodecahedron-angles 2) 0.03)
             (dodecahedron-render svg
                                  (aref dodecahedron-angles 0)
                                  (aref dodecahedron-angles 1)
                                  (aref dodecahedron-angles 2)))
           dodecahedron-viewer))))



(defun my-gaussian-random ()
  "Generate a random float from a standard Normal distribution N(0, 1)
using the Box-Muller transform."
  (let ((u1 (cl-random 1.0))
        (u2 (cl-random 1.0)))
    ;; Prevent log(0)
    (while (= u1 0.0)
      (setq u1 (cl-random 1.0)))
    (* (sqrt (* -2.0 (log u1)))
       (cos (* 2.0 float-pi u2)))))

(defun generate-sphere-sample-points (n &optional radius)
  "Generate N uniformly distributed 3D sample points on a sphere.
Each point is returned as a 3-element vector [X Y Z].
If RADIUS is non-nil, scale the sphere to RADIUS (defaults to 1.0)."
  (let ((r (or radius 1.0))
        (points (make-vector n nil)))
    (dotimes (i n points)
      (let* ((x (my-gaussian-random))
             (y (my-gaussian-random))
             (z (my-gaussian-random))
             ;; Calculate 3D magnitude
             (len (sqrt (+ (* x x) (* y y) (* z z)))))
        ;; Normalize vector and scale by radius
        (aset points i
              (vector (* r (/ x len))
                      (* r (/ y len))
                      (* r (/ z len))))))))

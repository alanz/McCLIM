;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998-2002 Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;  (c) copyright 1998-2000 Michael McDonald <mikemac@mikemac.com>
;;;  (c) copyright 2019 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; This file contains implementation of the region predicate protocol
;;; for several region classes. A few rules to follow:
;;;
;;; - use coordinate functions for comparisons
;;; - when possible, provide a method for a protocol class, and when
;;;   beneficial, also provide a method for standard class
;;; - all "region" parameters must be specialized
;;; - the most general specialization is BOUNDING-RECTANGLE
;;;
;;; Regions which must be handled:
;;;
;;;   - 0 dimensions: point
;;;   - 1 dimensions: polyline, elliptical-arc
;;;   - 2 dimensions: polygon, ellipse
;;;
;;; Additionally a line and a rectangle should be handled (they are
;;; frequently used as special cases of a polyline and a polygon).
;;;
;;; Methods should be defined from the most general to the most
;;; specific. Methods specialized to REGION, methods being a
;;; consequence of the dimensionality rule and finally methods
;;; specific to a particular region type. Unbounded regions and region
;;; sets have their methods defined elsewhere.

(in-package #:climi)

;;;   REGION-INTERSECTS-REGION-P region1 region2
;;;
;;;        Returns nil if region-intersection of the two regions region1 and
;;;        region2 would be +nowhere+; otherwise, it returns t.
;;;
;;;        aka region1 and region2 are not disjoint, aka A ∩ B ≠ ∅
;;;

;;; "generic" version
(defmethod region-intersects-region-p ((a bounding-rectangle) (b bounding-rectangle))
  (not (region-equal +nowhere+ (region-intersection a b))))

(defmethod region-intersects-region-p :around ((a bounding-rectangle) (b bounding-rectangle))
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* a)
    (multiple-value-bind (u1 v1 u2 v2) (bounding-rectangle* b)
      (cond ((and (<= u1 x2) (<= x1 u2)
                  (<= v1 y2) (<= y1 v2))
             (call-next-method))
            (t
             nil)))))

(defmethod region-intersects-region-p ((a standard-rectangle) (b standard-rectangle))
  (declare (ignorable a b))
  ;; for rectangles, the bounding rectangle test is correct, so if we
  ;; wind up here, we can just return T.
  t)

;;;   REGION-CONTAINS-REGION-P region1 region2
;;;
;;;        Returns T if all points in the region REGION1 are members of
;;;        the region REGION2; otherwise, it returns NIL.
;;;
;;;        aka region2 is a subset of region1, aka B\A = ∅
;;;

;;; "generic" version
(defmethod region-contains-region-p ((a bounding-rectangle) (b bounding-rectangle))
  (or (eq a b)
      (region-equal +nowhere+ (region-difference b a))))

(defmethod region-contains-region-p ((a standard-ellipse) (b standard-ellipse))
  (multiple-value-bind (bcx bcy) (ellipse-center-point* b)
    (and (region-contains-position-p a bcx bcy)
         (null (intersection-ellipse/ellipse a b))
         (or (null (ellipse-start-angle a))
             (multiple-value-bind (sx sy) (%ellipse-angle->position a (ellipse-start-angle a))
               (multiple-value-bind (ex ey) (%ellipse-angle->position a (ellipse-end-angle a))
                 (multiple-value-bind (cx cy) (ellipse-center-point* a)
                   (and (null (region-intersection b (make-line* sx sy cx cy)))
                        (null (region-intersection b (make-line* ex ey cx cy)))))))))))

;;; Ellipse is a convex object. That Implies that if each of the rectangle
;;; vertexes lies inside it, then whole rectangle fits as well. We take a
;;; special care for ellipses with start/end angle.
(defmethod region-contains-region-p ((a standard-ellipse) (b standard-rectangle))
  (with-standard-rectangle* (x1 y1 x2 y2) b
    (if (null (ellipse-start-angle a))
        (and (region-contains-position-p a x1 y1)
             (region-contains-position-p a x2 y1)
             (region-contains-position-p a x1 y2)
             (region-contains-position-p a x2 y2))
        (flet ((fits (l) (region-equal l (region-intersection l a))))
          (and (fits (make-line* x1 y1 x2 y1))
               (fits (make-line* x2 y1 x2 y2))
               (fits (make-line* x2 y2 x1 y2))
               (fits (make-line* x1 y2 x1 y1)))))))

(defmethod region-contains-region-p ((a standard-ellipse) (polygon standard-polygon))
  (if (null (ellipse-start-angle a))
      (map-over-polygon-coordinates
       #'(lambda (x y)
           (unless (region-contains-position-p a x y)
             (return-from region-contains-region-p nil)))
       polygon)
      (map-over-polygon-segments
       #'(lambda (x1 y1 x2 y2
                  &aux (line (make-line* x1 y1 x2 y2)))
           (unless (region-equal line (region-intersection line a))
             (return-from region-contains-region-p nil)))
       polygon))
  T)

(defmethod region-contains-region-p ((a bounding-rectangle) (b point))
  (region-contains-position-p a (point-x b) (point-y b)))

;;; "generic" version
(defmethod region-equal ((a bounding-rectangle) (b bounding-rectangle))
  (region-equal +nowhere+ (region-exclusive-or a b)))

;;; Dimensionality rule

(defmethod region-equal ((a point) (b path))  nil)
(defmethod region-equal ((a point) (b area))  nil)
(defmethod region-equal ((a path)  (b point)) nil)
(defmethod region-equal ((a path)  (b area))  nil)
(defmethod region-equal ((a area)  (b point)) nil)
(defmethod region-equal ((a area)  (b path))  nil)

(defmethod region-equal ((a point) (b point))
  (and (coordinate= (point-x a) (point-x b))
       (coordinate= (point-y a) (point-y b))))

(defmethod region-equal ((xs standard-rectangle-set) (ys standard-rectangle-set))
  ;; Our bands representation is canonic
  (equal (standard-rectangle-set-bands xs)
         (standard-rectangle-set-bands ys)))

(defmethod region-equal ((a standard-rectangle) (b standard-rectangle))
  (multiple-value-bind (x1 y1 x2 y2) (rectangle-edges* a)
    (multiple-value-bind (u1 v1 u2 v2) (rectangle-edges* b)
      (and (coordinate= x1 u1)
           (coordinate= y1 v1)
           (coordinate= x2 u2)
           (coordinate= y2 v2)))))

(defmethod region-equal ((a standard-rectangle) (b path)) nil)
(defmethod region-equal ((a path) (b standard-rectangle)) nil)

(defmethod region-equal ((a standard-line) (b standard-line))
  (or (and (region-equal (line-start-point a) (line-start-point b))
           (region-equal (line-end-point a) (line-end-point b)))
      (and (region-equal (line-start-point a) (line-end-point b))
           (region-equal (line-end-point a) (line-start-point b)))))
